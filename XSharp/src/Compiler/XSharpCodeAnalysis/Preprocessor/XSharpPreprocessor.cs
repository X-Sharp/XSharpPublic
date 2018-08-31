/*
   Copyright 2016-2017 XSharp B.V.

Licensed under the X# compiler source code License, Version 1.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.xsharp.info/licenses

Unless required by applicable law or agreed to in writing, software
Distributed under the License is distributed on an "as is" basis,
without warranties or conditions of any kind, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/
#define UDCSUPPORT
using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;
using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using System.Diagnostics;
using System.Collections.Immutable;
using System.Collections.Concurrent;

namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
    internal class XSharpPreprocessor
    {
        const string PPOPrefix = "//PP ";

        #region IncludeCache
        internal class PPIncludeFile
        {
            static ConcurrentDictionary<String, PPIncludeFile> cache = new ConcurrentDictionary<string, PPIncludeFile>(StringComparer.OrdinalIgnoreCase);

            static internal void ClearOldIncludes()
            {
                // Remove old includes that have not been used in the last 1 minutes
                var oldkeys = new List<string>();
                var compare = DateTime.Now.Subtract(new TimeSpan(0, 1, 0));
                foreach (var include in cache.Values)
                {
                    if (include.LastUsed < compare)
                    {
                        oldkeys.Add(include.FileName);
                    }
                }
                foreach (var key in oldkeys)
                {
                    PPIncludeFile oldFile;
                    cache.TryRemove(key, out oldFile);
                }
            }


            static internal PPIncludeFile Get(string fileName)
            {
                PPIncludeFile file = null;
                if (cache.ContainsKey(fileName))
                {
                    if (cache.TryGetValue(fileName, out file))
                    {
                        if (file.LastWritten != FileUtilities.GetFileTimeStamp(fileName))
                        {
                            cache.TryRemove(fileName, out file);
                            file = null;
                        }
                        else
                        {
                            file.LastUsed = DateTime.Now;
                            // Now clone the file so the tokens may be manipulated
                            file = file.Clone();
                        }
                    }
                }
                return file;
            }
            static internal PPIncludeFile Add(string fileName, IList<IToken> tokens, SourceText text, bool mustBeProcessed, ref bool newFile)
            {
                PPIncludeFile file;
                cache.TryGetValue(fileName, out file);
                if (file == null)
                {
                    newFile = true;
                    file = new PPIncludeFile(fileName, tokens, text, mustBeProcessed);
                    if (!cache.TryAdd(fileName, file))
                    {
                        PPIncludeFile oldFile;
                        if (cache.TryGetValue(fileName, out oldFile))
                        {
                            file = oldFile;
                            newFile = false;
                        }
                    }
                }
                else
                {
                    newFile = false;
                }
                return file;
            }
            internal DateTime LastWritten { get; private set; }
            internal string FileName { get; private set; }
            internal ImmutableArray<IToken> Tokens { get; private set; }
            internal SourceText Text { get; private set; }
            internal DateTime LastUsed { get; set; }
            internal bool MustBeProcessed { get; set; }

            internal PPIncludeFile(string name, IList<IToken> tokens, SourceText text, bool mustBeProcessed)
            {
                this.FileName = name;
                this.Text = text;
                this.LastUsed = DateTime.Now;
                this.LastWritten = FileUtilities.GetFileTimeStamp(this.FileName);
                this.Tokens = tokens.ToImmutableArray();
                this.MustBeProcessed = mustBeProcessed;

            }
            internal PPIncludeFile Clone()
            {
                return new PPIncludeFile(FileName, Tokens, Text, MustBeProcessed); ;
            }

        }

        #endregion

        class InputState
        {
            internal ITokenStream Tokens;
            internal int Index;
            internal string SourceFileName;
            internal string MappedFileName;
            internal int MappedLineDiff;
            internal bool isSymbol;
            internal string SymbolName;
            internal XSharpToken Symbol;
            internal InputState parent;
            internal PPRule udc;
            internal InputState(ITokenStream tokens)
            {
                Tokens = tokens;
                Index = 0;
                MappedLineDiff = 0;
                SourceFileName = null;
                parent = null;
                isSymbol = false;
            }

            internal int La()
            {
                if (Eof() && parent != null)
                    return parent.La();
                return Tokens.Get(Index).Type;
            }

            internal XSharpToken Lt()
            {
                if (Eof() && parent != null)
                    return parent.Lt();
                return (XSharpToken)Tokens.Get(Index);
            }

            internal bool Eof()
            {
                return Index >= Tokens.Size || Tokens.Get(Index).Type == IntStreamConstants.Eof;
            }

            internal bool Consume()
            {
                if (Eof())
                    return false;
                Index++;
                return true;
            }
        }

        XSharpLexer _lexer;
        ITokenStream _lexerStream;
        CSharpParseOptions _options;

        Encoding _encoding;

        SourceHashAlgorithm _checksumAlgorithm;

        IList<ParseErrorData> _parseErrors;
        bool _duplicateFile = false;

        IList<string> includeDirs;

        Dictionary<string, IList<XSharpToken>> symbolDefines;

        Dictionary<string, Func<XSharpToken>> macroDefines = new Dictionary<string, Func<XSharpToken>>(CaseInsensitiveComparison.Comparer);

        Stack<bool> defStates = new Stack<bool>();
        Stack<XSharpToken> regions = new Stack<XSharpToken>();
        string _fileName = null;
        InputState inputs;
        IToken lastToken = null;

        PPRuleDictionary cmdRules = new PPRuleDictionary();
        PPRuleDictionary transRules = new PPRuleDictionary();
        bool _hasCommandrules = false;
        bool _hasTransrules = false;
        int rulesApplied = 0;
        int defsApplied = 0;
        HashSet<string> activeSymbols = new HashSet<string>(/*CaseInsensitiveComparison.Comparer*/);

        bool _preprocessorOutput = false;
        Stream _ppoStream;

        internal Dictionary<string, SourceText> IncludedFiles = new Dictionary<string, SourceText>(CaseInsensitiveComparison.Comparer);

        public int MaxIncludeDepth { get; set; } = 16;

        public int MaxSymbolDepth { get; set; } = 16;

        public int MaxUDCDepth { get; set; } = 256;

        public string StdDefs { get; set; } = string.Empty;
        private void initStdDefines(CSharpParseOptions options, string fileName)
        {
            // Note Macros such as __ENTITY__ and  __SIG__ are handled in the transformation phase
            macroDefines.Add("__ARRAYBASE__", () => new XSharpToken(XSharpLexer.INT_CONST, _options.ArrayZero ? "0" : "1"));
            if (_options.ClrVersion == 2)
                macroDefines.Add("__CLR2__", () => new XSharpToken(XSharpLexer.TRUE_CONST));
            if (_options.ClrVersion == 4)
                macroDefines.Add("__CLR4__", () => new XSharpToken(XSharpLexer.TRUE_CONST));
            macroDefines.Add("__CLRVERSION__", () => new XSharpToken(XSharpLexer.STRING_CONST, "\"" + _options.ClrVersion.ToString() + ".0\""));
            macroDefines.Add("__DATE__", () => new XSharpToken(XSharpLexer.STRING_CONST, '"' + DateTime.Now.Date.ToString("yyyyMMdd") + '"'));
            macroDefines.Add("__DATETIME__", () => new XSharpToken(XSharpLexer.STRING_CONST, '"' + DateTime.Now.ToString() + '"'));
            if (_options.DebugEnabled)
                macroDefines.Add("__DEBUG__", () => new XSharpToken(XSharpLexer.TRUE_CONST));
            macroDefines.Add("__DIALECT__", () => new XSharpToken(XSharpLexer.STRING_CONST, '"' + options.Dialect.ToString() + '"'));
            switch (_options.Dialect)
            {
                case XSharpDialect.Core:
                    macroDefines.Add("__DIALECT_CORE__", () => new XSharpToken(XSharpLexer.TRUE_CONST));
                    break;
                case XSharpDialect.VO:
                    macroDefines.Add("__DIALECT_VO__", () => new XSharpToken(XSharpLexer.TRUE_CONST));
                    break;
                case XSharpDialect.Vulcan:
                    macroDefines.Add("__DIALECT_VULCAN__", () => new XSharpToken(XSharpLexer.TRUE_CONST));
                    break;
                case XSharpDialect.Harbour:
                    macroDefines.Add("__DIALECT_HARBOUR__", () => new XSharpToken(XSharpLexer.TRUE_CONST));
                    break;
                default:
                    break;
            }
            macroDefines.Add("__ENTITY__", () => new XSharpToken(XSharpLexer.STRING_CONST, "\"__ENTITY__\""));  // Handled later in Transformation phase
            macroDefines.Add("__FILE__", () => new XSharpToken(XSharpLexer.STRING_CONST, '"' + (inputs.SourceFileName ?? fileName) + '"'));
            macroDefines.Add("__LINE__", () => new XSharpToken(XSharpLexer.INT_CONST, inputs.Lt().Line.ToString()));
            macroDefines.Add("__MODULE__", () => new XSharpToken(XSharpLexer.STRING_CONST, '"' + (inputs.SourceFileName ?? fileName) + '"'));
            macroDefines.Add("__FUNCTIONS__", () => new XSharpToken(XSharpLexer.STRING_CONST, "\"__FUNCTIONS__\"")); // Handled later in Transformation phase
            macroDefines.Add("__SIG__", () => new XSharpToken(XSharpLexer.STRING_CONST, "\"__SIG__\"")); // Handled later in Transformation phase
            macroDefines.Add("__SRCLOC__", () => new XSharpToken(XSharpLexer.STRING_CONST, '"' + (inputs.SourceFileName ?? fileName) + " line " + inputs.Lt().Line.ToString() + '"'));
            macroDefines.Add("__SYSDIR__", () => new XSharpToken(XSharpLexer.STRING_CONST, '"' + options.SystemDir + '"'));
            macroDefines.Add("__TIME__", () => new XSharpToken(XSharpLexer.STRING_CONST, '"' + DateTime.Now.ToString("HH:mm:ss") + '"'));
            macroDefines.Add("__UTCTIME__", () => new XSharpToken(XSharpLexer.STRING_CONST, '"' + DateTime.Now.ToUniversalTime().ToString("HH:mm:ss") + '"'));
            macroDefines.Add("__VERSION__", () => new XSharpToken(XSharpLexer.STRING_CONST, '"' + global::XSharp.Constants.Version + '"'));
            macroDefines.Add("__WINDIR__", () => new XSharpToken(XSharpLexer.STRING_CONST, '"' + options.WindowsDir + '"'));
            macroDefines.Add("__WINDRIVE__", () => new XSharpToken(XSharpLexer.STRING_CONST, '"' + options.WindowsDir?.Substring(0, 2) + '"'));
            macroDefines.Add("__XSHARP__", () => new XSharpToken(XSharpLexer.TRUE_CONST));

            bool[] flags = { options.vo1,  options.vo2, options.vo3, options.vo4, options.vo5, options.vo6, options.vo7, options.vo8,
                                options.vo9, options.vo10, options.vo11, options.vo12, options.vo13, options.vo14, options.vo15, options.vo16 };
            for (int iOpt = 0; iOpt < flags.Length; iOpt++)
            {
                string flagName = String.Format("__VO{0}__", iOpt + 1);
                if (flags[iOpt])
                    macroDefines.Add(flagName, () => new XSharpToken(XSharpLexer.TRUE_CONST));
                else
                    macroDefines.Add(flagName, () => new XSharpToken(XSharpLexer.FALSE_CONST));
            }
            if (!options.NoStdDef && options.Kind != SourceCodeKind.Script)
            {
                // Todo: when the compiler option nostddefs is not set: read XSharpDefs.xh from the XSharp Include folder,//
                // and automatically include it.
                // read XsharpDefs.xh
                StdDefs = "xSharpDefs.xh";
                ProcessIncludeFile(StdDefs, null);
            }
        }


        internal void DumpStats()
        {
            DebugOutput("Preprocessor statistics");
            DebugOutput("-----------------------");
            DebugOutput("# of #defines    : {0}", this.symbolDefines.Count);
            DebugOutput("# of #translates : {0}", this.transRules.Count);
            DebugOutput("# of #commands   : {0}", this.cmdRules.Count);
            DebugOutput("# of macros      : {0}", this.macroDefines.Count);
            DebugOutput("# of defines used: {0}", this.defsApplied);
            DebugOutput("# of UDCs used   : {0}", this.rulesApplied);
        }

        private void _writeToPPO(String text, bool addCRLF )
        {
            // do not call t.Text when not needed.
            if (_preprocessorOutput)
            {
                var buffer = _encoding.GetBytes(text);
                _ppoStream.Write(buffer, 0, buffer.Length);
                if (addCRLF)
                {
                    buffer = _encoding.GetBytes("\r\n");
                    _ppoStream.Write(buffer, 0, buffer.Length);
                }
            }
        }

        private bool mustWriteToPPO()
        {
            return _preprocessorOutput && _ppoStream != null && inputs.parent == null;
        }

        internal void writeToPPO(string text, bool addCRLF = true)
        {
            if (mustWriteToPPO())
            {
                _writeToPPO(text, addCRLF);
            }
        }
        private void writeToPPO(IList<XSharpToken> tokens, bool prefix = false, bool prefixNewLines = false)
        {
            if (mustWriteToPPO())
            {
                if (tokens?.Count == 0)
                {
                    _writeToPPO("", true);
                    return;
                }
                // We cannot use the interval and fetch the text from the source stream,
                // because some tokens may come out of an include file or otherwise
                // so concatenate text on the fly
                var bld = new System.Text.StringBuilder(1024);
                if (prefix)
                {
                    bld.Append(PPOPrefix);
                }
                foreach (var t in tokens)
                {
                    bld.Append(t.Text);
                }
                if (prefixNewLines)
                {
                    bld.Replace("\n", "\n" + PPOPrefix);
                }
                _writeToPPO(bld.ToString(),true);
            }
        }

        internal void Close()
        {
            if (_ppoStream != null)
            {
                _ppoStream.Flush();
                _ppoStream.Dispose();
            }
            _ppoStream = null;
        }

        internal XSharpPreprocessor(XSharpLexer lexer, ITokenStream lexerStream, CSharpParseOptions options, string fileName, Encoding encoding, SourceHashAlgorithm checksumAlgorithm, IList<ParseErrorData> parseErrors)
        {
            PPIncludeFile.ClearOldIncludes();
            _lexer = lexer;
            _lexerStream = lexerStream;
            _options = options;
            _fileName = fileName;
            if (_options.VOPreprocessorBehaviour)
                symbolDefines = new Dictionary<string, IList<XSharpToken>>(CaseInsensitiveComparison.Comparer);
            else
                symbolDefines = new Dictionary<string, IList<XSharpToken>>(/* case sensitive */);
            _encoding = encoding;
            _checksumAlgorithm = checksumAlgorithm;
            _parseErrors = parseErrors;
            includeDirs = new List<string>(options.IncludePaths);
            if (!String.IsNullOrEmpty(fileName) && File.Exists(fileName))
            {
                includeDirs.Add(Path.GetDirectoryName(fileName));
                var ppoFile = FileNameUtilities.ChangeExtension(fileName, ".ppo");
                try
                {
                    _ppoStream = null;
                    _preprocessorOutput = _options.PreprocessorOutput;
                    if (FileNameUtilities.GetExtension(fileName).ToLower() == ".ppo")
                    {
                        _preprocessorOutput = false;
                    }
                    else
                    {
                        if (_preprocessorOutput)
                        {
                            _ppoStream = FileUtilities.CreateFileStreamChecked(File.Create, ppoFile, "PPO file");
                        }
                        else if (File.Exists(ppoFile))
                        {
                            File.Delete(ppoFile);
                        }
                    }
                }
                catch (Exception e)
                {
                    _parseErrors.Add(new ParseErrorData(ErrorCode.ERR_PreProcessorError, "Error processing PPO file: " + e.Message));
                }
            }
            // Add default IncludeDirs;
            if (!String.IsNullOrEmpty(options.DefaultIncludeDir))
            {
                string[] paths = options.DefaultIncludeDir.Split(new[] { ';' }, StringSplitOptions.RemoveEmptyEntries);
                foreach (var path in paths)
                {
                    includeDirs.Add(path);
                }
            }

            inputs = new InputState(lexerStream);
            foreach (var symbol in options.PreprocessorSymbols)
                symbolDefines[symbol] = null;

            initStdDefines(options, fileName);
        }

        internal void DebugOutput(string format, params object[] objects)
        {
            if (_options.ConsoleOutput != null)
            {
                _options.ConsoleOutput.WriteLine("PP: " + format, objects);
            }
        }


        /// <summary>
        /// Pre-processes the input stream. Reads #Include files, processes #ifdef commands and translations from #defines, macros and UDCs
        /// </summary>                                                               a
        /// <returns>Translated input stream</returns>
        internal IList<IToken> PreProcess()
        {
            var result = new List<IToken>(); ;
            XSharpToken t = Lt();
            List<XSharpToken> omitted = new List<XSharpToken>(); ;
            while (t.Type != IntStreamConstants.Eof)
            {
                // read until the next EOS
                var line = ReadLine(omitted);
                t = Lt();   // CRLF or EOS. Must consume now, because #include may otherwise add a new inputs
                Consume();
                if (line.Count > 0)
                {
                    line = ProcessLine(line);
                    if (line != null && line.Count > 0)
                    {
                        result.AddRange(line);
                    }
                }
                else
                {
                    if (omitted.Count > 0)
                        writeToPPO(omitted, false);
                    else
                        writeToPPO("");
                }
                if (t.Channel == XSharpLexer.DefaultTokenChannel)
                    result.Add(t);
            }
            doEOFChecks();
            return result;
        }

        int getFirstTokenType(IList<XSharpToken> line)
        {
            for (int i = 0; i < line.Count; i++)
            {
                switch (line[i].Channel)
                {
                    case XSharpLexer.DefaultTokenChannel:
                    case XSharpLexer.PREPROCESSORCHANNEL:
                        return line[i].Type;
                    default:
                        break;
                }
            }
            return XSharpLexer.Eof;
        }
        IList<XSharpToken> ProcessLine(IList<XSharpToken> line)
        {
            Debug.Assert(line.Count > 0);
            var nextType = getFirstTokenType(line);
            switch (nextType)
            {
                case XSharpLexer.PP_UNDEF:
                    doUnDefDirective(line);
                    line = null;
                    break;
                case XSharpLexer.PP_IFDEF:
                    doIfDefDirective(line, true);
                    line = null;
                    break;
                case XSharpLexer.PP_IFNDEF:
                    doIfDefDirective(line, false);
                    line = null;
                    break;
                case XSharpLexer.PP_ENDIF:
                    doEndifDirective(line);
                    line = null;
                    break;
                case XSharpLexer.PP_ELSE:
                    doElseDirective(line);
                    line = null;
                    break;
                case XSharpLexer.PP_LINE:
                    doLineDirective(line);
                    line = null;
                    break;
                case XSharpLexer.PP_ERROR:
                case XSharpLexer.PP_WARNING:
                    doErrorWarningDirective(line);
                    line = null;
                    break;
                case XSharpLexer.PP_INCLUDE:
                    doIncludeDirective(line);
                    line = null;
                    break;
                case XSharpLexer.PP_COMMAND:
                case XSharpLexer.PP_TRANSLATE:
                    doUDCDirective(line);
                    line = null;
                    break;
                case XSharpLexer.PP_DEFINE:
                    doDefineDirective(line);
                    line = null;
                    break;
                case XSharpLexer.PP_ENDREGION:
                    doEndRegionDirective(line);
                    line = null;
                    break;
                case XSharpLexer.PP_REGION:
                    doRegionDirective(line);
                    line = null;
                    break;
                case XSharpLexer.UDCSEP:
                    doUnexpectedUDCSeparator(line);
                    line = null;
                    break;
                default:
                    line = doNormalLine(line);
                    break;
            }
            return line;
        }

        /// <summary>
        /// Reads the a line from the input stream until the EOS token and skips hidden tokens
        /// </summary>
        /// <returns>List of tokens EXCLUDING the EOS but including statement separator char ;</returns>
        IList<XSharpToken> ReadLine(IList<XSharpToken> omitted)
        {
            Debug.Assert(omitted != null);
            var res = new List<XSharpToken>();
            omitted.Clear();
            XSharpToken t = Lt();
            while (t.Type != IntStreamConstants.Eof)
            {
                if (t.IsEOS() && t.Text != ";")
                    break;
                if (t.Channel != XSharpLexer.Hidden && t.Channel != XSharpLexer.XMLDOCCHANNEL)
                {
                    var nt = FixToken(t);
                    res.Add(nt);
                }
                else
                {
                    res.Add(t);
                }
                Consume();
                t = Lt();
            }
            return res;
        }

        /// <summary>
        /// Returns the name of the active source. Can be the main prg file, but also an active #include file
        /// </summary>
        string SourceName
        {
            get
            {
                return _fileName;
            }
        }


        XSharpToken GetSourceSymbol()
        {
            XSharpToken s = null;
            if (inputs.isSymbol)
            {
                var baseInputState = inputs;
                while (baseInputState.parent?.isSymbol == true)
                    baseInputState = baseInputState.parent;
                s = baseInputState.Symbol;
            }
            return s;
        }

        XSharpToken FixToken(XSharpToken token)
        {
            if (inputs.MappedLineDiff != 0)
                token.MappedLine = token.Line + inputs.MappedLineDiff;
            if (!string.IsNullOrEmpty(inputs.MappedFileName))
                token.MappedFileName = inputs.MappedFileName;
            //if (!string.IsNullOrEmpty(inputs.SourceFileName))
            //    token.SourceFileName = inputs.SourceFileName;
            if (inputs.isSymbol)
            {
                token.SourceSymbol = GetSourceSymbol();
                //token.SourceFileName = (token.SourceSymbol as XSharpToken).SourceFileName;
            }
            return token;
        }

        XSharpToken Lt()
        {
            return inputs.Lt();
        }

        void Consume()
        {
            while (!inputs.Consume() && inputs.parent != null)
            {
                if (inputs.isSymbol)
                    activeSymbols.Remove(inputs.SymbolName);
                inputs = inputs.parent;
            }
        }

        void InsertStream(string filename, ITokenStream input, XSharpToken symbol )
        {
            if (_options.ShowDefs)
            {
                if (symbol != null)
                {
                    var tokens = new List<XSharpToken>();
                    for (int i = 0; i < input.Size - 1; i++)
                    {
                        tokens.Add(new XSharpToken(input.Get(i)));
                    }
                    string text = tokens.AsString();
                    //if (text.Length > 20)
                    //    text = text.Substring(0, 20) + "...";
                    DebugOutput("File {0} line {1}:", _fileName, symbol.Line);
                    DebugOutput("Input stack: Insert value of token Symbol {0}, {1} tokens => {2}", symbol.Text, input.Size - 1, text);
                }
                else
                    DebugOutput("Input stack: Insert Stream {0}, # of tokens {1}", filename, input.Size - 1);
            }
            // Detect recursion
            var x = inputs;
            while (x != null)
            { 
                if (string.Compare(x.SourceFileName , filename, true) == 0)
                {
                    _parseErrors.Add(new ParseErrorData(symbol, ErrorCode.ERR_PreProcessorError, "Recursive include file ("+filename+") detected",filename));
                    return;
                }
                x = x.parent;
            }
            InputState s = new InputState(input);
            s.parent = inputs;
            s.SourceFileName = filename;
            s.SymbolName = symbol?.Text;
            s.Symbol = symbol;
            s.isSymbol = symbol != null;
            if (s.isSymbol)
            {
                activeSymbols.Add(s.SymbolName);
                s.MappedLineDiff = inputs.MappedLineDiff;
            }
            inputs = s;
        }

        bool IsActive()
        {
            return defStates.Count == 0 || defStates.Peek();
        }

        int IncludeDepth()
        {
            int d = 1;
            var o = inputs;
            while (o.parent != null)
            {
                if (!o.isSymbol)
                    d += 1;
                o = o.parent;
            }
            return d;
        }

        int SymbolDepth()
        {
            int d = 0;
            var o = inputs;
            while (o.parent != null && o.isSymbol)
            {
                d += 1;
                o = o.parent;
            }
            return d;
        }

        bool IsDefinedMacro(XSharpToken t)
        {
            return (t.Type == XSharpLexer.MACRO) ? macroDefines.ContainsKey(t.Text) : false;
        }
        IList<XSharpToken> stripWs(IList<XSharpToken> line)
        {
            IList<XSharpToken> result = new List<XSharpToken>();
            foreach (var token in line)
            {
                if (token.Channel == XSharpLexer.DefaultTokenChannel || token.Channel == XSharpLexer.PREPROCESSORCHANNEL)
                {
                    result.Add(token);
                }
            }
            return result;
        }

        void addDefine(IList<XSharpToken> line)
        {
            // Check to see if the define contains a LPAREN, and there is no space in between them. 
            // Then it is a pseudo function that we will store as a #xtranslate UDC
            // this returns a list that includes #define and the ID
            if (line.Count < 2)
            {
                var token = line[0];
                _parseErrors.Add(new ParseErrorData(token, ErrorCode.ERR_PreProcessorError, "Identifier expected"));
                return;
            }
            // token 1 is the Identifier
            // other tokens are optional and may contain a value
            XSharpToken def = line[1];
            if (line.Count > 2)
            {
                var first = line[2];
                if (first.Type == XSharpLexer.LPAREN
                    && first.StartIndex == def.StopIndex + 1)
                {
                    doUDCDirective(line);
                    return;
                }
            }
            if (XSharpLexer.IsIdentifier(def.Type) || XSharpLexer.IsKeyword(def.Type))
            {
                line.RemoveAt(0);  // remove #define
                line.RemoveAt(0);  // remove ID
                if (symbolDefines.ContainsKey(def.Text))
                {
                    // check to see if this is a new definition or a duplicate definition
                    var oldtokens = symbolDefines[def.Text];
                    var cOld = oldtokens.AsString();
                    var cNew = line.AsString();
                    def.SourceSymbol = null;    // make sure we point to the location in the include file when this happens  (and not to the location where we were included)
                    if (cOld == cNew)
                    {
                        // check to see if the same file has been added twice
                        var oldToken = oldtokens[0];
                        var newToken = line[0];
                        if (oldToken.TokenSource.SourceName != newToken.TokenSource.SourceName || oldToken.Line != newToken.Line)
                        {
                            _parseErrors.Add(new ParseErrorData(def, ErrorCode.WRN_DuplicateDefineSame, def.Text));
                        }
                        else 
                        {
                            if (! _duplicateFile)
                            {
                                _duplicateFile = true;
                                var includeName = newToken.SourceName;
                                var defText = def.Text;
                                if (inputs.Symbol != null)
                                {
                                    def = new XSharpToken(inputs.Symbol);
                                    def.SourceSymbol = null;
                                }
                                _parseErrors.Add(new ParseErrorData(def, ErrorCode.ERR_PreProcessorError, "Duplicate define (" + defText + ") found because include file \""+includeName+"\" was included twice"));
                            }
                        }
                    }
                    else
                    {
                        _parseErrors.Add(new ParseErrorData(def, ErrorCode.WRN_DuplicateDefineDiff, def.Text, cOld, cNew));
                    }
                }
                symbolDefines[def.Text] = line;
                if (_options.ShowDefs)
                {
                    DebugOutput("{0}:{1} add DEFINE {2} => {3}", def.FileName(), def.Line, def.Text, line.AsString());
                }
            }
            else
            {
                _parseErrors.Add(new ParseErrorData(def, ErrorCode.ERR_PreProcessorError, "Identifier expected"));
                return;
            }
        }

        void removeDefine(IList<XSharpToken> line)
        {
            var errToken = line[0];
            bool ok = true;
            line = stripWs(line);
            if (line.Count < 2)
            {
                ok = false;
            }
            XSharpToken def = line[1];
            if (XSharpLexer.IsIdentifier(def.Type) || XSharpLexer.IsKeyword(def.Type))
            {
                if (symbolDefines.ContainsKey(def.Text))
                    symbolDefines.Remove(def.Text);
            }
            else
            {
                errToken = def;
                ok = false;
            }
            if (!ok)
            {
                _parseErrors.Add(new ParseErrorData(errToken, ErrorCode.ERR_PreProcessorError, "Identifier expected"));
            }
        }

        void doUDCDirective(IList<XSharpToken> udc)
        {
            Debug.Assert(udc?.Count > 0);
            writeToPPO(udc, true, true);
            udc = stripWs(udc);
            if (udc.Count < 3)
            {
                _parseErrors.Add(new ParseErrorData(udc[0], ErrorCode.ERR_PreProcessorError, "Invalid UDC: '" + udc.AsString() + "'"));
                return;
            }
            var cmd = udc[0];
            PPErrorMessages errorMsgs;
            var rule = new PPRule(cmd, udc, out errorMsgs);
            if (rule.Type == PPUDCType.None)
            {
                if (errorMsgs.Count > 0)
                {
                    foreach (var s in errorMsgs)
                    {
                        _parseErrors.Add(new ParseErrorData(s.Token, ErrorCode.ERR_PreProcessorError, s.Message));
                    }
                }
                else
                {
                    _parseErrors.Add(new ParseErrorData(cmd, ErrorCode.ERR_PreProcessorError, "Invalid directive '" + cmd.Text + "' (are you missing the => operator?)"));
                }
            }
            else
            {
                if (cmd.Type == XSharpLexer.PP_COMMAND)
                {
                    // COMMAND and XCOMMAND can only match from beginning of line
                    cmdRules.Add(rule);
                    _hasCommandrules = true;
                }
                else
                {
                    // TRANSLATE and XTRANSLATE can also match from beginning of line
                    transRules.Add(rule);
                    _hasTransrules = true;
                    if (cmd.Type == XSharpLexer.PP_DEFINE)
                    {
                        rule.CaseInsensitive = _options.VOPreprocessorBehaviour;
                    }
                }
                if (_options.ShowDefs)
                {
                    DebugOutput("{0}:{1} add {2} {3}", cmd.FileName(), cmd.Line, cmd.Type == XSharpLexer.PP_DEFINE ? "DEFINE" : "UDC", rule.Name);
                }

            }
        }

        private Exception readFileContents( string fp, out string nfp, out SourceText text)
        {
            Exception ex = null;
            nfp = null;
            text = null;
            try
            {
                using (var data = new FileStream(fp, FileMode.Open, FileAccess.Read, FileShare.ReadWrite, bufferSize: 1, options: FileOptions.None))
                {
                    nfp = data.Name;
                    try
                    {
                        text = EncodedStringText.Create(data, _encoding, _checksumAlgorithm);
                    }
                    catch (Exception)
                    {
                        text = null;
                    }
                    if (text == null)
                    {
                        // Encoding problem ?
                        text = EncodedStringText.Create(data);
                    }

                }
            }
            catch (Exception e)
            {
                nfp = null;
                ex = e;
            }
            return ex;
        }

        private bool ProcessIncludeFile(string includeFileName, XSharpToken fileNameToken)
        {
            string nfp = null;
            SourceText text = null;
            Exception fileReadException = null;
            PPIncludeFile includeFile = null;
            List<String> dirs = new List<String>();
            dirs.Add(PathUtilities.GetDirectoryName(_fileName));
            foreach (var p in includeDirs)
            {
                dirs.Add(p);
            }
            if (_options.Verbose)
            {
                DebugOutput("Process include file: {0}", includeFileName);
            }
            foreach (var p in dirs)
            {
                bool rooted = Path.IsPathRooted(includeFileName);
                string fp;
                try
                {
                    fp = rooted || p == null ? includeFileName : Path.Combine(p, includeFileName);
                }
                catch (Exception e)
                {
                    _parseErrors.Add(new ParseErrorData(fileNameToken, ErrorCode.ERR_PreProcessorError, "Error combining path " + p + " and filename  " + includeFileName + " " + e.Message));
                    continue;
                }
                if (File.Exists(fp))
                {
                    if (_options.Verbose)
                    {
                        DebugOutput("Found include file on disk: {0}", fp);
                    }
                    includeFile = PPIncludeFile.Get(fp);
                    if (includeFile != null)
                    {
                        nfp = fp;
                        text = includeFile.Text;
                        if (_options.Verbose)
                        {
                            DebugOutput("Include file retrieved from cache: {0}", fp);
                        }
                        break;
                    }
                    else
                    {
                        var ex = readFileContents(fp, out nfp, out text);
                        if (ex != null && fileReadException == null)
                        {
                            fileReadException = ex;
                        }
                    }
                    if (rooted || nfp != null)
                        break;

                }
            }                
            if (nfp == null)
            {
                if (fileReadException != null)
                {
                    _parseErrors.Add(new ParseErrorData(fileNameToken, ErrorCode.ERR_PreProcessorError, "Error Reading include file '" + includeFileName + "': " + fileReadException.Message));
                }
                else
                {
                    _parseErrors.Add(new ParseErrorData(fileNameToken, ErrorCode.ERR_PreProcessorError, "Include file not found: '" + includeFileName + "'"));
                }

                return false;
            }
            else
            {
                if (!IncludedFiles.ContainsKey(nfp))
                {
                    IncludedFiles.Add(nfp, text);
                }
            }
            if (_options.ShowIncludes )
            {
                var fname = PathUtilities.GetFileName(this.SourceName);
                if (fileNameToken != null)
                {
                    fname = PathUtilities.GetFileName(fileNameToken.InputStream.SourceName);
                    DebugOutput("{0} line {1} Include {2}", fname, fileNameToken.Line, nfp);
                }
                else
                {
                    // Most likely the Standard Header file. Report for Line 0
                    DebugOutput("{0} line {1} Include {2}", fname, 0, nfp);
                }
            }
            
            if (includeFile == null)
            {
                // we have nfp and text with the file contents
                // now parse the stuff and insert in the cache
                var startTime = DateTime.Now;

                var stream = new AntlrInputStream(text.ToString()) { name = nfp };
                var lexer = new XSharpLexer(stream){ TokenFactory = XSharpTokenFactory.Default };
                var ct = new CommonTokenStream(lexer);
                ct.Fill();
                foreach (var e in lexer.LexErrors)
                {
                    _parseErrors.Add(e);
                }
                var endTime = DateTime.Now;
                if (_options.Verbose)
                {
                    DebugOutput("Lexed include file {0} milliseconds", (endTime - startTime).Milliseconds);
                }
                bool newFile = false;
                includeFile = PPIncludeFile.Add(nfp, ct.GetTokens(), text, lexer.MustBeProcessed, ref newFile);
                if (_options.Verbose)
                {
                    if (newFile)
                    {
                        DebugOutput("Added include file to cache {0}", nfp);
                    }
                    else
                    {
                        DebugOutput("Bummer: include file was already added to cache by another thread {0}", nfp);
                    }
                }

            }
            nfp = includeFile.FileName;
            if (_options.ParseLevel == ParseLevel.Complete || includeFile.MustBeProcessed )
            {
                var clone = includeFile.Tokens.CloneArray();
                var tokenSource = new ListTokenSource(clone, nfp);
                var tokenStream = new BufferedTokenStream(tokenSource);
                tokenStream.Fill();
                InsertStream(nfp, tokenStream, fileNameToken);
            }
            else
            {
                if (_options.Verbose)
                {
                    DebugOutput("Skipping Include File in Parse Mode {0} line {1}:", nfp, fileNameToken.Line);
                }
            }
            return true;

        }

        private bool IsDefined(string define)
        {
            // Handle /VO8 compiler option:
            // When /VO8 is active and the variable is defined and has a value of FALSE or a numeric value = 0
            // Then #ifdef is FALSE
            // otherwise #ifdef is TRUE
            // and when there is more than one token, then #ifdef is also TRUE
            bool isdefined= symbolDefines.ContainsKey(define);
            if (isdefined && _options.VOPreprocessorBehaviour)
            {
                var value = symbolDefines[define];
                if (value?.Count == 1)
                {
                    var deftoken = value[0];
                    if (deftoken.Type == XSharpLexer.FALSE_CONST)
                    {
                        isdefined = false;
                    }
                    else if (deftoken.Type == XSharpLexer.INT_CONST)
                    {
                        isdefined = Convert.ToInt64(deftoken.Text) != 0;
                    }
                }
            }
            return isdefined;
        }

        private bool isDefineAllowed(IList<XSharpToken> line, int iPos)
        {
            // DEFINE will not be accepted immediately after or before a DOT
            // So this will not be recognized:
            // #define Console
            // System.Console.WriteLine("zxc")
            // But this will , since there are spaces around the token
            // System. Console .WriteLine("zxc")
            Debug.Assert(line?.Count > 0);
            if (iPos > 0 && line[iPos-1].Type == XSharpLexer.DOT )
            {
                return false;
            }
            if (iPos < line.Count-1)
            {
                var token = line[iPos + 1];
                if (token.Type == XSharpParser.DOT )
                    return false;
            }
            return true;
        }
        #region Preprocessor Directives


        private void checkForUnexpectedPPInput(IList<XSharpToken> line, int nMax)
        {
            if (line.Count > nMax)
            {
                _parseErrors.Add(new ParseErrorData(line[nMax], ErrorCode.ERR_EndOfPPLineExpected));
            }
        }
        private void doRegionDirective(IList<XSharpToken> original)
        {
            var line = stripWs(original);
            Debug.Assert(line?.Count > 0);
            if (IsActive())
            {
                var token = line[0];
                regions.Push(token);
                writeToPPO(original,  true);
            }
            else
            {
                writeToPPO("");
            }
        }

        private void doEndRegionDirective(IList<XSharpToken> original)
        {
            var line = stripWs(original);
            Debug.Assert(line?.Count > 0);
            if (IsActive())
            {
                var token = line[0];
                if (regions.Count > 0)
                    regions.Pop();
                else
                    _parseErrors.Add(new ParseErrorData(token, ErrorCode.ERR_PreProcessorError, "#endregion directive without matching #region found"));
                writeToPPO(original, true);
            }
            else
            {
                writeToPPO("");
            }
            // ignore comments after #endregion
            //checkForUnexpectedPPInput(line, 1);
        }

        private void doDefineDirective(IList<XSharpToken> original)
        {
            var line = stripWs(original);
            Debug.Assert(line?.Count > 0);
            if (IsActive())
            {
                writeToPPO(original, true);
                addDefine(line);
            }
            else
            {
                writeToPPO("");
            }

        }

        private void doUnDefDirective(IList<XSharpToken> original)
        {
            var line = stripWs(original);
            Debug.Assert(line?.Count > 0);
            if (IsActive())
            {
                removeDefine(line);
                writeToPPO(original, true);
            }
            else
            {
                writeToPPO("");
            }
            checkForUnexpectedPPInput(line, 2);
        }

        private void doErrorWarningDirective(IList<XSharpToken> original)
        {
            var line = stripWs(original);
            Debug.Assert(line?.Count > 0);
            int nextType = line[0].Type;
            if (IsActive())
            {
                string text;
                XSharpToken ln;
                writeToPPO(original, true);
                ln = line[0];
                if (line.Count > 1)
                {
                    int start = line[1].StartIndex;
                    int end = line[line.Count - 1].StopIndex;
                    text = line[1].TokenSource.InputStream.GetText(new Interval(start, end));
                }
                else
                {
                    if (nextType == XSharpLexer.PP_ERROR)
                        text = "Empty error clause";
                    else
                        text = "Empty warning clause";

                }
                if (ln.SourceSymbol != null)
                    ln = ln.SourceSymbol;
                if (nextType == XSharpLexer.PP_WARNING)
                    _parseErrors.Add(new ParseErrorData(ln, ErrorCode.WRN_WarningDirective, text));
                else
                    _parseErrors.Add(new ParseErrorData(ln, ErrorCode.ERR_ErrorDirective, text));
                lastToken = ln;
            }
            else
            {
                writeToPPO( "");
            }
        }

        private void doIfDefDirective(IList<XSharpToken> original, bool isIfDef)
        {
            var line = stripWs(original);
            Debug.Assert(line?.Count > 0);
            if (IsActive())
            {
                var def = line[1];
                if (XSharpLexer.IsIdentifier(def.Type) || XSharpLexer.IsKeyword(def.Type))
                {
                    if (isIfDef)
                        defStates.Push(IsDefined(def.Text));
                    else
                        defStates.Push(!IsDefined(def.Text));
                }
                else if (def.Type == XSharpLexer.MACRO)
                {
                    if (isIfDef)
                        defStates.Push(IsDefinedMacro(def));
                    else
                        defStates.Push(!IsDefinedMacro(def));
                }
                else
                {
                    _parseErrors.Add(new ParseErrorData(def, ErrorCode.ERR_PreProcessorError, "Identifier expected"));
                }
                writeToPPO(original,  true);

            }
            else
            {
                defStates.Push(false);
                writeToPPO( "");
            }
            checkForUnexpectedPPInput(line, 2);
        }

        private void doElseDirective(IList<XSharpToken> original)
        {
            var line = stripWs(original);
            Debug.Assert(line?.Count > 0);
            writeToPPO(original, true);
            if (defStates.Count > 0)
            {
                bool a = defStates.Pop();
                if (IsActive())
                {
                    defStates.Push(!a);
                }
                else
                    defStates.Push(false);
            }
            else
            {
                _parseErrors.Add(new ParseErrorData(Lt(), ErrorCode.ERR_PreProcessorError, "Unexpected #else"));
            }
            checkForUnexpectedPPInput(line, 1);
        }

        private void doEndifDirective(IList<XSharpToken> original)
        {
            var line = stripWs(original);
            Debug.Assert(line?.Count > 0);
            if (defStates.Count > 0)
            {
                defStates.Pop();
                if (IsActive())
                {
                    writeToPPO(original, true);
                }
                else
                {
                    writeToPPO("");
                }
            }
            else
            {
                _parseErrors.Add(new ParseErrorData(Lt(), ErrorCode.ERR_UnexpectedDirective));
                writeToPPO(line, true);
            }
            checkForUnexpectedPPInput(line, 1);
        }

        private void doIncludeDirective(IList<XSharpToken> original)
        {
            var line = stripWs(original);
            Debug.Assert(line?.Count > 0);
            if (IsActive())
            {
                writeToPPO(original, true);
                if (IncludeDepth() == MaxIncludeDepth)
                {
                    _parseErrors.Add(new ParseErrorData(line[0], ErrorCode.ERR_PreProcessorError, "Reached max include depth: " + MaxIncludeDepth));
                }
                else
                {
                    var token = line[1];
                    if (token.Type == XSharpLexer.STRING_CONST)
                    {
                        string fileName = token.Text.Substring(1, token.Text.Length - 2);
                        ProcessIncludeFile(fileName, token);

                    }
                    else
                    {
                        _parseErrors.Add(new ParseErrorData(token, ErrorCode.ERR_PreProcessorError, "String literal expected"));
                    }
                }
            }
            else
            {
                writeToPPO("");
            }
            checkForUnexpectedPPInput(line, 2);
        }

        private void doLineDirective(IList<XSharpToken> original)
        {
            var line = stripWs(original);
            Debug.Assert(line?.Count > 0);
            if (IsActive())
            {
                writeToPPO(original, true);
                var ln = line[1];
                if (ln.Type == XSharpLexer.INT_CONST)
                {
                    inputs.MappedLineDiff = (int)ln.SyntaxLiteralValue(_options).Value - (ln.Line + 1);
                    if (line.Count > 2)
                    {
                        ln = line[2];
                    }
                    if (ln.Type == XSharpLexer.STRING_CONST)
                    {
                        inputs.SourceFileName = ln.Text.Substring(1, ln.Text.Length - 2);
                    }
                    else
                    {
                        _parseErrors.Add(new ParseErrorData(ln, ErrorCode.ERR_PreProcessorError, "String literal expected"));
                    }
                }
                else
                {
                    _parseErrors.Add(new ParseErrorData(ln, ErrorCode.ERR_PreProcessorError, "Integer literal expected"));
                }
            }
            else
            {
                writeToPPO("");
            }
            checkForUnexpectedPPInput(line, 3);
        }

        private void doUnexpectedUDCSeparator(IList<XSharpToken> line)
        {
            Debug.Assert(line?.Count > 0);
            var ln = line[0];
            writeToPPO(line, true);
            _parseErrors.Add(new ParseErrorData(ln, ErrorCode.ERR_PreProcessorError, "Unexpected UDC separator character found"));
        }

        private IList<XSharpToken> doNormalLine(IList<XSharpToken> line, bool write2PPO = true)
        {
            // Process the whole line in one go and apply the defines, macros and udcs
            // This is modeled after the way it is done in Harbour
            // 1) Look for and replace defines
            // 2) Look for and replace Macros (combined with 1) for performance)
            // 3) look for and replace (x)translates
            // 4) look for and replace (x)commands
            if (IsActive())
            {
                Debug.Assert(line?.Count > 0);
                IList<XSharpToken> result = null;
                bool changed = true;
                // repeat this loop as long as there are matches
                while (changed)
                {
                    changed = false;
                    if (line.Count > 0)
                    {
                        if (doProcessDefinesAndMacros(line, out result))
                        {
                            changed = true;
                            line = result;
                        }
                    }
                    if (_hasTransrules && line.Count > 0)
                    {
                        if (doProcessTranslates(line, out result))
                        {
                            changed = true;
                            line = result;
                        }
                    }
                    if (_hasCommandrules && line.Count > 0)
                    {
                        if (doProcessCommands(line, out result))
                        {
                            changed = true;
                            line = result;
                        }
                    }
                    if (changed && result != null)
                    {
                        result = _lexer.ReclassifyTokens(result);
                    }
                }
            }
            else
            {
                for (int i = 0; i < line.Count; i++)
                {
                    var t = line[i];
                    t.Original.Channel = XSharpLexer.DEFOUTCHANNEL;
                }
                line.Clear();
            }
            if (write2PPO)
            {
                if (line.Count > 0)
                    writeToPPO(line, false);
                else
                    writeToPPO("");
            }
            return line;
        }


        private IList<XSharpToken> copySource(IList<XSharpToken> line, int nCount)
        {
            var result = new List<XSharpToken>(line.Count);
            var temp = new XSharpToken[nCount];
            for (int i = 0; i < nCount; i++)
            {
                temp[i] = line[i];
            }
            result.AddRange(temp);
            return result;
        }
        private bool doProcessDefinesAndMacros(IList<XSharpToken> line, out IList<XSharpToken> result)
        {
            Debug.Assert(line?.Count > 0);
            // we loop in here because one define may add tokens that are defined by another
            // such as:
            // #define FOO 1
            // #define BAR FOO + 1
            // when the code is "? BAR" then we need to translate this to "? 1 + 1"
            // For performance reasons we assume there is nothing to do, so we only 
            // start allocating a result collection when a define is detected
            // otherwise we will simply return the original string
            bool hasChanged = false;
            IList<XSharpToken> tempResult = line;
            result = null;
            while (tempResult != null)
            {
                tempResult = null;
                // in a second iteration line will be the changed line
                for (int i = 0; i < line.Count; i++)
                {
                    var token = line[i];
                    IList<XSharpToken> deflist = null;
                    if (isDefineAllowed(line, i) && token.Text != null && symbolDefines.TryGetValue(token.Text, out deflist))
                    {
                        if (tempResult == null)
                        {
                            // this is the first define in the list
                            // allocate a result and copy the items 0 .. i-1 to the result
                            tempResult = copySource(line, i);
                        }
                        if (deflist != null)
                        {
                            foreach (var t in deflist)
                            {
                                var t2 = new XSharpToken(t);
                                t2.Channel = XSharpLexer.DefaultTokenChannel;
                                t2.SourceSymbol = token;
                                tempResult.Add(t2);
                            }
                        }
                        else
                        {
                            // add a space so error messages look proper
                            var t2 = new XSharpToken(XSharpLexer.WS, " <RemovedToken> ");
                            t2.Channel = XSharpLexer.Hidden;
                            t2.SourceSymbol = token;
                            tempResult.Add(t2);
                        }
                    }
                    else if (token.Type == XSharpLexer.MACRO)
                    {
                        // Macros that cannot be found are changed to ID
                        Func<XSharpToken> ft;
                        if (macroDefines.TryGetValue(token.Text, out ft))
                        {
                            var nt = ft();
                            if (nt != null)
                            {
                                nt.Line = token.Line;
                                nt.Column = token.Column;
                                nt.StartIndex = token.StartIndex;
                                nt.StopIndex = token.StopIndex;
                                nt.SourceSymbol = token;
                                if (tempResult == null)
                                {
                                    // this is the first macro in the list
                                    // allocate a result and copy the items 0 .. i-1 to the result
                                    tempResult = copySource(line, i);
                                }
                                tempResult.Add(nt);
                            }
                        }
                    }
                    else if (tempResult != null)
                    {
                        tempResult.Add(token);
                    }
                }
                if (tempResult != null)
                {
                    // copy temporary result to line for next iteration
                    line = tempResult;
                    result = line;
                    hasChanged = true;
                }
            }
            return hasChanged; ;
        }

        internal void AddParseError(ParseErrorData data)
        {
            _parseErrors.Add(data);
        }
        private bool doProcessTranslates(IList<XSharpToken> line, out IList<XSharpToken> result)
        {
            Debug.Assert(line?.Count > 0);
            var temp = new List<XSharpToken>();
            temp.AddRange(line);
            result = new List<XSharpToken>();
            var usedRules = new PPUsedRules(this, MaxUDCDepth);
            while (temp.Count > 0)
            {
                PPMatchRange[] matchInfo = null;
                var rule = transRules.FindMatchingRule(temp, out matchInfo);
                if (rule != null)
                {
                    temp = doReplace(temp, rule, matchInfo);
                    if (usedRules.HasRecursion(rule, temp))
                    {
                        // duplicate, so exit now
                        result.Clear();
                        return false;
                    }
                    // note that we do not add the result of the replacement to processed
                    // because it will processed further
                }
                else
                {
                    // first token of temp is not start of a #(x)translate. So add it to the result
                    // and try from second token etc
                    result.Add(temp[0]);
                    temp.RemoveAt(0);
                }
            }
            if (usedRules.Count > 0)
            {
                result.TrimLeadingSpaces();
                return true;
            }
            result = null;
            return false;
        }

        private List<IList<XSharpToken>> splitCommands(IList<XSharpToken> tokens, out IList<XSharpToken> separators)
        {
            var result = new List<IList<XSharpToken>>(10); 
            var current = new List<XSharpToken>(tokens.Count);
            separators = new List<XSharpToken>();
            foreach (var t in tokens)
            {
                if (t.Type == XSharpLexer.EOS)
                {
                    current.TrimLeadingSpaces();
                    result.Add(current);
                    current = new List<XSharpToken>();
                    separators.Add(t);
                }
                else
                {
                    current.Add(t);
                }
            }
            result.Add(current);
            return result;
        }
        private bool doProcessCommands(IList<XSharpToken> line, out IList<XSharpToken>  result)
        {
            Debug.Assert(line?.Count > 0);
            line = stripWs(line);
            result = null;
            if (line.Count == 0)
                return false;
            result = line;
            var usedRules = new PPUsedRules(this, MaxUDCDepth);
            while (true)
            {
                PPMatchRange[] matchInfo = null;
                var rule = cmdRules.FindMatchingRule(result, out matchInfo);
                if (rule == null)
                {
                    // nothing to do, so exit. Leave changed the way it is. This does not have to be the first iteration
                    break; 
                }
                result = doReplace(result, rule, matchInfo);
                if (usedRules.HasRecursion(rule, result))
                {
                    // duplicate so exit now
                    result.Clear();
                    return false;
                }
                // the UDC may have introduced a new semi colon and created more than one sub statement
                // so check to see and then process every statement
                IList<XSharpToken> separators;
                var cmds = splitCommands(result, out separators);
                Debug.Assert(cmds.Count == separators.Count + 1);
                if (cmds.Count <= 1)
                {
                    // single statement result. Try again to see if the new statement matches another UDC rule
                    continue; 
                }
                else
                {
                    // multi statement result. Process each statement separately (recursively) as a 'normal line'
                    // the replacement may have introduced the usage of a define, translate or macro
                    result.Clear();
                    for (int i = 0; i < cmds.Count; i++)
                    {
                        if (cmds[i].Count > 0)
                        {
                            cmds[i] = doNormalLine(cmds[i], false);
                            foreach (var token in cmds[i])
                            {
                                result.Add(token);
                            }
                            if (i < cmds.Count - 1)
                            {
                                result.Add(separators[i]);
                            }
                        }
                    }
                    // recursive processing should have done everything, so exit
                    break;
                }
            }
            if (usedRules.Count > 0)
            {
                result.TrimLeadingSpaces();
                return true;
            }
            result = null;
            return false;
        }

        private void doEOFChecks()
        {
            if (defStates.Count > 0)
            {
                _parseErrors.Add(new ParseErrorData(Lt(), ErrorCode.ERR_EndifDirectiveExpected));
            }
            while (regions.Count > 0)
            {
                var token = regions.Pop();
                _parseErrors.Add(new ParseErrorData(token, ErrorCode.ERR_EndRegionDirectiveExpected));
            }
        }

        #endregion

  
        private List<XSharpToken> doReplace(IList<XSharpToken> line, PPRule rule, PPMatchRange[] matchInfo)
        {
            Debug.Assert(line?.Count > 0);
            var res = rule.Replace(line, matchInfo);
            rulesApplied += 1;
            List<XSharpToken> result = new List<XSharpToken>();
            result.AddRange(res);
            if (_options.Verbose)
            {
                int lineNo;
                if (line[0].SourceSymbol != null)
                    lineNo = line[0].SourceSymbol.Line;
                else
                    lineNo = line[0].Line;
                DebugOutput("----------------------");
                DebugOutput("File {0} line {1}:", _fileName, lineNo);
                DebugOutput("   UDC   : {0}", rule.GetDebuggerDisplay());
                DebugOutput("   Input : {0}", line.AsString());
                DebugOutput("   Output: {0}", res.AsString());
            }
            return result;
        }
    }
}


