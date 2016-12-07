/*
   Copyright 2016 XSharp B.V.

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
//#define DUMP_UDC
using System;
using System.Collections.Generic;
using System.Text;
using System.Diagnostics;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;
using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using Antlr4.Runtime.Tree;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;

namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
    internal static class TokenListExtensions
    {
        internal static string AsString (this IList<IToken> tokens)
        {
            string result = "";
            if (tokens != null)
            {
                foreach (var t in tokens)
                    result += t.Text;
            }
            return result;
        }
        internal static IList<IToken> Clone(this IList<IToken> tokens)
        {
            var clone = new IToken[tokens.Count];
            int i = 0;
            foreach (var t in tokens)
            {
                clone[i] = new CommonToken(t);
                i++;
            }
            return clone; 
        }
    }
    internal class XSharpPreprocessor : ITokenSource
    {

        #region Static Properties
        static Dictionary<String, CachedIncludeFile> includecache = new Dictionary<string, CachedIncludeFile>(StringComparer.OrdinalIgnoreCase);


        internal static void ClearOldIncludes()
        {
            // Remove old includes that have not been used in the last 1 minutes
            lock (includecache)
            {
                var oldkeys = new List<string>();
                var compare = DateTime.Now.Subtract(new TimeSpan(0, 1, 0));
                foreach (var include in includecache.Values)
                {
                    if (include.LastUsed < compare)
                    {
                        oldkeys.Add(include.FileName);
                    }
                }
                foreach (var key in oldkeys)
                {
                    includecache.Remove(key);
                }
            }
        }

        internal static CachedIncludeFile GetIncludeFile(string fileName)
        {
            CachedIncludeFile file = null;
            lock (includecache)
            {
                if (includecache.ContainsKey(fileName))
                {
                    file = includecache[fileName];
                    if (file.LastWritten != PortableShim.File.GetLastWriteTimeUtc(fileName))
                    {
                        includecache.Remove(fileName);
                        return null;
                    }
                    //DebugOutput("Found include file in cache: {0}", fileName);
                }
            }
            if (file != null)
            {
                file.LastUsed = DateTime.Now;
            }
            return file;
        }
        internal static CachedIncludeFile  AddIncludeFile(string fileName, IList<IToken> tokens, SourceText text)
        {
            lock (includecache)
            {
                CachedIncludeFile file = GetIncludeFile(fileName);
                if (file == null)
                {
                    file = new CachedIncludeFile();
                    includecache.Add(fileName, file);
                    file.LastUsed = DateTime.Now;
                }
                file.Tokens = tokens.Clone();
                file.Text = text;
                file.FileName = fileName;
                //DebugOutput("Add include file to cache: {0}", fileName);
                file.LastWritten = PortableShim.File.GetLastWriteTimeUtc(fileName);
                return file;
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
            internal IToken Symbol;
            internal InputState parent;

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

            internal IToken Lt()
            {
                if (Eof() && parent != null)
                    return parent.Lt();
                return Tokens.Get(Index);
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

        internal class CachedIncludeFile
        {
            internal DateTime LastWritten { get; set; }
            internal String FileName { get; set; }
            internal IList<IToken> Tokens { get; set; }
            internal SourceText Text { get; set; }
            internal DateTime LastUsed { get; set; }
        }

        CSharpParseOptions _options;

        ITokenStream _input;

        Encoding _encoding;

        SourceHashAlgorithm _checksumAlgorithm;

        IList<ParseErrorData> _parseErrors;

        IList<string> includeDirs;

        Dictionary<string, IList<IToken>> symbolDefines ;

        Dictionary<string, Func<IToken>> macroDefines = new Dictionary<string, Func<IToken>>(/*CaseInsensitiveComparison.Comparer*/);

        Stack<bool> defStates = new Stack<bool> ();

        string _fileName = null;
        InputState inputs;
        IToken lastToken = null;

#if UDCSUPPORT
        List<XSharpPreprocessorRule> _rules = new List<XSharpPreprocessorRule>();
        bool _hasrules = false;
#endif
        HashSet<string> activeSymbols = new HashSet<string>(/*CaseInsensitiveComparison.Comparer*/);

        System.IO.Stream ppoStream;

        internal Dictionary<string, SourceText> IncludedFiles = new Dictionary<string, SourceText>();

        public int MaxIncludeDepth { get; set; } = 16;

        public int MaxSymbolDepth { get; set; } = 16;

        public string StdDefs { get; set; } = string.Empty;
        private void initStdDefines(CSharpParseOptions options, string fileName)
        {
            // Note Macros such as __ENTITY__ and  __SIG__ are handled in the transformation phase
            macroDefines.Add("__ARRAYBASE__", () => new CommonToken(XSharpLexer.INT_CONST, _options.ArrayZero ? "0" : "1"));
            macroDefines.Add("__CLR2__", () => new CommonToken(XSharpLexer.STRING_CONST, "\"__CLR2__\""));
            macroDefines.Add("__CLR4__", () => new CommonToken(XSharpLexer.STRING_CONST, "\"__CLR4__\""));
            macroDefines.Add("__CLRVERSION__", () => new CommonToken(XSharpLexer.STRING_CONST, "\"__CLRVERSION__\""));
            macroDefines.Add("__DATE__", () => new CommonToken(XSharpLexer.STRING_CONST, '"' + DateTime.Now.Date.ToString("yyyyMMdd") + '"'));
            macroDefines.Add("__DATETIME__", () => new CommonToken(XSharpLexer.STRING_CONST, '"' + DateTime.Now.ToString() + '"'));
            if (_options.DebugEnabled)
                macroDefines.Add("__DEBUG__", () => new CommonToken(XSharpLexer.TRUE_CONST));
            macroDefines.Add("__DIALECT__", () => new CommonToken(XSharpLexer.STRING_CONST, '"' + options.Dialect.ToString() + '"'));
            switch (_options.Dialect)
            {
                case XSharpDialect.Core:
                    macroDefines.Add("__DIALECT_CORE__", () => new CommonToken(XSharpLexer.TRUE_CONST));
                    break;
                case XSharpDialect.VO:
                    macroDefines.Add("__DIALECT_VO__", () => new CommonToken(XSharpLexer.TRUE_CONST));
                    break;
                case XSharpDialect.Vulcan:
                    macroDefines.Add("__DIALECT_VULCAN__", () => new CommonToken(XSharpLexer.TRUE_CONST));
                    break;
                default:
                    break;
            }
            macroDefines.Add("__ENTITY__", () => new CommonToken(XSharpLexer.STRING_CONST, "\"__ENTITY__\""));  // Handled later in Transformation phase
            macroDefines.Add("__FILE__", () => new CommonToken(XSharpLexer.STRING_CONST, '"' + (inputs.SourceFileName ?? fileName) + '"'));
            macroDefines.Add("__LINE__", () => new CommonToken(XSharpLexer.INT_CONST, inputs.Lt().Line.ToString()));
            macroDefines.Add("__MODULE__", () => new CommonToken(XSharpLexer.STRING_CONST, '"' + (inputs.SourceFileName ?? fileName) + '"'));
            macroDefines.Add("__SIG__", () => new CommonToken(XSharpLexer.STRING_CONST, "\"__SIG__\"")); // Handled later in Transformation phase
            macroDefines.Add("__SRCLOC__", () => new CommonToken(XSharpLexer.STRING_CONST, '"' + (inputs.SourceFileName ?? fileName) + " line " + inputs.Lt().Line.ToString() + '"'));
            macroDefines.Add("__SYSDIR__", () => new CommonToken(XSharpLexer.STRING_CONST, '"' + options.SystemDir + '"'));
            macroDefines.Add("__TIME__", () => new CommonToken(XSharpLexer.STRING_CONST, '"' + DateTime.Now.ToString("HH:mm:ss") + '"'));
            macroDefines.Add("__UTCTIME__", () => new CommonToken(XSharpLexer.STRING_CONST, '"' + DateTime.Now.ToUniversalTime().ToString("HH:mm:ss") + '"'));
            macroDefines.Add("__VERSION__", () => new CommonToken(XSharpLexer.STRING_CONST, '"' + global::XSharp.Constants.Version + '"'));
            macroDefines.Add("__WINDIR__", () => new CommonToken(XSharpLexer.STRING_CONST, '"' + options.WindowsDir + '"'));
            macroDefines.Add("__WINDRIVE__", () => new CommonToken(XSharpLexer.STRING_CONST, '"' + options.WindowsDir?.Substring(0, 2) + '"'));
            macroDefines.Add("__XSHARP__", () => new CommonToken(XSharpLexer.TRUE_CONST));

            bool[] flags  = { options.vo1,  options.vo2, options.vo3, options.vo4, options.vo5, options.vo6, options.vo7, options.vo8, 
                                options.vo9, options.vo10, options.vo11, options.vo12, options.vo13, options.vo14, options.vo15, options.vo16 };                                             
            for (int iOpt = 0; iOpt < flags.Length; iOpt++)
            {
                string flagName = String.Format("__VO{0}__", iOpt + 1);
                if (flags[iOpt])
                    macroDefines.Add(flagName, () => new CommonToken(XSharpLexer.TRUE_CONST));
                else
                    macroDefines.Add(flagName, () => new CommonToken(XSharpLexer.FALSE_CONST));
            }
            if (!options.NoStdDef)
            {
                // Todo: when the compiler option nostddefs is not set: read XSharpDefs.xh from the XSharp Include folder,//
                // and automatically include it.
                // read XsharpDefs.xh
                StdDefs = "xSharpDefs.xh";
                ProcessIncludeFile(null, StdDefs,true);
            }
        }

        private bool mustWriteToPPO(IToken t)
        {
            return ppoStream != null && t != null && (t.TokenSource.SourceName == _fileName || inputs.isSymbol);
        }

        private void writeToPPO(IToken t)
        {
            // do not call t.Text when not needed.
            if ( mustWriteToPPO(t))
            {
                var buffer = _encoding.GetBytes(t.Text);
                ppoStream.Write(buffer, 0, buffer.Length);
            }
        }

        private void writeToPPO(IToken t, string text)
        {
            if (mustWriteToPPO(t))
            {
                var buffer = _encoding.GetBytes(text);
                ppoStream.Write(buffer, 0, buffer.Length);
            }
        }
        internal void Close()
        {
            if (ppoStream != null)
            {
                ppoStream.Flush();
                ppoStream.Dispose();
            }
            ppoStream = null;
        }

        internal XSharpPreprocessor(ITokenStream input, CSharpParseOptions options, string fileName, Encoding encoding, SourceHashAlgorithm checksumAlgorithm, IList<ParseErrorData> parseErrors)
        {
            ClearOldIncludes();
            _options = options;
            _fileName = fileName;
            if (_options.VOPreprocessorBehaviour)
                symbolDefines = new Dictionary<string, IList<IToken>>(CaseInsensitiveComparison.Comparer);
            else
                symbolDefines = new Dictionary<string, IList<IToken>>(/* case sensitive */);
            _input = input;
            _encoding = encoding; 
            _checksumAlgorithm = checksumAlgorithm;
            _parseErrors = parseErrors;
            includeDirs = new List<string>(options.IncludePaths);
            if ( !String.IsNullOrEmpty(fileName) && PortableShim.File.Exists(fileName))
            {
                includeDirs.Add(System.IO.Path.GetDirectoryName(fileName));
                var ppoFile = FileNameUtilities.ChangeExtension(fileName, ".ppo");
                try
                {
                    ppoStream = null;
                    if (_options.PreprocessorOutput)
                    {
                        ppoStream = FileUtilities.CreateFileStreamChecked(PortableShim.File.Create, ppoFile, "PPO file");
                    }
                    else if (PortableShim.File.Exists(ppoFile))
                    {
                        PortableShim.File.Delete(ppoFile);
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
                string[] paths = options.DefaultIncludeDir.Split( new[] { ';' },StringSplitOptions.RemoveEmptyEntries);
                foreach (var path in paths)
                {
                    includeDirs.Add(path);
                }
            }

            inputs = new InputState(input);
            foreach (var symbol in options.PreprocessorSymbols)
                symbolDefines[symbol] = null;

            initStdDefines(options, fileName);
        }

        internal void DebugOutput(string format, params object[] objects)
        {
            _options.ConsoleOutput.WriteLine("PP: " + format, objects);
        }
        public int Column
        {
            get
            {
                return _input.TokenSource.Column;
            }
        }

        public ICharStream InputStream
        {
            get
            {
                return _input.TokenSource.InputStream;
            }
        }

        public int Line
        {
            get
            {
                return _input.TokenSource.Line;
            }
        }

        public string SourceName
        {
            get
            {
                return _input.TokenSource.SourceName;
            }
        }

        public ITokenFactory TokenFactory
        {
            get
            {
                return _input.TokenSource.TokenFactory;
            }

            set
            {
                _input.TokenSource.TokenFactory = value;
            }
        }

        void SkipHidden()
        {
            IToken t = Lt();
            while (t.Type != IntStreamConstants.Eof && t.Channel != TokenConstants.DefaultChannel && t.Channel != XSharpLexer.PREPROCESSOR)
            {
                Consume();
                t = Lt();
            }
        }

        IToken SkipToEol()
        {
            IToken t = Lt();
            while (t.Type != IntStreamConstants.Eof && t.Channel != TokenConstants.DefaultChannel)
            {
                Consume();
                if (t.Type == XSharpLexer.EOS && t.Text != ";")
                    break;
                t = Lt();
            }
            writeToPPO(t);
            return t;
        }

        void SkipEmpty()
        {
            IToken t = Lt();
            while (t.Type != IntStreamConstants.Eof && t.Channel != TokenConstants.DefaultChannel)
            {
                if (t.Type == XSharpLexer.EOS && t.Text != ";")
                    break;
                if (t.Channel == XSharpLexer.PREPROCESSOR)
                {
                    _parseErrors.Add(new ParseErrorData(t, ErrorCode.WRN_PreProcessorWarning, "Ignored input '"+t.Text+"'"));
                }
                Consume();
                t = Lt();
            }
        }

        void SkipInactive()
        {
            IToken t = Lt();
            while (t.Type != IntStreamConstants.Eof && t.Channel != TokenConstants.DefaultChannel)
            {
                ((CommonToken)t).Channel = XSharpLexer.DEFOUT;
                Consume();
                if (t.Type == XSharpLexer.EOS && t.Text != ";")
                    break;
                t = Lt();
            }
            if (t.Type == XSharpLexer.EOS)
                writeToPPO(t);
        }

        IToken GetSourceSymbol()
        {
            IToken s = null;
            if (inputs.isSymbol)
            {
                var baseInputState = inputs;
                while (baseInputState.parent?.isSymbol == true)
                    baseInputState = baseInputState.parent;
                s = baseInputState.Symbol;
            }
            return s;
        }

        CommonToken FixToken(IToken t)
        {
            CommonToken ct = (CommonToken)t;

             if (inputs.MappedLineDiff != 0)
                ct.MappedLine = t.Line + inputs.MappedLineDiff;
            if (!string.IsNullOrEmpty(inputs.MappedFileName))
                ct.MappedFileName = inputs.MappedFileName;
            if (!string.IsNullOrEmpty(inputs.SourceFileName))
                ct.SourceFileName = inputs.SourceFileName;
            if (inputs.isSymbol)
            {
                ct.SourceSymbol = GetSourceSymbol();
                ct.SourceFileName = (ct.SourceSymbol as CommonToken).SourceFileName;
            }
            return ct;
        }

        IList<IToken> ConsumeList()
        {
            IList<IToken> res = null;
            IToken t = Lt();
            while (t.Type != IntStreamConstants.Eof && t.Channel != TokenConstants.DefaultChannel)
            {
                if (t.Type == XSharpLexer.EOS && t.Text != ";")
                    break;
                if (t.Channel == XSharpLexer.PREPROCESSOR)
                {
                    if (res == null)
                        res = new List<IToken> ();
                    var nt = FixToken(new CommonToken(t));
                    nt.Channel = TokenConstants.DefaultChannel;
                    res.Add(nt);
                }
                Consume();
                t = Lt();
            }
            return res;
        }

        int La()
        {
            return inputs.La();
        }

        IToken Lt()
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

        void InsertStream(string filename, ITokenStream input, IToken symbol = null)
        {
            if ( _options.ShowDefs)
            {
                if (symbol != null)
                {
                    var tokens = new List<IToken>();
                    for (int i = 0; i < input.Size-1; i++)
                    {
                        tokens.Add(input.Get(i));
                    }
                    DebugOutput("Input stack: Insert value of token Symbol {0}, {1} tokens => {2}", symbol.Text, input.Size-1, tokens.AsString());
                }
                else
                    DebugOutput("Input stack: Insert Includefile Stream {0}, # of tokens {1}", filename, input.Size-1, symbol?.Text);
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

        bool IsActiveElseSkip()
        {
            if (IsActive())
                return true;
            SkipInactive();
            return false;
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

        bool IsDefinedMacro(IToken t)
        {
            return (t.Type == XSharpLexer.MACRO) ? macroDefines.ContainsKey(t.Text) : false;
        }

        void addDefine(IToken def)
        {
            if (XSharpLexer.IsIdentifier(def.Type) || XSharpLexer.IsKeyword(def.Type))
            {
                Consume();
                writeToPPO(def, "// #define " + def.Text + " ");
                var newtokens = ConsumeList();
                if (newtokens != null)
                {
                    foreach (var t in newtokens)
                    {
                        writeToPPO(t);
                    }
                }
                if (symbolDefines.ContainsKey(def.Text))
                {
                    // check to see if this is a new definition or a duplicate definition
                    var oldtokens = symbolDefines[def.Text];
                    var cOld = oldtokens.AsString();
                    var cNew = newtokens.AsString();
                    if (cOld == cNew)
                        _parseErrors.Add(new ParseErrorData(def, ErrorCode.WRN_DuplicateDefineSame, def.Text));
                    else
                        _parseErrors.Add(new ParseErrorData(def, ErrorCode.WRN_DuplicateDefineDiff, def.Text, cOld, cNew));
                }
                symbolDefines[def.Text] = newtokens;
                if (_options.ShowDefs)
                {
                    DebugOutput("Line {0}, add define {1} => {2}", def.Line, def.Text, newtokens.AsString());
                }
            }
            else
            {
                _parseErrors.Add(new ParseErrorData(def, ErrorCode.ERR_PreProcessorError, "Identifier expected"));
            }

        }
        void removeDefine(IToken def)
        {
            if (XSharpLexer.IsIdentifier(def.Type) || XSharpLexer.IsKeyword(def.Type))
            {
                Consume();
                writeToPPO(def,"// #undef " + def.Text );
                SkipEmpty();
                if (symbolDefines.ContainsKey(def.Text))
                    symbolDefines.Remove(def.Text);
                // undef for a symbol that is not defined is not seen as an error in VO and Vulcan
                //else
                //{
                //    _parseErrors.Add(new ParseErrorData(def, ErrorCode.WRN_PreProcessorWarning, "Symbol not defined: " + def.Text));
                //}
            }
            else
            {
                _parseErrors.Add(new ParseErrorData(def, ErrorCode.ERR_PreProcessorError, "Identifier expected"));

            }
        }

        void addRule(int token)
        {
            var cmd = Lt();
            //cmd = FixToken(cmd);
            Consume();
            var udc = ConsumeList();
#if UDCSUPPORT
            var rule = new XSharpPreprocessorRule(token, udc);
            if (rule.Type == RuleType.None)
            {
                if (rule.ErrorMessages?.Count > 0)
                {
                    foreach (var s in rule.ErrorMessages)
                    {
                        _parseErrors.Add(new ParseErrorData(s.Item1, ErrorCode.ERR_PreProcessorError, "Invalid directive '" + cmd.Text + ": " + s.Item2));
                    }
                }
                else
                {
                    _parseErrors.Add(new ParseErrorData(cmd, ErrorCode.ERR_PreProcessorError, "Invalid directive '" + cmd.Text + "' (are you missing the => operator?)"));
                }
            }
            else
            {
                _rules.Add(rule);
                _hasrules = true;
            }
#else
            _parseErrors.Add(new ParseErrorData(cmd, ErrorCode.ERR_PreProcessorError, "Directive '" + cmd.Text + "' not supported yet"));
#endif

        }

        private bool ProcessIncludeFile(IToken ln, string fn, bool StdDefine = false)
        {
            string nfp = null;
            SourceText text = null;
            Exception fileReadException = null;
            CachedIncludeFile cachedFile = null;
            List<String> dirs = new List<String>();
            dirs.Add(PathUtilities.GetDirectoryName(_fileName));
            foreach (var p in includeDirs)
            {
                dirs.Add(p);
            }
            foreach (var p in dirs)
            {
                bool rooted = System.IO.Path.IsPathRooted(fn);
                string fp;
                try
                {
                    fp = rooted ? fn : System.IO.Path.Combine(p, fn);
                }
                catch (Exception e)
                {
                    _parseErrors.Add(new ParseErrorData(ln, ErrorCode.ERR_PreProcessorError, "Error combining path " + p + " and filename  " + fn + " " + e.Message));
                    continue;
                }
                try
                {
                    using (var data = PortableShim.FileStream.Create(fp, PortableShim.FileMode.Open, PortableShim.FileAccess.Read, PortableShim.FileShare.ReadWrite, bufferSize: 1, options: PortableShim.FileOptions.None))
                    {
                        nfp = (string)PortableShim.FileStream.Name.GetValue(data);
                        cachedFile = GetIncludeFile(nfp);
                        if (cachedFile != null)
                        {
                            text = cachedFile.Text;
                        }
                        else
                        {
                            nfp = (string)PortableShim.FileStream.Name.GetValue(data);
                            text = EncodedStringText.Create(data, _encoding, _checksumAlgorithm);
                        }
                        if (! IncludedFiles.ContainsKey(nfp))
                            IncludedFiles.Add(nfp, text);
                        break;
                    }
                }
                catch (Exception e)
                {
                    if (fileReadException == null)
                        fileReadException = e;
                    nfp = null;
                }
                if (rooted)
                    break;
            }
            if (! StdDefine)
            {
                SkipEmpty();
                SkipToEol();
            }
            if (nfp == null)
            {
                if (fileReadException != null)
                {
                    _parseErrors.Add(new ParseErrorData(ln, ErrorCode.ERR_PreProcessorError, "Error Reading include file '" + fn + "': " + fileReadException.Message));
                }
                else
                {
                    _parseErrors.Add(new ParseErrorData(ln, ErrorCode.ERR_PreProcessorError, "Include file not found: '" + fn + "'"));
                }

                return false;
            }
            if (_options.ShowIncludes )
            {
                var fname = PathUtilities.GetFileName(this.SourceName);
                if (ln != null)
                {
                    fname = PathUtilities.GetFileName(ln.InputStream.SourceName);
                    DebugOutput("{0} line {1} Include {2}", fname, ln.Line, nfp);
                }
                else
                {
                    DebugOutput("{0} line {1} Include {2}", fname, 0, nfp);
                }

            }
            if (cachedFile == null)
            {
                // we have nfp and text with the file contents
                // now parse the stuff and insert in the cache
                //Debug.WriteLine("Uncached file {0} ", nfp);
                var stream = new AntlrInputStream(text.ToString());
                var lexer = new XSharpLexer(stream);
                var tokens = new CommonTokenStream(lexer);
                stream.name = nfp;
                tokens.Fill();
                InsertStream(nfp, tokens);
                AddIncludeFile(nfp, tokens.GetTokens(), text);
            }
            else
            {
                // we have a file cache item with the Tokens etc
                // Create a stream from the cached text and tokens
                // Clone the tokens to avoid problems when concurrently using the tokens
                var newTokens = cachedFile.Tokens.Clone();
                var tokenSource = new ListTokenSource(newTokens, cachedFile.FileName);
                var tokenStream = new BufferedTokenStream(tokenSource);
                tokenStream.Sync(newTokens.Count);
                InsertStream(cachedFile.FileName, tokenStream);
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

        private bool isDefineAllowed()
        {
            // DEFINE will not be accepted immediately after or before a DOT or COLON
            // So this will not be recognized:
            // #define Console
            // System.Console.WriteLine("zxc")
            // But this will , since there are spaces around the token
            // System. Console .WriteLine("zxc")

            if (lastToken != null)
            {
                if (lastToken.Type == XSharpLexer.DOT ||
                    lastToken.Type == XSharpLexer.COLON)
                    return false;
            }
            var index = inputs.Index;
            if (index < inputs.Tokens.Size)
            {
                var token = inputs.Tokens.Get(index + 1);
                if (token.Type == XSharpParser.DOT ||
                    token.Type == XSharpLexer.COLON)
                    return false;
            }
            return true;
        }

        [return: NotNull]
        public IToken NextToken()
        {
            while (true)
            {
                var nextType = La();
                if (inputs.isSymbol)
                {
                    switch (nextType)
                    {
                        case XSharpLexer.PP_DEFINE:
                        case XSharpLexer.PP_UNDEF:
                        case XSharpLexer.PP_IFDEF:
                        case XSharpLexer.PP_IFNDEF:
                        case XSharpLexer.PP_ENDIF:
                        case XSharpLexer.PP_ELSE:
                        case XSharpLexer.PP_LINE:
                        case XSharpLexer.PP_ERROR:
                        case XSharpLexer.PP_WARNING:
                        case XSharpLexer.PP_INCLUDE:
                        case XSharpLexer.PP_COMMAND:
                        case XSharpLexer.PP_TRANSLATE:
                        case XSharpLexer.PP_ENDREGION:
                        case XSharpLexer.PP_REGION:
                            nextType = XSharpLexer.WS;
                            break;
                    }
                }
                switch (nextType)
                {
                    case IntStreamConstants.Eof:
                        if (defStates.Count > 0)
                        {
                            _parseErrors.Add(new ParseErrorData(Lt(), ErrorCode.ERR_PreProcessorError, "'#endif' expected"));
                        }
                        return Lt();
                    case XSharpLexer.PP_DEFINE:
                        if (IsActiveElseSkip())
                        {
                            Consume();
                            SkipHidden();
                            var def = Lt();
                            addDefine(def);
                            SkipToEol();
                        }
                        break;
                    case XSharpLexer.PP_UNDEF:
                        if (IsActiveElseSkip())
                        {
                            Consume();
                            SkipHidden();
                            var def = Lt();
                            removeDefine(def);
                            SkipToEol();
                        }
                        break;
                    case XSharpLexer.PP_IFDEF:
                        if (IsActiveElseSkip())
                        {
                            Consume();
                            SkipHidden();
                            var def = Lt();
                            writeToPPO(def, "// #ifdef " + def.Text);
                            if (XSharpLexer.IsIdentifier(def.Type) || XSharpLexer.IsKeyword(def.Type))
                            {
                                Consume();
                                SkipEmpty();
                                defStates.Push(IsDefined(def.Text));
                            }
                            else if (def.Type == XSharpLexer.MACRO)
                            {
                                Consume();
                                SkipEmpty();
                                defStates.Push(IsDefinedMacro(def));
                            }
                            else
                            {
                                _parseErrors.Add(new ParseErrorData(def, ErrorCode.ERR_PreProcessorError, "Identifier expected"));
                            }
                            SkipToEol();
                        }
                        else {
                            defStates.Push(false);
                        }
                        break;
                    case XSharpLexer.PP_IFNDEF:
                        if (IsActiveElseSkip())
                        { 
                            Consume();
                            SkipHidden();
                            var def = Lt();
                            writeToPPO(def, "// #ifndef " + def.Text );
                            if (XSharpLexer.IsIdentifier(def.Type) || XSharpLexer.IsKeyword(def.Type))
                            {
                                Consume();
                                SkipEmpty();
                                defStates.Push(!IsDefined(def.Text));
                            }
                            else if (def.Type == XSharpLexer.MACRO)
                            {
                                Consume();
                                SkipEmpty();
                                defStates.Push(!IsDefinedMacro(def));
                            }
                            else
                            {
                                _parseErrors.Add(new ParseErrorData(def, ErrorCode.ERR_PreProcessorError, "Identifier expected"));
                            }
                            SkipToEol();
                        }
                        else {
                            defStates.Push(false);
                        }
                        break;
                    case XSharpLexer.PP_ENDIF:
                        if (defStates.Count > 0)
                        {
                            defStates.Pop();
                            if (IsActiveElseSkip())
                            {
                                writeToPPO(Lt(), "// #endif");
                                Consume();
                                SkipEmpty();
                                SkipToEol();
                            }
                        }
                        else
                        {
                            _parseErrors.Add(new ParseErrorData(Lt(), ErrorCode.ERR_PreProcessorError, "Unexpected #endif"));
                            SkipToEol();
                        }
                        break;
                    case XSharpLexer.PP_ELSE:
                        writeToPPO(Lt(), "// #else");
                        if (defStates.Count > 0)
                        {
                            bool a = defStates.Pop();
                            if (IsActiveElseSkip())
                            {
                                Consume();
                                defStates.Push(!a);
                                SkipEmpty();
                                SkipToEol();
                            }
                            else
                                defStates.Push(false);
                        }
                        else
                        {
                            _parseErrors.Add(new ParseErrorData(Lt(), ErrorCode.ERR_PreProcessorError, "Unexpected #else"));
                            SkipToEol();
                        }
                        break;
                    case XSharpLexer.PP_LINE:
                        if (IsActiveElseSkip()) {
                            Consume();
                            SkipHidden();
                            var ln = Lt();
                            if (ln.Type == XSharpLexer.INT_CONST)
                            {
                                Consume();
                                inputs.MappedLineDiff = (int)ln.SyntaxLiteralValue(_options).Value - (ln.Line + 1);
                                writeToPPO(ln, "// #line " + ln.Text);
                                SkipHidden();
                                ln = Lt();
                                if (ln.Type == XSharpLexer.STRING_CONST)
                                {
                                    Consume();
                                    inputs.SourceFileName = ln.Text.Substring(1, ln.Text.Length - 2);
                                    writeToPPO(ln);

                                }
                                else
                                {
                                    _parseErrors.Add(new ParseErrorData(ln, ErrorCode.ERR_PreProcessorError, "String literal expected"));
                                }
                                SkipEmpty();
                            }
                            else
                            {
                                _parseErrors.Add(new ParseErrorData(ln, ErrorCode.ERR_PreProcessorError, "Integer literal expected"));
                            }
                            SkipToEol();
                        }
                        break;
                    case XSharpLexer.PP_ERROR:
                        if (IsActiveElseSkip())
                        {
                            int iStart = Lt().StopIndex+1;
                            Consume();
                            SkipHidden();
                            var ln = SkipToEol();
                            int iEnd = ln.StartIndex;
                            var text = ln.TokenSource.InputStream.GetText(new Interval(iStart, iEnd - 1));
                            _parseErrors.Add(new ParseErrorData(ln, ErrorCode.ERR_UserError, text));
                        }
                        break;
                    case XSharpLexer.PP_WARNING:
                        if (IsActiveElseSkip())
                        {
                            int iStart = Lt().StopIndex + 1;
                            Consume();
                            SkipHidden();
                            var ln = SkipToEol();
                            int iEnd = ln.StartIndex;
                            var text = ln.TokenSource.InputStream.GetText(new Interval(iStart, iEnd - 1));
                            _parseErrors.Add(new ParseErrorData(ln, ErrorCode.WRN_UserWarning, text));
                        }
                        break;
                    case XSharpLexer.PP_INCLUDE:
                        if (IsActiveElseSkip())
                        {
                            writeToPPO(Lt(), "// #include ");
                            if (IncludeDepth() == MaxIncludeDepth)
                            {
                                _parseErrors.Add(new ParseErrorData(Lt(), ErrorCode.ERR_PreProcessorError, "Reached max include depth: " + MaxIncludeDepth));
                                SkipToEol();
                            }
                            else {
                                Consume();
                                SkipHidden();
                                var ln = Lt();
                                writeToPPO(ln);
                                if (ln.Type == XSharpLexer.STRING_CONST)
                                {
                                    Consume();
                                    string fn = ln.Text.Substring(1, ln.Text.Length - 2);
                                    lock(includecache)
                                    {
                                        ProcessIncludeFile(ln, fn);
                                    }

                                }
                                else
                                {
                                    _parseErrors.Add(new ParseErrorData(ln, ErrorCode.ERR_PreProcessorError, "String literal expected"));
                                    SkipToEol();
                                }
                            }
                        }
                        break;
                    case XSharpLexer.PP_COMMAND:
                    case XSharpLexer.PP_TRANSLATE:
                        addRule(nextType);
                        break;
                    case XSharpLexer.PP_ENDREGION:
                    case XSharpLexer.PP_REGION:
                        if (IsActiveElseSkip())
                            SkipToEol();
                        break;
                    default:
                        var t = Lt();
                        if (IsActive())
                        {
                            IList<IToken> tl;
                            if ((XSharpLexer.IsIdentifier(t.Type) || XSharpLexer.IsKeyword(t.Type)) && symbolDefines.TryGetValue(t.Text, out tl)
                                && isDefineAllowed())
                            {
                                Consume();
                                if (tl != null)
                                {
                                    if (XSharpLexer.IsKeyword(t.Type))
                                    {
                                        ((CommonToken)t).Type = XSharpLexer.ID;
                                    }
                                    if (SymbolDepth() == MaxSymbolDepth)
                                    {
                                        _parseErrors.Add(new ParseErrorData(Lt(), ErrorCode.ERR_PreProcessorError, "Reached max symbol replacement depth: " + MaxSymbolDepth));
                                    }
                                    else if (activeSymbols.Contains(t.Text))
                                    {
                                        _parseErrors.Add(new ParseErrorData(Lt(), ErrorCode.ERR_PreProcessorError, "Cyclic symbol replacement: " + t.Text));
                                    }
                                    else
                                    {
                                        var ts = new CommonTokenStream(new ListTokenSource(tl));
                                        ts.Fill();
                                        FixToken(t);
                                        InsertStream(null, ts, t);
                                    }
                                }
                            }
                            else
                            {
                                if (t.Type == XSharpLexer.MACRO)
                                {
                                    Func<IToken> ft;
                                    if (macroDefines.TryGetValue(t.Text, out ft))
                                    {
                                        var nt = ft();
                                        if (t == null) {
                                            break;
                                        }
                                        ((CommonToken)nt).Line = t.Line;
                                        ((CommonToken)nt).Column = t.Column;
                                        ((CommonToken)nt).StartIndex = t.StartIndex;
                                        ((CommonToken)nt).StopIndex = t.StopIndex;
                                        t = nt;
                                    }
                                    else
                                    {
                                        ((CommonToken)t).Type = XSharpLexer.ID;
                                    }
                                }
                                if (inputs.isSymbol)
                                {
                                    t = new CommonToken(t);
                                }
                                FixToken(t);
                                Consume();
                                lastToken = t;
                                writeToPPO(t);
                                return t;
                            }
                        }
                        else
                        {
                            // Token suppressed by Preprocessor
                            ((CommonToken)t).Channel = XSharpLexer.DEFOUT;
                            Consume();
                            if (t.Type == XSharpLexer.EOS)
                                writeToPPO(t);
                        }
                        break;
                }
            }
        }
    }
}
