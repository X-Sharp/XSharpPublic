//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
//#nullable disable
#define UDCSUPPORT
using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Diagnostics;
using System.Collections.Concurrent;
using System.Reflection;
using System.Runtime;
using System.Collections;
using System.Threading;
using XSharp.MacroCompiler.Syntax;

namespace XSharp.MacroCompiler.Preprocessor
{
    internal class XSharpPreprocessor
    {
        static Dictionary<string, string> embeddedHeaders = null;

        static void loadResources()
        {
            if (embeddedHeaders == null)
            {
                embeddedHeaders = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
                var asm = typeof(XSharpPreprocessor).GetTypeInfo().Assembly;
#if VSPARSER
                var strm = asm.GetManifestResourceStream("XSharp.VSParser.Preprocessor.StandardHeaders.resources");
#else
                var strm = asm.GetManifestResourceStream("LanguageService.CodeAnalysis.XSharp.Preprocessor.StandardHeaders.resources");
#endif
                var rdr = new System.Resources.ResourceReader(strm);
                foreach (DictionaryEntry item in rdr)
                {
                    embeddedHeaders.Add((string)item.Key, (string)item.Value);
                }
            }
        }

        const string PPOPrefix = "//PP ";

        #region IncludeCache
        internal class PPIncludeFile
        {
            static readonly ConcurrentDictionary<String, PPIncludeFile> cache = new ConcurrentDictionary<String, PPIncludeFile>(StringComparer.OrdinalIgnoreCase);

            internal static void ClearOldIncludes()
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

            internal static PPIncludeFile Get(string fileName)
            {
                PPIncludeFile file = null;
                if (cache.ContainsKey(fileName))
                {
                    if (cache.TryGetValue(fileName, out file))
                    {
                        DateTime timeStamp;
                        if (System.IO.File.Exists(fileName))
                            timeStamp = File.GetLastWriteTimeUtc(fileName);
                        else
                            timeStamp = DateTime.MinValue;
                        if (file.LastWritten != timeStamp)
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
            internal static PPIncludeFile Add(string fileName, IList<Token> tokens, string text, bool mustBeProcessed, ref bool newFile)
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
            internal IList<Token> Tokens { get; private set; }
            internal string Text { get; private set; }
            internal DateTime LastUsed { get; set; }
            internal bool MustBeProcessed { get; set; }

            internal PPIncludeFile(string name, IList<Token> tokens, string text, bool mustBeProcessed)
            {
                this.FileName = name;
                this.Text = text;
                this.LastUsed = DateTime.Now;
                if (File.Exists(this.FileName))
                {
                    this.LastWritten = File.GetLastWriteTimeUtc(this.FileName);
                }
                else
                {
                    this.LastWritten = DateTime.MinValue;
                }
                this.Tokens = new List<Token>(tokens);
                this.MustBeProcessed = mustBeProcessed;

            }
            internal PPIncludeFile Clone()
            {
                return new PPIncludeFile(FileName, Tokens, Text, MustBeProcessed); 
            }
        }

        #endregion

        class InputState
        {
            internal TokenSource Tokens;
            internal int Index;
            internal string SourceFileName;
            internal int MappedLineDiff;
            internal bool isSymbol;
            internal string SymbolName;
            internal Token Symbol;
            internal InputState parent;
            //internal string MappedFileName;
            //internal PPRule udc;

            internal InputState(TokenSource tokens)
            {
                Tokens = tokens;
                Index = 0;
                MappedLineDiff = 0;
                SourceFileName = null;
                parent = null;
                isSymbol = false;
            }

            internal TokenType La()
            {
                if (Eof() && parent != null)
                    return parent.La();
                return Tokens.Get(Index).type;
            }

            internal Token Lt()
            {
                if (Eof() && parent != null)
                    return parent.Lt();
                return (Token)Tokens.Get(Index);
            }

            internal bool Eof()
            {
                return Index >= Tokens.Size || Tokens.Get(Index).type == TokenType.EOF;
            }

            internal bool Consume()
            {
                if (Eof())
                    return false;
                Index++;
                return true;
            }
        }

        Lexer _lexer;
        MacroOptions _options;

        Encoding _encoding;

        bool _duplicateFile = false;

        IList<string> includeDirs;

        Dictionary<string, IList<Token>> symbolDefines;

        readonly Dictionary<string, Func<Token, Token>> macroDefines = new Dictionary<string, Func<Token, Token>>(StringComparer.OrdinalIgnoreCase);

        readonly Stack<bool> defStates = new Stack<bool>();
        readonly Stack<Token> regions = new Stack<Token>();
        readonly string _fileName = null;
        InputState inputs;
        Token lastToken = null;

        readonly PPRuleDictionary cmdRules = new PPRuleDictionary();
        readonly PPRuleDictionary transRules = new PPRuleDictionary();
        bool _hasCommandrules = false;
        bool _hasTransrules = false;
        int rulesApplied = 0;
        readonly int defsApplied = 0;
        readonly HashSet<string> activeSymbols = new HashSet<string>();

        readonly bool _preprocessorOutput = false;
        Stream _ppoStream;

        internal Dictionary<string, string> IncludedFiles = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);

        public int MaxIncludeDepth { get; set; } = 16;

        public int MaxSymbolDepth { get; set; } = 16;

        public int MaxUDCDepth { get; set; } = 256;

        public string StdDefs { get; set; } = string.Empty;
        private void initStdDefines(MacroOptions options, string fileName)
        {
            // Note Macros such as __ENTITY__ and  __SIG__ are handled in the transformation phase
            // Make sure you also update the MACROs in XSharpLexerCode.cs !
            macroDefines.Add("__ARRAYBASE__", (token) => new Token(TokenType.INT_CONST, _options.ArrayZero ? "0" : "1") { SourceSymbol = token });
/* nvk...
            if (_options.ClrVersion == 2)
                macroDefines.Add("__CLR2__", (token) => new Token(TokenType.TRUE_CONST, token));
            if (_options.ClrVersion == 4)
                macroDefines.Add("__CLR4__", (token) => new Token(TokenType.TRUE_CONST, token));
            macroDefines.Add("__CLRVERSION__", (token) => new Token(TokenType.STRING_CONST, "\"" + _options.ClrVersion.ToString() + ".0\"", token));
*/
            macroDefines.Add("__DATE__", (token) => new Token(TokenType.STRING_CONST, '"' + DateTime.Now.Date.ToString("yyyyMMdd") + '"') { SourceSymbol = token });
            macroDefines.Add("__DATETIME__", (token) => new Token(TokenType.STRING_CONST, '"' + DateTime.Now.ToString() + '"') { SourceSymbol = token });
            bool debug = false;
/* nvk...
#if VSPARSER
            if (options.PreprocessorSymbolsUpper.Contains("DEBUG"))
                debug = true;
            if (options.PreprocessorSymbolsUpper.Contains("NDEBUG"))
                debug = false;
#else
            if (options.PreprocessorSymbolNames.Contains((name) => name.ToUpper() == "DEBUG"))
                debug = true;
            if (options.PreprocessorSymbolNames.Contains((name) => name.ToUpper() == "NDEBUG"))
                debug = false;
#endif
*/
            if (debug)
            {
                macroDefines.Add("__DEBUG__", (token) => new Token(TokenType.TRUE_CONST) { SourceSymbol = token });
            }
            macroDefines.Add("__DIALECT__", (token) => new Token(TokenType.STRING_CONST, '"' + options.Dialect.ToString() + '"') { SourceSymbol = token });
            switch (_options.Dialect)
            {
                case XSharpDialect.Core:
                    macroDefines.Add("__DIALECT_CORE__", (token) => new Token(TokenType.TRUE_CONST) { SourceSymbol = token });
                    break;
                case XSharpDialect.VO:
                    macroDefines.Add("__DIALECT_VO__", (token) => new Token(TokenType.TRUE_CONST) { SourceSymbol = token });
                    macroDefines.Add("__VO__", (token) => new Token(TokenType.TRUE_CONST) { SourceSymbol = token });
                    break;
                case XSharpDialect.Vulcan:
                    macroDefines.Add("__DIALECT_VULCAN__", (token) => new Token(TokenType.TRUE_CONST) { SourceSymbol = token });
                    macroDefines.Add("__VULCAN__", (token) => new Token(TokenType.TRUE_CONST) { SourceSymbol = token });
                    break;
                case XSharpDialect.Harbour:
                    macroDefines.Add("__DIALECT_HARBOUR__", (token) => new Token(TokenType.TRUE_CONST) { SourceSymbol = token });
                    // Harbour always includes hbver.h . The macro is defined in that file.
                    //macroDefines.Add("__HARBOUR__", () => new Token(TokenType.TRUE_CONST));
                    break;
                case XSharpDialect.XPP:
                    macroDefines.Add("__DIALECT_XBASEPP__", (token) => new Token(TokenType.TRUE_CONST) { SourceSymbol = token });
                    macroDefines.Add("__XPP__", (token) => new Token(TokenType.STRING_CONST, '"' + global::XSharp.Constants.FileVersion + '"') { SourceSymbol = token });
                    break;
                case XSharpDialect.FoxPro:
                    macroDefines.Add("__DIALECT_FOXPRO__", (token) => new Token(TokenType.TRUE_CONST) { SourceSymbol = token });
                    break;
                default:
                    break;
            }
            macroDefines.Add("__ENTITY__", (token) => new Token(TokenType.STRING_CONST, "\"__ENTITY__\"") { SourceSymbol = token });  // Handled later in Transformation phase
            macroDefines.Add("__FILE__", (token) => new Token(TokenType.STRING_CONST, '"' + (inputs.SourceFileName ?? fileName) + '"') { SourceSymbol = token });
            macroDefines.Add("__LINE__", (token) => new Token(TokenType.INT_CONST, new SourceLocation(token.source.SourceText, token.start).Line.ToString()) { SourceSymbol = token });
            macroDefines.Add("__MODULE__", (token) => new Token(TokenType.STRING_CONST, '"' + (inputs.SourceFileName ?? fileName) + '"') { SourceSymbol = token });
            macroDefines.Add("__FUNCTION__", (token) => new Token(TokenType.STRING_CONST, "\"__FUNCTION__\"") { SourceSymbol = token }); // Handled later in Transformation phase
            macroDefines.Add("__FUNCTIONS__", (token) => new Token(TokenType.STRING_CONST, "\"__FUNCTIONS__\"") { SourceSymbol = token }); // Handled later in Transformation phase
            macroDefines.Add("__SIG__", (token) => new Token(TokenType.STRING_CONST, "\"__SIG__\"") { SourceSymbol = token }); // Handled later in Transformation phase
            macroDefines.Add("__SRCLOC__", (token) => new Token(TokenType.STRING_CONST, '"' + (inputs.SourceFileName ?? fileName) + " line " + new SourceLocation(token.source.SourceText, token.start).Line.ToString() + '"') { SourceSymbol = token });
//nvk...            macroDefines.Add("__SYSDIR__", (token) => new Token(TokenType.STRING_CONST, '"' + options.SystemDir + '"') { SourceSymbol = token });
            macroDefines.Add("__TIME__", (token) => new Token(TokenType.STRING_CONST, '"' + DateTime.Now.ToString("HH:mm:ss") + '"') { SourceSymbol = token });
            macroDefines.Add("__UTCTIME__", (token) => new Token(TokenType.STRING_CONST, '"' + DateTime.Now.ToUniversalTime().ToString("HH:mm:ss") + '"') { SourceSymbol = token });
            macroDefines.Add("__VERSION__", (token) => new Token(TokenType.STRING_CONST, '"' + global::XSharp.Constants.FileVersion + '"') { SourceSymbol = token });
//nvk...            macroDefines.Add("__WINDIR__", (token) => new Token(TokenType.STRING_CONST, '"' + options.WindowsDir + '"') { SourceSymbol = token });
//nvk...            macroDefines.Add("__WINDRIVE__", (token) => new Token(TokenType.STRING_CONST, '"' + options.WindowsDir?.Substring(0, 2) + '"') { SourceSymbol = token });
            macroDefines.Add("__XSHARP__", (token) => new Token(TokenType.TRUE_CONST) { SourceSymbol = token });
//nvk...            if (options.XSharpRuntime)
            {
                macroDefines.Add("__XSHARP_RT__", (token) => new Token(TokenType.TRUE_CONST) { SourceSymbol = token });
            }
/*nvk...
            bool[] flags = { options.vo1,  options.vo2, options.vo3, options.vo4, options.vo5, options.vo6, options.vo7, options.vo8,
                                options.vo9, options.vo10, options.vo11, options.vo12, options.vo13, options.vo14, options.vo15, options.vo16 };
            for (int iOpt = 0; iOpt < flags.Length; iOpt++)
            {
                string flagName = string.Format("__VO{0}__", iOpt + 1);
                macroDefines.Add(flagName, (token) => new Token(flags[iOpt] ? TokenType.TRUE_CONST : TokenType.FALSE_CONST, token));
            }
            macroDefines.Add("__XPP1__", (token) => new Token(options.xpp1 ? TokenType.TRUE_CONST : TokenType.FALSE_CONST, token));
            macroDefines.Add("__XPP2__", (token) => new Token(options.xpp2 ? TokenType.TRUE_CONST : TokenType.FALSE_CONST, token));
            macroDefines.Add("__FOX1__", (token) => new Token(options.fox1 ? TokenType.TRUE_CONST : TokenType.FALSE_CONST, token));
            macroDefines.Add("__FOX2__", (token) => new Token(options.fox2 ? TokenType.TRUE_CONST : TokenType.FALSE_CONST, token));
            if (!options.NoStdDef)
            {
                // Todo: when the compiler option nostddefs is not set: read XSharpDefs.xh from the XSharp Include folder,//
                // and automatically include it.
                // read XsharpDefs.xh
                StdDefs = options.StdDefs;
                ProcessIncludeFile(StdDefs, null);
            }
*/
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

        private void _writeToPPO(String text, bool addCRLF)
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
        private void writeToPPO(IList<Token> tokens, bool prefix = false, bool prefixNewLines = false)
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
                var bld = new StringBuilder(1024);
                if (prefix)
                {
                    bld.Append(PPOPrefix);
                }
                bool first = !prefix;
                foreach (var t in tokens)
                {
                    // Copy the trivia from the original first symbol on the line so the UDC has the proper indentlevel
                    if (first && t.SourceSymbol != null && t.SourceSymbol.HasTrivia && t.SourceSymbol.type == TokenType.UDC_KEYWORD)
                    {
                        bld.Append(t.SourceSymbol.TriviaAsText);
                    }
                    bld.Append(t.Text);
                    first = false;
                }
                if (prefixNewLines)
                {
                    bld.Replace("\n", "\n" + PPOPrefix);
                }
                _writeToPPO(bld.ToString(), true);
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
        internal void Error(string fileName, ErrorCode code, params object[] args)
        {
            if (!ErrorString.IsWarning(code))
                throw Compilation.Error(new SourceLocation() { FileName = fileName }, code, args);
        }
        internal void Error(Token token, ErrorCode code, params object[] args)
        {
            if (!ErrorString.IsWarning(code))
                throw token.Error(code, args);
        }

        internal XSharpPreprocessor(Lexer lexer, MacroOptions options, string fileName, Encoding encoding)
        {
            PPIncludeFile.ClearOldIncludes();
            _lexer = lexer;
            _options = options;
            _fileName = fileName;
            if (!_options.CaseSensitivePreprocessor)
            {
                symbolDefines = new Dictionary<string, IList<Token>>(StringComparer.OrdinalIgnoreCase);
            }
            else
            {
                symbolDefines = new Dictionary<string, IList<Token>>(/* case sensitive */);
            }
            _encoding = encoding;
            includeDirs = options.IncludePaths != null ? new List<string>(options.IncludePaths) : new List<string>();
            if (!string.IsNullOrEmpty(fileName) && File.Exists(fileName))
            {
                includeDirs.Add(Path.GetDirectoryName(fileName));
                var ppoFile = Path.ChangeExtension(fileName, ".ppo");
                try
                {
                    _ppoStream = null;
                    _preprocessorOutput = false; // nvk... _options.PreprocessorOutput;
                    if (Path.GetExtension(fileName).ToLower() == ".ppo")
                    {
                        _preprocessorOutput = false;
                    }
                    else
                    {
                        if (_preprocessorOutput)
                        {
                            _ppoStream = File.Create(ppoFile); // nvk... , "PPO file");
                        }
                        else if (File.Exists(ppoFile))
                        {
                            File.Delete(ppoFile);
                        }
                    }
                }
                catch (Exception e)
                {
                    Error(fileName, ErrorCode.PreProcessorError, "Error processing PPO file: " + e.Message);
                }
            }
            // Add default IncludeDirs;
            if (!string.IsNullOrEmpty(options.DefaultIncludeDir))
            {
                string[] paths = options.DefaultIncludeDir.Split(new[] { ';' }, StringSplitOptions.RemoveEmptyEntries);
                foreach (var path in paths)
                {
                    includeDirs.Add(path);
                }
            }

            inputs = new InputState(lexer.TokenSource);
            // Add defines from the command line.
            if (options.PreprocessorSymbols != null)
            {
                foreach (var symbol in options.PreprocessorSymbols)
                {
                    var tokens = new List<Token>();
                    symbolDefines[symbol] = tokens;
                }
            }

            initStdDefines(options, fileName);
        }

        internal void DebugOutput(string format, params object[] objects)
        {
/* nvk...
            if (_options.ConsoleOutput != null)
            {
                _options.ConsoleOutput.WriteLine("PP: " + format, objects);
            }
*/
        }

        /// <summary>
        /// Pre-processes the input stream. Reads #Include files, processes #ifdef commands and translations from #defines, macros and UDCs
        /// </summary>
        /// <returns>Translated input stream</returns>
        internal IList<Token> PreProcess()
        {
            var result = new List<Token>();
            Token t = Lt();
            List<Token> omitted = new List<Token>(); 
            while (t.type != TokenType.EOF)
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
                if (t.channel == Channel.DEFOUTCHANNEL)
                    result.Add(t);
            }
            doEOFChecks();
            return result;
        }

        static TokenType getFirstTokenType(IList<Token> line)
        {
            for (int i = 0; i < line.Count; i++)
            {
                switch (line[i].channel)
                {
                    case Channel.DEFOUTCHANNEL:
                    case Channel.PREPROCESSORCHANNEL:
                        return line[i].type;
                    default:
                        break;
                }
            }
            return TokenType.EOF;
        }
        IList<Token> ProcessLine(IList<Token> line)
        {
            Debug.Assert(line.Count > 0);
            var nextType = getFirstTokenType(line);
            switch (nextType)
            {
                case TokenType.PP_UNDEF:
                    doUnDefDirective(line);
                    line = null;
                    break;
                case TokenType.PP_IFDEF:
                    doIfDefDirective(line, true);
                    line = null;
                    break;
                case TokenType.PP_IFNDEF:
                    doIfDefDirective(line, false);
                    line = null;
                    break;
                case TokenType.PP_ENDIF:
                    doEndifDirective(line);
                    line = null;
                    break;
                case TokenType.PP_ELSE:
                    doElseDirective(line);
                    line = null;
                    break;
                case TokenType.PP_LINE:
                    doLineDirective(line);
                    line = null;
                    break;
                case TokenType.PP_ERROR:
                case TokenType.PP_WARNING:
                    doErrorWarningDirective(line);
                    line = null;
                    break;
                case TokenType.PP_INCLUDE:
                    doIncludeDirective(line);
                    line = null;
                    break;
                case TokenType.PP_COMMAND:
                case TokenType.PP_TRANSLATE:
                    doUDCDirective(line, true);
                    line = null;
                    break;
                case TokenType.PP_DEFINE:
                    doDefineDirective(line);
                    line = null;
                    break;
                case TokenType.PP_ENDREGION:
                    doEndRegionDirective(line);
                    line = null;
                    break;
                case TokenType.PP_REGION:
                    doRegionDirective(line);
                    line = null;
                    break;
                case TokenType.UDCSEP:
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
        IList<Token> ReadLine(IList<Token> omitted)
        {
            Debug.Assert(omitted != null);
            var res = new List<Token>();
            omitted.Clear();
            Token t = Lt();
            while (t.type != TokenType.EOF)
            {
                if (t.IsEOS() && t.Text != ";")
                    break;
                if (t.channel != Channel.HIDDENCHANNEL && t.channel != Channel.XMLDOCCHANNEL)
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

        Token GetSourceSymbol()
        {
            Token s = null;
            if (inputs.isSymbol)
            {
                var baseInputState = inputs;
                while (baseInputState.parent?.isSymbol == true)
                    baseInputState = baseInputState.parent;
                s = baseInputState.Symbol;
            }
            return s;
        }

        Token FixToken(Token token)
        {
//nvk...            if (inputs.MappedLineDiff != 0)
//nvk...                token.MappedLine = token.Location().Line + inputs.MappedLineDiff;
            //if (!string.IsNullOrEmpty(inputs.MappedFileName))
            //    token.MappedFileName = inputs.MappedFileName;
            //if (!string.IsNullOrEmpty(inputs.SourceFileName))
            //    token.SourceFileName = inputs.SourceFileName;
            if (inputs.isSymbol)
            {
                token.SourceSymbol = GetSourceSymbol();
                //token.SourceFileName = (token.SourceSymbol as Token).SourceFileName;
            }
            return token;
        }

        Token Lt()
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

        void InsertStream(string filename, TokenSource input, Token symbol )
        {
/* nvk...
            if (_options.ShowDefs)
            {
                if (symbol != null)
                {
                    var tokens = new List<Token>();
                    for (int i = 0; i < input.Size - 1; i++)
                    {
                        tokens.Add(new Token(input.Get(i)));
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
*/
            // Detect recursion
            var x = inputs;
            while (x != null)
            {
                if (string.Compare(x.SourceFileName , filename, true) == 0)
                {
                    Error(symbol, ErrorCode.PreProcessorError, "Recursive include file ("+filename+") detected",filename);
                    return;
                }
                x = x.parent;
            }
            InputState s = new InputState(input)
            {
                parent = inputs,
                SourceFileName = filename,
                SymbolName = symbol?.Text,
                Symbol = symbol,
                isSymbol = symbol != null
            };
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

        bool IsDefinedMacro(Token t)
        {
            return (t.type == TokenType.MACRO) ? macroDefines.ContainsKey(t.Text) : false;
        }
        static IList<Token> stripWs(IList<Token> line)
        {
            IList<Token> result = new List<Token>();
            foreach (var token in line)
            {
                if (token.channel == Channel.DEFOUTCHANNEL || token.channel == Channel.PREPROCESSORCHANNEL)
                {
                    result.Add(token);
                }
            }
            return result;
        }

        void addDefine(IList<Token> line, IList<Token> original)
        {
            // Check to see if the define contains a LPAREN, and there is no space in between them.
            // Then it is a pseudo function that we will store as a #xtranslate UDC
            // this returns a list that includes #define and the ID
            if (line.Count < 2)
            {
                var token = line[0];
                Error(token, ErrorCode.PreProcessorError, "Identifier expected");
                return;
            }
            // token 1 is the Identifier
            // other tokens are optional and may contain a value
            Token def = line[1];
            if (line.Count > 2)
            {
                var first = line[2];
                if (first.type == TokenType.LPAREN
                    && first.start == def.end)
                {
                    doUDCDirective(original, false);
                    return;
                }
            }
            if (def.IsIdentifier() || def.IsKeyword())
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
                    if (cOld == cNew && oldtokens?.Count > 0)
                    {
                        // check to see if the same file has been added twice
                        var oldToken = oldtokens[0];
                        var newToken = line[0];
                        if (oldToken.source.SourceName != newToken.source.SourceName || oldToken.Location().Line != newToken.Location().Line)
                        {
                            Error(def, ErrorCode.DuplicateDefineSame, def.Text);
                        }
                        else
                        {
                            if (! _duplicateFile)
                            {
                                _duplicateFile = true;
                                var includeName = newToken.source.SourceName;
                                var defText = def.Text;
                                if (inputs.Symbol != null)
                                {
                                    def = new Token(inputs.Symbol)
                                    {
                                        SourceSymbol = null
                                    };
                                }
                                Error(def, ErrorCode.PreProcessorWarning, "Duplicate define (" + defText + ") found because include file \""+includeName+"\" was included twice");
                            }
                        }
                    }
                    else
                    {
                        Error(def, ErrorCode.DuplicateDefineDiffWarning, def.Text, cOld, cNew);
                    }
                }
                symbolDefines[def.Text] = line;
/*nvk...                if (_options.ShowDefs)
                {
                    DebugOutput("{0}:{1} add DEFINE {2} => {3}", def.FileName(), def.Line, def.Text, line.AsString());
                } */
            }
            else
            {
                Error(def, ErrorCode.PreProcessorError, "Identifier expected");
                return;
            }
        }

        void removeDefine(IList<Token> line)
        {
            var errToken = line[0];
            bool ok = true;
            line = stripWs(line);
            if (line.Count < 2)
            {
                ok = false;
            }
            Token def = line[1];
            if (def.IsIdentifier() || def.IsKeyword())
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
                Error(errToken, ErrorCode.PreProcessorError, "Identifier expected");
            }
        }

        void doUDCDirective(IList<Token> udc, bool mustWrite)
        {
            Debug.Assert(udc?.Count > 0);
            if (!IsActive())
            {
                writeToPPO("");
                return;
            }
            if (mustWrite)
                writeToPPO(udc, true, true);
            udc = stripWs(udc);
            if (udc.Count < 3)
            {
                Error(udc[0], ErrorCode.PreProcessorError, "Invalid UDC: '" + udc.AsString() + "'");
                return;
            }
            var cmd = udc[0];
            PPErrorMessages errorMsgs;
            var rule = new PPRule(cmd, udc, out errorMsgs, _options);
            if (rule.Type == PPUDCType.None)
            {
                if (errorMsgs.Count > 0)
                {
                    foreach (var s in errorMsgs)
                    {
                        Error(s.Token, ErrorCode.PreProcessorError, s.Message);
                    }
                }
                else
                {
                    Error(cmd, ErrorCode.PreProcessorError, "Invalid directive '" + cmd.Text + "' (are you missing the => operator?)");
                }
            }
            else
            {
                if (cmd.type == TokenType.PP_COMMAND)
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
                    if (cmd.type == TokenType.PP_DEFINE)
                    {
                        rule.CaseInsensitive = _options.CaseSensitivePreprocessor == false;
                    }
                }
/* nvk...                if (_options.ShowDefs)
                {
                    DebugOutput("{0}:{1} add {2} {3}", cmd.FileName(), cmd.Line, cmd.type == TokenType.PP_DEFINE ? "DEFINE" : "UDC", rule.Name);
                } */
            }
        }
#if !VSPARSER
        private Exception readFileContents( string fp, out string nfp, out string text)
        {
            Exception ex = null;
            nfp = null;
            text = null;
            try
            {
                StreamReader sr = new StreamReader(fp, _encoding);

                using (var data = new FileStream(fp, FileMode.Open, FileAccess.Read, FileShare.ReadWrite, bufferSize: 1, options: FileOptions.None))
                {
                    nfp = data.Name;
                    try
                    {
                        var reader = new StreamReader(data, _encoding);
                        text = reader.ReadToEnd();
                    }
                    catch (Exception)
                    {
                        text = null;
                    }
                    if (text == null)
                    {
                        // Encoding problem ?
                        var reader = new StreamReader(data);
                        text = reader.ReadToEnd();
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
#endif
        private bool isObsoleteIncludeFile(string includeFileName, Token token)
        {
            string file = Path.GetFileName(includeFileName).ToLower();
            bool obsolete = false;
            string assemblyName = "";
            bool sdkdefs = _options.RuntimeAssemblies.HasFlag(RuntimeAssemblies.SdkDefines);
            switch (file)
            {
                case "errorcodes.vh":
                case "set.vh":
                case "set.ch":
                case "directry.ch":
                case "vosystemlibrary.vh":
                    obsolete = sdkdefs || _options.RuntimeAssemblies.HasFlag(RuntimeAssemblies.XSharpCore);
                    assemblyName = sdkdefs ? XSharpAssemblyNames.SdkDefines : XSharpAssemblyNames.XSharpCore;
                    break;
                case "voguiclasses.vh":
                    obsolete = sdkdefs || _options.RuntimeAssemblies.HasFlag(RuntimeAssemblies.VoGui);
                    assemblyName = sdkdefs ? XSharpAssemblyNames.SdkDefines : XSharpAssemblyNames.VoGui;
                    break;
                case "vointernetclasses.vh":
                    obsolete = sdkdefs || _options.RuntimeAssemblies.HasFlag(RuntimeAssemblies.VoInet);
                    assemblyName = sdkdefs ? XSharpAssemblyNames.SdkDefines : XSharpAssemblyNames.VoInet;
                    break;
                case "vorddclasses.vh":
                    obsolete = sdkdefs || _options.RuntimeAssemblies.HasFlag(RuntimeAssemblies.VoRdd);
                    assemblyName = sdkdefs ? XSharpAssemblyNames.SdkDefines : XSharpAssemblyNames.VoRdd;
                    break;
                case "voreportclasses.vh":
                    obsolete = sdkdefs || _options.RuntimeAssemblies.HasFlag(RuntimeAssemblies.VoReport);
                    assemblyName = sdkdefs ? XSharpAssemblyNames.SdkDefines : XSharpAssemblyNames.VoReport;
                    break;
                case "vosqlclasses.vh":
                    obsolete = sdkdefs || _options.RuntimeAssemblies.HasFlag(RuntimeAssemblies.VoSql);
                    assemblyName = sdkdefs ? XSharpAssemblyNames.SdkDefines : XSharpAssemblyNames.VoSql;
                    break;
                case "vosystemclasses.vh":
                    obsolete = sdkdefs || _options.RuntimeAssemblies.HasFlag(RuntimeAssemblies.VoSystem);
                    assemblyName = sdkdefs ? XSharpAssemblyNames.SdkDefines : XSharpAssemblyNames.VoSystem;
                    break;
                case "vowin32apilibrary.vh":
                    obsolete = sdkdefs || _options.RuntimeAssemblies.HasFlag(RuntimeAssemblies.VoWin32);
                    assemblyName = sdkdefs ? XSharpAssemblyNames.SdkDefines : XSharpAssemblyNames.VoWin32;
                    break;
                case "class.ch":
                    obsolete = _options.RuntimeAssemblies.HasFlag(RuntimeAssemblies.XSharpXPP);
                    assemblyName = XSharpAssemblyNames.XSharpXPP;
                    break;
                default:
                    obsolete = false;
                   break;
            }
            if (obsolete)
            {
                Error(token, ErrorCode.ObsoleteIncludeWarning, includeFileName, assemblyName+".dll");
            }
            return obsolete;
        }

        private bool ProcessIncludeFile(string includeFileName, Token fileNameToken)
        {
            string nfp = null;
            string text = null;
            Exception fileReadException = null;
            PPIncludeFile includeFile = null;
            if (isObsoleteIncludeFile(includeFileName,fileNameToken))
            {
                return true;
            }
            List<string> dirs = new List<string>() { Path.GetDirectoryName(_fileName) };
            foreach (var p in includeDirs)
            {
                dirs.Add(p);
            }
            if (includeFileName != null)
            { 
                var path = Path.GetDirectoryName(includeFileName);
                if (! String.IsNullOrEmpty(path) && !dirs.Contains(path))
                    dirs.Add(path);
            }
/*nvk...            if (_options.Verbose)
            {
                DebugOutput("Process include file: {0}", includeFileName);
            }*/
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
                    Error(fileNameToken, ErrorCode.PreProcessorError, "Error combining path " + p + " and filename  " + includeFileName + " " + e.Message);
                    continue;
                }
                if (File.Exists(fp))
                {
/*Nvk...                    if (_options.Verbose)
                    {
                        DebugOutput("Found include file on disk: {0}", fp);
                    }*/
                    includeFile = PPIncludeFile.Get(fp);
                    if (includeFile != null)
                    {
                        nfp = fp;
                        text = includeFile.Text;
/*nvk...                        if (_options.Verbose)
                        {
                            DebugOutput("Include file retrieved from cache: {0}", fp);
                        }*/
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
                loadResources();
                var baseName = Path.GetFileNameWithoutExtension(includeFileName).ToLower();
                if (embeddedHeaders.TryGetValue(baseName, out var source))
                {
                    var reader = new StreamReader(source);
                    text = reader.ReadToEnd();
                    nfp = includeFileName;
                }
            }
            if (nfp == null)
            {
                if (fileReadException != null)
                {
                    Error(fileNameToken, ErrorCode.PreProcessorError, "Error Reading include file '" + includeFileName + "': " + fileReadException.Message);
                }
                else
                {
                    Error(fileNameToken, ErrorCode.PreProcessorError, "Include file not found: '" + includeFileName + "'");
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
/*nvk...            if (_options.ShowIncludes )
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
            }*/

            if (includeFile == null)
            {
                // we have nfp and text with the file contents
                // now parse the stuff and insert in the cache
                var startTime = DateTime.Now;

                //var stream = new AntlrInputStream(text.ToString()) { name = nfp };
                var lexer = new Lexer(text, _options);
                var tokens = lexer.AllTokens();
/*nvk...                foreach (var e in lexer.LexErrors)
                {
                    addParseError(e);
                }*/
                var endTime = DateTime.Now;
/*nvk...                if (_options.Verbose)
                {
                    DebugOutput("Lexed include file {0} milliseconds", (endTime - startTime).Milliseconds);
                }*/
                bool newFile = false;
                includeFile = PPIncludeFile.Add(nfp, tokens, text, lexer.MustBeProcessed, ref newFile);
/*nvk...                if (_options.Verbose)
                {
                    if (newFile)
                    {
                        DebugOutput("Added include file to cache {0}", nfp);
                    }
                    else
                    {
                        DebugOutput("Bummer: include file was already added to cache by another thread {0}", nfp);
                    }
                }*/
            }
            nfp = includeFile.FileName;
            if (includeFile.MustBeProcessed || _lexer.HasPPIfdefs)
            {
                var clone = includeFile.Tokens.CloneArray();
                var tokenSource = new TokenSource(null);
                tokenSource.Tokens.AddRange(clone);
                tokenSource.SourceName = nfp;
                InsertStream(nfp, tokenSource, fileNameToken);
            }
            else
            {
/*nvk...                if (_options.Verbose)
                {
                    DebugOutput("Skipping Include File in Parse Mode {0} line {1}:", nfp, fileNameToken.Line);
                }*/
            }
            return true;

        }

        private bool IsDefined(string define, Token token)
        {
            // Handle /VO8 compiler option:
            // When /VO8 is active and the variable is defined and has a value of FALSE or a numeric value = 0
            // Then #ifdef is FALSE
            // otherwise #ifdef is TRUE
            // and when there is more than one token, then #ifdef is also TRUE
            bool isdefined= symbolDefines.ContainsKey(define);
            if (isdefined )
            {
                if (!_options.CaseSensitivePreprocessor)
                {
                    var value = symbolDefines[define];
                    if (value?.Count == 1)
                    {
                        var deftoken = value[0];
                        if (deftoken.type == TokenType.FALSE_CONST)
                        {
                            isdefined = false;
                        }
                        else if (deftoken.type == TokenType.INT_CONST)
                        {
                            isdefined = Convert.ToInt64(deftoken.Text) != 0;
                        }
                    }
                }
            }
            else
            {
                isdefined = macroDefines.ContainsKey(define);
                if (isdefined )
                {
                    if (!_options.CaseSensitivePreprocessor)
                    {
                        var value = macroDefines[define](token);
                        if (value != null)
                        {
                            if (value.type == TokenType.FALSE_CONST)
                            {
                                isdefined = false;
                            }
                            else if (value.type == TokenType.INT_CONST)
                            {
                                isdefined = Convert.ToInt64(value.Text) != 0;
                            }
                        }
                    }
                }
            }
            return isdefined;
        }

        private bool isDefineAllowed(IList<Token> line, int iPos)
        {
            // DEFINE will not be accepted immediately after or before a DOT
            // So this will not be recognized:
            // #define Console
            // System.Console.WriteLine("zxc")
            // But this will , since there are spaces around the token
            // System. Console .WriteLine("zxc")
            Debug.Assert(line?.Count > 0);
            if (iPos > 0 && line[iPos-1].type == TokenType.DOT )
            {
                return false;
            }
            if (iPos < line.Count-1)
            {
                var token = line[iPos + 1];
                if (token.type == TokenType.DOT )
                    return false;
            }
            return true;
        }
        #region Preprocessor Directives

        private void checkForUnexpectedPPInput(IList<Token> line, int nMax)
        {
            if (line.Count > nMax)
            {
                Error(line[nMax], ErrorCode.EndOfPPLineExpected);
            }
        }
        private void doRegionDirective(IList<Token> original)
        {
            var line = stripWs(original);
            if (line.Count < 2)
            {
                Error(line[0], ErrorCode.PreProcessorWarning, "Region name expected");
            }
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

        private void doEndRegionDirective(IList<Token> original)
        {
            var line = stripWs(original);
            Debug.Assert(line?.Count > 0);
            if (IsActive())
            {
                var token = line[0];
                if (regions.Count > 0)
                    regions.Pop();
                else
                    Error(token, ErrorCode.PreProcessorError, "#endregion directive without matching #region found");
                writeToPPO(original, true);
            }
            else
            {
                writeToPPO("");
            }
            // ignore comments after #endregion
            //checkForUnexpectedPPInput(line, 1);
        }

        private void doDefineDirective(IList<Token> original)
        {
            var line = stripWs(original);
            if (line.Count < 2)
            {
                Error(line[0], ErrorCode.PreProcessorError, "Identifier expected");
                return;
            }
            if (IsActive())
            {
                writeToPPO(original, true);
                addDefine(line, original);
            }
            else
            {
                writeToPPO("");
            }
        }

        private void doUnDefDirective(IList<Token> original)
        {
            var line = stripWs(original);
            if (line.Count < 2)
            {
                Error(line[0], ErrorCode.PreProcessorError, "Identifier expected");
                return;
            }
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

        private void doErrorWarningDirective(IList<Token> original)
        {
            var line = stripWs(original);
            TokenType nextType = line[0].type;
            if (IsActive())
            {
                string text;
                Token ln;
                ln = line[0];
                if (line.Count < 2)
                {
                    text = "<Empty message>";
                }
                else
                {

                    writeToPPO(original, true);
                    int start = line[1].start;
                    int end = line[line.Count - 1].end;
                    text = line[1].source.SourceText.Substring(start, end-start);
                }
                if (ln.SourceSymbol != null)
                    ln = ln.SourceSymbol;
                if (nextType == TokenType.PP_WARNING)
                    Error(ln, ErrorCode.WarningDirective, text);
                else
                    Error(ln, ErrorCode.ErrorDirective, text);
                lastToken = ln;
            }
            else
            {
                writeToPPO( "");
            }
        }

        private void doIfDefDirective(IList<Token> original, bool isIfDef)
        {
            var line = stripWs(original);
            if (line.Count < 2)
            {
                Error(line[0], ErrorCode.PreProcessorError, "Identifier expected");
                return;
            }
            if (IsActive())
            {
                var def = line[1];
                if (def.IsIdentifier() || def.IsKeyword())
                {
                    if (isIfDef)
                        defStates.Push(IsDefined(def.Text,def));
                    else
                        defStates.Push(!IsDefined(def.Text, def));
                }
                else if (def.type == TokenType.MACRO)
                {
                    if (isIfDef)
                        defStates.Push(IsDefinedMacro(def));
                    else
                        defStates.Push(!IsDefinedMacro(def));
                }
                else
                {
                    Error(def, ErrorCode.PreProcessorError, "Identifier expected");
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

        private void doElseDirective(IList<Token> original)
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
                Error(Lt(), ErrorCode.PreProcessorError, "Unexpected #else");
            }
            checkForUnexpectedPPInput(line, 1);
        }

        private void doEndifDirective(IList<Token> original)
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
                Error(Lt(), ErrorCode.PreProcessorError, "Unexpected #endif");
                writeToPPO(line, true);
            }
            checkForUnexpectedPPInput(line, 1);
        }

        private void doIncludeDirective(IList<Token> original)
        {
            var line = stripWs(original);
            if (line.Count < 2)
            {
                Error(line[0], ErrorCode.PreProcessorError, "Filename expected");
                return;
            }
            if (IsActive())
            {
                writeToPPO(original, true);
                if (IncludeDepth() == MaxIncludeDepth)
                {
                    Error(line[0], ErrorCode.PreProcessorError, "Reached max include depth: " + MaxIncludeDepth);
                }
                else
                {
                    var token = line[1];
                    if (token.type == TokenType.STRING_CONST)
                    {
                        string fileName = token.Text.Substring(1, token.Text.Length - 2);
                        ProcessIncludeFile(fileName, token);

                    }
                    else
                    {
                        Error(token, ErrorCode.PreProcessorError, "String literal expected");
                    }
                }
            }
            else
            {
                writeToPPO("");
            }
            checkForUnexpectedPPInput(line, 2);
        }

        private void doLineDirective(IList<Token> original)
        {
            var line = stripWs(original);
            Debug.Assert(line?.Count > 0);
            if (IsActive())
            {
                writeToPPO(original, true);
                var ln = line[1];
                if (ln.type == TokenType.INT_CONST)
                {
//nvk...                    inputs.MappedLineDiff = (int)ln.SyntaxLiteralValue(_options,null, null).Value - (ln.Line + 1);

                    if (line.Count > 2)
                    {
                        ln = line[2];
                    }
                    if (ln.type == TokenType.STRING_CONST)
                    {
                        inputs.SourceFileName = ln.Text.Substring(1, ln.Text.Length - 2);
                    }
                    else
                    {
                        Error(ln, ErrorCode.PreProcessorError, "String literal expected");
                    }
                }
                else
                {
                    Error(ln, ErrorCode.PreProcessorError, "Integer literal expected");
                }
            }
            else
            {
                writeToPPO("");
            }
            checkForUnexpectedPPInput(line, 3);
        }

        private void doUnexpectedUDCSeparator(IList<Token> line)
        {
            Debug.Assert(line?.Count > 0);
            var ln = line[0];
            writeToPPO(line, true);
            Error(ln, ErrorCode.PreProcessorError, "Unexpected UDC separator character found");
        }

        private IList<Token> doNormalLine(IList<Token> line, bool write2PPO = true)
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
                IList<Token> result = null;
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
//nvk...                        result = _lexer.ReclassifyTokens(result);
                    }
                }
            }
            else
            {
                for (int i = 0; i < line.Count; i++)
                {
                    var t = line[i];
//nvk...                    t.Original.channel = Channel.DEFOUTCHANNEL;
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

        private static IList<Token> copySource(IList<Token> line, int nCount)
        {
            var result = new List<Token>(line.Count);
            var temp = new Token[nCount];
            for (int i = 0; i < nCount; i++)
            {
                temp[i] = line[i];
            }
            result.AddRange(temp);
            return result;
        }
        private bool doProcessDefinesAndMacros(IList<Token> line, out IList<Token> result)
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
            IList<Token> tempResult = line;
            result = null;
            while (tempResult != null)
            {
                tempResult = null;
                // in a second iteration line will be the changed line
                for (int i = 0; i < line.Count; i++)
                {
                    var token = line[i];
                    IList<Token> deflist = null;
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
                                var t2 = new Token(t)
                                {
                                    channel = Channel.DEFOUTCHANNEL,
                                    SourceSymbol = token
                                };
                                tempResult.Add(t2);
                            }
                        }
                        else
                        {
                            // add a space so error messages look proper
                            var t2 = new Token(TokenType.WS, " <RemovedToken> ")
                            {
                                channel = Channel.HIDDENCHANNEL,
                                SourceSymbol = token
                            };
                            tempResult.Add(t2);
                        }
                    }
                    else if (token.type == TokenType.MACRO)
                    {
                        // Macros that cannot be found are changed to ID
                        Func<Token, Token> ft;
                        if (macroDefines.TryGetValue(token.Text, out ft))
                        {
                            var nt = ft(token);
                            if (nt != null)
                            {
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
            return hasChanged; 
        }

        private bool doProcessTranslates(IList<Token> line, out IList<Token> result)
        {
            Debug.Assert(line?.Count > 0);
            var temp = new List<Token>();
            temp.AddRange(line);
            result = new List<Token>();
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

        private List<IList<Token>> splitCommands(IList<Token> tokens, out IList<Token> separators)
        {
            var result = new List<IList<Token>>(10);
            var current = new List<Token>(tokens.Count);
            separators = new List<Token>();
            foreach (var t in tokens)
            {
                if (t.type == TokenType.EOS)
                {
                    current.TrimLeadingSpaces();
                    result.Add(current);
                    current = new List<Token>();
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
        private bool doProcessCommands(IList<Token> line, out IList<Token>  result)
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
                IList<Token> separators;
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
						    var res = ProcessLine(cmds[i]);
							if (res == null)
	                            cmds[i].Clear();
							else
								cmds[i] = res;

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
                }
            }
            if (usedRules.Count > 0)
            {
                // somerule => #Error 
                result.TrimLeadingSpaces();
                if (result[0].channel == Channel.PREPROCESSORCHANNEL)
                { 
                    result = ProcessLine(result);
                    if (result == null)
                    {
                        result = new List<Token>();
                    }
                }
                return true;
            }
            result = null;
            return false;
        }

        private void doEOFChecks()
        {
            if (defStates.Count > 0)
            {
                Error(Lt(), ErrorCode.EndifDirectiveExpected);
            }
            while (regions.Count > 0)
            {
                var token = regions.Pop();
                Error(token, ErrorCode.EndRegionDirectiveExpected);
            }
        }

        #endregion

        private List<Token> doReplace(IList<Token> line, PPRule rule, PPMatchRange[] matchInfo)
        {
            Debug.Assert(line?.Count > 0);
            var res = rule.Replace(line, matchInfo);
            rulesApplied += 1;
            var result = new List<Token>();
            result.AddRange(res);
/*nvk..            if (_options.Verbose)
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
            }*/
            return result;
        }
    }
}
