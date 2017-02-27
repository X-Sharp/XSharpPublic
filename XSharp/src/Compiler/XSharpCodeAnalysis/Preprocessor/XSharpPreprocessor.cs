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
using System.Diagnostics;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;
using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using Antlr4.Runtime.Tree;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;

namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
    internal class XSharpPreprocessor : ITokenSource
    {
        const string PPOPrefix = "//PP ";
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
        internal static CachedIncludeFile  AddIncludeFile(string fileName, XSharpToken[] tokens, SourceText text)
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
                file.Tokens = tokens;
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
            internal XSharpToken Symbol;
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

            internal XSharpToken Lt()
            {
                if (Eof() && parent != null)
                    return parent.Lt();
                return (XSharpToken) Tokens.Get(Index);
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
            internal XSharpToken[] Tokens { get; set; }
            internal SourceText Text { get; set; }
            internal DateTime LastUsed { get; set; }
        }

        XSharpLexer _lexer;
        CSharpParseOptions _options;

        CommonTokenStream _input;

        Encoding _encoding;

        SourceHashAlgorithm _checksumAlgorithm;

        IList<ParseErrorData> _parseErrors;

        IList<string> includeDirs;

        Dictionary<string, IList<XSharpToken>> symbolDefines ;

        Dictionary<string, Func<XSharpToken>> macroDefines = new Dictionary<string, Func<XSharpToken>>(/*CaseInsensitiveComparison.Comparer*/);

        Stack<bool> defStates = new Stack<bool> ();

        string _fileName = null;
        InputState inputs;
        IToken lastToken = null;

        PPRuleDictionary cmdRules = new PPRuleDictionary();
        PPRuleDictionary transRules = new PPRuleDictionary();
        bool _hasrules = false;

        HashSet<string> activeSymbols = new HashSet<string>(/*CaseInsensitiveComparison.Comparer*/);

        System.IO.Stream ppoStream;

        internal Dictionary<string, SourceText> IncludedFiles = new Dictionary<string, SourceText>();

        public int MaxIncludeDepth { get; set; } = 16;

        public int MaxSymbolDepth { get; set; } = 16;

        public string StdDefs { get; set; } = string.Empty;
        private void initStdDefines(CSharpParseOptions options, string fileName)
        {
            // Note Macros such as __ENTITY__ and  __SIG__ are handled in the transformation phase
            macroDefines.Add("__ARRAYBASE__", () => new XSharpToken(XSharpLexer.INT_CONST, _options.ArrayZero ? "0" : "1"));
            macroDefines.Add("__CLR2__", () => new XSharpToken(XSharpLexer.STRING_CONST, "\"__CLR2__\""));
            macroDefines.Add("__CLR4__", () => new XSharpToken(XSharpLexer.STRING_CONST, "\"__CLR4__\""));
            macroDefines.Add("__CLRVERSION__", () => new XSharpToken(XSharpLexer.STRING_CONST, "\"__CLRVERSION__\""));
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
                default:
                    break;
            }
            macroDefines.Add("__ENTITY__", () => new XSharpToken(XSharpLexer.STRING_CONST, "\"__ENTITY__\""));  // Handled later in Transformation phase
            macroDefines.Add("__FILE__", () => new XSharpToken(XSharpLexer.STRING_CONST, '"' + (inputs.SourceFileName ?? fileName) + '"'));
            macroDefines.Add("__LINE__", () => new XSharpToken(XSharpLexer.INT_CONST, inputs.Lt().Line.ToString()));
            macroDefines.Add("__MODULE__", () => new XSharpToken(XSharpLexer.STRING_CONST, '"' + (inputs.SourceFileName ?? fileName) + '"'));
            macroDefines.Add("__SIG__", () => new XSharpToken(XSharpLexer.STRING_CONST, "\"__SIG__\"")); // Handled later in Transformation phase
            macroDefines.Add("__SRCLOC__", () => new XSharpToken(XSharpLexer.STRING_CONST, '"' + (inputs.SourceFileName ?? fileName) + " line " + inputs.Lt().Line.ToString() + '"'));
            macroDefines.Add("__SYSDIR__", () => new XSharpToken(XSharpLexer.STRING_CONST, '"' + options.SystemDir + '"'));
            macroDefines.Add("__TIME__", () => new XSharpToken(XSharpLexer.STRING_CONST, '"' + DateTime.Now.ToString("HH:mm:ss") + '"'));
            macroDefines.Add("__UTCTIME__", () => new XSharpToken(XSharpLexer.STRING_CONST, '"' + DateTime.Now.ToUniversalTime().ToString("HH:mm:ss") + '"'));
            macroDefines.Add("__VERSION__", () => new XSharpToken(XSharpLexer.STRING_CONST, '"' + global::XSharp.Constants.Version + '"'));
            macroDefines.Add("__WINDIR__", () => new XSharpToken(XSharpLexer.STRING_CONST, '"' + options.WindowsDir + '"'));
            macroDefines.Add("__WINDRIVE__", () => new XSharpToken(XSharpLexer.STRING_CONST, '"' + options.WindowsDir?.Substring(0, 2) + '"'));
            macroDefines.Add("__XSHARP__", () => new XSharpToken(XSharpLexer.TRUE_CONST));

            bool[] flags  = { options.vo1,  options.vo2, options.vo3, options.vo4, options.vo5, options.vo6, options.vo7, options.vo8,
                                options.vo9, options.vo10, options.vo11, options.vo12, options.vo13, options.vo14, options.vo15, options.vo16 };
            for (int iOpt = 0; iOpt < flags.Length; iOpt++)
            {
                string flagName = String.Format("__VO{0}__", iOpt + 1);
                if (flags[iOpt])
                    macroDefines.Add(flagName, () => new XSharpToken(XSharpLexer.TRUE_CONST));
                else
                    macroDefines.Add(flagName, () => new XSharpToken(XSharpLexer.FALSE_CONST));
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

        private void _writeToPPO(String text)
        {
            // do not call t.Text when not needed.
            var buffer = _encoding.GetBytes(text);
            ppoStream.Write(buffer, 0, buffer.Length);
        }

        private bool mustWriteToPPO(XSharpToken t)
        {
            return ppoStream != null && t != null && (t?.TokenSource?.SourceName == _fileName || inputs.isSymbol);
        }

        private void writeToPPO(XSharpToken t)
        {
            // do not call t.Text when not needed.
            if ( mustWriteToPPO(t))
            {
                _writeToPPO(t.Text);
            }
            
        }

        private void writeToPPO(XSharpToken t, string text)
        {
            if (mustWriteToPPO(t))
            {
                _writeToPPO(text);
            }
        }
        private void writeToPPO(IList<XSharpToken> tokens, bool prefix = false, bool prefixNewLines = false)
        {
            XSharpToken first = null;
            XSharpToken last = null;
            foreach (var t in tokens)
            {
                if (first == null)
                    first = t;
                last = t;
            }
            if (first != null)
            {
                if (mustWriteToPPO(first))
                {
                    var interval = new Interval(first.StartIndex, last.StopIndex);
                    string text = first.TokenSource.InputStream.GetText(interval);
                    if (prefixNewLines)
                    {
                        text = text.Replace("\n", "\n" + PPOPrefix);
                    }
                    if (prefix)
                    {
                        text = PPOPrefix + text;
                    }
                    _writeToPPO(text);
                }
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

        internal XSharpPreprocessor(XSharpLexer lexer, CommonTokenStream input, CSharpParseOptions options, string fileName, Encoding encoding, SourceHashAlgorithm checksumAlgorithm, IList<ParseErrorData> parseErrors)
        {
            ClearOldIncludes();
            _lexer = lexer;
            _options = options;
            _fileName = fileName;
            if (_options.VOPreprocessorBehaviour)
                symbolDefines = new Dictionary<string, IList<XSharpToken>>(CaseInsensitiveComparison.Comparer);
            else
                symbolDefines = new Dictionary<string, IList<XSharpToken>>(/* case sensitive */);
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
            XSharpToken t = Lt();
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
            XSharpToken t = Lt();
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

        XSharpToken FixToken(XSharpToken  token)
        {
            if (inputs.MappedLineDiff != 0)
                token.MappedLine = token.Line + inputs.MappedLineDiff;
            if (!string.IsNullOrEmpty(inputs.MappedFileName))
                token.MappedFileName = inputs.MappedFileName;
            if (!string.IsNullOrEmpty(inputs.SourceFileName))
                token.SourceFileName = inputs.SourceFileName;
            if (inputs.isSymbol)
            {
                token.SourceSymbol = GetSourceSymbol();
                token.SourceFileName = (token.SourceSymbol as XSharpToken).SourceFileName;
            }
            return token;
        }

        IList<XSharpToken> ReadPPCommand()
        {
            IList<XSharpToken> res = new List<XSharpToken>();
            IToken  t = Lt();
            while (t.Type != IntStreamConstants.Eof && t.Channel != TokenConstants.DefaultChannel)
            {
                if (t.IsEOS() && t.Text != ";")
                    break;
                if (t.Channel == XSharpLexer.PREPROCESSOR)
                {

                    var nt = FixToken(new XSharpToken(t));
                    nt.Channel = TokenConstants.DefaultChannel;
                    res.Add(nt);
                }
                Consume();
                t = Lt();
            }
            return res;
        }
        IList<XSharpToken> ReadLine()
        {
            IList<XSharpToken> res = new List<XSharpToken>();
            XSharpToken t = Lt();
            while (t.Type != IntStreamConstants.Eof )
            {
                if (t.IsEOS() && t.Text != ";")
                    break;
                var nt = FixToken(new XSharpToken(t));
                nt.Channel = TokenConstants.DefaultChannel;
                res.Add(nt);
                Consume();
                t = Lt();
            }
            return res;
        }

        IList<XSharpToken> PeekLine()
        {
            var res = new List<XSharpToken>(); ;
            int i = inputs.Index;
            IToken t = inputs.Tokens.Get(i);
            while (t.Type != IntStreamConstants.Eof)
            {
                if (t.IsEOS()&& t.Text != ";")
                    break;
                if (t.Channel == XSharpLexer.DefaultTokenChannel)
                {
                    var nt = FixToken(new XSharpToken(t));
                    nt.Channel = TokenConstants.DefaultChannel;
                    res.Add(nt);
                }
                i += 1;
                t = inputs.Tokens.Get(i);
            }
            return res;
        }

        int La()
        {
            return inputs.La();
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

        void InsertStream(string filename, ITokenStream input, XSharpToken symbol = null)
        {
            if ( _options.ShowDefs)
            {
                if (symbol != null)
                {
                    var tokens = new List<XSharpToken>();
                    for (int i = 0; i < input.Size-1; i++)
                    {
                        tokens.Add(new XSharpToken(input.Get(i)));
                    }
                    string text = tokens.AsString();
                    if (text.Length > 20)
                        text = text.Substring(0, 20) + "...";
                    DebugOutput("Input stack: Insert value of token Symbol {0}, {1} tokens => {2}", symbol.Text, input.Size-1, text);
                }
                else
                    DebugOutput("Input stack: Insert Stream {0}, # of tokens {1}", filename, input.Size-1);
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

        bool IsDefinedMacro(XSharpToken t)
        {
            return (t.Type == XSharpLexer.MACRO) ? macroDefines.ContainsKey(t.Text) : false;
        }

        void addDefine(XSharpToken def)
        {
            if (XSharpLexer.IsIdentifier(def.Type) || XSharpLexer.IsKeyword(def.Type))
            {
                Consume();
                writeToPPO(def, PPOPrefix+"#define " + def.Text + " ");
                var newtokens = ReadPPCommand();
                writeToPPO(newtokens, false, true);
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
                    DebugOutput("{0}:{1}, add define {2} => {3}", def.FileName(), def.Line, def.Text, newtokens.AsString() );
                }
            }
            else
            {
                _parseErrors.Add(new ParseErrorData(def, ErrorCode.ERR_PreProcessorError, "Identifier expected"));
            }

        }
        void removeDefine(XSharpToken def)
        {
            if (XSharpLexer.IsIdentifier(def.Type) || XSharpLexer.IsKeyword(def.Type))
            {
                Consume();
                writeToPPO(def, PPOPrefix + "#undef " + def.Text );
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
            writeToPPO(cmd, PPOPrefix + cmd.Text+" ");
            var udc = ReadPPCommand();
            writeToPPO(udc,false, true);

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
                if (token == XSharpLexer.PP_COMMAND )
                {
                    // COMMAND and XCOMMAND can only match from beginning of line
                    cmdRules.Add(rule);
                }
                else
                {
                    // TRANSLATE and XTRANSLATE can also match from beginning of line
                    cmdRules.Add(rule);
                    transRules.Add(rule);
                }
                if (_options.ShowDefs)
                {
                    DebugOutput("{0}:{1} add UDC {2}", cmd.FileName(), cmd.Line, udc.AsString());
                }
                _hasrules = true;
            }
//#else
//            _parseErrors.Add(new ParseErrorData(cmd, ErrorCode.ERR_PreProcessorError, "Directive '" + cmd.Text + "' not supported yet"));
//#endif

        }

        private bool ProcessIncludeFile(XSharpToken ln, string fn, bool StdDefine = false)
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
                lexer.TokenFactory = XSharpTokenFactory.Default;
                var tokens = new CommonTokenStream(lexer);
                stream.name = nfp;
                tokens.Fill();
                InsertStream(nfp, tokens);
                foreach (var e in lexer.LexErrors)
                {
                    _parseErrors.Add(e);
                }
                var clone = tokens.GetTokens().ToArrayXSharpToken();
                AddIncludeFile(nfp, clone, text);
            }
            else
            {
                // we have a file cache item with the Tokens etc
                // Create a stream from the cached text and tokens
                // Clone the tokens to avoid problems when concurrently using the tokens
                var clone = cachedFile.Tokens.ToIListIToken();
                var tokenSource = new ListTokenSource(clone, cachedFile.FileName);
                var tokenStream = new BufferedTokenStream(tokenSource);
                tokenStream.Sync(clone.Count);
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
            // DEFINE will not be accepted immediately after or before a DOT
            // So this will not be recognized:
            // #define Console
            // System.Console.WriteLine("zxc")
            // But this will , since there are spaces around the token
            // System. Console .WriteLine("zxc")

            if (lastToken != null)
            {
                if (lastToken.Type == XSharpLexer.DOT )
                    return false;
            }
            var index = inputs.Index;
            if (index < inputs.Tokens.Size)
            {
                var token = inputs.Tokens.Get(index + 1);
                if (token.Type == XSharpParser.DOT )
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
                            writeToPPO(def, PPOPrefix+"#ifdef " + def.Text);
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
                            writeToPPO(def, PPOPrefix+"#ifndef " + def.Text );
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
                                writeToPPO(Lt(), PPOPrefix+"#endif");
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
                        writeToPPO(Lt(), PPOPrefix + "#else");
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
                                writeToPPO(ln, PPOPrefix + "#line " + ln.Text);
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
                    case XSharpLexer.PP_WARNING:
                        if (IsActiveElseSkip())
                        {
                            int iStart = Lt().StopIndex + 1;
                            int iEnd;
                            var tokens = ReadLine();
                            string text;
                            XSharpToken ln;          
                            if (tokens.Count > 1)
                            {
                                ln = tokens[1];
                                iStart = ln.StartIndex;
                                iEnd = tokens[tokens.Count - 1].StopIndex;
                                text = tokens[0].TokenSource.InputStream.GetText(new Interval(iStart, iEnd ));
                                text = text.Trim();
                                ln = tokens[0];
                            }
                            else
                            {
                                ln = tokens[0];
                                text = "Empty warning clause";
                            }
                            writeToPPO(tokens,true);
                            if (nextType == XSharpLexer.PP_WARNING)
                                _parseErrors.Add(new ParseErrorData(ln, ErrorCode.WRN_UserWarning, text));
                            else
                                _parseErrors.Add(new ParseErrorData(ln, ErrorCode.ERR_UserError, text));
                        }
                        break;
                    case XSharpLexer.PP_INCLUDE:
                        if (IsActiveElseSkip())
                        {
                            writeToPPO(Lt(), PPOPrefix + "#include ");
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
                        var done = false;
                        if (IsActive())
                        {
                            IList<XSharpToken> tl;

                            // Now see if this matches a UDC rule.
                            // When it does we read the whole line and check if we can find a matching rule

                            if (_hasrules)
                            {
                                var line = PeekLine();
                                if (line.Count > 0)
                                {
                                    PPMatchRange[] matchInfo;
                                    PPRule rule = null;
                                    bool bCmd = false; ;
                                    if (lastToken != null && (lastToken.Type == XSharpLexer.EOS ||
                                        lastToken.Type == XSharpLexer.NL))
                                    {
                                        rule = cmdRules.FindMatchingRule(line, out matchInfo);
                                        bCmd = true;
                                    }
                                    else
                                    {
                                        rule = transRules.FindMatchingRule(line, out matchInfo);
                                    }
                                    if (rule != null)
                                    {
                                        var result = rule.Replace(line, matchInfo);
                                        if (bCmd && result.Count > 0 && result[0].Type == XSharpLexer.SYMBOL_CONST)
                                        {
                                            // adjust when needed
                                            CommonToken ct = result[0] as CommonToken;
                                            if (ct != null)
                                            {
                                                int newType;
                                                if (_lexer.SymIds.TryGetValue(ct.Text, out newType))
                                                {
                                                    ct.Type = newType;
                                                }
                                            }

                                        }
                                        line = ReadLine();
                                        // insert whitespace from start of line into result
                                        var ws = new List<IToken>();
                                        XSharpToken first = null;
                                        for (int i = 0; i < line.Count; i++)
                                        {
                                            if (line[i].Type == XSharpLexer.WS)
                                                ws.Add(line[i]);
                                            else
                                            {
                                                first = line[i];
                                                break;
                                            }
                                        }
                                        // insert in reverse order
                                        for (int i = ws.Count -1; i >= 0; i--)
                                        {
                                            var token = new XSharpToken(ws[i]);
                                            token.Channel = XSharpLexer.Hidden;
                                            result.Insert(0, token);
                                        }
                                        var ts = new CommonTokenStream(new ListTokenSource( result.ToIListIToken()));
                                        ts.Fill();
                                        InsertStream("UDC "+rule.Key, ts,first);
                                        done = true;
                                    }
                                }

                            }
                            if (! done && (XSharpLexer.IsIdentifier(t.Type) || XSharpLexer.IsKeyword(t.Type)))
                            {
                                if ( symbolDefines.TryGetValue(t.Text, out tl) && isDefineAllowed())
                                {
                                    Consume();
                                    XSharpToken p = new XSharpToken(t);
                                    if (tl != null)
                                    {
                                        if (XSharpLexer.IsKeyword(p.Type))
                                        {
                                            p.Type = XSharpLexer.ID;
                                        }
                                        if (SymbolDepth() == MaxSymbolDepth)
                                        {
                                            _parseErrors.Add(new ParseErrorData(Lt(), ErrorCode.ERR_PreProcessorError, "Reached max symbol replacement depth: " + MaxSymbolDepth));
                                        }
                                        else if (activeSymbols.Contains(p.Text))
                                        {
                                            _parseErrors.Add(new ParseErrorData(Lt(), ErrorCode.ERR_PreProcessorError, "Cyclic symbol replacement: " + p.Text));
                                        }
                                        else
                                        {
                                            var ts = new CommonTokenStream(new ListTokenSource(tl.ToIListIToken()));
                                            ts.Fill();
                                            FixToken(p);
                                            InsertStream(null, ts, p);
                                            done = true;
                                        }
                                    }
                                }
                            }
                            if (! done)
                            {
                                if (t.Type == XSharpLexer.MACRO)
                                {
                                    Func<XSharpToken> ft;
                                    if (macroDefines.TryGetValue(t.Text, out ft))
                                    {
                                        var nt = ft();
                                        if (t == null) {
                                            break;
                                        }
                                        nt.Line = t.Line;
                                        nt.Column = t.Column;
                                        nt.StartIndex = t.StartIndex;
                                        nt.StopIndex = t.StopIndex;
                                        t = nt;
                                    }
                                    else
                                    {
                                        t.Type = XSharpLexer.ID;
                                    }
                                }
                                FixToken(t);
                                Consume();
                                if (lastToken != null && lastToken.Type == XSharpLexer.EOS && t.Type == XSharpLexer.EOS)
                                {
                                   t.Channel = XSharpLexer.Hidden;
                                }
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
