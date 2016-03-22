using System;
using System.Collections.Generic;
using System.Collections.Concurrent;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;
using InternalSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax;
using Antlr4.Runtime;
using Antlr4.Runtime.Atn;
using Antlr4.Runtime.Misc;
using Antlr4.Runtime.Tree;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;

namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
    internal class XSharpPreprocessor: ITokenSource
    {
        class InputState
        {
            internal ITokenStream Tokens;
            internal int Index;
            internal string SourceFileName;
            internal string MappedFileName;
            internal int MappedLineDiff;
            internal bool isSymbol;
            internal string Symbol;
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

        CSharpParseOptions _options;

        ITokenStream _input;

        Encoding _encoding;

        SourceHashAlgorithm _checksumAlgorithm;

        IList<ParseErrorData> _parseErrors;

        IEnumerable<string> includeDirs;

        Dictionary<string, IList<IToken>> symbolDefines = new Dictionary<string, IList<IToken>> (/*CaseInsensitiveComparison.Comparer*/);

        Dictionary<string, Func<IToken>> macroDefines = new Dictionary<string, Func<IToken>>(/*CaseInsensitiveComparison.Comparer*/);

        Stack<bool> defStates = new Stack<bool> ();

        InputState inputs;

        HashSet<string> activeSymbols = new HashSet<string>(/*CaseInsensitiveComparison.Comparer*/);

        internal Dictionary<string, SourceText> IncludedFiles = new Dictionary<string, SourceText>();

        public int MaxIncludeDepth { get; set; } = 16;

        public int MaxSymbolDepth { get; set; } = 16;

        internal XSharpPreprocessor(ITokenStream input, CSharpParseOptions options, string fileName, Encoding encoding, SourceHashAlgorithm checksumAlgorithm, IList<ParseErrorData> parseErrors)
        {
            _options = options;
            _input = input;
            _encoding = encoding;
            _checksumAlgorithm = checksumAlgorithm;
            _parseErrors = parseErrors;
            includeDirs = new List<string>(options.IncludePaths) { System.IO.Path.GetDirectoryName(fileName) };
            inputs = new InputState(input);
            foreach (var symbol in options.PreprocessorSymbols)
                symbolDefines[symbol] = null;
            macroDefines.Add("__ARRAYBASE__", () => new CommonToken(XSharpLexer.INT_CONST,_options.ArrayZero ? "0" : "1"));
            //macroDefines.Add("__CLR2__", () => null);
            //macroDefines.Add("__CLR4__", () => null);
            //macroDefines.Add("__CLRVERSION__", () => null);
            //macroDefines.Add("__DATE__", () => null);
            //macroDefines.Add("__DATETIME__", () => null);
            if (_options.DebugEnabled)
                macroDefines.Add("__DEBUG__", () => new CommonToken(XSharpLexer.TRUE_CONST));
            //macroDefines.Add("__ENTITY__", () => null);
            macroDefines.Add("__FILE__", () => new CommonToken(XSharpLexer.STRING_CONST, '"'+(inputs.SourceFileName ?? fileName)+'"'));
            macroDefines.Add("__LINE__", () => new CommonToken(XSharpLexer.INT_CONST, inputs.Lt().Line.ToString()));
            //macroDefines.Add("__MODULE__", () => null);
            //macroDefines.Add("__SIG__", () => null);
            //macroDefines.Add("__SRCLOC__", () => null);
            //macroDefines.Add("__SYSDIR__", () => null);
            //macroDefines.Add("__TIME__", () => null);
            //macroDefines.Add("__UTCTIME__", () => null);
            //macroDefines.Add("__VERSION__", () => null);
            //macroDefines.Add("__WINDIR__", () => null);
            //macroDefines.Add("__WINDRIVE__", () => null);
            macroDefines.Add("__XSHARP__", () => new CommonToken(XSharpLexer.TRUE_CONST));
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

        void SkipToEol()
        {
            IToken t = Lt();
            while (t.Type != IntStreamConstants.Eof && t.Channel != TokenConstants.DefaultChannel)
            {
                Consume();
                if (t.Type == XSharpLexer.EOS && t.Text != ";")
                    break;
                t = Lt();
            }
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
        }

        CommonToken FixToken(IToken t)
        {
            if (inputs.MappedLineDiff != 0)
                ((CommonToken)t).MappedLine = t.Line + inputs.MappedLineDiff;
            if (!string.IsNullOrEmpty(inputs.MappedFileName))
                ((CommonToken)t).MappedFileName = inputs.MappedFileName;
            if (!string.IsNullOrEmpty(inputs.SourceFileName))
                ((CommonToken)t).SourceFileName = inputs.SourceFileName;
            return (CommonToken)t;
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
                    activeSymbols.Remove(inputs.Symbol);
                inputs = inputs.parent;
            }
        }

        void InsertStream(string filename, ITokenStream input, string symbol = null)
        {
            InputState s = new InputState(input);
            s.parent = inputs;
            s.SourceFileName = filename;
            s.Symbol = symbol;
            s.isSymbol = symbol != null;
            if (s.isSymbol)
            {
                activeSymbols.Add(s.Symbol);
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
                            if (XSharpLexer.IsIdentifier(def.Type) || XSharpLexer.IsKeyword(def.Type))
                            {
                                Consume();
                                if (symbolDefines.ContainsKey(def.Text))
                                {
                                    _parseErrors.Add(new ParseErrorData(def, ErrorCode.WRN_PreProcessorWarning, "Symbol redefined: " + def.Text));
                                }
                                symbolDefines[def.Text] = ConsumeList();
                            }
                            else
                            {
                                _parseErrors.Add(new ParseErrorData(def, ErrorCode.ERR_PreProcessorError, "Identifier expected"));
                            }
                            SkipToEol();
                        }
                        break;
                    case XSharpLexer.PP_UNDEF:
                        if (IsActiveElseSkip())
                        {
                            Consume();
                            SkipHidden();
                            var def = Lt();
                            if (XSharpLexer.IsIdentifier(def.Type) || XSharpLexer.IsKeyword(def.Type))
                            {
                                Consume();
                                SkipEmpty();
                                if (symbolDefines.ContainsKey(def.Text))
                                    symbolDefines.Remove(def.Text);
                                else
                                {
                                    _parseErrors.Add(new ParseErrorData(def, ErrorCode.WRN_PreProcessorWarning, "Symbol not defined: " + def.Text));
                                }
                            }
                            else
                            {
                                _parseErrors.Add(new ParseErrorData(def, ErrorCode.ERR_PreProcessorError, "Identifier expected"));
                            }
                            SkipToEol();
                        }
                        break;
                    case XSharpLexer.PP_IFDEF:
                        if (IsActiveElseSkip())
                        {
                            Consume();
                            SkipHidden();
                            var def = Lt();
                            if (XSharpLexer.IsIdentifier(def.Type) || XSharpLexer.IsKeyword(def.Type))
                            {
                                Consume();
                                SkipEmpty();
                                defStates.Push(symbolDefines.ContainsKey(def.Text));
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
                            if (XSharpLexer.IsIdentifier(def.Type) || XSharpLexer.IsKeyword(def.Type))
                            {
                                Consume();
                                SkipEmpty();
                                defStates.Push(!symbolDefines.ContainsKey(def.Text));
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
                                inputs.MappedLineDiff = (int)ln.SyntaxLiteralValue().Value - (ln.Line + 1);
                                SkipHidden();
                                ln = Lt();
                                if (ln.Type == XSharpLexer.STRING_CONST)
                                {
                                    Consume();
                                    inputs.SourceFileName = ln.Text.Substring(1, ln.Text.Length - 2);

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
                            Consume();
                            SkipHidden();
                            var ln = Lt();
                            if (ln.Type == XSharpLexer.STRING_CONST)
                            {
                                Consume();
                                _parseErrors.Add(new ParseErrorData(ln, ErrorCode.ERR_UserError, ln.Text.Substring(1,ln.Text.Length-2)));
                                SkipEmpty();
                            }
                            else
                            {
                                _parseErrors.Add(new ParseErrorData(ln, ErrorCode.ERR_PreProcessorError, "String literal expected"));
                            }
                            SkipToEol();
                        }
                        break;
                    case XSharpLexer.PP_WARNING:
                        if (IsActiveElseSkip())
                        {
                            Consume();
                            SkipHidden();
                            var ln = Lt();
                            if (ln.Type == XSharpLexer.STRING_CONST)
                            {
                                Consume();
                                _parseErrors.Add(new ParseErrorData(ln, ErrorCode.WRN_UserWarning, ln.Text.Substring(1, ln.Text.Length - 2)));
                                SkipEmpty();
                            }
                            else
                            {
                                _parseErrors.Add(new ParseErrorData(ln, ErrorCode.ERR_PreProcessorError, "String literal expected"));
                            }
                            SkipToEol();
                        }
                        break;
                    case XSharpLexer.PP_INCLUDE:
                        if (IsActiveElseSkip())
                        {
                            if (IncludeDepth() == MaxIncludeDepth)
                            {
                                _parseErrors.Add(new ParseErrorData(Lt(), ErrorCode.ERR_PreProcessorError, "Reached max include depth: " + MaxIncludeDepth));
                                SkipToEol();
                            }
                            else {
                                Consume();
                                SkipHidden();
                                var ln = Lt();
                                if (ln.Type == XSharpLexer.STRING_CONST)
                                {
                                    Consume();
                                    string fn = ln.Text.Substring(1, ln.Text.Length - 2);
                                    string nfp = null;
                                    SourceText text = null;
                                    Exception fileReadException = null;
                                    foreach (var p in includeDirs)
                                    {
                                        bool rooted = System.IO.Path.IsPathRooted(fn);
                                        string fp = rooted ? fn : System.IO.Path.Combine(p, fn);
                                        try
                                        {
                                            using (var data = PortableShim.FileStream.Create(fp, PortableShim.FileMode.Open, PortableShim.FileAccess.Read, PortableShim.FileShare.ReadWrite, bufferSize: 1, options: PortableShim.FileOptions.None))
                                            {
                                                nfp = (string)PortableShim.FileStream.Name.GetValue(data);
                                                if (!IncludedFiles.TryGetValue(nfp, out text))
                                                {
                                                    text = EncodedStringText.Create(data, _encoding, _checksumAlgorithm);
                                                    IncludedFiles.Add(nfp, text);
                                                }
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
                                    SkipEmpty();
                                    SkipToEol();
                                    if (nfp != null && text != null)
                                    {
                                        var stream = new AntlrInputStream(text.ToString());
                                        var lexer = new XSharpLexer(stream);
                                        var tokens = new CommonTokenStream(lexer);
                                        tokens.Fill();
                                        InsertStream(nfp, tokens);
                                    }
                                    else
                                    {
                                        _parseErrors.Add(new ParseErrorData(ln, ErrorCode.ERR_PreProcessorError, "Include file not found: '" + fn + "'"));
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
                            if ((XSharpLexer.IsIdentifier(t.Type) || XSharpLexer.IsKeyword(t.Type)) && symbolDefines.TryGetValue(t.Text, out tl))
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
                                        InsertStream(null, ts, t.Text);
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
                                return t;
                            }
                        }
                        else {
                            ((CommonToken)t).Channel = XSharpLexer.DEFOUT;
                            Consume();
                        }
                        break;
                }
            }
        }
    }
}
