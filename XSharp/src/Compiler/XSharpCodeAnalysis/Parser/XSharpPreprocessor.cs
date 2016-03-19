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
            internal InputState parent;

            internal InputState(ITokenStream tokens)
            {
                Tokens = tokens;
                Index = 0;
                MappedLineDiff = 0;
                SourceFileName = null;
                parent = null;
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

        ITokenStream _input;

        Encoding _encoding;

        SourceHashAlgorithm _checksumAlgorithm;

        IList<ParseErrorData> _parseErrors;

        IEnumerable<string> includeDirs;

        Dictionary<string, List<IToken>> defines = new Dictionary<string, List<IToken>> (/*CaseInsensitiveComparison.Comparer*/);

        Stack<bool> defStates = new Stack<bool> ();

        InputState inputs;

        internal Dictionary<string, SourceText> IncludedFiles = new Dictionary<string, SourceText>();

        internal XSharpPreprocessor(ITokenStream input, IEnumerable<string> symbols, IEnumerable<string> IncludeDirs, Encoding encoding, SourceHashAlgorithm checksumAlgorithm, IList<ParseErrorData> parseErrors)
        {
            _input = input;
            _encoding = encoding;
            _checksumAlgorithm = checksumAlgorithm;
            _parseErrors = parseErrors;
            includeDirs = IncludeDirs;
            inputs = new InputState(input);
            foreach (var symbol in symbols)
                defines[symbol] = null;
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
                Consume();
                if (t.Channel == XSharpLexer.PREPROCESSOR)
                {
                    _parseErrors.Add(new ParseErrorData(t, ErrorCode.WRN_ParserWarning, "Ignored input '"+t.Text+"'"));
                }
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
                inputs = inputs.parent;
            }
        }

        void InsertStream(string filename, ITokenStream input)
        {
            InputState s = new InputState(input);
            s.parent = inputs;
            s.SourceFileName = filename;
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

        [return: NotNull]
        public IToken NextToken()
        {
            while (true)
            {
                switch (La())
                {
                    case IntStreamConstants.Eof:
                        if (defStates.Count > 0)
                        {
                            _parseErrors.Add(new ParseErrorData(Lt(), ErrorCode.ERR_ParserError, "'#endif' expected"));
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
                                SkipEmpty();
                                if (defines.ContainsKey(def.Text))
                                {
                                    _parseErrors.Add(new ParseErrorData(def, ErrorCode.WRN_ParserWarning, "Symbol redefined: " + def.Text));
                                }
                                defines[def.Text] = null;
                            }
                            else
                            {
                                _parseErrors.Add(new ParseErrorData(def, ErrorCode.ERR_ParserError, "Identifier expected"));
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
                                if (defines.ContainsKey(def.Text))
                                    defines.Remove(def.Text);
                                else
                                {
                                    _parseErrors.Add(new ParseErrorData(def, ErrorCode.WRN_ParserWarning, "Symbol not defined: " + def.Text));
                                }
                            }
                            else
                            {
                                _parseErrors.Add(new ParseErrorData(def, ErrorCode.ERR_ParserError, "Identifier expected"));
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
                                defStates.Push(defines.ContainsKey(def.Text));
                            }
                            else
                            {
                                _parseErrors.Add(new ParseErrorData(def, ErrorCode.ERR_ParserError, "Identifier expected"));
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
                                defStates.Push(!defines.ContainsKey(def.Text));
                            }
                            else
                            {
                                _parseErrors.Add(new ParseErrorData(def, ErrorCode.ERR_ParserError, "Identifier expected"));
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
                            _parseErrors.Add(new ParseErrorData(Lt(), ErrorCode.ERR_ParserError, "Unexpected #endif"));
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
                            _parseErrors.Add(new ParseErrorData(Lt(), ErrorCode.ERR_ParserError, "Unexpected #else"));
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
                                    _parseErrors.Add(new ParseErrorData(ln, ErrorCode.ERR_ParserError, "String literal expected"));
                                }
                                SkipEmpty();
                            }
                            else
                            {
                                _parseErrors.Add(new ParseErrorData(ln, ErrorCode.ERR_ParserError, "Integer literal expected"));
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
                                // TODO: show user-defined error
                                SkipEmpty();
                            }
                            else
                            {
                                _parseErrors.Add(new ParseErrorData(ln, ErrorCode.ERR_ParserError, "String literal expected"));
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
                                // TODO: show user-defined warning
                                SkipEmpty();
                            }
                            else
                            {
                                _parseErrors.Add(new ParseErrorData(ln, ErrorCode.ERR_ParserError, "String literal expected"));
                            }
                            SkipToEol();
                        }
                        break;
                    case XSharpLexer.PP_INCLUDE:
                        if (IsActiveElseSkip())
                        {
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
                                            text = EncodedStringText.Create(data, _encoding, _checksumAlgorithm);
                                            if (!IncludedFiles.ContainsKey(nfp))
                                            {
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
                                    _parseErrors.Add(new ParseErrorData(ln, ErrorCode.ERR_ParserError, "Include file not found: '"+fn+"'"));
                                }
                            }
                            else
                            {
                                _parseErrors.Add(new ParseErrorData(ln, ErrorCode.ERR_ParserError, "String literal expected"));
                                SkipToEol();
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
                        Consume();
                        {
                            int lineDiff = inputs.MappedLineDiff;
                            if (lineDiff != 0)
                            {
                                ((CommonToken)t).MappedLine = t.Line + lineDiff;
                            }
                            if (!string.IsNullOrEmpty(inputs.SourceFileName))
                            {
                                ((CommonToken)t).SourceFileName = inputs.SourceFileName;
                            }
                        }
                        if (IsActive())
                            return t;
                        else
                            ((CommonToken)t).Channel = XSharpLexer.DEFOUT;
                        break;
                }
            }
        }
    }
}
