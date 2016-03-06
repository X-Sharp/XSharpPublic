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
        ITokenStream _input;

        Dictionary<string, List<IToken>> defines = new Dictionary<string, List<IToken>> (/*CaseInsensitiveComparison.Comparer*/);

        Stack<bool> defStates = new Stack<bool> ();

        int currentIndex = 0;
        int lineDiff = 0;

        internal XSharpPreprocessor(ITokenStream input, IEnumerable<string> symbols)
        {
            _input = input;
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
            while (t.Type != IntStreamConstants.Eof && t.Channel != TokenConstants.DefaultChannel && t.Channel != XSharpLexer.PREPROCESSOR && t.Type != XSharpLexer.EOS)
            {
                Consume();
                t = Lt();
            }
        }

        void SkipPp()
        {
            IToken t = Lt();
            while (t.Type != IntStreamConstants.Eof && t.Channel != TokenConstants.DefaultChannel)
            {
                Consume();
                if (t.Type == XSharpLexer.EOS)
                    break;
                t = Lt();
            }
        }

        void SkipEmpty()
        {
            IToken t = Lt();
            while (t.Type != IntStreamConstants.Eof && t.Channel != TokenConstants.DefaultChannel)
            {
                Consume();
                if (t.Type == XSharpLexer.EOS)
                    break;
                if (t.Channel == XSharpLexer.PREPROCESSOR)
                {
                    // TODO: Warning
                }
                t = Lt();
            }
        }

        int La()
        {
            return _input.Get(currentIndex).Type;
        }

        IToken Lt()
        {
            return _input.Get(currentIndex);
        }

        void Consume()
        {
            currentIndex++;
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
                            // TODO: Error
                        }
                        return Lt();
                    case XSharpLexer.PP_DEFINE:
                        if (defStates.Count == 0 || defStates.Peek())
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
                                    // TODO: Warning
                                }
                                defines[def.Text] = null;
                            }
                            else
                            {
                                // TODO: Error
                            }
                        }
                        else
                            SkipPp();
                        break;
                    case XSharpLexer.PP_UNDEF:
                        if (defStates.Count == 0 || defStates.Peek())
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
                                    // TODO: Warning
                                }
                            }
                            else
                            {
                                // TODO: Error
                                SkipPp();
                            }
                        }
                        else
                            SkipPp();
                        break;
                    case XSharpLexer.PP_IFDEF:
                        if (defStates.Count == 0 || defStates.Peek())
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
                                // TODO: Error
                                SkipPp();
                            }
                        }
                        else {
                            defStates.Push(false);
                            SkipPp();
                        }
                        break;
                    case XSharpLexer.PP_IFNDEF:
                        if (defStates.Count == 0 || defStates.Peek())
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
                                // TODO: Error
                                SkipPp();
                            }
                        }
                        else {
                            defStates.Push(false);
                            SkipPp();
                        }
                        break;
                    case XSharpLexer.PP_ENDIF:
                        if (defStates.Count > 0)
                        {
                            Consume();
                            defStates.Pop();
                            SkipEmpty();
                        }
                        else
                        {
                            // TODO: error
                            SkipPp();
                        }
                        break;
                    case XSharpLexer.PP_ELSE:
                        if (defStates.Count > 0)
                        {
                            Consume();
                            defStates.Push(!defStates.Pop() && (defStates.Count == 0 || defStates.Peek()));
                            SkipEmpty();
                        }
                        else
                        {
                            // TODO: error
                            SkipPp();
                        }
                        break;
                    case XSharpLexer.PP_LINE:
                        if (defStates.Count == 0 || defStates.Peek()) {
                            Consume();
                            var ln = Lt();
                            if (ln.Type == XSharpLexer.INT_CONST)
                            {
                                Consume();
                                lineDiff = (int)ln.SyntaxLiteralValue().Value - (ln.Line + 1);
                                SkipEmpty();
                            }
                            else
                            {
                                // TODO: error
                                SkipPp();
                            }
                        }
                        else
                            SkipPp();
                        break;
                    case XSharpLexer.PP_ERROR:
                        if (defStates.Count == 0 || defStates.Peek())
                        {
                            Consume();
                            var ln = Lt();
                            if (ln.Type == XSharpLexer.STRING_CONST)
                            {
                                Consume();
                                // TODO: error
                                SkipEmpty();
                            }
                            else
                            {
                                // TODO: error
                                SkipPp();
                            }
                        }
                        else
                            SkipPp();
                        break;
                    case XSharpLexer.PP_WARNING:
                        if (defStates.Count == 0 || defStates.Peek())
                        {
                            Consume();
                            var ln = Lt();
                            if (ln.Type == XSharpLexer.STRING_CONST)
                            {
                                Consume();
                                // TODO: warning
                                SkipEmpty();
                            }
                            else
                            {
                                // TODO: error
                                SkipPp();
                            }
                        }
                        else
                            SkipPp();
                        break;
                    case XSharpLexer.PP_INCLUDE:
                        if (defStates.Count == 0 || defStates.Peek())
                        {
                            Consume();
                            var ln = Lt();
                            if (ln.Type == XSharpLexer.STRING_CONST)
                            {
                                Consume();
                                // TODO: load include file
                                SkipEmpty();
                            }
                            else
                            {
                                // TODO: error
                                SkipPp();
                            }
                        }
                        else
                            SkipPp();
                        break;
                    case XSharpLexer.PP_COMMAND:
                    case XSharpLexer.PP_TRANSLATE:
                    case XSharpLexer.PP_ENDREGION:
                    case XSharpLexer.PP_REGION:
                    default:
                        var t = Lt();
                        Consume();
                        if (lineDiff != 0)
                        {
                            ((CommonToken)t).Line = t.Line + lineDiff;
                        }
                        if (defStates.Count == 0 || defStates.Peek())
                            return t;
                        break;
                }
            }
        }
    }
}
