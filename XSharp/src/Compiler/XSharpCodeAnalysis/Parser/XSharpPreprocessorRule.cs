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
using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;
using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using Antlr4.Runtime.Tree;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using System.Diagnostics;
#if UDCSUPPORT
namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
    enum RuleType
    {
        None,
        Define = XSharpLexer.PP_DEFINE,                 // #define
        Command = XSharpLexer.PP_COMMAND,               // #command and #xcommand
        Translate = XSharpLexer.PP_TRANSLATE            // #translate and #xtranslate
    }
    enum RuleMarker
    {
        Token=1,
        MatchRegular = XSharpLexer.PP_MM_REGULAR,                // <idMarker>
        MatchList = XSharpLexer.PP_MM_LIST,                      // <idMarker,...>
        MatchRestricted = XSharpLexer.PP_MM_RESTRICTED,          // <idMarker:word list>
        MatchWild = XSharpLexer.PP_MM_WILD,                      // <*idMarker*>
        MatchExtended = XSharpLexer.PP_MM_EXTENDED,              // <(idMarker)> 
        MatchOptional = XSharpLexer.PP_MM_OPTIONAL,              // [....], can be nested

        ResultRegular = XSharpLexer.PP_RM_REGULAR,                // <idMarker> 
        ResultDumbStringify = XSharpLexer.PP_RM_DUMB_STRINGIFY,   // #<idMarker> 
        ResultNormalStringify = XSharpLexer.PP_RM_NORMAL_STRINGIFY,  // <"idMarker"> 
        ResultSmartStringify = XSharpLexer.PP_RM_SMART_STRINGIFY, // <(idMarker)>
        ResultBlockify = XSharpLexer.PP_RM_BLOCKIFY,              // <{idMarker}> 
        ResultLogify = XSharpLexer.PP_RM_LOGIFY,                  // <.idMarker.>
        ResultOptional = XSharpLexer.PP_RM_OPTIONAL               // [ ... ], can not be nested

    }

    [DebuggerDisplay("{GetDebuggerDisplay(), nq}")]

    internal class RuleElement
    {
        protected IToken _token;
        protected IToken[] _moretokens = null;
        protected bool _matched;
        protected RuleElement[] _children = null;
        protected RuleMarker _type;
        internal IToken Token { get { return _token; } }
        internal bool Optional { get { return _type == RuleMarker.ResultOptional || _type == RuleMarker.MatchOptional; } }
        internal bool HasChildren { get { return _children?.Length > 0; } }
        internal bool IsToken { get { return _type == RuleMarker.Token; } }
        internal IToken[] MoreTokens { get { return _moretokens; } set { _moretokens = value; } }
        internal bool Matched { get { return _matched; } set { _matched = value; } }
        internal RuleElement[] Children { get { return _children; } set { _children = value; } }
        internal RuleMarker Type { get { return _type; } }
        internal string GetDebuggerDisplay()
        {
            if (Children?.Length > 0)
                return "Optional [" + Children[0].GetDebuggerDisplay() + "]";
            else
                return _type.ToString() + " " + _token.Text;
        }

        internal bool IsMatched( ref  String  unmatched)
        {
            if (_type == RuleMarker.Token)
            {
                return true;
            }
            if (HasChildren)
            {
                for (int i = 0; i < Children.Length; i++)
                {
                    if (!Children[i].IsMatched( ref unmatched))
                        return false;
                }
                return true;
            }
            if (!Matched)
                unmatched = this.Token.Text;
            return Matched;
        }
        internal RuleElement(IToken token, RuleMarker type, bool matched = false)
        {
            _token = token;
            _type = type;
            _matched = matched;
        }

    }

    internal class XSharpPreprocessorRule
    {
        RuleType _type;
        RuleElement[] _matchtokens;
        RuleElement[] _resulttokens;
        Dictionary<String, RuleElement> _matchdict;
        internal IList<String> ErrorMessages;
        internal RuleType Type { get { return _type; } }
        internal XSharpPreprocessorRule(int type, IList<IToken> tokens)
        {
            _type = (RuleType)type;
            var ltokens = new IToken[tokens.Count];
            tokens.CopyTo(ltokens, 0);
            _matchdict = new Dictionary<string, RuleElement>();
            if (!SplitTokens(ltokens))
            {
                _type = RuleType.None;
            }
            _matchdict = null;
        }
        bool SplitTokens(IToken[] _tokens)
        {
            int iSeperatorPos = -1;
            for (int i = 0; i < _tokens.Length - 1; i++)
            {
                if (_tokens[i].Type == XSharpLexer.EQ &&
                    _tokens[i + 1].Type == XSharpLexer.GT)
                {
                    iSeperatorPos = i;
                    break;
                }
            }
            if (iSeperatorPos >= 0)
            {
                IToken[] _left;
                IToken[] _right;
                _left = new IToken[iSeperatorPos];
                _right = new IToken[_tokens.Length - iSeperatorPos - 2];
                Array.Copy(_tokens, _left, iSeperatorPos);
                Array.Copy(_tokens, iSeperatorPos + 2, _right, 0, _right.Length);
                _matchtokens = TokenizeLeft(_left);
                _resulttokens = TokenizeRight(_right);
                CheckMatchingTokens(_resulttokens);
                // check to see if everything has been matched

                String unmatched = String.Empty;
                foreach (var r in _resulttokens)
                {
                    if (!r.IsToken && !r.IsMatched(ref unmatched))
                    {
                        AddErrorMessage("Result Marker '" + unmatched + "' not found in match list");
                    }
                }
                return ErrorMessages == null || ErrorMessages.Count == 0;
            }
            return false;
        }
        void AddErrorMessage(string message)
        {
            if (ErrorMessages == null)
                ErrorMessages = new List<String>();
            ErrorMessages.Add(message);
        }
 
        void CheckMatchingTokens(RuleElement[] _results)
        {
            for (int r = 0; r < _results.Length; r++)
            {
                if (!_results[r].IsToken && ! _results[r].Matched)
                {
                    if (_matchdict.ContainsKey(_results[r].Token.Text.ToUpperInvariant()))
                    {
                        _results[r].Matched = true;
                    }
                }
                if (_results[r].HasChildren)
                {
                    CheckMatchingTokens(_results[r].Children);
                }
            }
        }
        RuleElement[] TokenizeLeft(IToken[] _left)
        {
            var result = new List<RuleElement>();
            List<IToken> more;
            IToken name;
            RuleElement element;
            for (int i = 0; i < _left.Length; i++)
            {
                var token = _left[i];
                switch (token.Type)
                {
                    case XSharpLexer.LT:
                        if (i < _left.Length - 2
                            && (_left[i + 1].Type == XSharpLexer.ID || XSharpLexer.IsKeyword(_left[i + 1].Type))
                            && _left[i + 2].Type == XSharpLexer.GT)
                        {
                            // <idMarker>
                            name = _left[i + 1];
                            element = new RuleElement(name, RuleMarker.MatchRegular);
                            result.Add(element);
                            if (!_matchdict.ContainsKey(name.Text.ToUpperInvariant()))
                            {
                                _matchdict.Add(name.Text.ToUpperInvariant(), element);
                            }

                            i += 2;
                        }
                        else if (i < _left.Length - 4
                            // <*idMarker*>
                            && _left[i + 1].Type == XSharpLexer.MULT
                            && (_left[i + 2].Type == XSharpLexer.ID || XSharpLexer.IsKeyword(_left[i + 2].Type))
                            && _left[i + 3].Type == XSharpLexer.MULT
                            && _left[i + 4].Type == XSharpLexer.GT)
                        {
                            name = _left[i + 2];
                            element = new RuleElement(name, RuleMarker.MatchWild);
                            result.Add(element);
                            if (!_matchdict.ContainsKey(name.Text.ToUpperInvariant()))
                            {
                                _matchdict.Add(name.Text.ToUpperInvariant(), element);
                            }

                            i += 4;
                        }
                        else if (i < _left.Length - 4
                            // <(idMarker)>
                            && _left[i + 1].Type == XSharpLexer.LPAREN
                            && (_left[i + 2].Type == XSharpLexer.ID || XSharpLexer.IsKeyword(_left[i + 2].Type))
                            && _left[i + 3].Type == XSharpLexer.RPAREN
                            && _left[i + 4].Type == XSharpLexer.GT)
                        {
                            name = _left[i + 2];
                            element = new RuleElement(name, RuleMarker.MatchExtended);
                            result.Add(element);
                            if (!_matchdict.ContainsKey(name.Text.ToUpperInvariant()))
                            {
                                _matchdict.Add(name.Text.ToUpperInvariant(), element);
                            }

                            i += 4;
                        }
                        else if (i < _left.Length - 6
                              && (_left[i + 1].Type == XSharpLexer.ID || XSharpLexer.IsKeyword(_left[i + 1].Type))
                              && _left[i + 2].Type == XSharpLexer.COMMA
                              && _left[i + 3].Type == XSharpLexer.DOT
                              && _left[i + 4].Type == XSharpLexer.DOT
                              && _left[i + 5].Type == XSharpLexer.DOT
                              && _left[i + 6].Type == XSharpLexer.GT)
                        {
                            // <idMarker,...>
                            name = _left[i + 1];
                            element = new RuleElement(name, RuleMarker.MatchList);
                            result.Add(element);
                            if (!_matchdict.ContainsKey(name.Text.ToUpperInvariant()))
                            {
                                _matchdict.Add(name.Text.ToUpperInvariant(), element);
                            }

                            i += 6;
                        }
                        else if (i < _left.Length - 3
                              && (_left[i + 1].Type == XSharpLexer.ID || XSharpLexer.IsKeyword(_left[i + 1].Type))
                              && _left[i + 2].Type == XSharpLexer.COLON
                              && (_left[i + 3].Type == XSharpLexer.ID || XSharpLexer.IsKeyword(_left[i + 3].Type)))
                        {
                            // <idMarker:word list>
                            name = _left[i + 1];
                            i += 3;
                            more = new List<IToken>();
                            while (i <_left.Length && _left[i].Type != XSharpLexer.GT)
                            {
                                more.Add(_left[i]);
                                i++;
                            }
                            element = new RuleElement(name, RuleMarker.MatchRestricted);
                            result.Add(element);
                            if (!_matchdict.ContainsKey(name.Text.ToUpperInvariant()))
                            {
                                _matchdict.Add(name.Text.ToUpperInvariant(), element);
                            }
                            var tokens = new IToken[more.Count];
                            more.CopyTo(tokens, 0);
                            element.MoreTokens = tokens;
                            result.Add(element);
                        }
                        break;
                    case XSharpLexer.LBRKT:
                        /*
                         * eat block nested
                         * [...]
                         */
                        more = new List<IToken>();
                        i++;
                        while (i < _left.Length && _left[i].Type != XSharpLexer.RBRKT)
                        {
                            more.Add(_left[i]);
                            i++;
                        }
                        // skip the closing RBRKT
                        if (i == _left.Length)
                        {
                            // throw an error 'missing RBRKT'
                        }
                        element = new RuleElement(token, RuleMarker.MatchOptional);
                        element.Children = TokenizeLeft(more.ToArray());
                        result.Add(element);
                        // Optinal tokens do not go into the dict. Thir children will go
                        break;
                    default:
                        result.Add(new RuleElement(token, RuleMarker.Token));
                        break;
                }

            }
            var matchtokens = new RuleElement[result.Count];
            result.CopyTo(matchtokens);
            return matchtokens;
        }
        RuleElement[] TokenizeRight(IToken[] _right)
        {
            var result = new List<RuleElement>();
            List<IToken> more;
            IToken name;
            RuleElement element;

            for (int i = 0; i < _right.Length; i++)
            {
                var token = _right[i];
                switch (token.Type)
                {
                    case XSharpLexer.NEQ:
                        /*
                        * match #<idMarker> 
                        */
                        if (i < _right.Length - 3
                            && _right[i + 1].Type == XSharpLexer.LT
                            && (_right[i + 2].Type == XSharpLexer.ID || XSharpLexer.IsKeyword(_right[i + 3].Type))
                            && _right[i + 3].Type == XSharpLexer.GT)
                        {
                            name = _right[i + 2];
                            result.Add(new RuleElement(name, RuleMarker.ResultDumbStringify));
                            i += 3;
                        }
                        else
                        {
                            result.Add(new RuleElement(token, RuleMarker.Token));
                        }
                        break;
                    case XSharpLexer.LT:
                        if (i < _right.Length - 2
                            && (_right[i + 1].Type == XSharpLexer.ID || XSharpLexer.IsKeyword(_right[i + 1].Type))
                            && _right[i + 2].Type == XSharpLexer.GT)
                        {
                            // <idMarker>
                            name = _right[i + 1];
                            result.Add(new RuleElement(name, RuleMarker.ResultRegular));
                            i += 2;
                        }
                        if (i < _right.Length - 2
                            && _right[i + 1].Type == XSharpLexer.STRING_CONST
                            && _right[i + 2].Type == XSharpLexer.GT)
                        {
                            // <"idMarker"> 
                            name = _right[i + 1];
                            result.Add(new RuleElement(name, RuleMarker.ResultNormalStringify));
                            i += 2;
                        }
                        if (i < _right.Length - 4
                            && _right[i + 1].Type == XSharpLexer.LPAREN
                            && (_right[i + 2].Type == XSharpLexer.ID || XSharpLexer.IsKeyword(_right[i + 2].Type))
                            && _right[i + 3].Type == XSharpLexer.RPAREN
                            && _right[i + 4].Type == XSharpLexer.GT)
                        {
                            // <(idMarker)>
                            name = _right[i + 2];
                            result.Add(new RuleElement(name, RuleMarker.ResultSmartStringify));
                            i += 4;
                        }
                        if (i < _right.Length - 4
                            && _right[i + 1].Type == XSharpLexer.LCURLY
                            && (_right[i + 2].Type == XSharpLexer.ID || XSharpLexer.IsKeyword(_right[i + 2].Type))
                            && _right[i + 3].Type == XSharpLexer.RCURLY
                            && _right[i + 4].Type == XSharpLexer.GT)
                        {
                            // <{idMarker}> 
                            name = _right[i + 2];
                            result.Add(new RuleElement(name, RuleMarker.ResultBlockify));
                            i += 4;
                        }
                        if (i < _right.Length - 4
                            && _right[i + 1].Type == XSharpLexer.DOT
                            && (_right[i + 2].Type == XSharpLexer.ID || XSharpLexer.IsKeyword(_right[i + 2].Type))
                            && _right[i + 3].Type == XSharpLexer.DOT
                            && _right[i + 4].Type == XSharpLexer.GT)
                        {
                            // <.idMarker.>
                            name = _right[i + 2];
                            result.Add(new RuleElement(name, RuleMarker.ResultLogify));
                            i += 4;
                        }

                        break;
                    case XSharpLexer.LBRKT:
                        /*
                         * eat block 
                        * [ ... ], can not be nested
                         */
                        more = new List<IToken>();
                        i++;
                        while (i < _right.Length && _right[i].Type != XSharpLexer.RBRKT)
                        {
                            more.Add(_right[i]);
                            i++;
                        }
                        // skip the closing RBRKT
                        if (i == _right.Length)
                        {
                            // throw an error 'missing RBRKT'
                        }
                        element = new RuleElement(token, RuleMarker.ResultOptional);
                        element.Children = TokenizeRight(more.ToArray());
                        result.Add(element);
                        break;
                    default:
                        result.Add(new RuleElement(token, RuleMarker.Token));
                        break;
                }

            }
            var resulttokens = new RuleElement[result.Count];
            result.CopyTo(resulttokens);
            return resulttokens;

        }
    }
}
#endif