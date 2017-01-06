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
//#define DUMP_UDC
using System;
using System.Collections.Generic;
using Antlr4.Runtime;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using System.Diagnostics;

#if UDCSUPPORT

namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
    enum RuleType
    {
        None,
        Define  = XSharpLexer.PP_DEFINE,                 // #define
        Command = XSharpLexer.PP_COMMAND,                // #command and #xcommand
        Translate = XSharpLexer.PP_TRANSLATE             // #translate and #xtranslate
    }
    enum RuleMarker
    {
        Token=1,
        MatchRegular,                                           // <idMarker>
        MatchList ,                                             // <idMarker,...>
        MatchRestricted ,                                       // <idMarker:word list>
        MatchWild ,                                             // <*idMarker*>
        MatchExtended ,                                         // <(idMarker)> 
        MatchOptional ,                                         // [....], can be nested

        ResultRegular ,                                         // <idMarker> 
        ResultDumbStringify ,                                   // #<idMarker> 
        ResultNormalStringify ,                                 // <"idMarker"> 
        ResultSmartStringify ,                                  // <(idMarker)>
        ResultBlockify ,                                        // <{idMarker}> 
        ResultLogify ,                                          // <.idMarker.>
        ResultOptional                                          // [ ... ], can not be nested
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

    [DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
    internal class XSharpPreprocessorRule
    {
        RuleType _type;
        RuleElement[] _matchtokens;
        RuleElement[] _resulttokens;
        Dictionary<String, RuleElement> _matchdict;
        List<Tuple<IToken, String>> _errorMessages;
        internal IList<Tuple<IToken, String>> ErrorMessages { get { return _errorMessages; } }
        internal RuleType Type { get { return _type; } }
        internal XSharpPreprocessorRule(int type, IList<IToken> tokens)
        {
            _type = (RuleType)type;
            var ltokens = new IToken[tokens.Count];
            tokens.CopyTo(ltokens, 0);
            _matchdict = new Dictionary<string, RuleElement>(StringComparer.OrdinalIgnoreCase);
            if (!ParseRuleTokens(ltokens))
            {
                _type = RuleType.None;
            }
            _matchdict = null;
#if DUMP_UDC
            var t = tokens[0];
            Debug.WriteLine("UDC: {0} {1,3} {2} {3}", t.InputStream.SourceName, t.Line, Type,  this.GetDebuggerDisplay());
#endif
        }
        bool ParseRuleTokens(IToken[] _tokens)
        {
            int iSeperatorPos = -1;
            for (int i = 0; i < _tokens.Length - 1; i++)
            {
                // Must be => without whitespace
                if (_tokens[i].Type == XSharpLexer.EQ &&
                    _tokens[i + 1].Type == XSharpLexer.GT &&
                    _tokens[i].Column+1 == _tokens[i+1].Column)
                {
                    iSeperatorPos = i;
                    break;
                }
            }
            if (iSeperatorPos >= 0)
            {
                IToken[] _left = new IToken[iSeperatorPos];
                IToken[] _right = new IToken[_tokens.Length - iSeperatorPos - 2];
                Array.Copy(_tokens, _left, iSeperatorPos);
                Array.Copy(_tokens, iSeperatorPos + 2, _right, 0, _right.Length);
                _matchtokens = AnalyzeMatchTokens(_left);
                _resulttokens = AnalyzeResultTokens(_right);
                CheckMatchingTokens(_resulttokens);
                // Check to see if all result tokens have been matched
                // Unmatched Match tokens is fine (they may be deleted from the output)

                string unmatched = String.Empty;
                foreach (var r in _resulttokens)
                {
                    if (!r.IsToken && !r.IsMatched(ref unmatched))
                    {
                        AddErrorMessage(r.Token , "Result Marker '" + unmatched + "' not found in match list");
                        
                    }
                }
                return ErrorMessages == null || ErrorMessages.Count == 0;
            }
            return false;
        }
        void AddErrorMessage(IToken token, string message)
        {
            if (_errorMessages == null)
                _errorMessages = new List<Tuple<IToken,String>>();
            ErrorMessages.Add(new Tuple<IToken, String>( token, message));
        }
        void CheckMatchingTokens(RuleElement[] results)
        {
            for (int r = 0; r < results.Length; r++)
            {
                if (!results[r].IsToken && ! results[r].Matched)
                {
                    var token = results[r].Token;
                    var name = token.Text;
                    if (token.Type == XSharpLexer.STRING_CONST)
                        name = name.Substring(1, name.Length - 2);
                    if (_matchdict.ContainsKey(name))
                    {
                        results[r].Matched = true;
                    }
                }
                if (results[r].HasChildren)
                {
                    CheckMatchingTokens(results[r].Children);
                }
            }
        }
        void addToDict(RuleElement element)
        {
            if (isNameToken(element.Token))
            {
                string name = element.Token.Text;
                if (!_matchdict.ContainsKey(name))
                {
                    _matchdict.Add(name, element);
                }
            }

        }
        bool isNameToken(IToken token)
        {
            return token.Type == XSharpLexer.ID || XSharpLexer.IsKeyword(token.Type);
        }
        RuleElement[] AnalyzeMatchTokens(IToken[] matchTokens)
        {
            var result = new List<RuleElement>();
            var max = matchTokens.Length;
            List<IToken> more;
            IToken name;
            RuleElement element;
            for (int i = 0; i < max; i++)
            {
                CommonToken token = (CommonToken) matchTokens[i];
                switch (token.Type)
                {

                    case XSharpLexer.LT:
                        if (i < max - 2
                            && isNameToken(matchTokens[i + 1])
                            && matchTokens[i + 2].Type == XSharpLexer.GT)
                        {
                            // <idMarker>
                            name = matchTokens[i + 1];
                            element = new RuleElement(name, RuleMarker.MatchRegular);
                            result.Add(element);
                            addToDict(element);
                            i += 2;
                        }
                        else if (i < max - 4
                            // <*idMarker*>
                            && matchTokens[i + 1].Type == XSharpLexer.MULT
                            && isNameToken(matchTokens[i + 2])
                            && matchTokens[i + 3].Type == XSharpLexer.MULT
                            && matchTokens[i + 4].Type == XSharpLexer.GT)
                        {
                            name = matchTokens[i + 2];
                            element = new RuleElement(name, RuleMarker.MatchWild);
                            result.Add(element);
                            addToDict(element);

                            i += 4;
                        }
                        else if (i < max - 4
                            // <(idMarker)>
                            && matchTokens[i + 1].Type == XSharpLexer.LPAREN
                            && isNameToken(matchTokens[i + 2])
                            && matchTokens[i + 3].Type == XSharpLexer.RPAREN
                            && matchTokens[i + 4].Type == XSharpLexer.GT)
                        {
                            name = matchTokens[i + 2];
                            element = new RuleElement(name, RuleMarker.MatchExtended);
                            result.Add(element);
                            addToDict( element);

                            i += 4;
                        }
                        else if (i < max - 6
                              && isNameToken(matchTokens[i + 1])
                              && matchTokens[i + 2].Type == XSharpLexer.COMMA
                              && matchTokens[i + 3].Type == XSharpLexer.DOT
                              && matchTokens[i + 4].Type == XSharpLexer.DOT
                              && matchTokens[i + 5].Type == XSharpLexer.DOT
                              && matchTokens[i + 6].Type == XSharpLexer.GT)
                        {
                            // <idMarker,...>
                            name = matchTokens[i + 1];
                            element = new RuleElement(name, RuleMarker.MatchList);
                            result.Add(element);
                            addToDict(element);
                            i += 6;
                        }
                        else if (i < max - 3
                              && isNameToken(matchTokens[i + 1])
                              && matchTokens[i + 2].Type == XSharpLexer.COLON)
                        {
                            // <idMarker:word list>
                            name = matchTokens[i + 1];
                            i += 3;
                            more = new List<IToken>();
                            while (i <max && matchTokens[i].Type != XSharpLexer.GT)
                            {
                                token = (CommonToken)matchTokens[i];
                                more.Add(token);
                                i++;
                            }
                            element = new RuleElement(name, RuleMarker.MatchRestricted);
                            result.Add(element);
                            addToDict(element);
                            var tokens = new IToken[more.Count];
                            more.CopyTo(tokens, 0);
                            element.MoreTokens = tokens;
                        }
                        break;
                    case XSharpLexer.LBRKT:
                        /*
                         * eat block nested
                         * [...]
                         */
                        var lbrkt = token;
                        if ( i < max-1 )   // must have at least [ name ] 
                        {
                            more = new List<IToken>();
                            i++;
                            var nestlevel = 1;
                            while (i < max )
                            {
                                token = (CommonToken)matchTokens[i];
                                if (token.Type == XSharpLexer.LBRKT)
                                    ++nestlevel;
                                if (token.Type == XSharpLexer.RBRKT)
                                {
                                    --nestlevel;
                                    if (nestlevel == 0)
                                        break;
                                }
                                more.Add(matchTokens[i]);
                                i++;
                            }
                            // more contains everything between the brackets including the first token
                            if (nestlevel != 0)
                            {
                                if (nestlevel > 0)
                                {
                                    AddErrorMessage(lbrkt, "Closing bracket ']' missing");
                                }
                                else
                                {
                                    AddErrorMessage(lbrkt,"Closing bracket ']' found with missing '['");
                                }
                            }
                            name = more[0];
                            element = new RuleElement(name, RuleMarker.MatchOptional);
                            element.Children = AnalyzeMatchTokens(more.ToArray());    
                            result.Add(element);
                            addToDict(element);

                        }
                        else
                        {
                            if ( i == max -1)
                            {
                                token = (CommonToken) matchTokens[max];
                                if (token.Type == XSharpLexer.RBRKT)
                                    AddErrorMessage(lbrkt, "Empty Optional Token found");
                                else
                                    AddErrorMessage(lbrkt,"Closing bracket ']' missing");
                            }
                            else
                            {
                                AddErrorMessage(lbrkt, "Closing bracket ']' missing");
                            }

                        }
                        break;
                    case XSharpLexer.BACKSLASH: // escape next token
                        if (i < max)
                        {
                            i++;
                            token = (CommonToken)matchTokens[i];
                            result.Add(new RuleElement(token, RuleMarker.Token));
                        }
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
        RuleElement[] AnalyzeResultTokens(IToken[] resultTokens)
        {
            var result = new List<RuleElement>();
            List<IToken> more;
            IToken name;
            RuleElement element;

            for (int i = 0; i < resultTokens.Length; i++)
            {
                var token = resultTokens[i];
                switch (token.Type)
                {
                    case XSharpLexer.NEQ:
                        /*
                        * match #<idMarker> 
                        */
                        if (i < resultTokens.Length - 3
                            && resultTokens[i + 1].Type == XSharpLexer.LT
                            && (resultTokens[i + 2].Type == XSharpLexer.ID || XSharpLexer.IsKeyword(resultTokens[i + 3].Type))
                            && resultTokens[i + 3].Type == XSharpLexer.GT)
                        {
                            name = resultTokens[i + 2];
                            result.Add(new RuleElement(name, RuleMarker.ResultDumbStringify));
                            i += 3;
                        }
                        else
                        {
                            result.Add(new RuleElement(token, RuleMarker.Token));
                        }
                        break;
                    case XSharpLexer.LT:
                        if (i < resultTokens.Length - 2
                            && (resultTokens[i + 1].Type == XSharpLexer.ID || XSharpLexer.IsKeyword(resultTokens[i + 1].Type))
                            && resultTokens[i + 2].Type == XSharpLexer.GT)
                        {
                            // <idMarker>
                            name = resultTokens[i + 1];
                            result.Add(new RuleElement(name, RuleMarker.ResultRegular));
                            i += 2;
                        }
                        if (i < resultTokens.Length - 2
                            && resultTokens[i + 1].Type == XSharpLexer.STRING_CONST
                            && resultTokens[i + 2].Type == XSharpLexer.GT)
                        {
                            // <"idMarker"> 
                            name = resultTokens[i + 1];
                            result.Add(new RuleElement(name, RuleMarker.ResultNormalStringify));
                            i += 2;
                        }
                        if (i < resultTokens.Length - 4
                            && resultTokens[i + 1].Type == XSharpLexer.LPAREN
                            && (resultTokens[i + 2].Type == XSharpLexer.ID || XSharpLexer.IsKeyword(resultTokens[i + 2].Type))
                            && resultTokens[i + 3].Type == XSharpLexer.RPAREN
                            && resultTokens[i + 4].Type == XSharpLexer.GT)
                        {
                            // <(idMarker)>
                            name = resultTokens[i + 2];
                            result.Add(new RuleElement(name, RuleMarker.ResultSmartStringify));
                            i += 4;
                        }
                        if (i < resultTokens.Length - 4
                            && resultTokens[i + 1].Type == XSharpLexer.LCURLY
                            && (resultTokens[i + 2].Type == XSharpLexer.ID || XSharpLexer.IsKeyword(resultTokens[i + 2].Type))
                            && resultTokens[i + 3].Type == XSharpLexer.RCURLY
                            && resultTokens[i + 4].Type == XSharpLexer.GT)
                        {
                            // <{idMarker}> 
                            name = resultTokens[i + 2];
                            result.Add(new RuleElement(name, RuleMarker.ResultBlockify));
                            i += 4;
                        }
                        if (i < resultTokens.Length - 4
                            && resultTokens[i + 1].Type == XSharpLexer.DOT
                            && (resultTokens[i + 2].Type == XSharpLexer.ID || XSharpLexer.IsKeyword(resultTokens[i + 2].Type))
                            && resultTokens[i + 3].Type == XSharpLexer.DOT
                            && resultTokens[i + 4].Type == XSharpLexer.GT)
                        {
                            // <.idMarker.>
                            name = resultTokens[i + 2];
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
                        while (i < resultTokens.Length && resultTokens[i].Type != XSharpLexer.RBRKT)
                        {
                            more.Add(resultTokens[i]);
                            i++;
                        }
                        // skip the closing RBRKT
                        if (i == resultTokens.Length)
                        {
                            // throw an error 'missing RBRKT'
                        }
                        element = new RuleElement(token, RuleMarker.ResultOptional);
                        element.Children = AnalyzeResultTokens(more.ToArray());
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
        internal string GetDebuggerDisplay()
        {
            if (_matchtokens.Length > 0)
            {
                string result = "";
                int i = 0;
                while (i < _matchtokens.Length && _matchtokens[i].Type == RuleMarker.Token)
                {
                    result += _matchtokens[i].Token.Text+" ";
                    i++;
                }
                return "Rule: " + result.Trim();
            }
            else
            {
                return "Rule: (empty)";
            }
        }
    }
}
#endif