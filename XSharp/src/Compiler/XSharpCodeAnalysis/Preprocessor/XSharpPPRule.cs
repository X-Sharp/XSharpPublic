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
using System;
using System.Collections.Generic;
using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using System.Diagnostics;


namespace Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax
{
    [DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
    internal class PPRule
    {
        PPUDCType _type;
        PPMatchToken[] _matchtokens;                    
        PPResultToken[] _resulttokens;
        PPErrorMessages _errorMessages;
        internal PPUDCType Type { get { return _type; } }
        internal PPRule(PPToken udc, IList<PPToken> tokens, out PPErrorMessages errorMessages)
        {
            switch (udc.Type)
            {
                case XSharpLexer.PP_COMMAND:
                    if (udc.Text.ToLower() == "#command")
                        _type = PPUDCType.Command;
                    else
                        _type = PPUDCType.XCommand; 
                    break;
                case XSharpLexer.PP_TRANSLATE:
                    if (udc.Text.ToLower() == "#translate")
                        _type = PPUDCType.Translate; 
                    else
                        _type = PPUDCType.XTranslate;
                    break;
                default:
                    _type = PPUDCType.Define;
                    break;
            }
            var ltokens = new PPToken[tokens.Count];
            tokens.CopyTo(ltokens, 0);
            _errorMessages = new PPErrorMessages();
            errorMessages = null;
            if (!parseRuleTokens(udc, ltokens))
            {
                _type = PPUDCType.None;
                errorMessages = _errorMessages;
            }

        }
        bool parseRuleTokens(PPToken udc, PPToken[] _tokens)
        {
            int iSeperatorPos = -1;
            var markers = new Dictionary<string, PPMatchToken>(StringComparer.OrdinalIgnoreCase);

            if (_tokens?.Length == 0)
            {
                addErrorMessage(udc, "UDC is empty");
                return false;
            }
            for (int i = 0; i < _tokens.Length - 1; i++)
            {
                // Must be => without whitespace
                if (_tokens[i].Type == XSharpLexer.UDCSEP)
                {
                    iSeperatorPos = i;
                    break;
                }
            }
            if (iSeperatorPos < 0)
            {
                addErrorMessage(udc, "token '=>' not found in UDC ");
                return false;
            }
            PPToken[] _left = new PPToken[iSeperatorPos];
            PPToken[] _right = new PPToken[_tokens.Length - iSeperatorPos - 1];
            Array.Copy(_tokens, _left, iSeperatorPos);
            Array.Copy(_tokens, iSeperatorPos + 1, _right, 0, _right.Length);
            _matchtokens = analyzeMatchTokens(_left, markers);
            _resulttokens = analyzeResultTokens(_right,0);
            if (!checkMatchingTokens(_resulttokens, markers))
            {
                // Check to see if all result tokens have been matched
                // Unmatched Match tokens is fine (they may be deleted from the output)

                string unmatched = String.Empty;
                foreach (var r in _resulttokens)
                {
                    if (r.IsMarker&& !r.Matched)
                    {
                        addErrorMessage(r.Token, $"Result Marker '{r.Key}' not found in match list");
                    }
                }
            }
            bool result= false;
            if (_errorMessages == null || _errorMessages.Count == 0)
            {
                result = true;
            }
            else
            {
                result = false;
            }
            return result;
        }
        void addErrorMessage(PPToken token, string message)
        {
            _errorMessages.Add(new PPErrorMessage( token, message));
        }
        bool  checkMatchingTokens(PPResultToken[] results, Dictionary<string, PPMatchToken> markers)
        {
            bool allOk = true;
            // Set all marker indices
            for (int m = 0; m < _matchtokens.Length; m++)
            {
                _matchtokens[m].Index = m;
            }
            for (int r = 0; r < results.Length; r++)
            {
                var restoken = results[r];
                if (restoken.IsMarker )
                {
                    var token = restoken.Token;
                    var name = restoken.Key;
                    if (markers.ContainsKey(name))
                    {
                        restoken.MatchMarker = markers[name];
                        restoken.Matched = true;
                    }
                    else
                    {
                        allOk = false;
                    }
                }
            }
            // we do not need the dictionary anymore. The rule has been analyzed
            return allOk;
        }
        void addToDict(Dictionary<string, PPMatchToken> markers , PPMatchToken element)
        {
            if (element.Token.IsName())
            {
                string name = element.Key;
                if (!markers.ContainsKey(name))
                {
                    markers.Add(name, element);
                }
                else
                {
                    addErrorMessage(element.Token, $"Duplicate Match marker {element.Key} found in UDC");
                }
            }

        }
        PPMatchToken[] analyzeMatchTokens(PPToken[] matchTokens, Dictionary<string, PPMatchToken> markers, int offset = 0, int nestLevel = 0)
        {
            var result = new List<PPMatchToken>();
            var max = matchTokens.Length;
            List<PPToken> more;
            PPToken name;
            PPMatchToken element;
            for (int i = 0; i < max; i++)
            {
                var token = matchTokens[i];
                switch (token.Type)
                {

                    case XSharpLexer.LT:
                        if (i < max - 2
                            && matchTokens[i + 1].IsName()
                            && matchTokens[i + 2].Type == XSharpLexer.GT)
                        {
                            // <idMarker>
                            name = matchTokens[i + 1];
                            element = new PPMatchToken(name, PPTokenType.MatchRegular);
                            result.Add(element);
                            addToDict(markers, element);
                            i += 2;
                        }
                        else if (i < max - 4
                            // <*idMarker*>
                            && matchTokens[i + 1].Type == XSharpLexer.MULT
                            && matchTokens[i + 2].IsName()
                            && matchTokens[i + 3].Type == XSharpLexer.MULT
                            && matchTokens[i + 4].Type == XSharpLexer.GT)
                        {
                            name = matchTokens[i + 2];
                            element = new PPMatchToken(name, PPTokenType.MatchWild);
                            result.Add(element);
                            addToDict(markers, element);

                            i += 4;
                        }
                        else if (i < max - 4
                            // <(idMarker)>
                            && matchTokens[i + 1].Type == XSharpLexer.LPAREN
                            && matchTokens[i + 2].IsName()
                            && matchTokens[i + 3].Type == XSharpLexer.RPAREN
                            && matchTokens[i + 4].Type == XSharpLexer.GT)
                        {
                            name = matchTokens[i + 2];
                            element = new PPMatchToken(name, PPTokenType.MatchExtended);
                            result.Add(element);
                            addToDict(markers, element);
                            i += 4;
                        }
                        else if (i < max - 6
                              && matchTokens[i + 1].IsName()
                              && matchTokens[i + 2].Type == XSharpLexer.COMMA
                              && matchTokens[i + 3].Type == XSharpLexer.DOT
                              && matchTokens[i + 4].Type == XSharpLexer.DOT
                              && matchTokens[i + 5].Type == XSharpLexer.DOT
                              && matchTokens[i + 6].Type == XSharpLexer.GT)
                        {
                            // <idMarker,...>
                            name = matchTokens[i + 1];
                            element = new PPMatchToken(name, PPTokenType.MatchList);
                            result.Add(element);
                            addToDict(markers, element);
                            i += 6;
                        }
                        else if (i < max - 3
                              && matchTokens[i + 1].IsName()
                              && matchTokens[i + 2].Type == XSharpLexer.COLON)
                        {
                            // <idMarker:word list>
                            name = matchTokens[i + 1];
                            i += 3;
                            more = new List<PPToken>();
                            while (i <max && matchTokens[i].Type != XSharpLexer.GT)
                            {
                                token = matchTokens[i];
                                if (token.Type != XSharpLexer.COMMA)
                                    more.Add(token);
                                i++;
                            }
                            element = new PPMatchToken(name, PPTokenType.MatchRestricted);
                            result.Add(element);
                            addToDict(markers, element);
                            var tokens = new PPToken[more.Count];
                            more.CopyTo(tokens, 0);
                            element.Children = tokens;
                        }
                        break;
                    case XSharpLexer.LBRKT:
                        /*
                         * eat block nested
                         * [...]
                         */
                        more = getNestedTokens(i, max, matchTokens);
                        if (more != null)
                        {
                            i = i + more.Count+1;
                            var nested = analyzeMatchTokens(more.ToArray(), markers, result.Count, nestLevel+1);
                            if (nested.Length > 0)
                            {
                                foreach (var el in nested)
                                {
                                    el.RuleTokenType |= PPTokenType.Optional;
                                }
                                result.AddRange(nested);
                            }
                        }

                        break;
                    case XSharpLexer.BACKSLASH: // escape next token
                        if (i < max)
                        {
                            i++;
                            token = matchTokens[i];
                            result.Add(new PPMatchToken(token, PPTokenType.Token));
                        }
                        break;
                    default:
                        result.Add(new PPMatchToken(token, PPTokenType.Token));
                        break;
                }

            }
            var matchtokens = new PPMatchToken[result.Count];
            result.CopyTo(matchtokens);
            return matchtokens;
        }

        List<PPToken> getNestedTokens(int start, int max, PPToken[] tokens)
        {
            PPToken token = tokens[start];
            List<PPToken> result = null;
            var lbrkt = token;
            if (start < max - 1)   // must have at least [ name ]
            {
                result = new List<PPToken>();
                start++;
                var nestlevel = 1;
                while (start < max)
                {
                    token = tokens[start];
                    if (token.Type == XSharpLexer.LBRKT)
                        ++nestlevel;
                    if (token.Type == XSharpLexer.RBRKT)
                    {
                        --nestlevel;
                        if (nestlevel == 0)
                        {
                            break;
                        }
                    }
                    result.Add(token);
                    start++;
                }
                // more contains everything between the brackets including the first token
                if (nestlevel != 0)
                {
                    if (nestlevel > 0)
                    {
                        addErrorMessage(lbrkt, "Closing bracket ']' missing");
                    }
                    else
                    {
                        addErrorMessage(lbrkt, "Closing bracket ']' found with missing '['");
                    }
                }
            }
            else
            {
                if (start == max - 1)
                {
                    token = tokens[max];
                    if (token.Type == XSharpLexer.RBRKT)
                        addErrorMessage(lbrkt, "Empty Optional Token found");
                    else
                        addErrorMessage(lbrkt, "Closing bracket ']' missing");
                }
                else
                {
                    addErrorMessage(lbrkt, "Closing bracket ']' missing");
                }

            }
            return result;
        }
        PPResultToken[] analyzeResultTokens(PPToken[] resultTokens, int nestLevel)
        {
            var result = new List<PPResultToken>();
            var max = resultTokens.Length;
            List<PPToken> more;
            PPToken name;
            int lastTokenIndex = -1;
            ITokenSource lastTokenSource = null;
            for (int i = 0; i < resultTokens.Length; i++)
            {
                var token = resultTokens[i];
                if (token.TokenSource == lastTokenSource && token.TokenIndex > lastTokenIndex+1 )
                {
                    // whitespace tokens have been skipped
                    var ppWs = new PPToken(token, XSharpLexer.WS, " ");
                    ppWs.Channel = XSharpLexer.Hidden;
                    result.Add(new PPResultToken(ppWs, PPTokenType.Token, nestLevel > 0));
                }
                lastTokenIndex = token.TokenIndex;
                lastTokenSource = token.TokenSource;
                switch (token.Type)
                {
                    case XSharpLexer.NEQ:
                        /*
                        * match #<idMarker>
                        */
                        if (i < resultTokens.Length - 3
                            && resultTokens[i + 1].Type == XSharpLexer.LT
                            && resultTokens[i + 2].IsName()
                            && resultTokens[i + 3].Type == XSharpLexer.GT)
                        {
                            name = resultTokens[i + 2];
                            result.Add(new PPResultToken(name, PPTokenType.ResultDumbStringify, nestLevel > 0));
                            i += 3;
                        }
                        else
                        {
                            result.Add(new PPResultToken(token, PPTokenType.Token, nestLevel > 0));
                        }
                        break;
                    case XSharpLexer.LT:
                        if (i < resultTokens.Length - 2
                            && resultTokens[i + 1].IsName()
                            && resultTokens[i + 2].Type == XSharpLexer.GT)
                        {
                            // <idMarker>
                            name = resultTokens[i + 1];
                            result.Add(new PPResultToken(name, PPTokenType.ResultRegular, nestLevel > 0));
                            i += 2;
                        }
                        if (i < resultTokens.Length - 2
                            && resultTokens[i + 1].Type == XSharpLexer.STRING_CONST
                            && resultTokens[i + 2].Type == XSharpLexer.GT)
                        {
                            // <"idMarker">
                            var t = resultTokens[i + 1];
                            name = new PPToken(t, XSharpLexer.ID, t.Text.Substring(1, t.Text.Length - 2));
                            result.Add(new PPResultToken(name, PPTokenType.ResultNormalStringify, nestLevel > 0));
                            i += 2;
                        }
                        if (i < resultTokens.Length - 4
                            && resultTokens[i + 1].Type == XSharpLexer.LPAREN
                            && resultTokens[i + 2].IsName()
                            && resultTokens[i + 3].Type == XSharpLexer.RPAREN
                            && resultTokens[i + 4].Type == XSharpLexer.GT)
                        {
                            // <(idMarker)>
                            name = resultTokens[i + 2];
                            result.Add(new PPResultToken(name, PPTokenType.ResultSmartStringify, nestLevel > 0));
                            i += 4;
                        }
                        if (i < resultTokens.Length - 4
                            && resultTokens[i + 1].Type == XSharpLexer.LCURLY
                            && resultTokens[i + 2].IsName()
                            && resultTokens[i + 3].Type == XSharpLexer.RCURLY
                            && resultTokens[i + 4].Type == XSharpLexer.GT)
                        {
                            // <{idMarker}>
                            name = resultTokens[i + 2];
                            result.Add(new PPResultToken(name, PPTokenType.ResultBlockify, nestLevel > 0));
                            i += 4;
                        }
                        if (i < resultTokens.Length - 4
                            && resultTokens[i + 1].Type == XSharpLexer.DOT
                            && resultTokens[i + 2].IsName()
                            && resultTokens[i + 3].Type == XSharpLexer.DOT
                            && resultTokens[i + 4].Type == XSharpLexer.GT)
                        {
                            // <.idMarker.>
                            name = resultTokens[i + 2];
                            result.Add(new PPResultToken(name, PPTokenType.ResultLogify, nestLevel > 0));
                            i += 4;
                        }

                        break;
                    case XSharpLexer.LBRKT:
                        /*
                         * eat block
                        * [ ... ], 
                        * when nested then this is seen as a repeated result clause
                         */
                        more = getNestedTokens(i, max, resultTokens);
                        if (more != null)
                        {
                            i = i + more.Count+1;
                            var nested = analyzeResultTokens(more.ToArray(), nestLevel +1);
                            if (nestLevel > 0)
                            {
                                foreach (var t in nested)
                                {
                                    t.RuleTokenType |= PPTokenType.Repeated;
                                }
                            }
                            result.AddRange(nested);
                        }
                        break;
                    case XSharpLexer.BACKSLASH: // escape next token
                        if (i < max)
                        {
                            i++;
                            token = resultTokens[i];
                            result.Add(new PPResultToken(token, PPTokenType.Token, nestLevel > 0));
                        }
                        break;

                    default:
                        result.Add(new PPResultToken(token, PPTokenType.Token, nestLevel > 0));
                        break;
                }

            }
            var resulttokens = new PPResultToken[result.Count];
            result.CopyTo(resulttokens);
            return resulttokens;

        }
        internal string Key
        {
            get
            {
                if (_matchtokens.Length > 0)
                {
                    var token = _matchtokens[0].Token;
                    if (token.IsName())
                        return token.Text.ToUpperInvariant();
                    else
                        return token.Text;
                }
                else
                    return "(Empty)";
            }
        }
        internal string Name
        {
            get
            {
                if (_matchtokens?.Length > 0)
                {
                    string result = "";
                    foreach (var token in _matchtokens)
                    {
                        result += token.SyntaxText + " ";
                    }
                    return result.Trim();
                }
                else
                {
                    return "(empty)";
                }
            }
        }
        internal string GetDebuggerDisplay()
        {
            return this.Name;
        }
        bool tokenEquals(PPToken lhs, PPToken rhs)
        {
            if (lhs != null && rhs != null)
                return stringEquals(lhs.Text, rhs.Text);
            return false;
        }
        bool stringEquals (string lhs, string rhs)
        {
            if (lhs?.Length <= 4)
                return String.Equals(lhs, rhs, StringComparison.OrdinalIgnoreCase);
            switch (this.Type)
            {
                case PPUDCType.Command:
                case PPUDCType.Translate:
                    // dBase/Clipper syntax 4 characters is enough
                    return String.Compare(lhs, 0, rhs, 0, rhs.Length, StringComparison.OrdinalIgnoreCase) == 0;
                case PPUDCType.XCommand:
                case PPUDCType.XTranslate:
                    return String.Equals(lhs, rhs, StringComparison.OrdinalIgnoreCase);
            }
            return false;
        }
        internal bool matchToken(PPMatchToken matchToken, ref int iRule, int iLastRule,  ref int iSource, IList<PPToken> tokens, PPMatchRange[] matchInfo)
        {
            PPToken sourceToken = tokens[iSource];
            PPToken ruleToken = matchToken.Token;
            int iEnd;
            switch (matchToken.RuleTokenType)
            {
                case PPTokenType.Token:
                    if (!this.tokenEquals(ruleToken, sourceToken))
                    {
                        if (!matchToken.IsOptional())
                            return false;
                        else
                        {
                            // check to see if the sourceToken matches a next token in our list
                            int iLoop = iRule+1;
                            while (iLoop < this._matchtokens.Length)
                            {
                                matchToken = this._matchtokens[iLoop];
                                if (matchToken.RuleTokenType == PPTokenType.Token)
                                {
                                    if (tokenEquals(matchToken.Token, sourceToken))
                                    {
                                        iRule = iLoop;
                                        break;
                                    }
                                    else
                                    {
                                        if (!matchToken.IsOptional())
                                            return false;
                                        matchInfo[iLoop] = PPMatchRange.Optional();
                                    }
                                }
                                iLoop++;
                                if (iLoop == this._matchtokens.Length)
                                    return false;
                            }
                        }
                    }
                    matchInfo[iRule] = PPMatchRange.Token(iSource);
                    iSource += 1;
                    iRule += 1;
                    break;
                case PPTokenType.MatchRegular:
                    // Matches an expression
                    // use Expression method to find the end of the list
                    // iEnd points to the next token after the expression
                    iEnd = matchExpression(iSource, tokens,null);
                    matchInfo[iRule] = PPMatchRange.Create(iSource, iEnd-1);
                    iSource = iEnd;
                    iRule += 1;
                    break;
                case PPTokenType.MatchList:
                    // ignore for now
                    iSource += 1;
                    iRule += 1;
                    break;

                case PPTokenType.MatchRestricted:
                    // match the words in the list.
                    // the token in the rule is the match marker
                    // the words to be checked are the MoreTokens
                    bool found = false;
                    PPToken lastToken = null;
                    foreach (var child in matchToken.Children)
                    {
                        lastToken = child;
                        if (tokenEquals(child, sourceToken))
                        {
                            found = true;
                            break;
                        }
                        
                    }
                    iEnd = iSource;
                    if (found && lastToken.Type == XSharpLexer.AMP )
                    {
                        // when the ampersand is the last token, then we also include the following token
                        var last = lastToken == matchToken.Children[matchToken.Children.Length - 1];
                        if (last && tokens.Count > iSource)
                        {
                            iEnd = iSource + 1;
                        }
                    }
                    if (!found && !matchToken.IsOptional())
                        return false;
                    matchInfo[iRule] = PPMatchRange.Create(iSource, iEnd);
                    iSource = iEnd + 1;
                    iRule += 1;
                    break;
                case PPTokenType.MatchWild:
                    matchInfo[iRule] = PPMatchRange.Create(iSource, tokens.Count);
                    return true;                    // matches anything until the end of the list
                case PPTokenType.MatchExtended:
                    // either match a single token or a token in parentheses
                    if (sourceToken.Type == XSharpLexer.LPAREN)
                    {
                        if (iSource > tokens.Count - 2)
                            return false;
                        if (tokens[iSource + 1].IsName() &&
                            tokens[iSource + 2].Type == XSharpLexer.RPAREN)
                        {
                            // Ok
                            matchInfo[iRule] = PPMatchRange.Create(iSource, iSource + 2 );
                            iSource += 1;
                            iRule += 1;
                        }
                        else
                        {
                            matchInfo[iRule] = PPMatchRange.Create ( iSource, iSource );
                            iSource += 1;
                            iRule += 1;

                        }
                    }
                    break;
            }
            return true;
        }
        internal bool Matches(IList<PPToken> tokens, out PPMatchRange[] matchInfo)
        {
            int iRule = 0;
            int iSource = 0;
            matchInfo = new PPMatchRange[_matchtokens.Length];
            while (iRule < _matchtokens.Length && iSource < tokens.Count)
            {
                var mtoken = _matchtokens[iRule];
                if (!matchToken(mtoken, ref iRule, _matchtokens.Length,  ref iSource, tokens, matchInfo))
                {
                    return false;
                }
            } 
            while (iRule < _matchtokens.Length)
            {
                // check to see if the remaining tokens are optional
                // when not, then there is no match
                if (!_matchtokens[iRule].IsOptional())
                {
                    return false;
                }
                matchInfo[iRule] = PPMatchRange.Optional();
                iRule++;
            }
            return true;

        }
        internal IList<PPToken> Replace(IList<PPToken> tokens, PPMatchRange[] matchInfo)
        {
            Debug.Assert(matchInfo.Length == _matchtokens.Length);
            IList<PPToken> result = new List<PPToken>();
            foreach (var resultToken in _resulttokens)
            {
                switch (resultToken.RuleTokenType)
                {
                    case PPTokenType.Token:
                        result.Add(resultToken.Token);
                        break;
                    case PPTokenType.ResultLogify:
                        // check to see if the token was matched.
                        lLogifyResult(resultToken, tokens, matchInfo, result);
                        break;
                    case PPTokenType.ResultBlockify:
                        blockifyResult(resultToken, tokens, matchInfo, result);
                        break;
                    case PPTokenType.ResultRegular:
                        regularResult(resultToken, tokens, matchInfo, result);
                        break;
                    case PPTokenType.ResultSmartStringify:
                    case PPTokenType.ResultNormalStringify:
                    case PPTokenType.ResultDumbStringify:
                        stringifyResult(resultToken, tokens, matchInfo, result);
                        break;
                }
            }
            // we need to determine the tokens at the end of the tokens list that are not matched 
            // in the results and then copy these to the result as well
            int last = -1;
            foreach (var m in matchInfo)
            {
                if (m.End > last)
                    last = m.End;
            }
            for (int i = last + 1; i < tokens.Count; i++)
            {
                result.Add(tokens[i]);                
            }
            return result;
        }
        void regularResult(PPResultToken rule, IList<PPToken> tokens, PPMatchRange[] matchInfo, IList<PPToken> result)
        {
            // write input text to the result text without no changes
            // for example:
            // #command SET CENTURY (<x>) => __SetCentury( <x> )
            // the output written is the literal text for x, so the token(s) x
            var range = matchInfo[rule.MatchMarker.Index];
            if (!range.Empty())
            {
                for (int i = range.Start; i <= range.End; i++)
                {
                    var token = tokens[i];
                    result.Add(token);
                }
            }
            return;
        }
        void blockifyResult(PPResultToken rule, IList<PPToken> tokens, PPMatchRange[] matchInfo, IList<PPToken> result)
        {
            // write output text as codeblock
            // so prefixes with "{||"and suffixes with "}
            // if the input is already a code block then nothing is changed
            // when the input is a list then each element in the list will be
            // converted to a code block
            var range = matchInfo[rule.MatchMarker.Index];
            if (!range.Empty())
            {
                bool addBlockMarker = true;
                int start, end;
                start = range.Start;
                end = range.End; ;
                if (end - start > 4)
                {
                    if (tokens[start].Type == XSharpLexer.LCURLY &&
                        tokens[start + 1].Type == XSharpLexer.PIPE &&
                        tokens[start + 2].Type == XSharpLexer.PIPE &&
                        tokens[end].Type == XSharpLexer.RCURLY)
                        addBlockMarker = false;

                }
                if (addBlockMarker)
                {
                    var t = tokens[start];
                    var nt = new PPToken(t, XSharpLexer.LCURLY, "{");
                    result.Add(nt);
                    nt = new PPToken(t, XSharpLexer.PIPE, "|");
                    result.Add(nt);
                    result.Add(nt);
                }
                for (int i = range.Start; i <= range.End; i++)
                {
                    var token = tokens[i];
                    result.Add(token);
                }
                if (addBlockMarker)
                {
                    var t = tokens[end];
                    var nt = new PPToken(t, XSharpLexer.RCURLY, "}");
                    result.Add(nt);
                }
            }
            return;
        }
        void stringifyResult(PPResultToken rule, IList<PPToken> tokens, PPMatchRange[] matchInfo, IList<PPToken> result)
        {
            var range = matchInfo[rule.MatchMarker.Index];
            PPToken newToken;
            switch (rule.RuleTokenType)
            {
                // Handle 3 kind of stringifies:
                case PPTokenType.ResultDumbStringify:
                    // return original contents
                    // #command SET COLOR TO [<*spec*>]        => SetColor( #<spec> )
                    // this changes
                    // SET COLOR TO r/w into SetColor("r/w")
                    // Note that this match rule uses a wild match marker: this matches everything until end of statement
                    // change type of token to STRING_CONST;
                    if (!range.Empty())
                    {
                        var start = range.Start;
                        var end = range.End;
                        var startindex = tokens[start].StartIndex;
                        var endindex = tokens[end].StopIndex;
                        var interval = new Interval(startindex, endindex);
                        string allText = tokens[start].TokenSource.InputStream.GetText(interval);
                        allText = "\"" + allText + "\"";
                        result.Add(new PPToken(tokens[start], XSharpLexer.STRING_CONST, allText));
                    }
                    else
                    {
                        // no match, then dumb stringify write an empty string
                        result.Add(new PPToken(tokens[0], XSharpLexer.NULL_STRING, "NULL_STRING"));
                    }
                    break;
                case PPTokenType.ResultNormalStringify:
                    // Delimit the input with string delimiters
                    if (!range.Empty())
                    {
                        var start = range.Start;
                        var end = range.End;
                        var startindex = tokens[start].StartIndex;
                        var endindex = tokens[end].StopIndex;
                        var interval = new Interval(startindex, endindex);
                        string allText = tokens[start].TokenSource.InputStream.GetText(interval);
                        allText = "\"" + allText + "\"";
                        result.Add(new PPToken(tokens[start], XSharpLexer.STRING_CONST, allText));
                    }
                    break;


                case PPTokenType.ResultSmartStringify:
                    // Only works when input text is delimited with parentheses
                    // if the match marker is a list then each element is stringified and it stays a list
                    // for example: 
                    // #command SET CENTURY <x:ON,OFF,&>      => __SetCentury( <(x)> )
                    // the contents of x must be converted to a string

                    bool addStringDelimiters = true;
                    if (!range.Empty())
                    {
                        for (int i = range.Start; i <= range.End; i++)
                        {
                            var token = tokens[i];
                            switch (token.Type)
                            {
                                case XSharpLexer.STRING_CONST:
                                case XSharpLexer.CHAR_CONST:
                                case XSharpLexer.ESCAPED_STRING_CONST:
                                case XSharpLexer.INTERPOLATED_STRING_CONST:
                                    result.Add(token);
                                    break;
                                case XSharpLexer.AMP:
                                    addStringDelimiters = false;
                                    break;
                                default:
                                    if (addStringDelimiters)
                                    {
                                        newToken = new PPToken(token, XSharpLexer.STRING_CONST, token.Text);
                                        newToken.Text = "\"" + token.Text + "\"";
                                    }
                                    else
                                    {
                                        newToken = new PPToken(token, XSharpLexer.ID, token.Text);
                                    }
                                    result.Add(newToken);
                                    break;
                            }
                        }
                    }
                    break;
            }
            return;
        }
        void lLogifyResult(PPResultToken rule, IList<PPToken> tokens, PPMatchRange[] matchInfo, IList<PPToken> result )
        {
            // when input is empty then return a literal token FALSE
            // else return a literal token TRUE
            // copy positional information from original token
            // If input is more than one token, then change the other tokens to type WhiteSpace
            // this is normally used with the restricted match marker
            //
            // #command INDEX ON <key> TO <(file)> [<u: UNIQUE>]                       ;
            //=> dbCreateIndex(                                                 ;
            //            <(file)>, <"key">, <{key}>,                     ;
            //            if( <.u.>, .t., NIL )                           ;
            //          )
            // When the Unique keyword is found in the input then the <.u.> marker is replaced with TRUE
            // else with .F.
            // In this case no .F. is written, to make sure that the global SetUnique() setting is used
            // Check all the matching tokens (it can be more than 1
            bool matched = false;
            int tokenPos = -1;
            var range = matchInfo[rule.MatchMarker.Index];
            if (!range.Empty())
            {
                tokenPos = range.Start;
                PPToken t;
                if (tokenPos != 0)
                {
                    IToken st = tokens[tokenPos];
                    if (matched)
                    {
                        t = new PPToken(st, XSharpLexer.TRUE_CONST, ".T.");
                    }
                    else
                    {
                        t = new PPToken(st, XSharpLexer.FALSE_CONST, ".F.");
                    }
                    result.Add(t);
                }
            }
            return;
        }

        bool tokenCanStartExpression(int pos, IList<PPToken> tokens)
        {
            PPToken token;
            token = tokens[pos];
            if (! token.NeedsLeft() && ! token.IsEndOfCommand())
            {
                return true;
            }
            return false;
        }
        int matchExpression(int start, IList<PPToken> tokens, PPToken stopToken)
        {
            if (!tokenCanStartExpression(start, tokens))
                return start;
            int current = start;
            int braceLevel = 0;
            int count = tokens.Count;
            int openBrace = 0;
            int closeBrace = 0;
            PPToken token;
            PPToken lastToken = null; 
            while (current < count)
            {
                // Check to see if there is a codeblock. 
                // matchCodeBlock ends at the position after of the (nested) codeblock(s)
                current = matchCodeBlock(current, tokens);
                if (current >= count)   
                    break;
                token = tokens[current];
                if (braceLevel > 0)
                {
                    if (token.Type == openBrace)
                        braceLevel += 1;
                    else if (token.Type == closeBrace)
                        braceLevel -= 1;
                }
                else if (token.Type == XSharpLexer.COMMA)
                {
                    // expression can only have comma's when inside braces
                    // so exit the loop
                    break;
                }
                // if Open brace, scan for close brace
                else if (token.IsOpen( ref closeBrace))
                {
                    openBrace = token.Type;
                    braceLevel++;
                }
                // check to see if we have 2 tokens that can follow
                // otherwise exit
                else if ( token.IsClose() 
                          || (lastToken.NeedsRight() && !token.IsPrimary())
                          || (stopToken != null && tokenEquals(stopToken, token))
                        )
                {
                    break;
                }
                else if (!lastToken.CanJoin(token))
                {
                    break;
                }
                else if (!token.IsNeutral())
                {
                    lastToken = token;
                }
                current++;
            }
            return current;
        }


        bool isStartOfCodeBlock(int start, IList<PPToken> tokens, ref int startOfBody)
        {
            int count = tokens.Count;
            startOfBody = start;
            if (start > count - 4)   // we need at least {||}
                return false;
            if (tokens[start].Type != XSharpLexer.LCURLY ||
                tokens[start+1].Type != XSharpLexer.PIPE)
                return false;
            // now scan for second pipe which indicates end of parameter list
            start = start + 2;
            var lasttype = XSharpLexer.COMMA;
            while (start < count)
            {
                var token = tokens[start];
                if (lasttype == XSharpLexer.COMMA && token.IsName())
                {
                    start = start + 1;
                    lasttype = XSharpLexer.ID;
                }
                else if (lasttype == XSharpLexer.ID && token.Type == XSharpLexer.COMMA)
                {
                    start = start + 1;
                    lasttype = XSharpLexer.COMMA;
                }
                else
                {
                    break;
                }
            }
            // End of parameter list
            bool Ok = start < count && tokens[start].Type == XSharpLexer.PIPE;
            if (Ok)
                startOfBody = start + 1;
            return Ok;
        }
        bool findEndOfCodeBlock(int start, IList<PPToken> tokens, ref int endOfBlock)
        {
            int count = tokens.Count;
            int nested = 0;
            int closeType = 0;
            while (start < count)
            {
                PPToken token = tokens[start];
                if (token.IsOpen( ref closeType))
                {
                    nested += 1;
                }
                if (token.Type == XSharpLexer.RCURLY && nested == 0)
                {
                    endOfBlock = start;
                    return true;
                }
                if (token.IsClose())
                {
                    nested -= 1;
                }
                start += 1;
            }
            return false;
        }
        int matchCodeBlock(int start, IList<PPToken> tokens)
        {
            int body = start;
            int count = tokens.Count;
            if (isStartOfCodeBlock(start, tokens, ref body))    // found start of codeblock, body now points to the last PIPE
            {
                int nested = 1;
                body = body + 1;
                while (body < count && nested >= 1)
                {
                    if (isStartOfCodeBlock(body, tokens, ref body)) // nested codeblock , body now points to the last PIPE of nested block
                    {
                        nested += 1;
                    }
                    else if (findEndOfCodeBlock(body, tokens, ref body)) // body points to the RCURLY of one of the blocks
                    {
                        nested -= 1;
                    }
                }
                body = body + 1;
            }
            return body;    // points to the first character after the codeblock or to the original start
        }
    }



}
