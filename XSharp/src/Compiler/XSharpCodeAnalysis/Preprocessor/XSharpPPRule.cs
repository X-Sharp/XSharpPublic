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
        PPMatchToken[] _matchTokensFlattened;
        int tokenCount = 0;
        PPResultToken[] _resulttokens;
        PPErrorMessages _errorMessages;
        internal bool CaseInsensitive = false;
        internal bool hasRepeats = false;
        internal bool hasOptionalResult = false;
        internal PPUDCType Type { get { return _type; } }
        internal PPRule(XSharpToken udc, IList<XSharpToken> tokens, out PPErrorMessages errorMessages)
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
                case XSharpLexer.PP_DEFINE:
                    // define in the form of #define FOO(x) x + 1
                    _type = PPUDCType.Define;
                    break;
                default:
                    _type = PPUDCType.None;
                    break;
            }
            tokens.RemoveAt(0);
            var ltokens = new XSharpToken[tokens.Count];
            tokens.CopyTo(ltokens, 0);
            _errorMessages = new PPErrorMessages();
            errorMessages = null;
            if (_type == PPUDCType.Define)
            {
                if (!parseDefineTokens(udc, ltokens))
                {
                    _type = PPUDCType.None;
                    errorMessages = _errorMessages;

                }
            }
            else
            {
                if (!parseRuleTokens(udc, ltokens))
                {
                    _type = PPUDCType.None;
                    errorMessages = _errorMessages;
                }
            }
            _matchTokensFlattened = null;
        }
        bool parseDefineTokens(XSharpToken udc, XSharpToken[] tokens)
        {
            // scan for the end paren
            int iSeperatorPos = -1;
            int iParenLevel = 0;
            for (int i = 0; i < tokens.Length - 1; i++)
            {
                // Look for RPAREN
                switch (tokens[i].Type)
                {
                    case XSharpLexer.LPAREN:
                        iParenLevel += 1;
                        break;
                    case XSharpLexer.RPAREN:
                        iParenLevel -= 1;
                        if (iParenLevel <= 0)
                        {
                            iSeperatorPos = i;
                            break;
                        }
                        break;
                }
                if (iSeperatorPos != -1)
                    break;
            }
            if (iSeperatorPos < 0)
            {
                addErrorMessage(udc, "token ')' not found in #DEFINE");
                return false;
            }
            if (iParenLevel != 0)
            {
                addErrorMessage(udc, "Unmatched number of Open and Close parentheses in UDC");
                return false;
            }
            XSharpToken[] left = new XSharpToken[iSeperatorPos + 1];
            XSharpToken[] right = new XSharpToken[tokens.Length - iSeperatorPos - 1];
            Array.Copy(tokens, left, iSeperatorPos + 1);
            Array.Copy(tokens, iSeperatorPos + 1, right, 0, right.Length);
            // Now create a list of match tokens and result tokens.
            // This is done inline since it is much simpler then for a UDC
            var matchTokens = new List<PPMatchToken>();
            var resultTokens = new List<PPResultToken>();
            // inside a #define the tokens are case sensitive
            var markers = new Dictionary<string, PPMatchToken>(StringComparer.Ordinal);
            bool hasSeenLParen = false;
            bool hasErrors = false;
            for (int i = 0; i < left.Length && !hasErrors; i++)
            {
                var token = left[i];
                if (token.IsName())
                {
                    if (hasSeenLParen)
                    {
                        var mtoken = new PPMatchToken(token, PPTokenType.MatchRegular);
                        matchTokens.Add(mtoken);
                        markers.Add(token.Text, mtoken);
                    }
                    else
                    {
                        matchTokens.Add(new PPMatchToken(token, PPTokenType.Token));
                    }
                }
                else
                {
                    switch (token.Type)
                    {
                        case XSharpLexer.LPAREN:
                            matchTokens.Add(new PPMatchToken(token, PPTokenType.Token));
                            hasSeenLParen = true;
                            break;
                        case XSharpLexer.COMMA:
                        case XSharpLexer.RPAREN:
                            matchTokens.Add(new PPMatchToken(token, PPTokenType.Token));
                            break;
                        default:
                            addErrorMessage(udc, $"unexpected token '{token.Text}' found in #DEFINE");
                            hasErrors = true;
                            break;
                    }

                }
            }
            for (int i = 0; i < right.Length && !hasErrors; i++)
            {
                var token = right[i];
                if (token.IsName())
                {
                    if (markers.ContainsKey(token.Text))
                    {
                        var mtoken = markers[token.Text];
                        var rtoken = new PPResultToken(token, PPTokenType.ResultRegular);
                        rtoken.MatchMarker = mtoken;
                        resultTokens.Add(rtoken);
                    }
                    else
                    {
                        var rtoken = new PPResultToken(token, PPTokenType.Token);
                        resultTokens.Add(rtoken);
                    }
                }
                else
                {
                    var rtoken = new PPResultToken(token, PPTokenType.Token);
                    resultTokens.Add(rtoken);
                }
            }
            _matchtokens = matchTokens.ToArray();
            var linearlist = new List<PPMatchToken>();
            addMatchTokens(_matchtokens, linearlist);
            _matchTokensFlattened = linearlist.ToArray();
            tokenCount = _matchTokensFlattened.Length;
            _resulttokens = resultTokens.ToArray();
            checkMatchingTokens(_resulttokens, markers);
            return _errorMessages == null || _errorMessages.Count == 0;
        }

        bool parseRuleTokens(XSharpToken udc, XSharpToken[] tokens)
        {
            int iSeperatorPos = -1;
            var markers = new Dictionary<string, PPMatchToken>(StringComparer.OrdinalIgnoreCase);

            if (tokens?.Length == 0)
            {
                addErrorMessage(udc, "UDC is empty");
                return false;
            }
            for (int i = 0; i < tokens.Length; i++)
            {
                // Must be => without whitespace
                if (tokens[i].Type == XSharpLexer.UDCSEP)
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
            XSharpToken[] left = new XSharpToken[iSeperatorPos];
            XSharpToken[] right = new XSharpToken[tokens.Length - iSeperatorPos - 1];
            Array.Copy(tokens, left, iSeperatorPos);
            Array.Copy(tokens, iSeperatorPos + 1, right, 0, right.Length);
            _matchtokens = analyzeMatchTokens(left, markers);
            _resulttokens = analyzeResultTokens(right, 0);
            if (!checkMatchingTokens(_resulttokens, markers))
            {
                // Check to see if all result tokens have been matched
                // Unmatched Match tokens is fine (they may be deleted from the output)

                foreach (var r in _resulttokens)
                {
                    if (r.IsMarker && r.MatchMarker == null)
                    {
                        addErrorMessage(r.Token, $"Result Marker '{r.Key}' not found in match list");
                    }
                }
            }
            if (hasOptionalResult)
            {
                checkForRepeat(udc);
            }
            return _errorMessages == null || _errorMessages.Count == 0;
        }
        void checkForRepeat(XSharpToken udc)
        {
            var keys = new List<string>();
            var flattened = new List<PPResultToken>();
            foreach (var restoken in _resulttokens)
            {
                if (restoken.OptionalElements != null)
                {
                    foreach (var child in restoken.OptionalElements)
                    {
                        if (!child.IsToken && !child.IsOptional)
                        {
                            flattened.Add(child);
                            if (!keys.Contains(child.Token.Text.ToLower()))
                            {
                                keys.Add(child.Token.Text.ToLower());
                            }
                        }
                    }
                }
                if (!restoken.IsToken && !restoken.IsOptional)
                {
                    flattened.Add(restoken);
                    if (!keys.Contains(restoken.Token.Text.ToLower()))
                        keys.Add(restoken.Token.Text.ToLower());
                }
            }
            foreach (var restoken in flattened)
            {
                var tokenName = restoken.Token.Text.ToLower();
                foreach (string name in keys)
                {

                    if (tokenName != name && isRepeatToken(tokenName, name))
                    {
                        if (tokenName.EndsWith("n", StringComparison.OrdinalIgnoreCase))
                        {
                            restoken.IsRepeat = true;
                            if (restoken.MatchMarker != null)
                            {
                                restoken.MatchMarker.IsRepeat = true;
                            }
                        }
                        hasRepeats = true;
                    }
                }
            }
            if (hasRepeats)
            {
                foreach (var m in _matchtokens)
                {
                    if (m.Children?.Length > 0)
                    {
                        foreach (var c in m.Children)
                        {
                            if (c.IsRepeat)
                            {
                                m.IsRepeat = true;
                                break;
                            }
                        }
                    }
                }
                foreach (var r in _resulttokens)
                {
                    if (r.OptionalElements?.Length > 0)
                    {
                        foreach (var c in r.OptionalElements)
                        {
                            if (c.IsRepeat)
                            {
                                r.IsRepeat = true;
                                break;
                            }
                        }
                    }
                }
            }
        }


        bool isRepeatToken(string left, string right, bool first = true)
        {
            if (left.EndsWith("n", StringComparison.OrdinalIgnoreCase))
            {
                if (string.Compare(left, 0, right, 0, left.Length - 1, StringComparison.OrdinalIgnoreCase) == 0)
                {
                    // exp1 and expn 
                    if (left.Length == right.Length && right.EndsWith("1"))
                        return true;
                    // exp and expn
                    if (left.Length == right.Length + 1)
                        return true;
                }

            }
            // try the other way around
            if (first)
                return isRepeatToken(right, left, false);
            return false;
        }
        void addErrorMessage(XSharpToken token, string message)
        {
            _errorMessages.Add(new PPErrorMessage(token, message));
        }
        bool checkMatchingTokens(PPResultToken[] results, Dictionary<string, PPMatchToken> markers)
        {
            bool allOk = true;
            // Set all marker indices
            if (_matchTokensFlattened == null)
            {
                addErrorMessage(this._matchtokens[0].Token, "Syntax Error in Preprocessor Rule");
                return false;
            }
            for (int i = 0; i < _matchTokensFlattened?.Length; i++)
            {
                var mt = _matchTokensFlattened[i];
                mt.Index = i;
            }
            for (int r = 0; r < results.Length; r++)
            {
                var restoken = results[r];
                if (restoken.IsMarker)
                {
                    var token = restoken.Token;
                    var name = restoken.Key;
                    if (markers.ContainsKey(name))
                    {
                        restoken.MatchMarker = markers[name];
                    }
                    else
                    {
                        allOk = false;
                    }
                }
                if (restoken.RuleTokenType == PPTokenType.ResultOptional)
                {
                    if (restoken.OptionalElements != null)
                    {
                        allOk = allOk && checkMatchingTokens(restoken.OptionalElements, markers);
                    }
                }
            }
            // we do not need the dictionary anymore. The rule has been analyzed
            return allOk;
        }
        void addToDict(Dictionary<string, PPMatchToken> markers, PPMatchToken element)
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
        PPMatchToken[] analyzeMatchTokens(XSharpToken[] matchTokens, Dictionary<string, PPMatchToken> markers, int offset = 0, int nestLevel = 0)
        {
            var result = new List<PPMatchToken>();
            var max = matchTokens.Length;
            List<XSharpToken> more;
            XSharpToken name;
            PPMatchToken element;
            for (int i = 0; i < max; i++)
            {
                var token = matchTokens[i];
                switch (token.Type)
                {

                    case XSharpLexer.LT:
                        // These conditions match IsName() as last condition 
                        // because the other matches are faster
                        if (i < max - 2
                            && matchTokens[i + 2].Type == XSharpLexer.GT
                            && matchTokens[i + 1].IsName())
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
                            && matchTokens[i + 3].Type == XSharpLexer.MULT
                            && matchTokens[i + 4].Type == XSharpLexer.GT
                            && matchTokens[i + 2].IsName())
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
                            && matchTokens[i + 3].Type == XSharpLexer.RPAREN
                            && matchTokens[i + 4].Type == XSharpLexer.GT
                            && matchTokens[i + 2].IsName())

                        {
                            name = matchTokens[i + 2];
                            element = new PPMatchToken(name, PPTokenType.MatchExtended);
                            result.Add(element);
                            addToDict(markers, element);
                            i += 4;
                        }
                        else if (i < max - 4
                              && matchTokens[i + 2].Type == XSharpLexer.COMMA
                              && matchTokens[i + 3].Type == XSharpLexer.ELLIPSIS
                              && matchTokens[i + 4].Type == XSharpLexer.GT
                              && matchTokens[i + 1].IsName())
                        {
                            // <idMarker,...>
                            name = matchTokens[i + 1];
                            element = new PPMatchToken(name, PPTokenType.MatchList);
                            result.Add(element);
                            addToDict(markers, element);
                            i += 4;
                        }
                        else if (i < max - 3
                              && matchTokens[i + 2].Type == XSharpLexer.COLON
                              && matchTokens[i + 1].IsName())
                        {
                            // <idMarker:word list, separated with commas>
                            name = matchTokens[i + 1];
                            i += 3;
                            more = new List<XSharpToken>();
                            while (i < max && matchTokens[i].Type != XSharpLexer.GT)
                            {
                                token = matchTokens[i];
                                more.Add(token);
                                i++;
                            }
                            if (i == max)
                            {
                                // end of list found and not a GT then the close tag is missing
                                _errorMessages.Add(new PPErrorMessage(name, $"Bad match marker {name.Text} misses end Tag '>'"));
                            }
                            element = new PPMatchToken(name, PPTokenType.MatchRestricted);
                            result.Add(element);
                            addToDict(markers, element);
                            var tokens = new XSharpToken[more.Count];
                            more.CopyTo(tokens, 0);
                            element.Tokens = tokens;
                        }
                        else
                        {
                            // Most likely this is a normal < character ?
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
                            i = i + more.Count + 1;

                            var nested = analyzeMatchTokens(more.ToArray(), markers, result.Count, nestLevel + 1);
                            PPMatchToken first = null;
                            if (nested.Length > 0)
                            {
                                // the '[' is added to the result list
                                // and the nested elements are added as children
                                // the type for '[' is MatchOptional
                                first = nested[0];
                                PPMatchToken marker = null;
                                foreach (var e in nested)
                                {
                                    if (e.IsMarker)
                                        marker = e;
                                }
                                string key = marker == null ? "**"+first.Key: marker.Key;
                                element = new PPMatchToken(token, PPTokenType.MatchOptional, key);
                                element.Children = nested;
                                result.Add(element);
                            }
                        }

                        break;
                    case XSharpLexer.RBRKT:
                        addErrorMessage(token, "Closing bracket ']' found with missing '['");
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
            // Now check for the tokens following list and repeat markers
            // So we know how to find the end of the list
            // For the command below the <list,..> token is ended when the
            // following tokens are found: OFF, TO, FOR, WHILE, NEXT, RECORD, REST and ALL
            /*
             * #command LIST [<list,...>]                                              ;
                         [<off:OFF>]                                                    ;
                         [<toPrint: TO PRINTER>]                                        ;
                         [TO FILE <(toFile)>]                                           ;
                         [FOR <for>]                                                    ;
                         [WHILE <while>]                                                ;
                         [NEXT <next>]                                                  ;
                         [RECORD <rec>]                                                 ;
                         [<rest:REST>]                                                  ;
                         [ALL]                                                          ;
             *
             */
            // The stopTokens list may contain duplicate elements. In the example above
            // it will have TO twice. That is not a problem.
            var mt = new PPMatchToken[result.Count];
            result.CopyTo(mt);
            if (nestLevel == 0)
            {
                for (int i = 0; i < mt.Length; i++)
                {
                    var marker = mt[i];
                    if (marker.RuleTokenType == PPTokenType.MatchList || marker.IsRepeat)
                    {
                        var stopTokens = new List<XSharpToken>();
                        findStopTokens(mt, i + 1, stopTokens);
                        marker.Tokens = stopTokens.ToArray();
                    }
                    if (marker.IsOptional)
                    {
                        foreach (var child in marker.Children)
                        {
                            if (child.RuleTokenType == PPTokenType.MatchList)
                            {
                                var stopTokens = new List<XSharpToken>();
                                findStopTokens(mt, i + 1, stopTokens);
                                child.Tokens = stopTokens.ToArray();
                                break;
                            }
                        }
                    }
                }
            }
            // build linear list of matchtokens
            if (nestLevel == 0)
            {
                var linearlist = new List<PPMatchToken>();
                addMatchTokens(mt, linearlist);
                _matchTokensFlattened = linearlist.ToArray();
                tokenCount = _matchTokensFlattened.Length;
            }
            return mt;
        }
        void addMatchTokens(PPMatchToken[] source, List<PPMatchToken> list)
        {
            foreach (var token in source)
            {
                list.Add(token);
                if (token.Children != null)
                {
                    addMatchTokens(token.Children, list);
                }
            }
        }

        void findStopTokens(PPMatchToken[] matchmarkers, int iStart, IList<XSharpToken> stoptokens)
        {
            bool finished = false;
            for (int j = iStart; j < matchmarkers.Length && !finished; j++)
            {
                var next = matchmarkers[j];
                switch (next.RuleTokenType)
                {
                    case PPTokenType.MatchOptional:
                        // get first token in the Children of the optional clause
                        findStopTokens(next.Children, 0, stoptokens);
                        break;
                    case PPTokenType.MatchRestricted:
                        foreach (var token in next.Tokens)
                        {
                            if (token.Type != XSharpLexer.COMMA)
                                stoptokens.Add(token);
                        }
                        break;
                    case PPTokenType.Token:
                        stoptokens.Add(next.Token);
                        finished = true;
                        break;
                }
            }
        }

        List<XSharpToken> getNestedTokens(int start, int max, XSharpToken[] tokens)
        {
            XSharpToken token = tokens[start];
            List<XSharpToken> result = null;
            bool missing = false;
            var lbrkt = token;
            if (start < max - 1)   // must have at least [ name ]
            {
                result = new List<XSharpToken>();
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
                if (nestlevel > 0)
                {
                    missing = true;
                }
            }
            if (result == null || result.Count == 0)
            {
                token = tokens[max - 1];
                if (token.Type == XSharpLexer.RBRKT)
                    addErrorMessage(lbrkt, "Empty Optional clause found");
                else
                    missing = true;
            }
            if (missing)
                addErrorMessage(lbrkt, "Unclosed optional clause found (']' is missing)");
            return result;
        }
        PPResultToken[] analyzeResultTokens(XSharpToken[] resultTokens, int nestLevel)
        {
            var result = new List<PPResultToken>();
            var max = resultTokens.Length;
            List<XSharpToken> more;
            XSharpToken name;
            int lastTokenIndex = -1;
            ITokenSource lastTokenSource = null;
            for (int i = 0; i < resultTokens.Length; i++)
            {
                var token = resultTokens[i];
                if (token.TokenSource == lastTokenSource && token.TokenIndex > lastTokenIndex + 1)
                {
                    // whitespace tokens have been skipped
                    var ppWs = new XSharpToken(token, XSharpLexer.WS, " ");
                    ppWs.Channel = ppWs.OriginalChannel = XSharpLexer.Hidden;
                    result.Add(new PPResultToken(ppWs, PPTokenType.Token));
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
                            result.Add(new PPResultToken(name, PPTokenType.ResultDumbStringify));
                            i += 3;
                        }
                        else
                        {
                            result.Add(new PPResultToken(token, PPTokenType.Token));
                        }
                        break;
                    case XSharpLexer.LT:
                        if (i < resultTokens.Length - 2
                            && resultTokens[i + 2].Type == XSharpLexer.GT
                            && resultTokens[i + 1].IsName())
                        {
                            // <idMarker>
                            name = resultTokens[i + 1];
                            result.Add(new PPResultToken(name, PPTokenType.ResultRegular));
                            i += 2;
                        }
                        else if (i < resultTokens.Length - 2
                            && resultTokens[i + 1].Type == XSharpLexer.STRING_CONST
                            && resultTokens[i + 2].Type == XSharpLexer.GT)
                        {
                            // <"idMarker">
                            var t = resultTokens[i + 1];
                            name = new XSharpToken(t, XSharpLexer.ID, t.Text.Substring(1, t.Text.Length - 2));
                            result.Add(new PPResultToken(name, PPTokenType.ResultNormalStringify));
                            i += 2;
                        }
                        else if (i < resultTokens.Length - 4
                            && resultTokens[i + 1].Type == XSharpLexer.LPAREN
                            && resultTokens[i + 3].Type == XSharpLexer.RPAREN
                            && resultTokens[i + 4].Type == XSharpLexer.GT
                            && resultTokens[i + 2].IsName())
                        {
                            // <(idMarker)>
                            name = resultTokens[i + 2];
                            result.Add(new PPResultToken(name, PPTokenType.ResultSmartStringify));
                            i += 4;
                        }
                        else if (i < resultTokens.Length - 4
                            && resultTokens[i + 1].Type == XSharpLexer.LCURLY
                            && resultTokens[i + 3].Type == XSharpLexer.RCURLY
                            && resultTokens[i + 4].Type == XSharpLexer.GT
                            && resultTokens[i + 2].IsName())
                        {
                            // <{idMarker}>
                            name = resultTokens[i + 2];
                            result.Add(new PPResultToken(name, PPTokenType.ResultBlockify));
                            i += 4;
                        }
                        else if (i < resultTokens.Length - 4
                            && resultTokens[i + 1].Type == XSharpLexer.DOT
                            && resultTokens[i + 3].Type == XSharpLexer.DOT
                            && resultTokens[i + 4].Type == XSharpLexer.GT
                            && resultTokens[i + 2].IsName())
                        {
                            // <.idMarker.>
                            name = resultTokens[i + 2];
                            result.Add(new PPResultToken(name, PPTokenType.ResultLogify));
                            i += 4;
                        }

                        break;
                    case XSharpLexer.LBRKT:
                        /*
                         * eat block
                        * [ ... ], 
                        * when nested then this is seen as a repeated result clause
                         */
                        if (nestLevel == 0)
                        {
                            more = getNestedTokens(i, max, resultTokens);
                            if (more != null)
                            {
                                i = i + more.Count + 1;
                                var nested = analyzeResultTokens(more.ToArray(), nestLevel + 1);
                                PPResultToken marker = null;
                                foreach (var e in nested)
                                {
                                    if (e.IsMarker)
                                    {
                                        marker = e;
                                        break;
                                    }
                                }
                                if (marker == null)
                                {
                                    _errorMessages.Add(new PPErrorMessage(token, "Repeated result block does not contain a match marker"));
                                }
                                else
                                {
                                    var element = new PPResultToken(token, PPTokenType.ResultOptional, marker.Key);
                                    element.OptionalElements = nested;
                                    result.Add(element);
                                    hasOptionalResult = true;

                                }

                            }
                        }
                        else
                        {
                            _errorMessages.Add(new PPErrorMessage(token, "Nested result markers are not allowed"));
                        }
                        break;
                    case XSharpLexer.BACKSLASH: // escape next token
                        if (i < max)
                        {
                            i++;
                            token = resultTokens[i];
                            result.Add(new PPResultToken(token, PPTokenType.Token));
                        }
                        break;

                    default:
                        result.Add(new PPResultToken(token, PPTokenType.Token));
                        break;
                }

            }
            var resulttokens = new PPResultToken[result.Count];
            result.CopyTo(resulttokens);
            return resulttokens;

        }
        /// <summary>
        /// Returns the key that is stored in the RuleTable. Depending on the type of the UDC
        /// This is the while first word or the first word abbreviated to 4 chars (dBase compatible)
        /// </summary>
        internal string LookupKey
        {
            get
            {
                string key = this.Key;
                if (key.Length > 4)
                {
                    if (_type == PPUDCType.Command || _type == PPUDCType.Translate)
                    {
                        key = key.Substring(0, 4);
                    }
                }
                return key;
            }
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
                    result += this.Type.ToString() + " ";
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
        bool tokenEquals(XSharpToken lhs, XSharpToken rhs)
        {
            if (lhs != null && rhs != null)
                return stringEquals(lhs.Text, rhs.Text);
            return false;
        }
        bool stringEquals(string lhs, string rhs)
        {
            var mode = StringComparison.OrdinalIgnoreCase;
            if (this.Type == PPUDCType.Define && !CaseInsensitive)
            {
                mode = StringComparison.Ordinal;    // case sensitive
            }
            if (lhs?.Length <= 4)
            {
                return String.Equals(lhs, rhs, mode);
            }
            switch (this.Type)
            {
                case PPUDCType.Command:
                case PPUDCType.Translate:
                    // dBase/Clipper syntax 4 characters is enough
                    return String.Compare(lhs, 0, rhs, 0, rhs.Length, mode) == 0;
                case PPUDCType.XCommand:
                case PPUDCType.XTranslate:
                case PPUDCType.Define:
                    return String.Equals(lhs, rhs, mode);
            }
            return false;
        }

        internal bool matchListToken(PPMatchToken mToken, IList<XSharpToken> tokens, ref int iSource, PPMatchRange[] matchInfo)
        {
            // This should match a list of expressions
            // until one of the tokens in mToken.Tokens is found
            // comma's are NOT required
            // See for example Std.ch from Clipper:
            /*
             * #command @ <row>, <col> SAY <sayxpr>                                    ;
                        [<sayClauses,...>]                              ;
                        GET <var>                                       ;
                        [<getClauses,...>]                              ;
                      => @ <row>, <col> SAY <sayxpr> [<sayClauses>]                     ;
                       ; @ Row(), Col()+1 GET <var> [<getClauses>]


             */
            if (mToken.RuleTokenType != PPTokenType.MatchList)
                return false;
            var stopTokens = mToken.Tokens;
            int iStart = iSource;
            var matches = new List<Tuple<int, int>>();
            bool stopTokenFound = false;
            while (iSource < tokens.Count)
            {
                var token = tokens[iSource];
                // check to see if the current token is in the stoptokens list
                foreach (var stopToken in stopTokens)
                {
                    if (tokenEquals(stopToken, token))
                    {
                        stopTokenFound = true;
                        break;
                    }
                }
                if (stopTokenFound)
                {
                    break;
                }
                // after matchExpresssion iSource points to the next token after the expression
                int iEnd = matchExpression(iSource, tokens, null);
                if (iEnd != iSource)
                {
                    matches.Add(new Tuple<int, int>(iSource, iEnd - 1));
                    iSource = iEnd;
                }
                // IsOperator included comma, ellipses etc.
                else if (XSharpLexer.IsOperator(token.Type))
                {
                    matches.Add(new Tuple<int, int>(iSource, iSource));
                    iSource += 1;
                }
                else
                {
                    return false;
                }
            }
            foreach (var m in matches)
            {
                matchInfo[mToken.Index].SetPos(m.Item1, m.Item2);
            }
            return true;
        }
        internal bool matchToken(PPMatchToken mToken, ref int iRule, int iLastRule, ref int iSource, IList<XSharpToken> tokens, PPMatchRange[] matchInfo, IList<XSharpToken> matchedWithToken)
        {
            XSharpToken sourceToken = tokens[iSource];
            XSharpToken ruleToken = mToken.Token;
            int iChild = 0;
            int iEnd;
            bool found = false;
            while (sourceToken.Type == XSharpLexer.WS)
            {
                iSource += 1;
                sourceToken = tokens[iSource];
            }
            switch (mToken.RuleTokenType)
            {
                case PPTokenType.Token:
                    if (this.tokenEquals(ruleToken, sourceToken))
                    {
                        matchInfo[mToken.Index].SetToken(iSource);
                        iSource += 1;
                        iRule += 1;
                        found = true;
                        matchedWithToken.Add(sourceToken);
                    }
                    break;
                case PPTokenType.MatchRegular:
                    // Matches an expression
                    // use Expression method to find the end of the list
                    // iEnd points to the next token after the expression
                    iEnd = matchExpression(iSource, tokens, null);
                    int iLastUsed = iEnd - 1;
                    // truncate spaces at the end
                    iLastUsed = trimHiddenTokens(tokens, iSource, iLastUsed);

                    matchInfo[mToken.Index].SetPos(iSource, iLastUsed);
                    iSource = iEnd;
                    iRule += 1;
                    found = true;
                    break;
                case PPTokenType.MatchList:
                    // ignore for now
                    if (matchListToken(mToken, tokens, ref iSource, matchInfo))
                    {
                        iRule += 1;
                        found = true;
                    }
                    break;

                case PPTokenType.MatchRestricted:
                    // match the words in the list.
                    // the token in the rule is the match marker
                    // the words to be checked are the MoreTokens
                    // This list includes the original commas because some restricted markers can have more than word:
                    // LIST ....  [<toPrint: TO PRINTER>] 
                    // where others have a list of single words
                    // #command SET CENTURY <x:ON,OFF,&>      => __SetCentury( <(x)> ) 
                    XSharpToken lastToken = null;
                    int iLast = mToken.Tokens.Length - 1;
                    int iMatch = 0;
                    int iCurrent = iSource;
                    for (iChild = 0; iChild <= iLast; iChild++)
                    {
                        var child = mToken.Tokens[iChild];
                        lastToken = child;
                        if (child.Type == XSharpLexer.COMMA)
                        {
                            iMatch = 0;
                        }
                        else if (tokenEquals(child, sourceToken))
                        {
                            iMatch += 1;
                            if (iChild == iLast) // No token following this one
                            {
                                break;
                            }
                            // next token comma ?
                            var next = mToken.Tokens[iChild + 1];
                            if (next.Type == XSharpLexer.COMMA)
                            {
                                break;
                            }
                            iCurrent += 1;
                            sourceToken = tokens[iCurrent];
                        }
                        else
                        {
                            iMatch = 0;
                        }
                    }
                    found = iMatch > 0;
                    if (found)
                    {
                        iEnd = iCurrent;
                        if (lastToken.Type == XSharpLexer.AMP)
                        {
                            // when the ampersand is the last token, then we also include the following token
                            // This happens when we match the rule
                            // #command SET CENTURY <x:ON,OFF,&>      => __SetCentury( <(x)> ) 
                            // with the source SET CENTURY &MyVar
                            // Thus generates the output __SetCentury((MyVar))
                            if (lastToken == mToken.Tokens[mToken.Tokens.Length - 1] && tokens.Count > iEnd)
                            {
                                iEnd += 1;
                                iSource = iEnd;
                            }
                        }
                        // truncate spaces at the end
                        iEnd = trimHiddenTokens(tokens, iSource, iEnd);
                        matchInfo[mToken.Index].SetPos(iSource, iEnd);
                        for (int i = iSource; i <= iEnd; i++)
                        {
                            matchedWithToken.Add(tokens[i]);
                        }
                        iSource = iEnd + 1;
                        iRule += 1;
                    }
                    break;
                case PPTokenType.MatchWild:
                    iEnd = tokens.Count - 1;
                    // truncate spaces at the end
                    iEnd = trimHiddenTokens(tokens, iSource, iEnd);
                    matchInfo[mToken.Index].SetPos(iSource, iEnd);
                    iSource = tokens.Count;
                    iRule += 1;
                    found = true;                    // matches anything until the end of the list
                    break;
                case PPTokenType.MatchExtended:
                    // either match a single token or a token in parentheses
                    if (sourceToken.Type == XSharpLexer.LPAREN)
                    {
                        if (iSource < tokens.Count - 2 &&
                            tokens[iSource + 1].IsName() &&
                            tokens[iSource + 2].Type == XSharpLexer.RPAREN)
                        {
                            // Ok
                            matchInfo[mToken.Index].SetPos(iSource, iSource + 2);
                            iSource += 1;
                            iRule += 1;
                            found = true;
                        }
                    }
                    else // match single token
                    {
                        matchInfo[mToken.Index].SetPos(iSource, iSource);
                        iSource += 1;
                        iRule += 1;
                        found = true;
                    }
                    break;
                case PPTokenType.MatchOptional:
                    // 
                    // Get sublist of optional token and match with source
                    var optional = mToken.Children;
                    bool optfound = true;
                    int iOriginal = iSource;
                    iChild = 0;
                    var children = matchInfo[mToken.Index].Children;
                    while (iChild < optional.Length && iSource < tokens.Count && optfound)
                    {
                        var mchild = optional[iChild];
                        if (!matchToken(mchild, ref iChild, matchInfo.Length, ref iSource, tokens, matchInfo, matchedWithToken))
                        {
                            optfound = false;
                        }
                    }
                    if (optfound)
                    {
                        found = true;
                        if (!mToken.IsRepeat)
                        {
                            iRule += 1;
                        }
                        iEnd = iSource - 1;
                        // truncate spaces at the end
                        iEnd = trimHiddenTokens(tokens, iSource, iEnd);
                        matchInfo[mToken.Index].SetPos(iOriginal, iEnd);
                    }
                    else
                        iSource = iOriginal;
                    break;
            }
            return found;
        }
        int trimHiddenTokens(IList<XSharpToken> tokens, int start, int end)
        {
            while (tokens[end].Channel == XSharpLexer.Hidden && start < end)
            {
                end--;
            }

            return end;
        }
        internal bool Matches(IList<XSharpToken> tokens, out PPMatchRange[] matchInfo)
        {
            int iRule = 0;
            int iSource = 0;
            matchInfo = new PPMatchRange[tokenCount];
            List<XSharpToken> matchedWithToken = new List<XSharpToken>();
            int firstOptional = -1;
            bool hasSkippedMarkers = false;
            while (iRule < _matchtokens.Length && iSource < tokens.Count)
            {
                var mtoken = _matchtokens[iRule];
                if (mtoken.IsOptional && firstOptional == -1)
                    firstOptional = iRule;
                if (!matchToken(mtoken, ref iRule, _matchtokens.Length, ref iSource, tokens, matchInfo, matchedWithToken))
                {
                    if (!mtoken.IsOptional)
                        return false;
                    matchInfo[mtoken.Index].SetSkipped();
                    hasSkippedMarkers = true;
                    iRule++;
                }
            }
            // try to match remaining optional match markers until no matching 
            // items are found or until the end of the token list is reached
            while (hasSkippedMarkers)
            {
                hasSkippedMarkers = false;
                bool hasMatchedAnItem = false;
                for (int i = firstOptional; i < _matchtokens.Length && iSource < tokens.Count; i++)
                {
                    var mtoken = _matchtokens[i];
                    if (matchInfo[mtoken.Index].MatchCount == 0 || mtoken.IsOptional)   // optional tokens may appear more than once
                    {
                        if (!matchToken(mtoken, ref iRule, _matchtokens.Length, ref iSource, tokens, matchInfo, matchedWithToken))
                        {
                            hasSkippedMarkers = true;
                        }
                        else
                        {
                            hasMatchedAnItem = true;
                        }
                    }
                }
                if (!hasMatchedAnItem)
                    break;
            }
            // #command and #xcommand should match all tokens on the input line
            if (iSource < tokens.Count && (this.Type == PPUDCType.Command || this.Type == PPUDCType.XCommand))
            {
                if (tokens[iSource].Type != XSharpLexer.EOS)
                    return false;
            }
            // check to see if the remaining tokens are optional, when not, then there is no match
            while (iRule < _matchtokens.Length)
            {
                if (!_matchtokens[iRule].IsOptional)
                {
                    return false;
                }
                if (matchInfo[iRule].Empty)
                {
                    matchInfo[iRule].SetSkipped();
                }
                iRule++;
            }


            // Now mark the tokens that were matched with tokens in the UDC with the keyword color
            // Since our token may be a clone, we change the Type of the source token
            foreach (var token in matchedWithToken)
            {
                if (token.Type == XSharpLexer.ID)
                {
                    token.Original.Type = XSharpLexer.UDC_KEYWORD;
                }
            }

            return true;

        }
        internal IList<XSharpToken> Replace(IList<XSharpToken> tokens, PPMatchRange[] matchInfo)
        {
            Debug.Assert(matchInfo.Length == tokenCount);
            return Replace(_resulttokens, tokens, matchInfo, 0);

        }
        internal IList<XSharpToken> Replace(PPResultToken[] resulttokens, IList<XSharpToken> tokens, PPMatchRange[] matchInfo, int offset)
        {
            Debug.Assert(matchInfo.Length == tokenCount);
            List<XSharpToken> result = new List<XSharpToken>();
            foreach (var resultToken in resulttokens)
            {
                switch (resultToken.RuleTokenType)
                {
                    case PPTokenType.Token:
                        tokenResult(resultToken, tokens, matchInfo, result, offset);
                        break;
                    case PPTokenType.ResultLogify:
                        // check to see if the token was matched.
                        logifyResult(resultToken, tokens, matchInfo, result, offset);
                        break;
                    case PPTokenType.ResultBlockify:
                        blockifyResult(resultToken, tokens, matchInfo, result, offset);
                        break;
                    case PPTokenType.ResultRegular:
                        regularResult(resultToken, tokens, matchInfo, result, offset);
                        break;
                    case PPTokenType.ResultSmartStringify:
                    case PPTokenType.ResultNormalStringify:
                    case PPTokenType.ResultDumbStringify:
                        stringifyResult(resultToken, tokens, matchInfo, result, offset);
                        break;
                    case PPTokenType.ResultOptional:
                        repeatedResult(resultToken, tokens, matchInfo, result, offset);
                        break;
                }
            }
            // we need to determine the tokens at the end of the tokens list that are not matched 
            // in the results and then copy these to the result as well
            if (offset == 0)
            {
                var source = tokens[0];
                if (source.SourceSymbol != null)
                {
                    source = source.SourceSymbol;
                }
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
                foreach (var t in result)
                {
                    if (t.Channel == XSharpLexer.PREPROCESSORCHANNEL)
                    {
                        t.Channel = t.OriginalChannel = XSharpLexer.DefaultTokenChannel;
                    }
                    t.SourceSymbol = source;
                }
            }
            result.TrimLeadingSpaces();
            return result;
        }

        void tokenResult(PPResultToken rule, IList<XSharpToken> tokens, PPMatchRange[] matchInfo, IList<XSharpToken> result, int offset)
        {

            var newToken = new XSharpToken(rule.Token);
            newToken.SourceSymbol = tokens[0];
            result.Add(newToken);

        }
        void repeatedResult(PPResultToken rule, IList<XSharpToken> tokens, PPMatchRange[] matchInfo, IList<XSharpToken> result, int offset)
        {
            if (rule.MatchMarker != null)
            {
                var index = rule.MatchMarker.Index;
                var mm = matchInfo[index];
                if (mm.MatchCount > 0)
                {
                    if (mm.IsList)  // multiple results, so repeat for each element in the Children list
                    {
                        int repeats = mm.MatchCount;
                        for (int i = 0; i < repeats; i++)
                        {
                            var block = Replace(rule.OptionalElements, tokens, matchInfo, i);
                            foreach (var e in block)
                            {
                                result.Add(e);
                            }
                        }
                    }
                    else
                    {
                        var block = Replace(rule.OptionalElements, tokens, matchInfo, 0);
                        foreach (var e in block)
                        {
                            result.Add(e);
                        }
                    }
                }
            }
            return;
        }


        void regularResult(PPResultToken rule, IList<XSharpToken> tokens, PPMatchRange[] matchInfo, IList<XSharpToken> result, int offset)
        {
            // write input text to the result text without no changes
            // for example:
            // #command SET CENTURY (<x>) => __SetCentury( <x> )
            // the output written is the literal text for x, so the token(s) x
            var range = matchInfo[rule.MatchMarker.Index];
            if (!range.Empty)
            {
                // No special handling for List markers. Everything is copied including commas etc.
                if (range.MatchCount > 1)
                {
                    range = range.Children[offset];
                }
                for (int i = range.Start; i <= range.End && i < tokens.Count; i++)
                {
                    var token = tokens[i];
                    result.Add(token);
                }
            }
            return;
        }

        void blockifySingleResult(PPResultToken rule, IList<XSharpToken> tokens, PPMatchRange range, IList<XSharpToken> result)
        {
            int start = range.Start;
            int end = range.End;
            bool addBlockMarker = true;
            XSharpToken nt;
            XSharpToken t;
            if (range.Length == 1)
            {
                // for comma's and other separators
                if (XSharpLexer.IsOperator(tokens[start].Type))
                {
                    result.Add(tokens[start]);
                    return;
                }
            }
            if (end - start > 4)
            {
                if (tokens[start].Type == XSharpLexer.LCURLY &&
                    (tokens[start + 1].Type == XSharpLexer.PIPE ||
                     tokens[start + 1].Type == XSharpLexer.OR) &&
                    tokens[end].Type == XSharpLexer.RCURLY)
                {
                    addBlockMarker = false;
                }
            }
            if (addBlockMarker)
            {
                t = tokens[start];
                nt = new XSharpToken(t, XSharpLexer.LCURLY, "{");
                nt.Channel = XSharpLexer.DefaultTokenChannel;
                result.Add(nt);
                nt = new XSharpToken(t, XSharpLexer.OR, "||");
                nt.Channel = XSharpLexer.DefaultTokenChannel;
                result.Add(nt);
            }
            for (int i = start; i <= end; i++)
            {
                var token = tokens[i];
                result.Add(token);
            }
            if (addBlockMarker)
            {
                t = tokens[end];
                nt = new XSharpToken(t, XSharpLexer.RCURLY, "}");
                nt.Channel = XSharpLexer.DefaultTokenChannel;
                result.Add(nt);
            }
        }


        void blockifyResult(PPResultToken rule, IList<XSharpToken> tokens, PPMatchRange[] matchInfo, IList<XSharpToken> result, int offset)
        {
            // write output text as codeblock
            // so prefixes with "{||"and suffixes with "}
            // if the input is already a code block then nothing is changed
            // when the input is a list then each element in the list will be
            // converted to a code block
            var range = matchInfo[rule.MatchMarker.Index];
            if (!range.Empty)
            {
                if (range.MatchCount > 1)
                {
                    range = range.Children[offset];
                }
                if (rule.MatchMarker.RuleTokenType == PPTokenType.MatchList && range.IsList)
                {
                    foreach (var element in range.Children)
                    {
                        blockifySingleResult(rule, tokens, element, result);
                    }
                }
                else
                {
                    blockifySingleResult(rule, tokens, range, result);
                }
            }
            return;
        }

        void stringifySingleResult(PPResultToken rule, IList<XSharpToken> tokens, PPMatchRange range, IList<XSharpToken> result)
        {
            XSharpToken newToken;
            var start = range.Start;
            var end = range.End;
            if (range.Length == 1)
            {
                // for comma's and other separators
                if (XSharpLexer.IsOperator(tokens[start].Type))
                {
                    result.Add(tokens[start]);
                    return;
                }
            }

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
                    if (!range.Empty)
                    {
                        // we cannot take an interval and the source stream. That will not work if a transformation has been done already
                        var sb = new System.Text.StringBuilder();
                        sb.Append('"');
                        for (int i = start; i <= end; i++)
                        {
                            var token = tokens[i];
                            sb.Append(token.Text);
                        }
                        sb.Append('"');
                        var nt = new XSharpToken(tokens[start], XSharpLexer.STRING_CONST, sb.ToString());
                        nt.Channel = XSharpLexer.DefaultTokenChannel;
                        result.Add(nt);
                    }
                    else
                    {
                        // no match, then dumb stringify write an empty string
                        var nt = new XSharpToken(tokens[0], XSharpLexer.NULL_STRING, "NULL_STRING");
                        nt.Channel = XSharpLexer.DefaultTokenChannel;
                        result.Add(nt);
                    }
                    break;
                case PPTokenType.ResultNormalStringify:
                    // Delimit the input with string delimiters
                    if (!range.Empty)
                    {
                        // we cannot take an interval and the source stream. That will not work if a transformation has been done already
                        var sb = new System.Text.StringBuilder();
                        sb.Append('"');
                        for (int i = start; i <= end; i++)
                        {
                            var token = tokens[i];
                            sb.Append(token.Text);
                        }
                        sb.Append('"');
                        var nt = new XSharpToken(tokens[start], XSharpLexer.STRING_CONST, sb.ToString());
                        nt.Channel = XSharpLexer.DefaultTokenChannel;
                        result.Add(nt);
                    }
                    break;


                case PPTokenType.ResultSmartStringify:
                    // Only works when input text is delimited with parentheses
                    // if the match marker is a list then each element is stringified and it stays a list
                    // for example: 
                    // #command SET CENTURY <x:ON,OFF,&>      => __SetCentury( <(x)> )
                    // the contents of x must be converted to a string

                    if (!range.Empty)
                    {
                        if (start == end)
                        {
                            // single token
                            var token = tokens[start];
                            switch (token.Type)
                            {
                                case XSharpLexer.CHAR_CONST:
                                case XSharpLexer.STRING_CONST:
                                case XSharpLexer.ESCAPED_STRING_CONST:
                                case XSharpLexer.INTERPOLATED_STRING_CONST:
                                    result.Add(token);
                                    break;
                                default:
                                    newToken = new XSharpToken(token, XSharpLexer.STRING_CONST, token.Text);
                                    newToken.Text = "\"" + token.Text + "\"";
                                    newToken.Channel = XSharpLexer.DefaultTokenChannel;
                                    result.Add(newToken);
                                    break;
                            }
                        }
                        else
                        {
                            bool addDelimiters = true;
                            if (tokens[start].Type == XSharpLexer.LPAREN &&
                                tokens[end].Type == XSharpLexer.RPAREN)
                                addDelimiters = false;
                            var sb = new System.Text.StringBuilder();
                            if (addDelimiters)
                                sb.Append('"');
                            for (int i = start; i <= end; i++)
                            {
                                var token = tokens[i];
                                sb.Append(token.Text);
                            }
                            if (addDelimiters)
                                sb.Append('"');
                            newToken = new XSharpToken(tokens[start], XSharpLexer.STRING_CONST, sb.ToString());
                            newToken.Channel = XSharpLexer.DefaultTokenChannel;
                            result.Add(newToken);
                        }
                    }
                    break;
            }
            return;

        }

        void stringifyResult(PPResultToken rule, IList<XSharpToken> tokens, PPMatchRange[] matchInfo, IList<XSharpToken> result, int offset)
        {
            var range = matchInfo[rule.MatchMarker.Index];
            if (!range.Empty)
            {
                if (range.MatchCount > 1)
                {
                    range = range.Children[offset];
                }
                if (rule.MatchMarker.RuleTokenType == PPTokenType.MatchList && range.IsList)
                {
                    bool first = true;
                    foreach (var element in range.Children)
                    {
                        if (!first)
                            result.Add(new XSharpToken(XSharpLexer.COMMA, ","));
                        stringifySingleResult(rule, tokens, element, result);
                        first = false;

                    }
                }
                else
                {
                    stringifySingleResult(rule, tokens, range, result);
                }
            }

        }

        void logifyResult(PPResultToken rule, IList<XSharpToken> tokens, PPMatchRange[] matchInfo, IList<XSharpToken> result, int offset)
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

            var range = matchInfo[rule.MatchMarker.Index];
            if (!range.Empty)
            {
                if (range.MatchCount > 1)
                {
                    range = range.Children[offset];
                }
                XSharpToken t = tokens[range.Start];
                t = new XSharpToken(t, XSharpLexer.TRUE_CONST, ".T.");
                t.Channel = XSharpLexer.DefaultTokenChannel;
                result.Add(t);
            }
            else
            {
                // No token to read the line/column from
                result.Add(new XSharpToken(XSharpLexer.FALSE_CONST, ".F."));
            }
            return;
        }

        bool tokenCanStartExpression(int pos, IList<XSharpToken> tokens)
        {
            XSharpToken token;
            token = tokens[pos];
            if (!token.NeedsLeft() && !token.IsEndOfCommand())
            {
                return true;
            }
            return false;
        }
        int matchExpression(int start, IList<XSharpToken> tokens, XSharpToken stopToken)
        {
            if (!tokenCanStartExpression(start, tokens))
                return start;
            int current = start;
            int braceLevel = 0;
            int count = tokens.Count;
            int openBrace = 0;
            int closeBrace = 0;
            XSharpToken token;
            XSharpToken lastToken = null;
            while (current < count)
            {
                token = tokens[current];
                if (braceLevel > 0)
                {
                    if (token.Type == openBrace)
                    {
                        braceLevel += 1;
                    }
                    else if (token.Type == closeBrace)
                    {
                        braceLevel -= 1;
                    }
                    if (braceLevel == 0)
                    {
                        // the expression could be something like
                        // Foo()+Bar()
                        // So continue normally
                        lastToken = token;
                    }
                }
                else if (token.Type == XSharpLexer.WS)
                {
                    ;
                }
                else if (token.Type == XSharpLexer.COMMA)
                {
                    // expression can only have comma's when inside braces, parens or brackets
                    // so exit the loop
                    break;
                }
                // if Open brace, scan for close brace
                else if (token.IsOpen(ref closeBrace))
                {
                    openBrace = token.Type;
                    braceLevel++;
                }
                // check to see if we have 2 tokens that can follow
                // otherwise exit
                else if (token.IsClose()
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
    }
}



