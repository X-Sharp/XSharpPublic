//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using XSharp.MacroCompiler.Syntax;

namespace XSharp.MacroCompiler.Preprocessor
{
    using XSharpLexer = TokenType;
    using XSharpToken = Token;
    using CSharpParseOptions = MacroOptions;
    [Flags]
    internal enum PPRuleFlags : byte
    {
        None = 0,
        HasRepeats = 1 << 0,
        HasOptionalResult = 1 << 1,
        HasOptionalMatch = 1 << 2,
        VOPreprocessorBehaviour = 1 << 3
    }
    [DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
    internal class PPRule
    {
        private readonly PPUDCType _type;
        private PPMatchToken[] _matchtokens;
        private PPMatchToken[] _matchTokensFlattened;
        private int _tokenCount = 0;
        private PPResultToken[] _resulttokens;
        private readonly PPErrorMessages _errorMessages;
        internal PPRuleFlags _flags;
        internal bool VOPreprocessorBehaviour => _flags.HasFlag(PPRuleFlags.VOPreprocessorBehaviour);
        internal bool hasRepeats => _flags.HasFlag(PPRuleFlags.HasRepeats);
        internal bool hasOptionalResult => _flags.HasFlag(PPRuleFlags.HasOptionalResult);
        internal bool hasOptionalMatch => _flags.HasFlag(PPRuleFlags.HasOptionalMatch);
        internal int _firstOptionalMatchToken = -1;
        internal bool hasMultiKeys => _matchtokens.Length > 0 && _matchtokens[0].RuleTokenType == PPTokenType.MatchRestricted;
        private readonly CSharpParseOptions _options;
        internal PPUDCType Type { get { return _type; } }
        internal bool isCommand => _type == PPUDCType.Command || _type == PPUDCType.XCommand || _type == PPUDCType.YCommand; 

        internal PPRule(XSharpToken udc, IList<XSharpToken> tokens, out PPErrorMessages errorMessages, CSharpParseOptions options)
        {
            _options = options;
            switch (udc.Type)
            {
                case XSharpLexer.PP_COMMAND:
                    if (udc.Text.ToLower() == "#command")
                        _type = PPUDCType.Command;
                    else if (udc.Text.ToLower() == "#xcommand")
                        _type = PPUDCType.XCommand;
                    else 
                        _type = PPUDCType.YCommand;
                    break;
                case XSharpLexer.PP_TRANSLATE:
                    if (udc.Text.ToLower().StartsWith("#trans"))
                        _type = PPUDCType.Translate;
                    else if (udc.Text.ToLower() == "#xtranslate")
                        _type = PPUDCType.XTranslate;
                    else
                        _type = PPUDCType.YTranslate;
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
            // inside a #define the tokens are case sensitive, depending in the /vo8 option
            var markers = new Dictionary<string, PPMatchToken>(_options.VOPreprocessorBehaviour ? StringComparer.OrdinalIgnoreCase : StringComparer.Ordinal);
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
                        var rtoken = new PPResultToken(token, PPTokenType.ResultRegular)
                        {
                            MatchMarker = mtoken
                        };
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
            var udctoken = new PPMatchToken(new XSharpToken(XSharpLexer.ID, "udc"), PPTokenType.MatchWholeUDC);
            addToDict(markers, udctoken);
            matchTokens.Add(udctoken);
            _matchtokens = matchTokens.ToArray();
            var linearlist = new List<PPMatchToken>();
            addMatchTokens(_matchtokens, linearlist);
            _matchTokensFlattened = linearlist.ToArray();
            _tokenCount = _matchTokensFlattened.Length;
            _resulttokens = resultTokens.ToArray();
            checkMatchingTokens(_resulttokens, markers);
            return _errorMessages == null || _errorMessages.Count == 0;
        }

        bool parseRuleTokens(XSharpToken udc, XSharpToken[] tokens)
        {
            int iSeperatorPos = -1;
            // Inside #command, #translate etc the markers are always case insensitive
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
                        _flags |= PPRuleFlags.HasRepeats;
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
                        if (m.IsRepeat)
                        {
                            foreach (var c in m.Children)
                            {
                                c.IsRepeat = true;
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
                        if (r.IsRepeat)
                        {
                            foreach (var c in r.OptionalElements)
                            {
                                c.IsRepeat = true;
                            }
                        }
                    }
                }
            }
        }
        bool canAbbreviate => _type == PPUDCType.Command || _type == PPUDCType.Translate;
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
                restoken.Index = r;
                if (restoken.IsMarker)
                {
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
            if (element.Token.IsName() || element.Token.Type == XSharpLexer.SYMBOL_CONST || element.IsWholeUDC)
            {
                string name = element.Key.ToLower();
                if (name == "udc" && !element.IsWholeUDC)
                {
                    addErrorMessage(element.Token, $"The Match marker name '{element.Key}' is reserved");
                }
                else
                {
                    if (!markers.ContainsKey(name))
                    {
                        markers.Add(name, element);
                    }
                    else
                    {
                        addErrorMessage(element.Token, $"Duplicate Match marker '{element.Key}' found in UDC");
                    }
                }
            }

        }
        PPMatchToken[] analyzeMatchTokens(XSharpToken[] matchTokens, Dictionary<string, PPMatchToken> markers, int nestLevel = 0)
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
                        if (matchTokens.La(i + 2) == XSharpLexer.GT
                            && matchTokens.IsName(i + 1))
                        {
                            // <idMarker>
                            name = matchTokens[i + 1];
                            element = new PPMatchToken(name, PPTokenType.MatchRegular);
                            result.Add(element);
                            addToDict(markers, element);
                            i += 2;
                        }
                        else if (matchTokens.La(i + 2) == XSharpLexer.GT
                            && matchTokens.La(i + 1) == XSharpLexer.SYMBOL_CONST)
                        {
                            // Xbase++ Addition
                            // <#idMarker>
                            // duplicate so we can change the name
                            name = new XSharpToken(matchTokens[i + 1]);
                            name.Value = name.Text.Substring(1);
                            element = new PPMatchToken(name, PPTokenType.MatchSingle);
                            result.Add(element);
                            addToDict(markers, element);
                            i += 2;
                        }
                        else if (matchTokens.La(i + 1) == XSharpLexer.MULT
                            && matchTokens.La(i + 3) == XSharpLexer.MULT
                            && matchTokens.La(i + 4) == XSharpLexer.GT
                            && matchTokens.IsName(i + 2))
                        {
                            // <*idMarker*>
                            name = matchTokens[i + 2];
                            element = new PPMatchToken(name, PPTokenType.MatchWild);
                            result.Add(element);
                            addToDict(markers, element);
                            i += 4;
                        }
                        else if (
                            matchTokens.La(i + 1) == XSharpLexer.LPAREN
                            && matchTokens.La(i + 3) == XSharpLexer.RPAREN
                            && matchTokens.La(i + 4) == XSharpLexer.GT
                            && matchTokens.IsName(i + 2))
                        {
                            // <(idMarker)>
                            name = matchTokens[i + 2];
                            element = new PPMatchToken(name, PPTokenType.MatchExtended);
                            result.Add(element);
                            addToDict(markers, element);
                            i += 4;
                        }
                        else if (matchTokens.La(i + 1) == XSharpLexer.MOD
                            && matchTokens.La(i + 3) == XSharpLexer.MOD
                            && matchTokens.La(i + 4) == XSharpLexer.GT
                            && matchTokens.IsName(i + 2))
                        {
                            // <%idMarker%>
                            name = matchTokens[i + 2];
                            element = new PPMatchToken(name, PPTokenType.MatchLike);
                            result.Add(element);
                            addToDict(markers, element);
                            i += 4;
                        }
                        else if (matchTokens.La(i + 2) == XSharpLexer.COMMA
                              && matchTokens.La(i + 3) == XSharpLexer.ELLIPSIS
                              && matchTokens.La(i + 4) == XSharpLexer.GT
                              && matchTokens.IsName(i + 1))
                        {
                            // <idMarker,...>
                            name = matchTokens[i + 1];
                            element = new PPMatchToken(name, PPTokenType.MatchList);
                            result.Add(element);
                            addToDict(markers, element);
                            i += 4;
                        }
                        else if (matchTokens.La(i + 2) == XSharpLexer.COLON
                              && matchTokens.IsName(i + 1))
                        {
                            // <idMarker:word list, separated with commas>
                            name = matchTokens[i + 1];
                            i += 3;
                            more = new List<XSharpToken>();
                            while (i < max && matchTokens[i].Type != XSharpLexer.GT)
                            {
                                token = matchTokens[i];
                                more.Add(token);            // we include the commas on purpose
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
                        _flags |= PPRuleFlags.HasOptionalMatch;
                        if (_firstOptionalMatchToken == -1)
                            _firstOptionalMatchToken = i;
                        more = getNestedTokens(i, max, matchTokens);
                        if (more != null)
                        {
                            i = i + more.Count + 1;

                            var nested = analyzeMatchTokens(more.ToArray(), markers, nestLevel + 1);
                            if (nested.Length > 0)
                            {
                                // the '[' is added to the result list
                                // and the nested elements are added as children
                                // the type for '[' is MatchOptional
                                PPMatchToken first = nested[0];
                                PPMatchToken marker = null;
                                foreach (var e in nested)
                                {
                                    if (e.IsMarker)
                                        marker = e;
                                }
                                string key = marker == null ? "Token:" + first.Key : marker.Key;
                                element = new PPMatchToken(token, PPTokenType.MatchOptional, key)
                                {
                                    Children = nested
                                };
                                result.Add(element);
                            }
                        }

                        break;
                    //case XSharpLexer.RBRKT:
                    //    addErrorMessage(token, "Closing bracket ']' found with missing '['");
                    //    break;
                    case XSharpLexer.BACKSLASH: // escape next token
                        if (i < max - 1)
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
            if (nestLevel == 0)
            {
                element = new PPMatchToken(new XSharpToken(XSharpLexer.ID, "udc"), PPTokenType.MatchWholeUDC);
                addToDict(markers, element);
                result.Add(element);
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
            // build linear list of matchtokens
            if (nestLevel == 0)
            {
                var linearlist = new List<PPMatchToken>();
                addMatchTokens(mt, linearlist);
                _matchTokensFlattened = linearlist.ToArray();
                _tokenCount = _matchTokensFlattened.Length;
                for (int i = 0; i < _tokenCount; i++)
                {
                    var marker = _matchTokensFlattened[i];
                    if (marker.RuleTokenType.HasStopTokens() || marker.IsRepeat)
                    {
                        var stopTokens = new List<XSharpToken>();
                        if (hasOptionalMatch)
                        {
                            findStopTokens(mt, 1, stopTokens, false);
                        }
                        else
                        {
                            // stop tokens start after this token
                            findStopTokens(mt, i + 1, stopTokens, true);
                        }
                        marker.StopTokens = stopTokens.ToArray();
                    }
                    if (marker.RuleTokenType.HasSingleStopToken())
                    {
                        if (i < _tokenCount - 1)
                        {
                            var next = _matchTokensFlattened[i + 1];
                            if (!next.IsOptional)
                            {
                                var stopTokens = new List<XSharpToken>
                                {
                                    next.Token
                                };
                                marker.StopTokens = stopTokens.ToArray();
                            }
                            else if (next.Children != null)
                            {
                                // the tokens of the optional follower
                                // should end the expression parsing of this token
                                var stopTokens = new List<XSharpToken>();
                                foreach (var child in next.Children)
                                {
                                    if (child.RuleTokenType == PPTokenType.Token)
                                    {
                                        stopTokens.Add(child.Token);
                                    }
                                }
                                marker.StopTokens = stopTokens.ToArray();
                            }
                        }
                    }
                    if (marker.IsOptional && !marker.IsWholeUDC)
                    {
                        foreach (var child in marker.Children)
                        {
                            if (child.RuleTokenType.HasStopTokens())
                            {

                                var stopTokens = new List<XSharpToken>();
                                if (child.RuleTokenType == PPTokenType.MatchList)
                                    findStopTokens(mt, i + 1, stopTokens);
                                else
                                    findStopTokens(mt, 1, stopTokens);

                                child.StopTokens = stopTokens.ToArray();
                                break;
                            }
                        }
                    }
                }

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

        void findStopTokens(PPMatchToken[] matchmarkers, int iStart, IList<XSharpToken> stoptokens, bool onlyFirstNonOptional = false)
        {
            var done = false;
            for (int j = iStart; j < matchmarkers.Length && !done; j++)
            {
                var next = matchmarkers[j];
                switch (next.RuleTokenType)
                {
                    case PPTokenType.MatchOptional:
                        // get first token in the Children of the optional clause
                        findStopTokens(next.Children, 0, stoptokens, true);
                        break;
                    case PPTokenType.MatchRestricted:
                        foreach (var token in next.Tokens)
                        {
                            if (canAddStopToken(stoptokens, token, out var _))
                            {
                                stoptokens.Add(token);
                            }
                        }
                        break;
                    case PPTokenType.MatchSingle:
                    case PPTokenType.Token:
                        if (canAddStopToken(stoptokens, next.Token, out var found))
                        {
                            stoptokens.Add(next.Token);
                            if (onlyFirstNonOptional)
                                done = true;
                        }
                        else if (found && onlyFirstNonOptional)
                        {
                            done = true;
                        }
                        break;
                }
            }
        }
        bool canAddStopToken(IList<XSharpToken> stoptokens, XSharpToken token, out bool found)
        {
            found = false;
            if (token.Type == XSharpLexer.COMMA)
                return false;
            foreach (var element in stoptokens)
            {
                // do not add tokens that are already in the list.
                if (tokenEquals(element, token))
                {
                    found = true;
                    return false;
                }
            }
            // At this moment we do not allow constants or NULL to be a stop token
            return token.IsIdentifier() || token.IsOperator() || token.IsKeyword();
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
            XSharpToken lastToken = null;
            for (int i = 0; i < resultTokens.Length; i++)
            {
                var token = resultTokens[i];
/*nvk                if (token.Source == lastTokenSource && token.Index > lastTokenIndex + 1)
                {
                    // whitespace tokens have been skipped
                    var ppWs = new XSharpToken(token, TokenType.WS, " ");
                    ppWs.Channel = Channel.Hidden;
                    result.Add(new PPResultToken(ppWs, PPTokenType.Token));
                }*/
                switch (token.Type)
                {
                    case XSharpLexer.NEQ2:
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
                        else if (i < resultTokens.Length - 4
                            && resultTokens[i + 1].Type == XSharpLexer.NOT
                            && resultTokens[i + 3].Type == XSharpLexer.NOT
                            && resultTokens[i + 4].Type == XSharpLexer.GT
                            && resultTokens[i + 2].IsName())
                        {
                            // <!idMarker!>
                            name = resultTokens[i + 2];
                            result.Add(new PPResultToken(name, PPTokenType.ResultNotEmpty));
                            i += 4;
                        }
                        break;
                    case XSharpLexer.LBRKT:
                        /*
                         * eat block
                        * [ ... ], 
                        * when nested then this is seen as a repeated result clause
                         */
                        _flags |= PPRuleFlags.HasOptionalResult;
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
                                    var element = new PPResultToken(token, PPTokenType.ResultOptional, marker.Key)
                                    {
                                        OptionalElements = nested
                                    };
                                    result.Add(element);
                                    _flags |= PPRuleFlags.HasOptionalResult;

                                }

                            }
                        }
                        else
                        {
                            _errorMessages.Add(new PPErrorMessage(token, "Nested result markers are not allowed"));
                        }
                        break;
                    case XSharpLexer.BACKSLASH: // escape next token
                        if (i < max - 1)
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
                var current = token;
                if (current != null && lastToken != null && lastToken.Type == XSharpLexer.ID)
                {
                    // No whitespace after an ID. We will joined IDs when possible
                    if (lastToken.End == current.Start - 1)
                    {
                        var last = result.LastOrDefault();
                        if (last != null)
                        {
                            last.CombineWithPrevious = true;
                        }
                    }
                }
                lastToken = resultTokens[i];
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
                    if (canAbbreviate)
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
        internal string[] Keys
        {
            get
            {
                if (!hasMultiKeys)
                {
                    return new string[] { this.Key };
                }
                var result = new List<string>();
                foreach (var token in _matchtokens[0].Tokens)
                {
                    var key = token.Text.ToUpperInvariant();
                    if (key.Length > 4)
                    {
                        if (canAbbreviate)
                        {
                            key = key.Substring(0, 4);
                        }
                    }
                    result.Add(key);
                }
                return result.ToArray();
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
                        if (token.RuleTokenType != PPTokenType.MatchWholeUDC)
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

        bool IsStopToken(PPMatchToken mtoken, XSharpToken token)
        {
            if (mtoken.StopTokens != null && mtoken.StopTokens.Length > 0)
            {
                foreach (var stopToken in mtoken.StopTokens)
                {
                    if (tokenEquals(stopToken, token))
                    {
                        return true;
                    }
                }

            }
            return false;
        }
        bool tokenEquals(XSharpToken lhs, XSharpToken rhs)
        {
            return tokenEquals(this.Type, lhs, rhs, this.VOPreprocessorBehaviour);
        }
        static bool tokenEquals(PPUDCType type, XSharpToken lhs, XSharpToken rhs, bool voPPBehavior)
        {
            if (lhs != null && rhs != null)
                return stringEquals(type, lhs.Text, rhs.Text, voPPBehavior);
            return false;
        }
        static bool stringEquals(PPUDCType type, string lhs, string rhs, bool voPPBehavior)
        {
            // #command, #translate, #xcommand and #xtranslate are always case insensitive
            // for #define this depends on the setting of /vo8
            var mode = StringComparison.OrdinalIgnoreCase;
            if (type == PPUDCType.Define && voPPBehavior)
            {
                mode = StringComparison.Ordinal;    // case sensitive
            }
            if (lhs?.Length <= 4 && !(type== PPUDCType.YCommand || type == PPUDCType.YTranslate))
            {
                return string.Equals(lhs, rhs, mode);
            }
            switch (type)
            {
                case PPUDCType.Command:
                case PPUDCType.Translate:
                    // dBase/Clipper syntax 4 characters is enough
                    return string.Compare(lhs, 0, rhs, 0, Math.Max(rhs.Length, 4), mode) == 0;
                case PPUDCType.XCommand:
                case PPUDCType.XTranslate:
                case PPUDCType.Define:
                    return string.Equals(lhs, rhs, mode);
                case PPUDCType.YCommand:
                case PPUDCType.YTranslate:
                    return string.Compare(lhs, rhs, StringComparison.Ordinal) == 0;
                default:
                    break;
            }
            return false;
        }

        bool matchLikeToken(PPMatchToken mToken, IList<XSharpToken> tokens, ref int iSource, PPMatchRange[] matchInfo)
        {
            int start = iSource;
            int end = iSource;
            bool found = false;
            for (int iChild = iSource; iChild < tokens.Count; iChild++)
            {
                var token = tokens[iChild];
                var stopTokenFound = IsStopToken(mToken, token);
                if (stopTokenFound)
                {
                    break;
                }
                bool add;
                switch (token.Type)
                {
                    case XSharpLexer.COMMA:
                    case XSharpLexer.ID:
                        add = true;
                        break;
                    default:
                        if (token.IsWildCard())
                            add = true;
                        else
                            add = false;
                        break;
                }
                if (add)
                {
                    found = true;
                    end = iChild;
                }
                else
                    break;
            }
            if (found)
            {
                matchInfo[mToken.Index].SetPos(start, end);
                iSource = end + 1;
            }
            return found;
        }
        bool matchOptionalToken(PPMatchToken mToken, IList<XSharpToken> tokens, ref int iSource, PPMatchRange[] matchInfo, IList<XSharpToken> matchedWithToken, ref int iRule)
        {
            var optional = mToken.Children;
            bool optfound = false;
            bool wasmatched = false;
            int iOriginal = iSource;
            int iChild = 0;
            PPMatchRange[] copyMatchInfo = new PPMatchRange[matchInfo.Length];
            Array.Copy(matchInfo, copyMatchInfo, matchInfo.Length);
            var originalMatchedLen = matchedWithToken.Count;
            while (iChild < optional.Length && iSource < tokens.Count)
            {
                var mchild = optional[iChild];
                if (!matchToken(mchild, ref iChild, matchInfo.Length, ref iSource, tokens, copyMatchInfo, matchedWithToken))
                {
                    /*
                     Some optional tokens have optional children. In that case we can still match the optional
                     token even when its child is not there, like the fldN in the rule below.
                     If you have problems understanding this, please imagine how it was for me to write all of this
                     and emulate the old Clipper, XPP and Harbour preprocessors...
                     #command  REPLACE [<fld1> WITH <val1> [,<fldN> WITH <valN> ] ] ;
                     [   FOR <for>] [ WHILE <whl>] [  NEXT <nxt>] [RECORD <rcd>] [ <rst: REST>] [ ALL ] =>  dbEval( {|| FIELD-><fld1> := <val1>[, ;
                            FIELD-><fldN> := <valN>]  }, __EBCB(<for>), __EBCB(<whl>), <nxt>, <rcd>, <.rst.>)

                     **/
                    if (!mchild.IsOptional && !wasmatched)
                    {
                        // the comma is not optional. But do not mark the comma as not found when there no repeat group at all
                        optfound = false;
                    }
                    break;
                }
                else
                {
                    optfound = true;
                    if (iChild == optional.Length && mchild.IsRepeat)
                    {
                        // if we have matched the last of a repeat group like the fldN in the sample above
                        // and when the key ends with 'n' and the previous key ends with '1' then there may be more
                        // repeated groups. In that case keep matching the optional group.
                        iChild = 0;
                        wasmatched = true;
                    }
                }
            }
            if (optfound)
            {
                Array.Copy(copyMatchInfo, matchInfo, matchInfo.Length);
                if (!mToken.IsRepeat || wasmatched)
                {
                    iRule += 1;
                }
                var iEnd = iSource - 1;
                // truncate spaces at the end
                iEnd = trimHiddenTokens(tokens, iSource, iEnd);
                matchInfo[mToken.Index].SetPos(iOriginal, iEnd);
            }
            else
            {
                // a token such as [FOO BAR] may have matched FOO but not BAR. Remove FOO from the list then.
                while (matchedWithToken.Count > originalMatchedLen)
                {
                    matchedWithToken.RemoveAt(matchedWithToken.Count - 1);
                }
                iSource = iOriginal;
            }
            return optfound;
        }
        bool matchExtendedToken(PPMatchToken mToken, IList<XSharpToken> tokens, ref int iSource, PPMatchRange[] matchInfo, IList<XSharpToken> matchedWithToken)
        {
            int iStart = iSource;
            var iend = -1;
            bool found = true;
            if (matchAmpersandToken(mToken, tokens, iStart, ref iend))
            {
                matchInfo[mToken.Index].SetPos(iStart, iend);
                iSource = iend + 1;
            }
            else if (matchFileName(tokens, iStart, ref iend))
            {
                matchInfo[mToken.Index].SetPos(iStart, iend);
                iSource = iend + 1;
            }
            else
            {
                found = matchExpression(iSource, tokens, mToken.StopToken, out int iEnd);
                if (found)
                {
                    matchInfo[mToken.Index].SetPos(iSource, iEnd);
                    iSource = iEnd + 1;
                }
            }
            return found;

        }
        bool matchRestrictedToken(PPMatchToken mToken, IList<XSharpToken> tokens, ref int iSource, PPMatchRange[] matchInfo, IList<XSharpToken> matchedWithToken)
        {
            // match the words in the list.
            // the token in the rule is the match marker
            // the words to be checked are the MoreTokens
            // This list includes the original commas because some restricted markers can have more than word:
            // LIST ....  [<toPrint: TO PRINTER>] 
            // where others have a list of single words
            // #command SET CENTURY <x:ON,OFF,&>      => __SetCentury( <(x)> )
            XSharpToken lastToken = null;
            XSharpToken matchedToken = null;
            int iLast = mToken.Tokens.Length - 1;
            int iMatch = 0;
            int iCurrent = iSource;
            int iEnd;
            for (var iChild = 0; iChild <= iLast; iChild++)
            {
                var tokenFromUDC = mToken.Tokens[iChild];
                lastToken = tokenFromUDC;
                if (tokenEquals(tokenFromUDC, tokens[iCurrent]) ||
                    tokenFromUDC.Type == XSharpLexer.AMP && tokens[iCurrent].Type == XSharpLexer.LPAREN)
                {
                    iMatch += 1;
                    matchedToken = tokenFromUDC;
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
                }
                else
                {
                    iMatch = 0;
                }
            }
            var found = iMatch > 0;
            if (found)
            {
                iEnd = iCurrent;
                iSource = iEnd;
                if (lastToken.Type == XSharpLexer.AMP)
                {
                    // when the ampersand is the last token, then we also include the following token
                    // This happens when we match the rule
                    // #command SET CENTURY <x:ON,OFF,&>      => __SetCentury( <(x)> ) 
                    // with the source SET CENTURY &MyVar
                    // This generates the output __SetCentury((MyVar))
                    // And this code
                    // SET CENTURY ("on")
                    // Should Generate __SetCentury(("on"))
                    if (tokens[iCurrent].Type == XSharpLexer.LPAREN)
                    {
                        iCurrent += 1;
                        int nested = 1;
                        while (nested > 0 && (iCurrent < tokens.Count))
                        {
                            switch (tokens[iCurrent].Type)
                            {
                                case XSharpLexer.LPAREN:
                                case XSharpLexer.LBRKT:
                                case XSharpLexer.LCURLY:
                                    nested++;
                                    break;
                                case XSharpLexer.RPAREN:
                                case XSharpLexer.RBRKT:
                                case XSharpLexer.RCURLY:
                                    nested--;
                                    break;
                                default:
                                    break;
                            }
                            iCurrent++;
                        }
                        iEnd = iCurrent-1;
                    }
                    else
                    {
                        if (lastToken == mToken.Tokens[mToken.Tokens.Length - 1] && tokens.Count > iEnd)
                        {
                            iEnd += 1;
                        }
                    }
                }
                // truncate spaces at the end
                iEnd = trimHiddenTokens(tokens, iSource, iEnd);
                matchInfo[mToken.Index].SetPos(iSource, iEnd);
                // when a single token is matched and that is not the Ampersand
                // then mark this as a token match
                if (matchedToken != null && matchedToken.Type != XSharpLexer.AMP && iSource == iEnd)
                    matchInfo[mToken.Index].SetToken(iSource);

                for (int i = iSource; i <= iEnd; i++)
                {
                    matchedWithToken.Add(tokens[i]);
                }
                iSource = iEnd + 1;
            }
            return found;
        }
        bool matchListToken(PPMatchToken mToken, IList<XSharpToken> tokens, ref int iSource, PPMatchRange[] matchInfo)
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
            var matches = new List<Tuple<int, int>>();
            while (iSource < tokens.Count)
            {
                var token = tokens[iSource];
                if (token.Type != XSharpLexer.COMMA)
                {
                    if (IsStopToken(mToken, token))
                    {
                        break;
                    }
                }
                // after matchExpresssion iLastUsed points to the last token of the expression
                if (matchExpression(iSource, tokens, mToken.StopToken, out int iLastUsed))
                {
                    matches.Add(new Tuple<int, int>(iSource, iLastUsed));
                    iSource = iLastUsed + 1;
                }
                // IsOperator included comma, ellipses etc.
                else if (token.IsOperator())
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

        bool matchSingleToken(PPMatchToken mToken, IList<XSharpToken> tokens, ref int iSource, PPMatchRange[] matchInfo)
        {
            // XPP addition
            // add all normal tokens and operators until whitespace
            var iEnd = iSource + 1;
            while (iEnd < tokens.Count && !tokens[iEnd].HasTrivia)
            {
                iEnd++;
            }
            iEnd -= 1;
            matchInfo[mToken.Index].SetPos(iSource, iEnd);
            iSource = iEnd + 1;
            return true;
        }

        bool matchToken(PPMatchToken mToken, ref int iRule, int iLastRule, ref int iSource, IList<XSharpToken> tokens,
            PPMatchRange[] matchInfo, IList<XSharpToken> matchedWithToken)
        {
            XSharpToken sourceToken = tokens[iSource];
            XSharpToken ruleToken = mToken.Token;
            int iEnd;
            bool found = false;
            while (sourceToken.Type == XSharpLexer.WS || sourceToken.Type == XSharpLexer.ML_COMMENT)
            {
                iSource += 1;
                if (iSource == tokens.Count)
                    return false;
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
                    // iLastUsed indicates the last token that is part of the expression
                    found = matchExpression(iSource, tokens, mToken.StopToken, out int iLastUsed);
                    if (found)
                    {
                        // truncate spaces at the end
                        iLastUsed = trimHiddenTokens(tokens, iSource, iLastUsed);

                        matchInfo[mToken.Index].SetPos(iSource, iLastUsed);
                        iSource = iLastUsed + 1;
                        iRule += 1;
                    }

                    break;
                case PPTokenType.MatchLike:
                    // This matches one or more name with an optional * or ? character.
                    if (matchLikeToken(mToken, tokens, ref iSource, matchInfo))
                    {
                        iRule += 1;
                        found = true;
                    }
                    break;
                case PPTokenType.MatchList:
                    if (matchListToken(mToken, tokens, ref iSource, matchInfo))
                    {
                        iRule += 1;
                        found = true;
                    }
                    break;

                case PPTokenType.MatchRestricted:
                    if (matchRestrictedToken(mToken, tokens, ref iSource, matchInfo, matchedWithToken))
                    {
                        iRule += 1;
                        found = true;
                    }
                    break;
                case PPTokenType.MatchSingle:
                    if (matchSingleToken(mToken, tokens, ref iSource, matchInfo))
                    {
                        iRule += 1;
                        found = true;
                    }
                    break;
                case PPTokenType.MatchWild:
                    // matches anything until the end of the list or until a stop token is found
                    if (mToken.StopTokens?.Length == 0)
                    {
                        iEnd = tokens.Count - 1;
                    }
                    else
                    {
                        iEnd = tokens.Count - 1;
                        for (int iToken = iSource; iToken < tokens.Count; iToken++)
                        {
                            var token = tokens[iToken];
                            if (IsStopToken(mToken, token))
                            {
                                iEnd = iToken - 1;
                                break;
                            }
                        }

                    }
                    // truncate spaces at the end
                    iEnd = trimHiddenTokens(tokens, iSource, iEnd);
                    matchInfo[mToken.Index].SetPos(iSource, iEnd);
                    iSource = iEnd + 1;
                    iRule += 1;
                    found = true;
                    break;
                case PPTokenType.MatchExtended:
                    // either match a single token, a token in parentheses
                    if (matchExtendedToken(mToken, tokens, ref iSource, matchInfo, matchedWithToken))
                    {
                        iRule += 1;
                        found = true;
                    }
                    break;
                case PPTokenType.MatchOptional:
                    // 
                    // Get sublist of optional token and match with source
                    if (matchOptionalToken(mToken, tokens, ref iSource, matchInfo, matchedWithToken, ref iRule))
                    {
                        found = true;
                    }
                    break;
            }
            return found;
        }
        int trimHiddenTokens(IList<XSharpToken> tokens, int start, int end)
        {
            while (tokens[end].Channel == Channel.Hidden && start < end)
            {
                end--;
            }

            return end;
        }
        internal bool Matches(IList<XSharpToken> tokens, out PPMatchRange[] matchInfo)
        {
            int iRule = 0;
            int iSource = 0;
            matchInfo = new PPMatchRange[_tokenCount];
            List<XSharpToken> matchedWithToken = new List<XSharpToken>();
            int firstOptional = -1;
            bool hasSkippedMarkers = false;
            // skip the last token: this is the WholeUdc token
            while (iRule < _matchtokens.Length - 1 && iSource < tokens.Count)
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
            if (iSource < tokens.Count && this.isCommand)
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
            var wholeUdc = _matchtokens[_matchtokens.Length - 1];
            int last = 0;
            {
                foreach (var info in matchInfo)
                {
                    if (info.End > last)
                        last = info.End;
                }
            }
            matchInfo[wholeUdc.Index].SetPos(0, last);

            // Now mark the tokens that were matched with tokens in the UDC with the keyword color
            // Since our token may be a clone, we change the Type of the source token
#if VSPARSER
            foreach (var token in matchedWithToken)
            {
                if (!token.IsOperator())
                {
                    token.Type = XSharpLexer.UDC_KEYWORD;
                    token.UDCLocation = this._matchtokens[0].Token;
                    token.UDCType = (byte)this.Type;
                }
            }
#endif
            return true;

        }
        internal IList<XSharpToken> Replace(IList<XSharpToken> tokens, PPMatchRange[] matchInfo)
        {
            Debug.Assert(matchInfo.Length == _tokenCount);
            return Replace(_resulttokens, tokens, matchInfo, 0, true);

        }
        internal IList<XSharpToken> Replace(PPResultToken[] resulttokens, IList<XSharpToken> tokens, PPMatchRange[] matchInfo, int offset, bool isTopLevel)
        {
            Debug.Assert(matchInfo.Length == _tokenCount);
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
                    case PPTokenType.ResultNotEmpty:
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
            if (isTopLevel)
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
                for (int i = 0; i < result.Count; i++)
                {
                    var t = result[i];
                    if (t is not XSharpPPToken)
                    {
                        t = new XSharpPPToken(t, source);
                        result[i] = t;
                    }
                    if (t.Channel == Channel.PreProcessor)
                    {
                        // Leave PP commands alone so we can redirect input to #error or #warning
                        if (t.Type < XSharpLexer.PP_FIRST || t.Type > XSharpLexer.PP_LAST)
                        {
                            t.Channel = Channel.Default;
                        }
                    }
                }
            }
            //result.TrimLeadingSpaces();
            // after certain tokens (->, and . for example) keywords must be changed to identifiers
            adjustKeywordsToIdentifiers(result);
            return result;
        }
        void adjustKeywordsToIdentifiers(IList<XSharpToken> result)
        {
            XSharpToken lasttoken = null;
            foreach (var token in result)
            {
                if (lasttoken != null)
                {
                    if (token.IsKeyword())
                    {
                        if (lasttoken.IsMemberOperator())
                        {
                            token.Type = XSharpLexer.ID;
                            // This makes sure that the token in the editor has the ID color
// nvk                                token.Original.type = XSharpLexer.ID; 
                        }
                    }
                    if (token.Type == XSharpLexer.DOT)
                    {
                        // tokens before a DOT are changed to ID
                        // with the exception of the tokens listed below
                        switch (lasttoken.Type)
                        {
                            case XSharpLexer.SELF:
                            case XSharpLexer.SUPER:
// nvk TODO
// case TokenType.FOX_M:
                                // leave alone
                                break;
                            default:
                                if (lasttoken.IsKeyword())
                                {
                                    lasttoken.Type = XSharpLexer.ID;
                                    // This makes sure that the token in the editor has the ID color
// nvk                                    lasttoken.Original.type = XSharpLexer.ID;
                                }
                                break;
                        }
                    }
                }
                lasttoken = token;
            }
        }
        void tokenResult(PPResultToken resultToken, IList<XSharpToken> tokens, PPMatchRange[] matchInfo, IList<XSharpToken> result, int offset)
        {
            // Link the new token to the first token of the UDC
            // so for the command #xcommand WAIT [<msg>]                  => _wait( <msg> )
            // the LPAREN and RPAREN will also be linked to the WAIT keyword in the source.
            var newToken = new XSharpPPToken(resultToken.Token, tokens[0]);
            result.Add(newToken);
        }
        void repeatedResult(PPResultToken resultToken, IList<XSharpToken> tokens, PPMatchRange[] matchInfo, IList<XSharpToken> result, int offset)
        {
            // when the repeated result started with a "common" match marker, such as the <(a)> in the example below
            // we would match it, even when one of more of the repeated match markers were missing
            /*
             #command REPLACE <(f1)> WITH <v1> [, <(fN)> WITH <vN> ] <x:IN,ALIAS> <(a)>                                             ;
            => DbAutoLock(<(a)>), __FieldSetWa(<(a)>, <(f1)>,<v1>) [,__FieldSetWa(<(a)>,<(fN)>,<vN>)], DbAutoUnLock(<(a)>)
            */
            // we now make sure that all the tokens that have a match marker (so also fN and vN ) are matched.
            if (resultToken.MatchMarker != null && resultToken.OptionalElements?.Length > 0 && resultToken.IsRepeat)
            {
                foreach (var token in resultToken.OptionalElements)
                {
                    if (token.MatchMarker != null)
                    {
                        var index = token.MatchMarker.Index;
                        var mm = matchInfo[index];
                        if (mm.MatchCount == 0)
                        {
                            return;
                        }
                    }
                }
            }

            if (resultToken.MatchMarker != null)
            {
                var index = resultToken.MatchMarker.Index;
                var mm = matchInfo[index];
                if (mm.MatchCount > 0)
                {
                    if (mm.IsList)  // multiple results, so repeat for each element in the Children list
                    {
                        int repeats = mm.MatchCount;
                        for (int i = 0; i < repeats; i++)
                        {
                            var block = Replace(resultToken.OptionalElements, tokens, matchInfo, i, false);
                            result.AddRange(block);
                        }
                    }
                    else
                    {
                        if (resultToken.IsRepeat)
                        {
                            // determine the # of repeats in the source
                            var max = resultToken.RepeatCount(matchInfo);
                            for (int i = 0; i < max; i++)
                            {
                                var block = Replace(resultToken.OptionalElements, tokens, matchInfo, i, false);
                                result.AddRange(block);
                            }
                        }
                        else
                        {
                            var block = Replace(resultToken.OptionalElements, tokens, matchInfo,0, false);
                            result.AddRange(block);
                        }
                    }
                }
            }
            return;
        }
        void regularResult(PPResultToken resultToken, IList<XSharpToken> tokens, PPMatchRange[] matchInfo, IList<XSharpToken> result, int offset = 0)
        {
            // write input text to the result text without no changes
            // for example:
            // #command SET CENTURY (<x>) => __SetCentury( <x> )
            // the output written is the literal text for x, so the token(s) x
            var range = matchInfo[resultToken.MatchMarker.Index];
            if (resultToken.MatchMarker.RuleTokenType == PPTokenType.MatchList)
            {
                processMatchList(resultToken, tokens, range, result, regularSingleResult);
            }
            else
            {
                if (!range.Empty && range.MatchCount > 1)
                {
                    range = range.Children[offset];
                }
                regularSingleResult(resultToken, tokens, range, result);
            }
            return;
        }

        void regularSingleResult(PPResultToken resultToken, IList<XSharpToken> tokens, PPMatchRange range, IList<XSharpToken> result)
        {
            if (!range.Empty)
            {
                // when a regular result marker is prefixed with another token without whitespace
                // then combine the two. The type becomes the type of the first
                bool combine = resultToken.CombineWithPrevious;
                for (int i = range.Start; i <= range.End && i < tokens.Count; i++)
                {
                    var token = tokens[i];
                    var last = result.LastOrDefault();
                    if (combine)
                    {
                        if (last != null && last.Type == XSharpLexer.ID)
                        {
                            last.Value += token.Text;
                        }
                        combine = false;
                    }
                    else
                    {
                        result.Add(token);
                    }
                }
            }
            else if (resultToken.RuleTokenType == PPTokenType.ResultNotEmpty)
            {
                if (_options.Dialect != XSharpDialect.Core)
                {
                    var nilToken = new XSharpToken(XSharpLexer.NIL, "NIL");
                    result.Add(nilToken);
                }
                else
                {
                    // in core dialect this generates a NULL
                    var nullToken = new XSharpToken(XSharpLexer.NULL, "NULL");
                    result.Add(nullToken);
                }
            }
        }
        void blockifySingleResult(PPResultToken resultToken, IList<XSharpToken> tokens, PPMatchRange range, IList<XSharpToken> result)
        {
            int start = range.Start;
            int end = range.End;
            bool addBlockMarker = true;
            XSharpToken nt;
            XSharpToken t;
            if (range.Length == 1)
            {
                // for comma's and other separators
                if (tokens[start].IsOperator())
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
                nt = new XSharpToken(t, XSharpLexer.LCURLY, "{")
                {
                    Channel = Channel.Default
                };
                result.Add(nt);
                nt = new XSharpToken(t, XSharpLexer.OR, "||")
                {
                    Channel = Channel.Default
                };
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
                nt = new XSharpToken(t, XSharpLexer.RCURLY, "}")
                {
                    Channel = Channel.Default
                };
                result.Add(nt);
            }
        }

        delegate void processMethod(PPResultToken resultToken, IList<XSharpToken> tokens, PPMatchRange range, IList<XSharpToken> result);
        void processMatchList(PPResultToken resultToken, IList<XSharpToken> tokens, PPMatchRange range, IList<XSharpToken> result, processMethod action)
        {
            var list = new List<PPMatchRange>();
            if (range.IsList)
            {
                list.AddRange(range.Children);
            }
            else
            {
                // find commas in the list
                list = splitCommaSeparatedRange(range, tokens);
            }
            bool first = true;
            foreach (var element in list)
            {
                if (!first)
                    result.Add(new XSharpToken(XSharpLexer.COMMA, ","));
                else
                    first = false;
                action(resultToken, tokens, element, result);
            }

        }
        void blockifyResult(PPResultToken resultToken, IList<XSharpToken> tokens, PPMatchRange[] matchInfo, IList<XSharpToken> result, int offset)
        {
            // write output text as codeblock
            // so prefixes with "{||"and suffixes with "}
            // if the input is already a code block then nothing is changed
            // when the input is a list then each element in the list will be
            // converted to a code block
            var range = matchInfo[resultToken.MatchMarker.Index];
            if (!range.Empty)
            {
                if (range.MatchCount > 1)
                {
                    range = range.Children[offset];
                }
                if (resultToken.MatchMarker.RuleTokenType == PPTokenType.MatchList)
                {
                    processMatchList(resultToken, tokens, range, result, blockifySingleResult);
                }
                else
                {
                    blockifySingleResult(resultToken, tokens, range, result);
                }
            }
            return;
        }

        bool matchAmpersandToken(PPMatchToken mToken, IList<XSharpToken> tokens, int start, ref int end)
        {
            end = -1;
            if (tokens.Count >= start + 1 && tokens[start].Type == XSharpLexer.AMP)
            {
                if (matchExpression(start + 1, tokens, mToken.StopToken, out end))
                {
                    return true;
                }
            }
            return false;
        }

        static bool IsFilenameSeparator(XSharpToken token)
        {
            switch (token.Type)
            {
                case XSharpLexer.BACKSLASH:
                case XSharpLexer.DOT:
                    return true;
                default:
                    break;
            }
            return false;
        }

        static bool IsFilenamePart(XSharpToken token)
        {
            switch (token.Type)
            {
                case XSharpLexer.ID:
                case XSharpLexer.INT_CONST:         // file or folder may be a number
                case XSharpLexer.DATE_CONST:         // file or folder may be 2020.01.01
                case XSharpLexer.REAL_CONST:         // file or folder may be 1.2
                case XSharpLexer.SYMBOL_CONST:
                    return true;
                default:
                    return token.IsName();
            }
        }

        static bool matchFileName(IList<XSharpToken> tokens, int start, ref int end)
        {
            if (start >= tokens.Count - 2)
                return false;
            int current = start;
            var t1 = tokens[start];
            var t2 = tokens[start + 1];
            var t3 = tokens[start + 2];
            if (t1.IsName() && t2.Type == XSharpLexer.COLON && t2.Start == t1.Start + 1)          // C: .....
            {
                current = start + 2;
                if (IsFilenameSeparator(t3))
                {
                    current += 1;
                }
            }
            else if (t1.Type == XSharpLexer.DOT && t2.Type == XSharpLexer.BACKSLASH && t2.Start == t1.Start + 1)    // .\ .... 
            {
                current = start + 2;
            }
            else if (t1.Type == XSharpLexer.DOT && t2.Type == XSharpLexer.DOT && t3.Type == XSharpLexer.BACKSLASH
                && t2.Start == t1.Start + 1 && t3.Start == t2.Start + 1)    // ..\ .....
            {
                current = start + 3;
            }
            else if (t1.Type == XSharpLexer.BACKSLASH && t2.Type == XSharpLexer.BACKSLASH && IsFilenamePart(t3)
                && t2.Start == t1.Start + 1 && t3.Start == t2.Start + 1)    // \\Computer\ID .....
            {
                if (start < tokens.Count - 4)
                {
                    var t4 = tokens[start + 3];
                    if (t4.Type == XSharpLexer.BACKSLASH )
                    {
                        current = start + 4;
                    }
                    else
                    {
                        return false;
                    }
                }
            }
            else if (IsFilenamePart(t1) && IsFilenameSeparator(t2))     // a\ or b\
            {
                current = start + 2;
            }
            else
            {
                return false;
            }
            end = current-1;
            bool expectName = true;
            XSharpToken last = tokens[current - 1];
            while (current < tokens.Count)
            {
                // embedded whitespace exits the loop
                var token = tokens[current];
                if (token.Start > last.End)
                {
                    break;
                }
                if (expectName)
                {
                    if (IsFilenamePart(token))
                    {
                        end = current;
                        current++;
                    }
                    else
                    {
                        break;
                    }
                }
                else
                {
                    if (IsFilenameSeparator(token))
                    {
                        end = current;
                        current++;
                    }
                    else
                    {
                        break;
                    }
                }
                last = token;
                expectName = !expectName;
            }
            return true;
        }

        void stringifySingleResult(PPResultToken rule, IList<XSharpToken> tokens, PPMatchRange range, IList<XSharpToken> result)
        {
            var start = range.Start;
            var end = range.End;
            if (range.Length == 1)
            {
                // for comma's and other separators
                if (tokens[start].IsOperator())
                {
                    result.Add(tokens[start]);
                    return;
                }
            }

            XSharpToken joinTokensToString(IList<XSharpToken> toks, int s, int e)
            {
                var sb = new System.Text.StringBuilder();
                sb.Append('"');
                for (int i = s; i <= e; i++)
                {
                    var token = toks[i];
                    if (i > s && token.HasTrivia)
                    {
                        sb.Append(token.TriviaAsText);
                    }
                    var text = token.Text;
                    if (text.StartsWith("@@"))
                        text = text.Substring(2);
                    sb.Append(text);
                }
                sb.Append('"');
                var t = new XSharpToken(toks[s], XSharpLexer.STRING_CONST, sb.ToString())
                {
                    Channel = Channel.Default
                };
                return t;
            }
            void addTokens(IList<XSharpToken> res, IList<XSharpToken> toks, int s, int e)
            {
                for (int i = s; i <= e; i++)
                {
                    var token = toks[i];
                    //if (i > start + 1 && token.HasTrivia)
                    //{
                    //    res.AddRange(token.Trivia);
                    //}
                    res.Add(token);
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
                        result.Add(joinTokensToString(tokens, start, end));
                    }
                    else
                    {
                        // no match, then dumb stringify write an empty string
                        var nt = new XSharpToken(tokens[0], XSharpLexer.NULL_STRING, "NULL_STRING")
                        {
                            Channel = Channel.Default
                        };
                        result.Add(nt);
                    }
                    break;
                case PPTokenType.ResultNormalStringify:
                    // Delimit the input with string delimiters
                    if (!range.Empty)
                    {
                        result.Add(joinTokensToString(tokens, start, end));
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
                            if (token.IsString())
                            {
                                result.Add(token);
                            }
                            else
                            {
                                result.Add(joinTokensToString(tokens, start, end));
                            }
                        }
                        else
                        {
                            if (tokens[start].Type == XSharpLexer.AMP)
                            {
                                // do not include the &
                                addTokens(result, tokens, start + 1, end);
                            }
                            else if (tokens[start].Type == XSharpLexer.LPAREN &&
                                tokens[end].Type == XSharpLexer.RPAREN)
                            {
                                // do not include ( and )
                                addTokens(result, tokens, start + 1, end - 1);
                            }
                            else
                            {
                                var nt = joinTokensToString(tokens, start, end);
                                result.Add(nt);
                            }
                        }
                    }
                    break;
            }
            return;

        }
        List<PPMatchRange> splitCommaSeparatedRange(PPMatchRange range, IList<XSharpToken> tokens)
        {
            var list = new List<PPMatchRange>();
            var start = range.Start;
            for (var i = range.Start; i <= range.End; i++)
            {
                var token = tokens[i];
                if (token.Type == XSharpLexer.COMMA)
                {
                    var item = new PPMatchRange();
                    item.SetPos(start, i - 1);
                    list.Add(item);
                    start = i + 1;
                }
            }
            var element = new PPMatchRange();
            element.SetPos(start, range.End);
            list.Add(element);
            return list;

        }
        void stringifyResult(PPResultToken rule, IList<XSharpToken> tokens, PPMatchRange[] matchInfo, IList<XSharpToken> result, int offset)
        {
            var range = matchInfo[rule.MatchMarker.Index];
            if (range.Empty)
            {
                stringifySingleResult(rule, tokens, range, result);
            }
            else
            {
                if (range.MatchCount > 1)
                {
                    range = range.Children[offset];
                }
                if (rule.MatchMarker.RuleTokenType == PPTokenType.MatchList)
                {
                    processMatchList(rule, tokens, range, result, stringifySingleResult);
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
            if (rule.MatchMarker.RuleTokenType == PPTokenType.MatchList)
            {
                processMatchList(rule, tokens, range, result, logifySingleResult);
            }
            else
            {
                if (!range.Empty && range.MatchCount > 1)
                {
                    range = range.Children[offset];
                }
                logifySingleResult(rule, tokens, range, result);
            }
            return;
        }
        void logifySingleResult(PPResultToken rule, IList<XSharpToken> tokens, PPMatchRange range, IList<XSharpToken> result)
        {
            if (!range.Empty)
            {
                XSharpToken t = tokens[range.Start];
                t = new XSharpToken(t, XSharpLexer.TRUE_CONST, ".T.")
                {
                    Channel = Channel.Default
                };
                result.Add(t);
            }
            else
            {
                // No token to read the line/column from
                result.Add(new XSharpToken(XSharpLexer.FALSE_CONST, ".F."));
            }
        }
        static bool tokenCanStartExpression(int pos, IList<XSharpToken> tokens)
        {
            XSharpToken token = tokens[pos];
            if (!token.NeedsLeft() && !token.IsEndOfCommand())
            {
                return true;
            }
            return false;
        }
        internal bool matchExpression(int start, IList<XSharpToken> tokens, XSharpToken stopToken, out int lastUsed)
        {
            return matchExpression(this.Type, start, tokens, stopToken, VOPreprocessorBehaviour, out lastUsed);
        }
        internal static bool matchExpression(PPUDCType type, int start, IList<XSharpToken> tokens,
            XSharpToken stopToken, bool voPPBehavior, out int lastUsed)
        {
            lastUsed = start;
            if (!tokenCanStartExpression(start, tokens))
            {
                return false;
            }
            int current = start;
            int braceLevel = 0;
            int count = tokens.Count;
            TokenType openBrace = 0;
            TokenType closeBrace = 0;
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
                else if (stopToken != null && tokenEquals(type, stopToken, token, voPPBehavior))
                {
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
                          || (lastToken.NeedsRight() && !token.IsPrimaryOrPrefix())
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
            lastUsed = current - 1;
            return current != start;
        }
    }
}
