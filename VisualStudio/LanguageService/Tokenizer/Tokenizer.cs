//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
//------------------------------------------------------------------------------

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.VisualStudio.Text;
using XSharpModel;
using LanguageService.SyntaxTree;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.CodeAnalysis.XSharp;
using System.Collections.Immutable;
using static XSharp.Parser.VsParser;
using Microsoft.VisualStudio.Threading;
using Microsoft.VisualStudio.Shell;

namespace XSharp.LanguageService
{

    /// <summary>
    /// Static class Tools. Offer services to get TokenList, Search members, ...
    /// </summary>
    ///
    public static class XSharpTokenTools
    {

        public static bool StringEquals(string lhs, string rhs)
        {
            return XSharpCompletionSource.StringEquals(lhs, rhs);
        }

        internal static IList<IToken> GetTokenListBeforeCaret(XSharpSearchLocation location, out CompletionState state)
        {
            var tokens = GetTokenList(location, out state, false);
            var result = new List<IToken>();
            foreach (XSharpToken token in tokens)
            {
                if (token.Position <= location.Position)
                    result.Add(token);
            }
            return result;
        }

        /// <summary>
        /// Call the classifier to classify the buffer. This fills the linestate and lines array
        /// </summary>
        /// <param name="location"></param>
        static void ClassifyBuffer(XSharpSearchLocation location)
        {
            var classifier = location.Snapshot.TextBuffer.GetClassifier();
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await classifier.ClassifyWhenNeededAsync();
            });
        }


        private static XSharpSearchLocation AdjustStartLineNumber(XSharpSearchLocation location)
        {
            var line = location.LineNumber;
            var doc = location.GetDocument();
            var lineFlags = doc.LineState;
            while (line > 0 && lineFlags.Get(line-1, out var flags) && flags.HasFlag(LineFlags.Continued))
            {
                line--;
            }
            return location.With(line, location.Position);
        }


        internal static IList<IToken> GetTokensUnderCursor(XSharpSearchLocation location, out CompletionState state)
        {

            var tokens = GetTokenList(location, out state, true, true).Where((t) => t.Channel == XSharpLexer.DefaultTokenChannel).ToList();
            // Find "current" token

            if (tokens.Count > 0)
            {
                var tokenUnderCursor = tokens.Count - 1;
                for (int i = tokens.Count - 1; i >= 0; i--)
                {
                    var token = tokens[i];
                    if (token.StartIndex <= location.Position && token.StopIndex >= location.Position)
                    {
                        tokenUnderCursor = i;
                        break;
                    }
                }
                // when the cursor is on a closing token then subtract one
                switch (tokens[tokenUnderCursor].Type)
                {
                    case XSharpLexer.RPAREN:
                    case XSharpLexer.RCURLY:
                    case XSharpLexer.RBRKT:
                    case XSharpLexer.GT:
                        tokenUnderCursor -= 1;
                        break;
                }

                // check to see if there is an open token ('<', '(', '{', '[' ) to the left of this.
                // when there is we want to delete everything up to and including the open token
                // so when the cursor is on the word 'String' in List<System.String>
                // then we delete "List<"
                // When there is a closing token before cursor like in "MyFunction():DoSomething" (when the cursor is on DoSomething)
                // then do not delete the expression before the colon
                // however when it is "MyFunction(DoSomething)"
                // then the token should be DoSomething and "MyFunction(" should be deleted
                bool done = false;
                for (int itoken = tokenUnderCursor - 1; itoken >= 0; itoken--)
                {
                    switch (tokens[itoken].Type)
                    {
                        case XSharpLexer.GT:
                        case XSharpLexer.RPAREN:
                        case XSharpLexer.RCURLY:
                        case XSharpLexer.RBRKT:
                            done = true;
                            break;
                        case XSharpLexer.LT:
                        case XSharpLexer.COMMA:
                        case XSharpLexer.LPAREN:
                        case XSharpLexer.LCURLY:
                        case XSharpLexer.LBRKT:
                            // delete tokens upto and including this one
                            tokens.RemoveRange(0, itoken + 1);
                            tokenUnderCursor -= (itoken + 1); // adjust position
                            done = true;
                            break;
                        default:
                            break;
                    }
                    if (done)
                        break;
                }
                if (tokenUnderCursor < 0)
                    tokenUnderCursor = 0;
                var selectedToken = tokens[tokenUnderCursor];
                var nextToken = tokenUnderCursor < tokens.Count - 1 ? tokens[tokenUnderCursor + 1] : null;
                done = false;
                switch (selectedToken.Type)
                {
                    case XSharpLexer.NAMEOF:
                    case XSharpLexer.TYPEOF:
                    case XSharpLexer.SIZEOF:
                    case XSharpLexer.SELF:
                    case XSharpLexer.SUPER:
                        if (nextToken != null && nextToken.Type == XSharpLexer.LPAREN)
                        {
                            return tokens;
                        }
                        break;
                    default:
                        if (XSharpLexer.IsKeyword(selectedToken.Type))
                        {
                            tokens.Clear();
                            tokens.Add(selectedToken);
                            return tokens;
                        }
                        break;
                }
                // When we are not on a Keyword then we need to walk back in the tokenlist to see
                // if we can evaluate the expression
                // This could be:
                // System.String.Compare()   // static method cal or method call
                // SomeVar:MethodCall()      // method call
                // Left(...)                 // function call
                // SomeId                    // local, global etc
                // SomeType.Id               // Static property or normal property
                // SomeVar:Id                // Instance field or property
                // If the token list contains with a RCURLY, RBRKT or RPAREN
                // Then strip everything until the matching LCURLY, LBRKT or LPAREN is found
                // So "MyFunction(a,b,c):D" becomes  "MyFunction():D"
                var list = new XSharpTokenList(tokens);
                tokens = new List<IToken>();
                while (!list.Eoi())
                {
                    var token = list.ConsumeAndGet();
                    switch (token.Type)
                    {
                        case XSharpLexer.LCURLY:
                            tokens.Add(token);
                            if (list.Contains(XSharpLexer.RCURLY))
                            {
                                // this may return false when the RCURLY belongs to another LCURLY
                                if (list.ConsumeUntilEndToken(XSharpLexer.RCURLY, out var endToken))
                                    tokens.Add(endToken);
                            }
                            break;
                        case XSharpLexer.LPAREN:
                            tokens.Add(token);
                            if (list.Contains(XSharpLexer.RPAREN))
                            {
                                // this may return false when the RPAREN belongs to another LPAREN
                                if (list.ConsumeUntilEndToken(XSharpLexer.RPAREN, out var endToken))
                                    tokens.Add(endToken);
                            }
                            break;
                        case XSharpLexer.LBRKT:
                            tokens.Add(token);
                            if (list.Contains(XSharpLexer.RBRKT))
                            {
                                // this may return false when the RBRKT belongs to another LBRKT
                                if (list.ConsumeUntilEndToken(XSharpLexer.RBRKT, out var endToken))
                                    tokens.Add(endToken);
                            }
                            break;
                        case XSharpLexer.DOT:
                        case XSharpLexer.COLON:
                        case XSharpLexer.SELF:
                        case XSharpLexer.SUPER:
                            tokens.Add(token);
                            break;
                        default:
                            tokens.Add(token);
                            if (XSharpLexer.IsOperator(token.Type))
                            {
                                done = true;
                            }
                            if (token.Type == XSharpLexer.VAR)
                            {
                                done = true;
                            }
                            else if (XSharpLexer.IsKeyword(token.Type) &&
                                !XSharpLexer.IsPositionalKeyword(token.Type)
                                )
                            {
                                done = true;
                            }
                            break;
                    }
                }
                // now result has the list of tokens starting with the cursor
                // we only keep:
                // ID, DOT, COLON, LPAREN, LBRKT, RBRKT
                // when we detect another token we truncate the list there
                if (tokens.Count > 0)
                {
                    var lastType = tokens[0].Type;
                    for (int i = tokenUnderCursor + 1; i < tokens.Count && !done; i++)
                    {
                        var token = tokens[i];
                        switch (token.Type)
                        {
                            case XSharpLexer.ID:
                            case XSharpLexer.DOT:
                            case XSharpLexer.COLON:
                            case XSharpLexer.LPAREN:
                            case XSharpLexer.LCURLY:
                            case XSharpLexer.LBRKT:
                                lastType = tokens[i].Type;
                                break;
                            case XSharpLexer.LT:
                                int gtPos = findTokenInList(tokens, i + 1, XSharpLexer.GT);
                                if (lastType == XSharpLexer.ID && gtPos > 0)
                                {
                                    gtPos += 1;
                                    tokens.RemoveRange(gtPos, tokens.Count - gtPos);
                                    done = true;
                                    break;
                                }
                                else
                                {
                                    goto default;
                                }
                            default:
                                tokens.RemoveRange(i, tokens.Count - i);
                                done = true;
                                break;
                        }
                    }
                }
            }
            // check for extra lparen, lcurly at the end
            int count = tokens.Count;
            if (count > 2 && count < tokens.Count - 2)
            {
                if (tokens[count - 2].Type == XSharpLexer.LPAREN)
                {
                    switch (tokens[count - 1].Type)
                    {
                        case XSharpLexer.LPAREN:
                        case XSharpLexer.LCURLY:
                            tokens.RemoveAt(count - 1);
                            break;
                    }
                }
            }
            return tokens;
        }

        private static int findTokenInList(IList<IToken> list, int startpos, int tokenToFind)
        {
            for (var j = startpos; j < list.Count; j++)
            {
                var token = list[j];
                if (token.Type == tokenToFind)
                {
                    return j;
                }
            }
            return -1;
        }

        internal static IList<IToken> GetTokenList(XSharpSearchLocation location, out CompletionState state,
            bool includeKeywords = false, bool underCursor = false)
        {
            location = AdjustStartLineNumber(location);
            var xdocument = location.GetDocument();
            var tokens =  xdocument.GetTokensInLine(location.LineNumber);
            //
            state = CompletionState.General;
            if (tokens.Count == 0)
                return tokens;
            // if the token appears after comma or paren then strip the tokens
            // now look forward and find the first token that is on or after the triggerpoint
            var result = new List<IToken>();
            var last = XSharpLexer.Eof;
            bool allowdot = location.Project?.ParseOptions?.AllowDotForInstanceMembers ?? false;
            var cursorPos = location.Position;
            var done = false;
            var list = new XSharpTokenList(tokens);
            bool afterOut = false;

            while (!done && !list.Eoi())
            {
                IToken lasttoken = result.LastOrDefault();
                var token = (XSharpToken) list.ConsumeAndGet();
                int openToken = 0;
                XSharpToken closeToken = null;
                bool isHit = token.StartIndex <= cursorPos && token.StopIndex >= cursorPos && underCursor;
                bool isNotLast = token.StopIndex < location.Position - 1;
                if (token.StartIndex > cursorPos)
                {
                    // after the cursor we only include the open tokens
                    // so we can see if the id under the cursor is a method, constructor etc
                    switch (token.Type)
                    {
                        case XSharpLexer.WS:
                        case XSharpLexer.LPAREN:
                        case XSharpLexer.LCURLY:
                        case XSharpLexer.LBRKT:
                            break;
                        case XSharpLexer.LT:
                            // if this is a generic type
                            // then add the complete
                            bool first = true;
                            bool endoflist = false;
                            while (!endoflist)
                            {
                                endoflist = true;
                                if (list.La1 == XSharpLexer.ID || XSharpLexer.IsType(list.La1))
                                {
                                    if (list.La2 == XSharpLexer.GT || list.La2 == XSharpLexer.COMMA)
                                    {
                                        if (first)
                                        {
                                            result.Add(token);
                                            first = false;
                                        }
                                        result.Add(list.ConsumeAndGet()); // la1
                                        result.Add(list.ConsumeAndGet()); // la2
                                        endoflist = false;
                                    }
                                }
                            }
                            continue;
                        default:
                            done = true;
                            break;
                    }
                    if (done)
                        continue;
                }
                switch (token.Type)
                {
                    // after these tokens we "restart" the list
                    case XSharpLexer.EOS:
                        if (token.Position < cursorPos && token != tokens.Last() && last != XSharpLexer.LINE_CONT)
                        {
                            // an EOS inside a line before the cursor
                            // so there are 2 or more statements on the same line
                            // clear the first statement
                            result.Clear();
                            state = CompletionState.General;
                        }
                        else
                        {
                            // Exit loop, ignore the rest of the statements
                            done = true;
                        }
                        continue;
                    case XSharpLexer.WS:
                    case XSharpLexer.Eof:
                        continue;
                    case XSharpLexer.TO:
                    case XSharpLexer.UPTO:
                    case XSharpLexer.DOWNTO:
                    case XSharpLexer.IN:
                        if (!isHit)
                        {
                            result.Clear();
                            if (isNotLast) // there has to be a space after the token
                            {
                                state = CompletionState.General;
                            }
                            else
                                state = CompletionState.None;
                        }
                        else
                        {
                            result.Add(token);
                        }
                        break;
                    case XSharpLexer.LCURLY:
                        state = CompletionState.Constructors;
                        result.Add(token);
                        break;
                    case XSharpLexer.LPAREN:
                        state = CompletionState.StaticMembers | CompletionState.InstanceMembers;
                        result.Add(token);
                        break;
                    case XSharpLexer.LBRKT:
                        state = CompletionState.Brackets;
                        result.Add(token);
                        break;
                    case XSharpLexer.VALUE:
                        token.Type = XSharpLexer.ID;
                        result.Add(token);
                        break;
                    case XSharpLexer.ID:
                    case XSharpLexer.NAMEOF:
                    case XSharpLexer.TYPEOF:
                    case XSharpLexer.SIZEOF:
                        result.Add(token);
                        break;
                    case XSharpLexer.RCURLY:
                    case XSharpLexer.RPAREN:
                    case XSharpLexer.RBRKT:
                        bool add = true;
                        if (result.Count > 0 && token == list.LastOrDefault)
                        {

                            if (lasttoken.Type == XSharpLexer.COLON ||
                                lasttoken.Type == XSharpLexer.DOT)
                            {
                                // closing char after colon or dot
                                add = false;
                                done = true;
                            }
                        }
                        if (add)
                        {
                            result.Add(token);
                            // delete everything between parens, curly braces and brackets closing token before cursor pos
                            if (token.Position < location.Position)
                            {
                                closeToken = token;
                                if (token.Type == XSharpLexer.RCURLY)
                                    openToken = XSharpLexer.LCURLY;
                                else if (token.Type == XSharpLexer.RPAREN)
                                    openToken = XSharpLexer.LPAREN;
                                else if (token.Type == XSharpLexer.RBRKT)
                                    openToken = XSharpLexer.LBRKT;
                            }
                        }
                        break;
                    case XSharpLexer.STATIC:        // These tokens are all before a namespace of a (namespace dot) type
                        if (isNotLast) // there has to be a space after the token
                        {
                            state = CompletionState.General;
                        }
                        else
                            state = CompletionState.None;
                        break;
                    case XSharpLexer.USING:
                         if (isNotLast) // there has to be a space after the token
                         {
                            list.Expect(XSharpLexer.WS);

                            if (list.Expect(XSharpLexer.STATIC))
                            {
                                state = CompletionState.Namespaces | CompletionState.Types;
                                result.Clear();
                            }
                            else if (list.La1 == XSharpLexer.ID)
                            {
                                state = CompletionState.Namespaces;
                                result.Clear();
                            }

                        }
                        break;

                    case XSharpLexer.MEMBER:
                        if (isNotLast) // there has to be a space after the token
                            state = CompletionState.StaticMembers;
                        else
                            state = CompletionState.None;
                        break;
                    case XSharpLexer.OUT:
                        afterOut = true;
                        result.Add(token);
                        break;

                    case XSharpLexer.AS:
                    case XSharpLexer.IS:
                    case XSharpLexer.REF:
                    case XSharpLexer.INHERIT:
                        if (!isHit && ! afterOut)
                        {
                            result.Clear();
                        }
                        else
                        {
                            result.Add(token);
                        }
                        if (isNotLast) // there has to be a space after the token
                            state = CompletionState.Namespaces | CompletionState.Types | CompletionState.Inherit;
                        else
                            state = CompletionState.None;
                        break;

                    case XSharpLexer.IMPLEMENTS:
                        result.Clear();
                        if (isNotLast)
                            state = CompletionState.Namespaces | CompletionState.Interfaces;
                        else
                            state = CompletionState.None;

                        break;
                    case XSharpLexer.COLON:
                        state = CompletionState.InstanceMembers;
                        result.Add(token);
                        break;
                    case XSharpLexer.DOT:
                        if (lasttoken != null)
                        {
                            switch (lasttoken.Type)
                            {
                                // Dot after } should only list Instance Members
                                case XSharpLexer.RCURLY:
                                case XSharpLexer.RPAREN:
                                case XSharpLexer.RBRKT:
                                    state = CompletionState.InstanceMembers;
                                    break;
                                case XSharpLexer.ID:
                                default:
                                    if (!state.HasFlag(CompletionState.Namespaces))
                                        state = CompletionState.Namespaces | CompletionState.Types | CompletionState.StaticMembers;
                                    if (allowdot)
                                        state |= CompletionState.InstanceMembers;
                                    break;
                            }
                        }
                        result.Add(token);
                        break;

                    case XSharpLexer.QMARK:
                        if (result.Count != 0)       // when at start of line then do not add. Otherwise it might be a Nullable type or conditional access expression
                            result.Add(token);
                        break;
                    case XSharpLexer.QQMARK:
                        if (result.Count != 0)       // when at start of line then do not add. Otherwise it might be a binary expression
                            result.Add(token);
                        break;

                    case XSharpLexer.BACKSLASH:
                    case XSharpLexer.BACKBACKSLASH:
                        // this should only be seen at start of line
                        // clear the list to be sure
                        result.Clear();
                        break;
                    case XSharpLexer.NAMESPACE:
                        state = CompletionState.Namespaces;
                        break;
                    case XSharpLexer.COMMA:
                        afterOut = false;
                        state = CompletionState.General;
                        result.Add(token);
                        break;
                    case XSharpLexer.ASSIGN_OP:
                    case XSharpLexer.COLONCOLON:
                    case XSharpLexer.SELF:
                    case XSharpLexer.SUPER:
                        state = CompletionState.General;
                        result.Add(token);
                        break;
                    default:
                        if (state == CompletionState.None)
                            state = CompletionState.General;
                        if (XSharpLexer.IsPseudoFunction(token.Type))
                        {
                            result.Add(token);
                        }
                        else if (XSharpLexer.IsOperator(token.Type))
                        {
                            result.Add(token);
                        }
                        else if (XSharpLexer.IsType(token.Type))
                        {
                            result.Add(token);
                        }
                        else if (XSharpLexer.IsConstant(token.Type))
                        {
                            result.Add(token);
                        }
                        else if (XSharpLexer.IsKeyword(token.Type) && includeKeywords)   // For code completion we want to include keywords
                        {
                            token.Text = XSettings.FormatKeyword(token.Text);
                            result.Add(token);
                        }
                        break;
                }
                last = token.Type;
                // remove everything between parens, curly braces or brackets when the closing token is before the cursor
                if (openToken != 0 && closeToken != null)
                {
                    var iLast = result.Count - 1;
                    int count = 0;
                    while (iLast >= 0 && result[iLast] != closeToken)
                    {
                        iLast--;
                    }
                    int closeType = closeToken.Type;
                    while (iLast >= 0)
                    {
                        var type = result[iLast].Type;
                        if (type == closeType)
                        {
                            count += 1;
                        }
                        else if (type == openToken)
                        {
                            count -= 1;
                            if (count == 0)
                            {
                                if (iLast < result.Count - 1)
                                {
                                    result.RemoveRange(iLast + 1, result.Count - iLast - 2);
                                }
                                break;
                            }
                        }
                        iLast -= 1;
                    }
                }
            }

            // when the list ends with a comma, drop the ending comma. Why ?
            if (result.Count > 0)
            {
                var end = result.Last();
                if (end.Type == XSharpLexer.COMMA)
                {
                    result.RemoveAt(result.Count - 1);
                }
            }
            return result;
        }

        public static XSourceTypeSymbol FindNamespace(int position, XFile file)
        {
            if (file == null)
            {
                return null;
            }
            if (file.TypeList == null)
                return null;
            //
            XSourceTypeSymbol found = null;
            foreach (XSourceTypeSymbol eltType in file.TypeList.Values)
            {

                if (eltType.Interval.ContainsInclusive(position))
                {
                    if (eltType.Kind.IsType() && eltType.Kind != Kind.Delegate)
                    {
                        found = eltType;
                    }
                    if (eltType.Kind == Kind.Namespace)
                        return eltType;
                }
            }
            //
            if (found != null)
            {
                string name = found.Name;
                if (found.Namespace?.Length > 0)
                    name = found.Namespace + "." + name;
                var pos = name.LastIndexOf('.');
                if (pos > 0)
                {
                    name = name.Substring(0, pos);
                }
                var nSpace = new XSourceNamespaceSymbol(name, found.Range, found.Interval, file);
                return nSpace;
            }
#if TRACE
                // a source file without a namespace is really not a problem
                //Support.Debug(String.Format("Cannot find namespace at position {0} in file {0} .", position, fileName));
#endif
            return null;
        }

    }


}
