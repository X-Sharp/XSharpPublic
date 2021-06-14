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

namespace XSharp.LanguageService
{
    [Flags]
    public enum CompletionState
    {
        None = 0,
        General = 1 << 0,
        Namespaces = 1 << 1,
        Types = 1 << 2,
        Interfaces = 1 << 3,
        StaticMembers = 1 << 4,
        InstanceMembers = 1 << 5,
        Constructors = 1 << 6,
        Brackets = 1 << 7,

    }
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

        /// <summary>
        /// Retrieve a List of Token, based on a position in buffer.
        /// Moving back in the buffer, all tokens are retrieved and stored.
        /// </summary>
        /// <param name="triggerPointPosition">The position in the buffer </param>
        /// <param name="triggerPointLineNumber"></param>
        /// <param name="bufferText"></param>
        /// <param name="stopToken">The IToken that stops the move backwards</param>
        /// <param name="fromGotoDefn">Indicate if the call is due to Goto Definition</param>
        /// <param name="file">XFile object to use for the context</param>
        /// <param name="fromMember">The Member containing the position</param>
        /// <param name="includeKeywords">Should keywords be included in the tokenlist</param>
        /// <returns></returns>
        internal static List<XSharpToken> GetTokenList(XSharpSearchLocation location, out CompletionState state, bool includeKeywords = false)
        {
            var bufferText = location.Snapshot.GetText();
            return _getTokenList(location, bufferText, out state, includeKeywords);
        }

        private static List<XSharpToken> _getTokenList(XSharpSearchLocation location, 
            string bufferText, out CompletionState state, bool includeKeywords)
        {
            //////////////////////////////////////
            //////////////////////////////////////
            // Try to speedup the process, Tokenize only the Member source if possible (and not the FULL source text)
            var fromMember = location.Member;
            if (fromMember != null && fromMember.Interval.Start < location.Position && fromMember.Kind.HasBody())
            {
                // if the trigger point is after the end of the member then the member information is old and
                // we should use that position to determine the end of the buffer to scan
                // And make sure we also add some more because that does not hurt.
                int nWidth;
                if (location.Position > fromMember.Interval.Stop)
                {
                    nWidth = location.Position - fromMember.Interval.Start + 500;
                }
                else
                {
                    nWidth = fromMember.Interval.Width + 500; 
                }
                nWidth = Math.Min(nWidth, bufferText.Length - fromMember.Interval.Start);
                bufferText = bufferText.Substring(fromMember.Interval.Start, nWidth);
                // Adapt the positions.
                location = location.With(location.LineNumber - fromMember.Range.StartLine,
                    location.Position - fromMember.Interval.Start + 1);
            }
            else
            {
                // no need to parse the whole buffer. It could be huge
                int maxLen = location.Position + 500;
                if (maxLen > bufferText.Length)
                    maxLen = bufferText.Length;
                bufferText = bufferText.Substring(0, maxLen);
                // no need to adjust the line and position since we are working from the start of the buffer
            }


            ITokenStream tokenStream;
            var reporter = new ErrorIgnorer();
            // Get compiler options
            XSharpParseOptions parseoptions;
            string fileName;
            if (location.File != null)
            {
                var prj = location.File.Project.ProjectNode;
                parseoptions = prj.ParseOptions;
                fileName = location.File.FullPath;
            }
            else
            {
                parseoptions = XSharpParseOptions.Default;
                fileName = "MissingFile.prg";
            }


            bool ok = Lex(bufferText, fileName, parseoptions, reporter, out tokenStream);
            var stream = tokenStream as BufferedTokenStream;
            return GetTokenList(location, stream, out state, includeKeywords);
        }


        internal static IList<XSharpToken> GetTokensUnderCursor(XSharpSearchLocation location, BufferedTokenStream tokens)
        {

            var result = GetTokenList(location, tokens, out var _, true);
            // Find "current" token
            if (result.Count > 0)
            {
                var tokenUnderCursor = 0;
                for (int i = 0; i < result.Count; i++)
                {
                    var token = result[i];
                    if (token.StartIndex <= location.Position && token.StopIndex >= location.Position)
                    {
                        tokenUnderCursor = i;
                        break;
                    }
                }
                // now walk back in the list and find if there are '(', '{' or '[' before the first token.
                // when there are then delete the tokens upto the
                bool done = false;
                int lastToken = XSharpLexer.EOS;
                for (int i = tokenUnderCursor; i >= 0; i--)
                {
                    var token = result[i];
                    switch (token.Type)
                    {
                        case XSharpLexer.LCURLY:
                        case XSharpLexer.LPAREN:
                        case XSharpLexer.LBRKT:
                            done = true;
                            break;
                        case XSharpLexer.RCURLY:
                        case XSharpLexer.RPAREN:
                        case XSharpLexer.RBRKT:
                            if (lastToken != XSharpLexer.DOT && lastToken != XSharpLexer.COLON)
                            {
                                done = true;
                            }
                            break;
                        case XSharpLexer.DOT:
                        case XSharpLexer.COLON:
                        case XSharpLexer.SELF:
                        case XSharpLexer.SUPER:
                            break;
                        default:
                            if (XSharpLexer.IsOperator(token.Type))
                            {
                                done = true;
                            }
                            else if (XSharpLexer.IsKeyword(token.Type) &&
                                !XSharpLexer.IsPositionalKeyword(token.Type))
                            {
                                done = true;
                            }
                            break;
                    }
                    if (done)
                    {
                        result.RemoveRange(0, i + 1);
                        break;
                    }
                    lastToken = token.Type;
                }
            }
            // check for extra lparen, lcurly at the end
            int count = result.Count;
            if (count > 2)
            {
                if (result[count-2].Type == XSharpLexer.LPAREN)
                {
                    switch (result[count - 1].Type)
                    {
                        case XSharpLexer.LPAREN:
                        case XSharpLexer.LCURLY:
                            result.RemoveAt(count - 1);
                            break;
                    }
                }
            }
            return result;
        }

        internal static List<XSharpToken> GetTokenList(XSharpSearchLocation location, BufferedTokenStream tokens,
            out CompletionState state, bool includeKeywords = false)
        {
            var tokenList = new List<XSharpToken>();
            //
            state = CompletionState.General ;
            if (tokens == null)
                return tokenList;
            // the line numbers in the IToken are 1 based. Vs is 0 based.
            int oneBasedLineNumber = location.LineNumber + 1;
            var line = tokens.GetTokens().Where(
                 t => t.Channel == XSharpLexer.DefaultTokenChannel &&
                 t.Line == oneBasedLineNumber).ToList();
            if (line.Count == 0)
                return tokenList;
            // if the token appears after comma or paren then strip the tokens 
            // now look forward and find the first token that is on or after the triggerpoint
            var last = XSharpLexer.Eof;
            bool allowdot = location.Project?.ParseOptions?.AllowDotForInstanceMembers ?? false;
            foreach (XSharpToken token in line)
            {
                int open = 0;
                // only add open tokens to the list after the trigger point
                if (token.StartIndex >= location.Position )
                {
                    // Generic types. 
                    if (token.Type == XSharpLexer.LT
                        && (state.HasFlag(CompletionState.Types) || state.HasFlag(CompletionState.Interfaces)))
                    {
                        var start = line.IndexOf(token);
                        for (int i = start; i < line.Count; i++)
                        {
                            var element = (XSharpToken) line[i];
                            tokenList.Add(element);
                            if (element.Type == XSharpLexer.GT)
                                break;
                        }
                        break;
                    }
                    if (!isOpenToken(token.Type))
                    {
                        break;
                    }
                }
                switch (token.Type)
                {
                    // after these tokens we "restart" the list
                    case XSharpLexer.WS:
                    case XSharpLexer.EOS:
                    case XSharpLexer.Eof:
                        continue;
                    case XSharpLexer.TO:
                    case XSharpLexer.UPTO:
                    case XSharpLexer.DOWNTO:
                    case XSharpLexer.IN:
                        state = CompletionState.General;
                        tokenList.Clear();
                        break;
                    case XSharpLexer.LCURLY:
                        state = CompletionState.Constructors;
                        tokenList.Add(token);
                        break;
                    case XSharpLexer.LPAREN:
                        state = CompletionState.StaticMembers | CompletionState.InstanceMembers;
                        tokenList.Add(token);
                        break;
                    case XSharpLexer.LBRKT:
                        state = CompletionState.Brackets;
                        tokenList.Add(token);
                        break;
                    case XSharpLexer.ID:
                        tokenList.Add(token);
                        break;
                    case XSharpLexer.RCURLY:
                        tokenList.Add(token);
                        open = XSharpLexer.LCURLY;
                        break;
                    case XSharpLexer.RPAREN:
                        tokenList.Add(token);
                        open = XSharpLexer.LPAREN;
                        break;
                    case XSharpLexer.RBRKT:
                        tokenList.Add(token);
                        open = XSharpLexer.LBRKT;
                        break;
                    case XSharpLexer.STATIC:        // These tokens are all before a namespace of a (namespace dot) type
                        if (state == CompletionState.Namespaces)
                            state |= CompletionState.Types;
                        else
                            state = CompletionState.General;
                        break;
                    case XSharpLexer.USING:
                        state = CompletionState.Namespaces;
                        break;

                    case XSharpLexer.AS:
                    case XSharpLexer.IS:
                    case XSharpLexer.REF:
                    case XSharpLexer.INHERIT:
                        tokenList.Clear();
                        state = CompletionState.Namespaces | CompletionState.Types;
                        break;

                    case XSharpLexer.IMPLEMENTS:
                        tokenList.Clear();
                        state = CompletionState.Namespaces | CompletionState.Interfaces;
                        break;
                    case XSharpLexer.COLON:
                        state = CompletionState.InstanceMembers;
                        tokenList.Add(token);
                        break;
                    case XSharpLexer.DOT:
                        if (!state.HasFlag(CompletionState.Namespaces))
                        {
                            state = CompletionState.Namespaces | CompletionState.Types | CompletionState.StaticMembers;
                            if (allowdot)
                                state |= CompletionState.InstanceMembers;
                        }
                        tokenList.Add(token);
                        break;

                    case XSharpLexer.QMARK: 
                        if (tokenList.Count != 0)       // when at start of line then do not add. Otherwise it might be a Nullable type or conditional access expression
                            tokenList.Add(token);
                        break;
                    case XSharpLexer.QQMARK:
                        if (tokenList.Count != 0)       // when at start of line then do not add. Otherwise it might be a binary expression
                            tokenList.Add(token);
                        break;

                    case XSharpLexer.BACKSLASH:
                    case XSharpLexer.BACKBACKSLASH:
                        // this should only be seen at start of line
                        // clear the list to be sure
                        tokenList.Clear();
                        break;

                    case XSharpLexer.COMMA:
                    case XSharpLexer.ASSIGN_OP:
                    case XSharpLexer.COLONCOLON:
                    case XSharpLexer.SELF:
                    case XSharpLexer.SUPER:
                        state = CompletionState.General;
                        tokenList.Add(token);
                        break;
                    default:
                        state = CompletionState.General;
                        if (XSharpLexer.IsOperator(token.Type))
                        {
                            tokenList.Add(token);
                        }
                        else if (XSharpLexer.IsType(token.Type))
                        {
                            tokenList.Add(token);
                        }
                        else if (XSharpLexer.IsConstant(token.Type))
                        {
                            tokenList.Add(token);
                        }
                        if (XSharpLexer.IsKeyword(token.Type) && includeKeywords)   // For code completion we want to include keywords
                        {
                            tokenList.Add(token);
                        }
                        break;
                }
                last = token.Type;
                // if we just read a close token then find the matching open token in the list and remove everything in between
                if (open != 0)
                {
                    var iLast = tokenList.Count - 1;
                    while (iLast >= 0)
                    {
                        if (tokenList[iLast].Type == open)
                        {
                            if (iLast < tokenList.Count - 1)
                            {
                                tokenList.RemoveRange(iLast + 1, tokenList.Count - iLast - 2);
                            }
                            break;
                        }
                        iLast -= 1;
                    }
                }
            }
            // when the list ends with a comma, drop the ending comma. Why ?
            if (tokenList.Count > 0)
            {
                var end = tokenList.Last();
                if (end.Type == XSharpLexer.COMMA)
                {
                    tokenList.RemoveAt(tokenList.Count - 1);
                }
            }
            return tokenList;
        }
        private static bool isOpenToken(int token)
        {
            switch (token)
            {
                case XSharpLexer.LCURLY:
                case XSharpLexer.LPAREN:
                case XSharpLexer.LBRKT:
                    return true;
            }
            return false;
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
                    if (eltType.Kind.IsType())
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
                XSourceTypeSymbol nSpace = new XSourceTypeSymbol(name, Kind.Namespace, Modifiers.Public, found.Range, found.Interval, file);
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
