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
        public static List<XSharpToken> GetTokenList(int triggerPointPosition, int triggerPointLineNumber,
            ITextSnapshot snapshot, out CompletionState state, XFile file, XSourceMemberSymbol fromMember, bool includeKeywords = false)
        {
            var bufferText = snapshot.GetText();
            return _getTokenList(triggerPointPosition, triggerPointLineNumber, bufferText, out state, file, fromMember, includeKeywords);
        }

        private static List<XSharpToken> _getTokenList(int triggerPointPosition, int triggerPointLineNumber,
            string bufferText, out CompletionState state, XFile file, XSourceMemberSymbol fromMember, bool includeKeywords)
        {
            //////////////////////////////////////
            //////////////////////////////////////
            // Try to speedup the process, Tokenize only the Member source if possible (and not the FULL source text)
            if (fromMember != null && fromMember.Interval.Start < triggerPointPosition && fromMember.Kind.HasBody())
            {
                // if the trigger point is after the end of the member then the member information is old and
                // we should use that position to determine the end of the buffer to scan
                // And make sure we also add some more because that does not hurt.
                int nWidth;
                if (triggerPointPosition > fromMember.Interval.Stop)
                {
                    nWidth = triggerPointPosition - fromMember.Interval.Start + 500;
                }
                else
                {
                    nWidth = fromMember.Interval.Width + 500;
                }
                nWidth = Math.Min(nWidth, bufferText.Length - fromMember.Interval.Start);
                bufferText = bufferText.Substring(fromMember.Interval.Start, nWidth);
                // Adapt the positions.
                triggerPointPosition = triggerPointPosition - fromMember.Interval.Start + 1;
                triggerPointLineNumber = triggerPointLineNumber - (fromMember.Range.StartLine);
            }
            else
            {
                // no need to parse the whole buffer. It could be huge
                int maxLen = triggerPointPosition + 500;
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
            if (file != null)
            {
                var prj = file.Project.ProjectNode;
                parseoptions = prj.ParseOptions;
                fileName = file.FullPath;
            }
            else
            {
                parseoptions = XSharpParseOptions.Default;
                fileName = "MissingFile.prg";
            }


            bool ok = Lex(bufferText, fileName, parseoptions, reporter, out tokenStream);
            var stream = tokenStream as BufferedTokenStream;
            return GetTokenList(triggerPointPosition, triggerPointLineNumber, stream, out state, includeKeywords);
        }


        public static List<XSharpToken> GetTokenList(int triggerPointPosition, int triggerPointLineNumber, BufferedTokenStream tokens,
            out CompletionState state, bool includeKeywords = false)
        {
            var tokenList = new List<XSharpToken>();
            //
            state = CompletionState.General ;
            if (tokens == null)
                return tokenList;
            // the line numbers in the IToken are 1 based. Vs is 0 based.
            int oneBasedLineNumber = triggerPointLineNumber + 1;
            var line = tokens.GetTokens().Where(
                 t => t.Channel == XSharpLexer.DefaultTokenChannel &&
                 t.Line == oneBasedLineNumber).ToList();
            if (line.Count == 0)
                return tokenList;
            // if the token appears after comma or paren then strip the tokens 
            // now look forward and find the first token that is on or after the triggerpoint
            var last = XSharpLexer.Eof;
            foreach (XSharpToken token in line)
            {
                int open = 0;
                // only add open tokens to the list after the trigger point
                if (token.StartIndex >= triggerPointPosition )
                {
                    if (token.Type == XSharpLexer.LT
                        && (state.HasFlag(CompletionState.Types) || state.HasFlag(CompletionState.Interfaces)))
                    {
                        var start = line.IndexOf(token);
                        for (int i = 0; i < line.Count; i++)
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
                            if (XSettings.EditorUseDotAsUniversalSelector)
                                state |= CompletionState.InstanceMembers;
                        }
                        tokenList.Add(token);
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

  
        internal static IToken GetPreviousToken(ITokenStream tokens, IToken currentToken, bool checkLine)
        {
            XSharpToken prev = null;
            if (currentToken != null)
            {
                prev = (XSharpToken)currentToken;
                int Line = prev.Line;
                do
                {
                    if (prev.OriginalTokenIndex == 0)
                        return null;
                    prev = (XSharpToken)tokens.Get(prev.OriginalTokenIndex - 1);
                    if (checkLine && (prev.Line != Line))
                    {
                        prev = null;
                    }
                } while ((prev != null) && (string.IsNullOrWhiteSpace(prev.Text)));
            }
            return prev;
        }

        internal static IToken GetPreviousToken(ITokenStream tokens, IToken currentToken)
        {
            return GetPreviousToken(tokens, currentToken, true);
        }


        static internal SnapshotPoint FindEndOfCurrentToken(SnapshotPoint ssp, ITextSnapshot snapshot)
        {
            var done = false;
            while (ssp.Position < snapshot.Length && !done)
            {
                var c = ssp.GetChar();
                switch (c)
                {
                    case '(':
                    case '{':
                    case '[':
                    case ':':
                    case '.':
                        done = true;
                        ssp -= 1;
                        break;
                    default:
                        if (char.IsLetterOrDigit(c) || c == '_')
                            ssp += 1;
                        else
                            done = true;
                        break;
                }
            }
            return ssp;
        }

    }


}
