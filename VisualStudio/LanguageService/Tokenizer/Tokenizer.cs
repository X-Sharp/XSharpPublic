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
    /// <summary>
    /// Static class Tools. Offer services to get TokenList, Search members, ...
    /// </summary>
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
            ITextSnapshot snapshot, out IToken stopToken, XFile file, XSourceMemberSymbol fromMember, bool includeKeywords = false)
        {
            var bufferText = snapshot.GetText();
            return _getTokenList(triggerPointPosition, triggerPointLineNumber, bufferText, out stopToken, file, fromMember, includeKeywords);
        }

        private static List<XSharpToken> _getTokenList(int triggerPointPosition, int triggerPointLineNumber,
            string bufferText, out IToken stopToken, XFile file, XSourceMemberSymbol fromMember, bool includeKeywords)
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
            return GetTokenList(triggerPointPosition, triggerPointLineNumber, stream, out stopToken, includeKeywords);
        }


        public static List<XSharpToken> GetTokenList(int triggerPointPosition, int triggerPointLineNumber, BufferedTokenStream tokens, out IToken stopToken, bool includeKeywords = false)
        {
            var tokenList = new List<XSharpToken>();
            //
            stopToken = null;
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
                if (token.StartIndex >= triggerPointPosition && !isOpenToken(token.Type))
                    break;
                switch (token.Type)
                {
                    // after these tokens we "restart" the list
                    case XSharpLexer.TO:
                    case XSharpLexer.UPTO:
                    case XSharpLexer.DOWNTO:
                    case XSharpLexer.IN:
                        tokenList.Clear();
                        break;
                    case XSharpLexer.LCURLY:
                    case XSharpLexer.LPAREN:
                    case XSharpLexer.LBRKT:
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
                    case XSharpLexer.COMMA:
                    case XSharpLexer.ASSIGN_OP:
                    case XSharpLexer.USING:
                    case XSharpLexer.AS:
                    case XSharpLexer.IS:
                    case XSharpLexer.REF:
                    case XSharpLexer.IMPLEMENTS:
                    case XSharpLexer.INHERIT:
                    case XSharpLexer.DOT:
                    case XSharpLexer.COLON:
                    case XSharpLexer.COLONCOLON:
                    case XSharpLexer.SELF:
                    case XSharpLexer.SUPER:
                        tokenList.Add(token);
                        break;
                    default:
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
                        if (XSharpLexer.IsKeyword(token.Type) && includeKeywords)
                        {
                            tokenList.Add(token);
                        }
                        break;
                }
                last = token.Type;
                // find open token on the list and remove everything in between
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
            if (tokenList.Count > 0)
            {
                var end = tokenList[tokenList.Count - 1];
                if (end.Type == XSharpLexer.COMMA)
                {
                    tokenList.RemoveAt(tokenList.Count - 1);
                }
            }
            //if (tokenList.Count > 1)
            //{
            //    //var pos = tokenList.Count - 2;
                //while (pos >= 0)
                //{
                //    // after open token or comma then remove the tokens 
                //    var previous = tokenList[pos].Type;
                //    if (previous == XSharpLexer.LPAREN
                //        || previous == XSharpLexer.LCURLY
                //        || previous == XSharpLexer.LBRKT
                //        || previous == XSharpLexer.COMMA)
                //    {
                //        tokenList.RemoveRange(0, pos + 1);
                //        break;
                //    }
                //    pos -= 1;
                //}
            //}
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
        private static bool isCloseToken(int token)
        {
            switch (token)
            {
                case XSharpLexer.RCURLY:
                case XSharpLexer.RPAREN:
                case XSharpLexer.RBRKT:
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
