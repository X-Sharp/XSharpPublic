//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Package;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.VisualStudio.OLE.Interop;

namespace XSharp.LanguageService
{
    class XSharpSource : Source
    {
        public XSharpSource(XSharpLanguageService service,
                        IVsTextLines textLines,
                        Colorizer colorizer)
            : base(service, textLines, colorizer)
        {

        }


        public override TextSpan CommentSpan(TextSpan span)
        {
            TextSpan span2 = span;
            CommentInfo commentFormat = this.GetCommentFormat();
            using (new CompoundAction(this, "CommentSelection"))
            {
                if (commentFormat.UseLineComments && !string.IsNullOrEmpty(commentFormat.LineStart))
                {
                    if (span.iStartIndex == 0 && span.iEndIndex == 0)
                    {
                        if (span.iEndLine > span.iStartLine)
                        {
                            span.iEndLine--;
                        }
                    }
                    // Use our own to align comment
                    span2 = this.xsCommentLines(span, commentFormat.LineStart);
                    return span2;
                }
                if (!string.IsNullOrEmpty(commentFormat.BlockStart) && !string.IsNullOrEmpty(commentFormat.BlockEnd))
                {
                    span2 = this.CommentBlock(span, commentFormat.BlockStart, commentFormat.BlockEnd);
                }
            }
            return span2;
        }

        private TextSpan xsCommentLines(TextSpan span, String commentStart)
        {
            int commentpos = 0;
            // First Search the min position for comment
            for (int line = span.iStartLine; line <= span.iEndLine; line++)
            {
                int pos = ScanToNonWhitespaceChar(line);
                if (pos < GetLineLength(line))
                {
                    commentpos = Math.Min(commentpos, pos);
                }
            }
            // Apply
            for (int line = span.iStartLine; line <= span.iEndLine; line++)
            {
                if (ScanToNonWhitespaceChar(line) < GetLineLength(line))
                {
                    SetText(line, commentpos, line, commentpos, commentStart);
                }
            }
            return span;
        }


        //public override void BeginParse()
        //{
        //    base.BeginParse();
        //}
        //public override ParseRequest BeginParse(int line, int idx, TokenInfo info, ParseReason reason, IVsTextView view, ParseResultHandler callback)
        //{
        //    return base.BeginParse(line, idx, info, reason, view, callback);
        //}
        public override void OnCommand(IVsTextView textView, VSConstants.VSStd2KCmdID command, char ch)
        {
            base.OnCommand(textView, command, ch);
            // Vulcan formats the keywords here.
            
        }

    }
}
