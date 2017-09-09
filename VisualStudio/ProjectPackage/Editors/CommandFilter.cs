//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.ComponentModel.Composition;
using System.Diagnostics;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.VisualStudio.Utilities;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Package;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio.Editor;
using Microsoft.VisualStudio.Language.Intellisense;
using LanguageService.SyntaxTree;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using System.Collections.Generic;
using System.Reflection;
using Microsoft.VisualStudio.Text.Operations;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Text.Editor.OptionsExtensionMethods;
using XSharpColorizer;
using XSharpModel;
using XSharpLanguage;
using System.Linq;

namespace XSharp.Project
{
    internal sealed class CommandFilter : IOleCommandTarget
    {
        ICompletionSession _completionSession;
        public ITextView TextView { get; private set; }
        public ICompletionBroker CompletionBroker { get; private set; }
        public IOleCommandTarget Next { get; set; }



        ISignatureHelpBroker SignatureBroker;
        ISignatureHelpSession _signatureSession;
        ITextStructureNavigator m_navigator;
        IBufferTagAggregatorFactoryService Aggregator;




        public CommandFilter(IWpfTextView textView, ICompletionBroker completionBroker, ITextStructureNavigator nav, ISignatureHelpBroker signatureBroker, IBufferTagAggregatorFactoryService aggregator)
        {
            m_navigator = nav;

            _completionSession = null;
            _signatureSession = null;

            TextView = textView;
            CompletionBroker = completionBroker;
            SignatureBroker = signatureBroker;
            Aggregator = aggregator;
        }

        private char GetTypeChar(IntPtr pvaIn)
        {
            return (char)(ushort)Marshal.GetObjectForNativeVariant(pvaIn);
        }

        public int Exec(ref Guid pguidCmdGroup, uint nCmdID, uint nCmdexecopt, IntPtr pvaIn, IntPtr pvaOut)
        {
            bool handled = false;
            bool completeAndStart = false;
            int hresult = VSConstants.S_OK;

            // 1. Pre-process
            if (pguidCmdGroup == VSConstants.VSStd2K)
            {
                switch ((VSConstants.VSStd2KCmdID)nCmdID)
                {

                    case VSConstants.VSStd2KCmdID.AUTOCOMPLETE:
                    case VSConstants.VSStd2KCmdID.COMPLETEWORD:
                    case VSConstants.VSStd2KCmdID.SHOWMEMBERLIST:
                        CancelSignatureSession();
                        handled = StartCompletionSession(nCmdID, '\0');
                        break;
                    case VSConstants.VSStd2KCmdID.RETURN:
                        handled = CompleteCompletionSession(false);
                        break;
                    case VSConstants.VSStd2KCmdID.UP:
                    case VSConstants.VSStd2KCmdID.DOWN:
                        //FormatLine(false);
                        break;
                    case VSConstants.VSStd2KCmdID.TAB:
                        handled = CompleteCompletionSession(true);
                        break;
                    case VSConstants.VSStd2KCmdID.CANCEL:
                        handled = CancelCompletionSession();
                        break;
                    case VSConstants.VSStd2KCmdID.PARAMINFO:
                        StartSignatureSession(false);
                        break;
                    case VSConstants.VSStd2KCmdID.BACKSPACE:
                        if (_signatureSession != null)
                        {
                            int pos = TextView.Caret.Position.BufferPosition;
                            if (pos > 0)
                            {
                                // get previous char
                                var previous = TextView.TextBuffer.CurrentSnapshot.GetText().Substring(pos - 1, 1);
                                if (previous == "(" || previous == "{")
                                {
                                    _signatureSession.Dismiss();
                                }
                            }

                        }
                        break;

                    case VSConstants.VSStd2KCmdID.TYPECHAR:
                        char ch = GetTypeChar(pvaIn);
                        if (_completionSession != null)
                        {
                            if (Char.IsLetterOrDigit(ch) || ch == '_')
                                Filter();
                            else
                                CancelCompletionSession();
                        }
                        // Comma starts signature session
                        if (ch == ',')
                        {
                            StartSignatureSession(true);
                        }
                        break;
                }
            }
            else if (pguidCmdGroup == VSConstants.GUID_VSStandardCommandSet97)
            {
                switch ((VSConstants.VSStd97CmdID)nCmdID)
                {
                    case VSConstants.VSStd97CmdID.GotoDefn:
                        GotoDefn();
                        return VSConstants.S_OK;
                }
            }


            if (!handled)
                hresult = Next.Exec(pguidCmdGroup, nCmdID, nCmdexecopt, pvaIn, pvaOut);

            if (ErrorHandler.Succeeded(hresult))
            {
                if (pguidCmdGroup == Microsoft.VisualStudio.VSConstants.VSStd2K)
                {
                    switch ((VSConstants.VSStd2KCmdID)nCmdID)
                    {
                        case VSConstants.VSStd2KCmdID.TYPECHAR:
                            char ch = GetTypeChar(pvaIn);
                            if (_completionSession != null)
                            {
                                if (completeAndStart)
                                {
                                    StartCompletionSession(nCmdID, ch);
                                }
                            }
                            else
                            {
                                switch (ch)
                                {
                                    case ':':
                                    case '.':
                                        CancelSignatureSession();
                                        StartCompletionSession(nCmdID, ch);
                                        break;
                                    case '(':
                                    case '{':
                                        StartSignatureSession(false);
                                        break;
                                    case ')':
                                    case '}':
                                        if (_signatureSession != null)
                                        {
                                            _signatureSession.Dismiss();
                                            _signatureSession = null;
                                        }
                                        break;
                                    default:
                                        break;
                                }
                            }
                            break;
                        case VSConstants.VSStd2KCmdID.BACKSPACE:
                            Filter();
                            break;
#if SMARTINDENT
                        case VSConstants.VSStd2KCmdID.FORMATDOCUMENT:
                            FormatDocument();
                            break;
#endif
                        case VSConstants.VSStd2KCmdID.RETURN:
                            FormatLine(true);
                            break;
                        case VSConstants.VSStd2KCmdID.COMPLETEWORD:
                            break;
                    }
                }
            }

            return hresult;
        }


        /*
         * public static void SetText(this ITextBuffer buffer, params string[] lines)
        {
            var text = String.Join(Environment.NewLine, lines);
            var edit = buffer.CreateEdit(EditOptions.DefaultMinimalChange, 0, null);
            edit.Replace(new Span(0, buffer.CurrentSnapshot.Length), text);
            edit.Apply();
}
         * */


        private void FormatLine(bool previous)
        {
            //
            var package = XSharp.Project.XSharpProjectPackage.Instance;
            _textManager = package.GetTextManager();
            _optionsPage = package.GetIntellisenseOptionsPage();
            getOptions();
            getKeywords();
            //
            SnapshotPoint caret = this.TextView.Caret.Position.BufferPosition;
            var buffer = this.TextView.TextBuffer;
            ITextSnapshotLine line = caret.GetContainingLine();
            // On what line are we ?
            int lineNumber = line.LineNumber;
            int? indentation = null;
            if ((lineNumber > 0) && previous)
            {
                // We need to analyze the Previous line
                //lineNumber = lineNumber - 1;
                //line = line.Snapshot.GetLineFromLineNumber(lineNumber);
                //
                indentation = GetDesiredIndentation(line);
            }
            //
            var editSession = buffer.CreateEdit(); // EditOptions.DefaultMinimalChange, 0, null);
            try
            {
                CommandFilterHelper.FormatLine(this.Aggregator, this.TextView, editSession, line, indentation);
            }
            finally
            {
                editSession.Apply();
            }
        }


#if SMARTINDENT

        private void FormatDocument()
        {
            var buffer = this.TextView.TextBuffer;
            //            int tabSize = this.TextView.Options.GetTabSize();

            // Try to retrieve an already parsed list of Tags
            XSharpClassifier xsClassifier = null;
            if (buffer.Properties.ContainsProperty(typeof(XSharpClassifier)))
            {
                xsClassifier = buffer.Properties[typeof(XSharpClassifier)] as XSharpClassifier;
            }
            //
            if (xsClassifier != null)
            {
                //
                ITextSnapshot snapshot = xsClassifier.Snapshot;
                SnapshotSpan Span = new SnapshotSpan(snapshot, 0, snapshot.Length);
                System.Collections.Immutable.IImmutableList<Microsoft.VisualStudio.Text.Classification.ClassificationSpan> classifications = xsClassifier.GetRegionTags();
                // We cannot use SortedList, because we may have several Classification that start at the same position
                List<Microsoft.VisualStudio.Text.Classification.ClassificationSpan> sortedTags = new List<Microsoft.VisualStudio.Text.Classification.ClassificationSpan>();
                foreach (var tag in classifications)
                {
                    sortedTags.Add(tag);
                }
                sortedTags.Sort((a, b) => a.Span.Start.Position.CompareTo(b.Span.Start.Position));
                //
                Stack<Span> regionStarts = new Stack<Microsoft.VisualStudio.Text.Span>();
                List<Tuple<Span, Span>> regions = new List<Tuple<Microsoft.VisualStudio.Text.Span, Microsoft.VisualStudio.Text.Span>>();
                //
                foreach (var tag in sortedTags)
                {
                    //
                    if (tag.ClassificationType.IsOfType(XSharpColorizer.ColorizerConstants.XSharpRegionStartFormat))
                    {
                        //
                        regionStarts.Push(tag.Span.Span);
                    }
                    else if (tag.ClassificationType.IsOfType(XSharpColorizer.ColorizerConstants.XSharpRegionStopFormat))
                    {
                        if (regionStarts.Count > 0)
                        {
                            var start = regionStarts.Pop();
                            //
                            regions.Add(new Tuple<Span, Span>(start, tag.Span.Span));
                        }
                    }
                }
                //Now, we have a list of Regions Start/Stop
                var editSession = buffer.CreateEdit();
                try
                {
                    var lines = buffer.CurrentSnapshot.Lines;
                    int indentSize = 0;
                    foreach (var snapLine in lines)
                    {
                        indentSize = getDesiredIndentation(snapLine, regions, indentSize);
                        //
                        CommandFilterHelper.FormatLine(this.Aggregator, this.TextView, editSession, snapLine, indentSize);
                    }
                    //
                }
                finally
                {
                    editSession.Apply();
                }
            }
        }

        private int getDesiredIndentation(ITextSnapshotLine snapLine, List<Tuple<Span, Span>> regions, int previousIndentSize)
        {
            var package = XSharp.Project.XSharpProjectPackage.Instance;
            var optionsPage = package.GetIntellisenseOptionsPage();
            bool alignDoCase = optionsPage.AlignDoCase;
            bool alignMethod = optionsPage.AlignMethod;
            //
            // Read the IndentSize (and not the TabSize !!)
            int indentSize = this.TextView.Options.GetIndentSize();
            int tabSize = this.TextView.Options.GetTabSize();
            int indentValue = 0;
            int mlCmtSpaces = 0;
            //
            List<IMappingTagSpan<IClassificationTag>> tags = getTagsInLine(snapLine);
            // In Tuple Regions, the items are :
            // Item1 is Start
            // Item2 is End
            foreach (var region in regions)
            {
                //
                var currentRegionSpan = new SnapshotSpan(snapLine.Snapshot, Span.FromBounds(region.Item1.Start, region.Item2.End));
                var startLine = currentRegionSpan.Start.GetContainingLine();
                var endLine = currentRegionSpan.End.GetContainingLine();
                // The line is inside a region ?
                if ((snapLine.LineNumber >= startLine.LineNumber) && (snapLine.LineNumber <= endLine.LineNumber))
                {
                    //
                    // What kind of region ?
                    String tagType = getFirstKeywordInLine(startLine);
                    // Skip comment and using regions
                    if ((tagType == "//") || (tagType == "USING"))
                    {
                        continue;
                    }
                    if (tagType == "/*")
                    {
                        // Get the current indentation
                        SnapshotSpan sSpan = new SnapshotSpan(snapLine.Start, snapLine.End);
                        String lineText = sSpan.GetText();
                        lineText = lineText.Replace("\t", new String(' ', tabSize));
                        mlCmtSpaces = (lineText.Length - lineText.TrimStart().Length);
                        // What is the difference with the start
                        sSpan = new SnapshotSpan(startLine.Start, startLine.End);
                        lineText = sSpan.GetText();
                        lineText = lineText.Replace("\t", new String(' ', tabSize));
                        mlCmtSpaces = mlCmtSpaces - (lineText.Length - lineText.TrimStart().Length);
                        //
                        continue;
                    }
                    // Region Start
                    if (snapLine.LineNumber == startLine.LineNumber)
                    {
                        // Move back keywords
                        switch (tagType)
                        {
                            case "ELSEIF":
                            case "ELSE":
                            case "CATCH":
                            case "FINALLY":
                                indentValue--;
                                break;
                        }
                        // Some Users wants CASE/OTHERWISE to be aligned to the opening DO CASE
                        // Check for a setting
                        if (alignDoCase)
                        {
                            // Move back keywords
                            switch (tagType)
                            {
                                case "CASE":
                                case "OTHERWISE":
                                    indentValue--;
                                    break;
                            }
                        }
                    }
                    else if (snapLine.LineNumber > startLine.LineNumber)
                    {
                        if (snapLine.LineNumber < endLine.LineNumber)
                        {
                            // Don't indent the inner code
                            switch (tagType)
                            {
                                case "ELSEIF":
                                case "ELSE":
                                case "CATCH":
                                case "FINALLY":
                                    continue;
                            }
                            // Some Users wants CASE/OTHERWISE to be aligned to the opening DO CASE
                            // Check for a setting
                            if (alignDoCase)
                            {
                                // Don't indent
                                switch (tagType)
                                {
                                    case "CASE":
                                    case "OTHERWISE":
                                        continue;
                                }
                            }
                            //
                            if (alignMethod)
                            {
                                // Code block
                                switch (tagType)
                                {
                                    case "FUNCTION":
                                    case "PROCEDURE":
                                    case "METHOD":
                                    case "PROPERTY":
                                    case "ACCESS":
                                    case "ASSIGN":
                                    case "CONSTRUCTOR":
                                    case "DESTRUCTOR":
                                    case "OPERATOR":
                                        continue;
                                }
                            }
                            //
                            indentValue++;
                        }
                        else if (snapLine.LineNumber == endLine.LineNumber)
                        {
                            if (!alignMethod)
                            {
                                // no closing keyword
                                switch (tagType)
                                {
                                    case "FUNCTION":
                                    case "PROCEDURE":
                                    case "METHOD":
                                    case "PROPERTY":
                                    case "ACCESS":
                                    case "ASSIGN":
                                    case "CONSTRUCTOR":
                                    case "DESTRUCTOR":
                                    case "OPERATOR":
                                        indentValue++;
                                        break;
                                }
                            }
                            //
                            if (!alignDoCase)
                            {
                                // Don't indent
                                switch (tagType)
                                {
                                    case "CASE":
                                    case "OTHERWISE":
                                        indentValue++;
                                        break;
                                }
                            }
                        }
                    }
                }
            }
            // This should NOT happen
            if (indentValue < 0)
            {
                indentValue = 0;
            }
            //
            return (indentValue * indentSize) + mlCmtSpaces;
        }


        /// <summary>
        /// Retrieve all Tags in the Line
        /// </summary>
        /// <param name="line"></param>
        /// <returns></returns>
        private List<IMappingTagSpan<IClassificationTag>> getTagsInLine(ITextSnapshotLine line)
        {
            //
            SnapshotSpan lineSpan = new SnapshotSpan(line.Start, line.Length);
            //
            var buffer = this.TextView.TextBuffer;
            var tagAggregator = Aggregator.CreateTagAggregator<IClassificationTag>(buffer);
            var tags = tagAggregator.GetTags(lineSpan);
            List<IMappingTagSpan<IClassificationTag>> tagList = new List<IMappingTagSpan<IClassificationTag>>();
            foreach (var tag in tags)
            {
                tagList.Add(tag);
            }
            return tagList;
        }

        private String getFirstKeywordInLine(ITextSnapshotLine line)
        {
            String keyword = "";
            List<IMappingTagSpan<IClassificationTag>> tagList = getTagsInLine(line);
            //
            if (tagList.Count > 0)
            {
                int tagIndex = 0;
                while (tagIndex < tagList.Count)
                {
                    IClassificationTag currentTag = tagList[tagIndex].Tag;
                    IMappingSpan currentSpan = tagList[tagIndex].Span;
                    var buffer = this.TextView.TextBuffer;
                    //
                    if (currentTag.ClassificationType.IsOfType("keyword"))
                    {
                        var spans = currentSpan.GetSpans(buffer);
                        if (spans.Count > 0)
                        {
                            SnapshotSpan kwSpan = spans[0];
                            keyword = kwSpan.GetText();
                            keyword = keyword.ToUpper();
                            // it could be modifier...
                            switch (keyword)
                            {
                                case "PROTECTED":
                                case "INTERNAL":
                                case "HIDDEN":
                                case "PRIVATE":
                                case "EXPORT":
                                case "PUBLIC":
                                case "STATIC":
                                case "SEALED":
                                case "ABSTRACT":
                                case "VIRTUAL":
                                case "PARTIAL":
                                    tagIndex++;
                                    keyword = "";
                                    continue;
                                default:
                                    break;
                            }
                        }
                    }
                    else if (currentTag.ClassificationType.IsOfType("comment"))
                    {
                        //
                        keyword = "//";
                        var spans = currentSpan.GetSpans(buffer);
                        if (spans.Count > 0)
                        {
                            SnapshotSpan kwSpan = spans[0];
                            keyword = kwSpan.GetText();
                            if (keyword.Length >= 2)
                            {
                                keyword = keyword.Substring(0, 2);
                            }
                        }
                    }
                    // out please
                    break;
                };
            }
            return keyword;
        }
#endif
        private void GotoDefn()
        {
            // First, where are we ?
            int caretPos = this.TextView.Caret.Position.BufferPosition.Position;
            int lineNumber = this.TextView.Caret.Position.BufferPosition.GetContainingLine().LineNumber;
            String currentText = this.TextView.TextBuffer.CurrentSnapshot.GetText();
            XSharpModel.XFile file = this.TextView.TextBuffer.GetFile();
            if (file == null)
                return;
            // Then, the corresponding Type/Element if possible
            IToken stopToken;
            //ITokenStream tokenStream;
            List<String> tokenList = XSharpLanguage.XSharpTokenTools.GetTokenList(caretPos, lineNumber, currentText, out stopToken, true, file);
            // Check if we can get the member where we are
            XSharpModel.XTypeMember member = XSharpLanguage.XSharpTokenTools.FindMember(caretPos, file);
            XSharpModel.XType currentNamespace = XSharpLanguage.XSharpTokenTools.FindNamespace(caretPos, file);
            // LookUp for the BaseType, reading the TokenList (From left to right)
            XSharpLanguage.CompletionElement gotoElement;
            String currentNS = "";
            if (currentNamespace != null)
            {
                currentNS = currentNamespace.Name;
            }
            XSharpModel.CompletionType cType = XSharpLanguage.XSharpTokenTools.RetrieveType(file, tokenList, member, currentNS, stopToken, out gotoElement, currentText);
            //
            if ((gotoElement != null) && (gotoElement.XSharpElement != null))
            {
                // Ok, find it ! Let's go ;)
                gotoElement.XSharpElement.OpenEditor();
            }
            //
        }


#region Completion Session
        private void Filter()
        {
            if (_completionSession == null)
                return;
            _completionSession.SelectedCompletionSet.Filter();
            _completionSession.SelectedCompletionSet.SelectBestMatch();
            //_currentSession.SelectedCompletionSet.Recalculate();
        }

        bool CancelCompletionSession()
        {
            if (_completionSession == null)
                return false;

            _completionSession.Dismiss();

            return true;
        }

        bool CompleteCompletionSession(bool force)
        {
            if (_completionSession == null)
                return false;

            if (!_completionSession.SelectedCompletionSet.SelectionStatus.IsSelected && !force)
            {
                _completionSession.Dismiss();
                return false;
            }
            else
            {
                //
                _completionSession.Commit();
                return true;
            }
        }

        bool StartCompletionSession(uint nCmdId, char typedChar)
        {
            if (_completionSession != null)
                return false;

            SnapshotPoint caret = TextView.Caret.Position.BufferPosition;
            ITextSnapshot snapshot = caret.Snapshot;

            if (!CompletionBroker.IsCompletionActive(TextView))
            {
                _completionSession = CompletionBroker.CreateCompletionSession(TextView, snapshot.CreateTrackingPoint(caret, PointTrackingMode.Positive), true);
            }
            else
            {
                _completionSession = CompletionBroker.GetSessions(TextView)[0];
            }

            _completionSession.Dismissed += OnCompletionSessionDismiss;
            _completionSession.Committed += OnCompletionSessionCommitted;

            _completionSession.Properties["Command"] = nCmdId;
            _completionSession.Properties["Char"] = typedChar;
            _completionSession.Properties["Type"] = null;
            try
            {
                _completionSession.Start();
            }
            catch (Exception e)
            {
                Support.Debug("Startcompletion failed:" + e.Message);
            }
            return true;
        }

        private void OnCompletionSessionCommitted(object sender, EventArgs e)
        {
            // it MUST be the case....
            if (_completionSession.SelectedCompletionSet.SelectionStatus.Completion != null)
            {
                if (_completionSession.SelectedCompletionSet.SelectionStatus.Completion.InsertionText.EndsWith("("))
                {
                    XSharpModel.CompletionType cType = null;
                    if (_completionSession.Properties["Type"] != null)
                    {
                        cType = (XSharpModel.CompletionType)_completionSession.Properties["Type"];
                    }
                    string method = _completionSession.SelectedCompletionSet.SelectionStatus.Completion.InsertionText;
                    method = method.Substring(0, method.Length - 1);
                    StartSignatureSession(false, cType, method);
                }
            }
            //
        }

        private void OnCompletionSessionDismiss(object sender, EventArgs e)
        {
            _completionSession = null;
        }
#endregion


#region Signature Session
        bool StartSignatureSession(bool comma, XSharpModel.CompletionType cType = null, string methodName = null)
        {
            if (_signatureSession != null)
                return false;
            int startLineNumber = this.TextView.Caret.Position.BufferPosition.GetContainingLine().LineNumber;
            SnapshotPoint ssp = this.TextView.Caret.Position.BufferPosition;
            // when coming from the completionlist then there is no need to check a lot of stuff
            // we can then simply lookup the method and that is it.
            // Also no need to filter on visibility since that has been done in the completionlist already !
            XSharpLanguage.CompletionElement gotoElement = null;
            if (cType != null && methodName != null)
            {
                cType = XSharpLanguage.XSharpTokenTools.SearchMethodTypeIn(cType, methodName, XSharpModel.Modifiers.Private, false, out gotoElement);

            }
            else
            {
                // First, where are we ?
                int caretPos;
                int lineNumber = startLineNumber;
                //
                do
                {
                    if (ssp.Position == 0)
                        break;
                    ssp = ssp - 1;
                    char leftCh = ssp.GetChar();
                    if ((leftCh == '(') || (leftCh == '{'))
                        break;
                    lineNumber = ssp.GetContainingLine().LineNumber;
                } while (startLineNumber == lineNumber);
                //
                caretPos = ssp.Position;
                String currentText = this.TextView.TextBuffer.CurrentSnapshot.GetText();
                XSharpModel.XFile file = this.TextView.TextBuffer.GetFile();
                if (file == null)
                    return false;
                // Then, the corresponding Type/Element if possible
                IToken stopToken;
                //ITokenStream tokenStream;
                List<String> tokenList = XSharpLanguage.XSharpTokenTools.GetTokenList(caretPos, lineNumber, currentText, out stopToken, true, file);
                // Check if we can get the member where we are
                XSharpModel.XTypeMember member = XSharpLanguage.XSharpTokenTools.FindMember(caretPos, file);
                XSharpModel.XType currentNamespace = XSharpLanguage.XSharpTokenTools.FindNamespace(caretPos, file);
                // LookUp for the BaseType, reading the TokenList (From left to right)
                String currentNS = "";
                if (currentNamespace != null)
                {
                    currentNS = currentNamespace.Name;
                }
                cType = XSharpLanguage.XSharpTokenTools.RetrieveType(file, tokenList, member, currentNS, stopToken, out gotoElement, currentText);
            }
            //
            if ((gotoElement != null) && (gotoElement.IsInitialized))
            {
                // Not sure that this if() is still necessary ...
                if ((gotoElement.XSharpElement != null) && (gotoElement.XSharpElement.Kind == XSharpModel.Kind.Class))
                {
                    XSharpModel.XType xType = gotoElement.XSharpElement as XSharpModel.XType;
                    if (xType != null)
                    {
                        foreach (XSharpModel.XTypeMember mbr in xType.Members)
                        {
                            if (String.Compare(mbr.Name, "constructor", true) == 0)
                            {
                                gotoElement = new XSharpLanguage.CompletionElement(mbr);
                                break;
                            }
                        }
                    }
                }

                SnapshotPoint caret = TextView.Caret.Position.BufferPosition;
                ITextSnapshot snapshot = caret.Snapshot;
                //
                if (!SignatureBroker.IsSignatureHelpActive(TextView))
                {
                    _signatureSession = SignatureBroker.CreateSignatureHelpSession(TextView, snapshot.CreateTrackingPoint(caret, PointTrackingMode.Positive), true);
                }
                else
                {
                    _signatureSession = SignatureBroker.GetSessions(TextView)[0];
                }

                _signatureSession.Dismissed += OnSignatureSessionDismiss;
                if (gotoElement.XSharpElement != null)
                {
                    _signatureSession.Properties["Element"] = gotoElement.XSharpElement;
                }
                else if (gotoElement.SystemElement != null)
                {
                    _signatureSession.Properties["Element"] = gotoElement.SystemElement;
                }
                else if (gotoElement.CodeElement != null)
                {
                    _signatureSession.Properties["Element"] = gotoElement.CodeElement;
                }
                _signatureSession.Properties["Line"] = startLineNumber;
                _signatureSession.Properties["Start"] = ssp.Position;
                _signatureSession.Properties["Length"] = TextView.Caret.Position.BufferPosition.Position - ssp.Position;
                _signatureSession.Properties["Comma"] = comma;

                try
                {
                    _signatureSession.Start();
                }
                catch (Exception e)
                {
                    Support.Debug("Start Signature session failed:" + e.Message);
                }
            }
            //
            return true;
        }

        bool CancelSignatureSession()
        {
            if (_signatureSession == null)
                return false;

            _signatureSession.Dismiss();
            return true;
        }

        private void OnSignatureSessionDismiss(object sender, EventArgs e)
        {
            _signatureSession = null;
        }
#endregion

        public int QueryStatus(ref Guid pguidCmdGroup, uint cCmds, OLECMD[] prgCmds, IntPtr pCmdText)
        {
            if (pguidCmdGroup == VSConstants.VSStd2K)
            {
                switch ((VSConstants.VSStd2KCmdID)prgCmds[0].cmdID)
                {
                    case VSConstants.VSStd2KCmdID.AUTOCOMPLETE:
                    case VSConstants.VSStd2KCmdID.COMPLETEWORD:
                        prgCmds[0].cmdf = (uint)OLECMDF.OLECMDF_ENABLED | (uint)OLECMDF.OLECMDF_SUPPORTED;
                        return VSConstants.S_OK;
                }
            }
            else if (pguidCmdGroup == VSConstants.GUID_VSStandardCommandSet97)
            {
                switch ((VSConstants.VSStd97CmdID)prgCmds[0].cmdID)
                {
                    case VSConstants.VSStd97CmdID.GotoDefn:
                        prgCmds[0].cmdf = (uint)OLECMDF.OLECMDF_ENABLED | (uint)OLECMDF.OLECMDF_SUPPORTED;
                        return VSConstants.S_OK;
                }
            }
            return Next.QueryStatus(pguidCmdGroup, cCmds, prgCmds, pCmdText);
        }


        #region SmartIndent
        // SmartIndent
        private int _lastIndentValue;
        private int _tabSize;
        private bool _alignDoCase;
        private bool _alignMethod;
        private vsIndentStyle _indentStyle;
        //private IEditorOptions _options;
        private OptionsPages.IntellisenseOptionsPage _optionsPage;
        IVsTextManager4 _textManager;
        private String[] _indentKeywords;
        private String[] _codeBlockKeywords;
        private String[][] _middleKeywords;
        private String[][] _specialKeywords;
        private String[][] _specialOutdentKeywords;


        private void getOptions()
        {
            _alignDoCase = _optionsPage.AlignDoCase;
            _alignMethod = _optionsPage.AlignMethod;
            var languagePreferences = new LANGPREFERENCES3[1];
            languagePreferences[0].guidLang = GuidStrings.guidLanguageService;
            var result = _textManager.GetUserPreferences4(pViewPrefs: null, pLangPrefs: languagePreferences, pColorPrefs: null);
            if (result == VSConstants.S_OK)
            {
                _indentStyle = languagePreferences[0].IndentStyle;
                _tabSize = (int)languagePreferences[0].uTabSize;
            }
        }

        #region Keywords Definitions
        private void getKeywords()
        {
            // Build list for Indent tokens
            _indentKeywords = this.getIndentKeywords();
            // Start of Method, Function, ...
            _codeBlockKeywords = this.getStartOfCodeKeywords();
            // Middle Keywords : ELSE, ELSEIF, ...
            _middleKeywords = this.getMiddleKeywords();
            // Name is Self-explanatory
            _specialKeywords = this.getSpecialMiddleKeywords();
            // Build list for Outdent tokens
            _specialOutdentKeywords = this.getSpecialOutdentKeywords();
        }


        private String[] getIndentKeywords()
        {
            // "DO" is removed by getFirstKeywordInLine(), so it is useless here...
            return new String[]{
                "DO","FOR","FOREACH","WHILE","IF",
                "BEGIN","TRY","REPEAT",
                "INTERFACE","ENUM","CLASS","STRUCTURE","VOSTRUCT"};
        }

        private String[][] getSpecialOutdentKeywords()
        {
            // These are keywords that we have between other keywords
            // "CASE" is the keyword that will trigger the process
            // "DO,SWITCH,BEGIN" is the list of possible start keyword
            // ...
            return new String[][]
            {
                new String[]{ "ENDIF","IF" },
                new String[]{ "ENDCASE", "DO" },
                new String[]{ "NEXT", "FOR,FOREACH" },
                new String[]{ "UNTIL", "REPEAT" },
                new String[]{ "END", "BEGIN,DO,IF,TRY,WHILE" },
                new String[]{ "ENDDO", "DO,WHILE" }
            };
        }

        private String[] getStartOfCodeKeywords()
        {
            // 
            return new String[]{
                "FUNCTION","PROCEDURE","CONSTRUCTOR","DESTRUCTOR","PROPERTY",
                "ACCESS","ASSIGN","METHOD","OPERATOR","GET","SET" };
        }

        private String[][] getMiddleKeywords()
        {
            // These are keywords that we have between other keywords
            //
            // "ELSE" is the keyword that will trigger the process
            // "IF" is the keyword to align to
            // ...
            return new String[][]
            {
                new String[]{ "ELSE","IF" },
                new String[]{ "ELSEIF", "IF" },
                new String[]{ "FINALLY", "TRY" },
                new String[]{ "CATCH", "TRY" },
                new String[]{ "RECOVER", "BEGIN" }
            };
        }

        private String[][] getSpecialMiddleKeywords()
        {
            // These are keywords that we have between other keywords
            // "CASE" is the keyword that will trigger the process
            // "DO,SWITCH,BEGIN" is the list of possible start keyword
            // ...
            return new String[][]
            {
                new String[]{ "CASE","DO,SWITCH,BEGIN" },
                new String[]{ "OTHERWISE", "DO,SWITCH,BEGIN" }
            };
        }
        #endregion

        private void Options_OptionChanged(object sender, EditorOptionChangedEventArgs e)
        {
            getOptions();

        }

        private int? GetDesiredIndentation(ITextSnapshotLine line)
        {
            try
            {
                // Update Please... as the event doesn't seems to work
                getOptions();
                //
                if (_indentStyle != vsIndentStyle.vsIndentStyleSmart)
                    return null;
                // How many spaces do we need ?
                int indentValue = 0;
                string outdentToken;
                // On what line are we ?
                int lineNumber = line.LineNumber;
                if (lineNumber > 0)
                {
                    // We need to analyze the Previous line
                    lineNumber = lineNumber - 1;
                    ITextSnapshotLine prevLine = line.Snapshot.GetLineFromLineNumber(lineNumber);
                    bool doSkipped;
                    string keyword = getFirstKeywordInLine(prevLine, out doSkipped, out indentValue);
                    //
                    if (indentValue > -1)
                        _lastIndentValue = indentValue;
                    // ok, now check what we have, starting the previous line
                    if (!String.IsNullOrEmpty(keyword) && !doSkipped)
                    {
                        // Start of a block of code ?
                        if (_codeBlockKeywords.Contains<String>(keyword))
                        {
                            if (!_alignMethod)
                            {
                                indentValue += _tabSize;
                            }
                        }
                        else if (_indentKeywords.Contains<String>(keyword))
                        {
                            //
                            indentValue += _tabSize;
                        }
                        else if ((outdentToken = this.searchSpecialOutdentKeyword(keyword)) != null)
                        {
                            if (this.hasRegions())
                            {
                                // We are aligning on the Open Token
                                indentValue = alignToOpenToken(prevLine);
                                if (indentValue < 0)
                                    indentValue = 0;
                            }
                            else
                            {
                                // Ok, let's try to make it smooth...
                                int? specialOutdentValue = null;
                                // The startToken is a list of possible tokens
                                specialOutdentValue = alignToSpecificTokens(line, outdentToken);
                                if (specialOutdentValue != null)
                                {
                                    indentValue = (int)specialOutdentValue;
                                }
                            }
                            // De-Indent previous line !!!
                            var buffer = this.TextView.TextBuffer;
                            var editSession = buffer.CreateEdit();
                            try
                            {
                                XSharp.Project.CommandFilterHelper.FormatLine(this.Aggregator, this.TextView, editSession, prevLine, indentValue);
                            }
                            finally
                            {
                                editSession.Apply();
                            }
                        }
                        else
                        {
                            string startToken = this.searchMiddleKeyword(keyword);
                            int? specialIndentValue = null;
                            if (startToken != null)
                            {
                                // Retrieve the Indentation for the previous line
                                specialIndentValue = alignToSpecificTokens(line, startToken);
                            }
                            else
                            {
                                // We could have "special" middle keyword : CASE or OTHERWISE
                                startToken = this.searchSpecialMiddleKeyword(keyword);
                                if (startToken != null)
                                {
                                    // The startToken is a list of possible tokens
                                    specialIndentValue = alignToSpecificTokens(line, startToken);
                                    // The can be aligned to SWITCH/DO CASE or indented
                                    if (!_alignDoCase)
                                    {
                                        specialIndentValue += _tabSize;
                                    }
                                }
                            }
                            if (specialIndentValue != null)
                            {
                                // and Indent the new line
                                indentValue = (int)specialIndentValue + _tabSize;
                                // And apply
                                // De-Indent previous line !!!
                                var buffer = this.TextView.TextBuffer;
                                var editSession = buffer.CreateEdit();
                                try
                                {
                                    XSharp.Project.CommandFilterHelper.FormatLine(this.Aggregator, this.TextView, editSession, prevLine, specialIndentValue);
                                }
                                finally
                                {
                                    editSession.Apply();
                                }
                            }
                        }
                        if (indentValue < 0)
                            indentValue = 0;
                        //
                        _lastIndentValue = indentValue;
                    }
                    //
                    return _lastIndentValue;
                }
            }
            catch (Exception ex)
            {
                Trace.WriteLine("SmartIndent.GetDesiredIndentation Exception : " + ex.Message);
            }
            return _lastIndentValue;
        }

        private string searchMiddleKeyword(string keyword)
        {
            string startToken = null;
            for (int i = 0; i < this._middleKeywords.Length; i++)
            {
                var pair = this._middleKeywords[i];
                if (String.Compare(keyword, pair[0], true) == 0)
                {
                    startToken = pair[1];
                    break;
                }
            }
            return startToken;
        }

        private string searchSpecialMiddleKeyword(string keyword)
        {
            string startToken = null;
            for (int i = 0; i < this._specialKeywords.Length; i++)
            {
                var pair = this._specialKeywords[i];
                if (String.Compare(keyword, pair[0], true) == 0)
                {
                    startToken = pair[1];
                    break;
                }
            }
            return startToken;
        }

        private string searchSpecialOutdentKeyword(string keyword)
        {
            string startToken = null;
            for (int i = 0; i < this._specialOutdentKeywords.Length; i++)
            {
                var pair = this._specialOutdentKeywords[i];
                if (String.Compare(keyword, pair[0], true) == 0)
                {
                    startToken = pair[1];
                    break;
                }
            }
            return startToken;
        }

        private int? alignToSpecificToken_(ITextSnapshotLine currentLine, String token)
        {
            int indentValue = 0;
            bool found = false;
            try
            {
                // On what line are we ?
                int lineNumber = currentLine.LineNumber;
                // We need to analyze the Previous line
                lineNumber = lineNumber - 1;
                while (lineNumber > 0)
                {
                    // We need to analyze the Previous line
                    lineNumber = lineNumber - 1;
                    ITextSnapshotLine line = currentLine.Snapshot.GetLineFromLineNumber(lineNumber);
                    List<IMappingTagSpan<IClassificationTag>> tagList = getTagsInLine(line);
                    String keyword = "";
                    //
                    if (tagList.Count > 0)
                    {
                        var buffer = this.TextView.TextBuffer;
                        IMappingSpan currentSpan = tagList[0].Span;
                        /*
                        SnapshotPoint? snapPointFirst = currentSpan.Start.GetPoint(buffer, PositionAffinity.Predecessor);
                        // Extract the start of line
                        SnapshotSpan toIndent = new SnapshotSpan(line.Start, snapPointFirst.Value.Position - line.Start.Position);
                        String startOfLine = toIndent.GetText();
                        // Convert Tabs to Spaces
                        startOfLine = startOfLine.Replace("\t", new String(' ', _tabSize));
                        // So, at least, to align to previous line, we will need...
                        indentValue = startOfLine.Length;
                        */
                        String startOfLine = line.GetText();
                        startOfLine = startOfLine.Replace("\t", new String(' ', _tabSize));
                        // So, at least, to align to previous line, we will need...
                        indentValue = (startOfLine.Length - startOfLine.TrimStart(' ').Length);
                        //
                        IClassificationTag currentTag = tagList[0].Tag;
                        currentSpan = tagList[0].Span;
                        //
                        if (currentTag.ClassificationType.IsOfType("keyword"))
                        {
                            var spans = currentSpan.GetSpans(buffer);
                            if (spans.Count > 0)
                            {
                                SnapshotSpan kwSpan = spans[0];
                                keyword = kwSpan.GetText();
                                keyword = keyword.ToUpper();
                                if (keyword == token)
                                {
                                    found = true;
                                    break;
                                }
                            }
                        }
                        // 
                        indentValue = 0;
                    }
                }

            }
            finally
            {
                //
            }
            //
            if (found)
                return indentValue;
            else
                return null;

        }

        private int? alignToSpecificTokens(ITextSnapshotLine currentLine, String tokenList)
        {
            int indentValue = 0;
            bool found = false;
            Stack<String> context = new Stack<String>();
            //
            try
            {
                String[] possibleTokens = tokenList.Split(',');
                // On what line are we ?
                int lineNumber = currentLine.LineNumber;
                // We need to analyze the Previous line
                lineNumber = lineNumber - 1;
                while (lineNumber > 0)
                {
                    // We need to analyze the Previous line
                    lineNumber = lineNumber - 1;
                    ITextSnapshotLine line = currentLine.Snapshot.GetLineFromLineNumber(lineNumber);
                    List<IMappingTagSpan<IClassificationTag>> tagList = getTagsInLine(line);
                    String currentKeyword = "";
                    //
                    if (tagList.Count > 0)
                    {
                        var buffer = this.TextView.TextBuffer;
                        IMappingSpan currentSpan = tagList[0].Span;
                        /*
                        SnapshotPoint? snapPointFirst = currentSpan.Start.GetPoint(buffer, PositionAffinity.Predecessor);
                        // Extract the start of line
                        SnapshotSpan toIndent = new SnapshotSpan(line.Start, snapPointFirst.Value.Position - line.Start.Position);
                        String startOfLine = toIndent.GetText();
                        // Convert Tabs to Spaces
                        startOfLine = startOfLine.Replace("\t", new String(' ', _tabSize));
                        // So, at least, to align to previous line, we will need...
                        indentValue = startOfLine.Length;
                        */
                        String startOfLine = line.GetText();
                        startOfLine = startOfLine.Replace("\t", new String(' ', _tabSize));
                        // So, at least, to align to previous line, we will need...
                        indentValue = (startOfLine.Length - startOfLine.TrimStart(' ').Length);
                        //
                        IClassificationTag currentTag = tagList[0].Tag;
                        currentSpan = tagList[0].Span;
                        //
                        if (currentTag.ClassificationType.IsOfType("keyword"))
                        {
                            var spans = currentSpan.GetSpans(buffer);
                            if (spans.Count > 0)
                            {
                                SnapshotSpan kwSpan = spans[0];
                                currentKeyword = kwSpan.GetText();
                                currentKeyword = currentKeyword.ToUpper();
                                if (possibleTokens.Contains<String>(currentKeyword))
                                {
                                    if (context.Count == 0)
                                    {
                                        found = true;
                                        break;
                                    }
                                    else
                                    {
                                        tokenList = context.Pop();
                                        possibleTokens = tokenList.Split(',');
                                    }
                                }
                                // Here we should also check for nested construct or we might get false positive...
                                string outdentToken;
                                if ((outdentToken = this.searchSpecialOutdentKeyword(currentKeyword)) != null)
                                {
                                    context.Push(tokenList);
                                    tokenList = outdentToken;
                                    possibleTokens = tokenList.Split(',');
                                }
                            }
                        }
                        // 
                        indentValue = 0;
                    }
                }
            }
            finally
            {
                //
            }
            //
            if (found)
                return indentValue;
            else
                return null;

        }

        private String getKeywordAt(List<IMappingTagSpan<IClassificationTag>> tagList, int tagIndex)
        {
            string keyword = null;
            if (tagIndex < tagList.Count)
            {
                var buffer = TextView.TextBuffer;
                IClassificationTag currentTag = tagList[tagIndex].Tag;
                IMappingSpan currentSpan = tagList[tagIndex].Span;
                //
                if (currentTag.ClassificationType.IsOfType("keyword"))
                {
                    var spans = currentSpan.GetSpans(buffer);
                    if (spans.Count > 0)
                    {
                        SnapshotSpan kwSpan = spans[0];
                        keyword = kwSpan.GetText();
                        keyword = keyword.ToUpper();
                    }
                }
            }
            return keyword;
        }


        private bool hasRegions()
        {
            bool result = false;
            //
            var buffer = TextView.TextBuffer;
            // Try to retrieve an already parsed list of Tags
            XSharpClassifier xsClassifier = null;
            if (buffer.Properties.ContainsProperty(typeof(XSharpClassifier)))
            {
                xsClassifier = buffer.Properties[typeof(XSharpClassifier)] as XSharpClassifier;
            }
            if (xsClassifier != null)
            {
                System.Collections.Immutable.IImmutableList<Microsoft.VisualStudio.Text.Classification.ClassificationSpan> classifications = xsClassifier.GetRegionTags();
                result = (classifications.Count > 0);
            }
            return result;
        }

        private int alignToOpenToken(ITextSnapshotLine currentLine)
        {
            int indentValue = 0;
            try
            {
                int lineNumber = currentLine.LineNumber;
                //
                var buffer = TextView.TextBuffer;
                // Try to retrieve an already parsed list of Tags
                XSharpClassifier xsClassifier = null;
                if (buffer.Properties.ContainsProperty(typeof(XSharpClassifier)))
                {
                    xsClassifier = buffer.Properties[typeof(XSharpClassifier)] as XSharpClassifier;
                }

                if (xsClassifier != null)
                {
                    //
                    ITextSnapshot snapshot = xsClassifier.Snapshot;
                    SnapshotSpan Span = new SnapshotSpan(snapshot, 0, snapshot.Length);
                    System.Collections.Immutable.IImmutableList<Microsoft.VisualStudio.Text.Classification.ClassificationSpan> classifications = xsClassifier.GetRegionTags();
                    // We cannot use SortedList, because we may have several Classification that start at the same position
                    List<Microsoft.VisualStudio.Text.Classification.ClassificationSpan> sortedTags = new List<Microsoft.VisualStudio.Text.Classification.ClassificationSpan>();
                    foreach (var tag in classifications)
                    {
                        sortedTags.Add(tag);
                    }
                    sortedTags.Sort((a, b) => a.Span.Start.Position.CompareTo(b.Span.Start.Position));
                    //
                    Stack<Microsoft.VisualStudio.Text.Classification.ClassificationSpan> startStack = new Stack<Microsoft.VisualStudio.Text.Classification.ClassificationSpan>();
                    foreach (var tag in sortedTags)
                    {
                        // Is it a Region ?
                        if (tag.ClassificationType.IsOfType(ColorizerConstants.XSharpRegionStartFormat))
                        {
                            startStack.Push(tag);
                        }
                        else if (tag.ClassificationType.IsOfType(ColorizerConstants.XSharpRegionStopFormat))
                        {
                            //
                            var startTag = startStack.Pop();
                            var startLine = startTag.Span.Start.GetContainingLine();
                            // Looking for an End

                            var endLine = tag.Span.End.GetContainingLine();
                            if (endLine.LineNumber == lineNumber)
                            {
                                // Where is the start ?
                                SnapshotSpan sSpan = new SnapshotSpan(startLine.Start, startLine.End);
                                String lineText = sSpan.GetText();
                                lineText = lineText.Replace("\t", new String(' ', _tabSize));
                                // 
                                indentValue = (lineText.Length - lineText.TrimStart().Length);
                                break;
                            }
                        }
                    }
                }
            }
            finally
            {

            }
            //
            return indentValue;
        }


        /// <summary>
        /// Get the first keyword in Line. The modifiers (Private, Protected, ... ) are ignored
        /// If the first Keyword is a Comment, "//" is returned
        /// </summary>
        /// <param name="line">The line to analyze</param>
        /// <param name="doSkipped">Bool value indicating if a "DO" keyword has been skipped</param>
        /// <param name="minIndent"></param>
        /// <returns></returns>
        private String getFirstKeywordInLine(ITextSnapshotLine line, out bool doSkipped, out int minIndent)
        {
            minIndent = -1;
            doSkipped = false;
            List<IMappingTagSpan<IClassificationTag>> tagList = getTagsInLine(line);
            String keyword = "";
            //
            if (tagList.Count > 0)
            {
                var buffer = this.TextView.TextBuffer;
                IMappingSpan currentSpan = tagList[0].Span;
                /////////////////////////////////////////// 
                //SnapshotPoint? snapPointFirst = currentSpan.Start.GetPoint(buffer, PositionAffinity.Predecessor);
                //// Extract the start of line
                //SnapshotSpan toIndent = new SnapshotSpan(line.Start, snapPointFirst.Value.Position - line.Start.Position);
                //String startOfLine = toIndent.GetText();
                //// Convert Tabs to Spaces
                //startOfLine = startOfLine.Replace("\t", new String(' ', _tabSize));
                //// So, at least, to align to previous line, we will need...
                //minIndent = startOfLine.Length;
                ////////////////////////////////////////////
                String startOfLine = line.GetText();
                startOfLine = startOfLine.Replace("\t", new String(' ', _tabSize));
                // So, at least, to align to previous line, we will need...
                minIndent = (startOfLine.Length - startOfLine.TrimStart(' ').Length);
                //
                int tagIndex = 0;
                while (tagIndex < tagList.Count)
                {
                    IClassificationTag currentTag = tagList[tagIndex].Tag;
                    currentSpan = tagList[tagIndex].Span;
                    //
                    if (currentTag.ClassificationType.IsOfType("keyword"))
                    {
                        var spans = currentSpan.GetSpans(buffer);
                        if (spans.Count > 0)
                        {
                            SnapshotSpan kwSpan = spans[0];
                            keyword = kwSpan.GetText();
                            keyword = keyword.ToUpper();
                            // it could be modifier...
                            switch (keyword)
                            {
                                case "PROTECTED":
                                case "INTERNAL":
                                case "HIDDEN":
                                case "PRIVATE":
                                case "EXPORT":
                                case "PUBLIC":
                                case "STATIC":
                                case "SEALED":
                                case "ABSTRACT":
                                case "VIRTUAL":
                                case "PARTIAL":
                                    tagIndex++;
                                    keyword = "";
                                    continue;
                                case "DO":
                                    tagIndex++;
                                    keyword = "";
                                    doSkipped = true;
                                    continue;
                                default:
                                    break;
                            }
                        }
                    }
                    else if (currentTag.ClassificationType.IsOfType("comment"))
                    {
                        //
                        keyword = "//";
                    }
                    // out please
                    break;
                };
            }
            return keyword;
        }



        #endregion


    }

    static class CommandFilterHelper
    {
        /// <summary>
        /// Format the Keywords and Identifiers in the Line, using the EditSession
        /// </summary>
        /// <param name="editSession"></param>
        /// <param name="line"></param>
        static public void FormatLine(IBufferTagAggregatorFactoryService Aggregator, ITextView TextView, ITextEdit editSession, ITextSnapshotLine line, int? desiredIndentation)
        {
            //
            var package = XSharp.Project.XSharpProjectPackage.Instance;
            var optionsPage = package.GetIntellisenseOptionsPage();
            int kwCase = optionsPage.KeywordCase;
            bool syncIdentifier = optionsPage.IdentifierCase;
            //
            SnapshotSpan lineSpan = new SnapshotSpan(line.Start, line.Length);
            //
            SnapshotPoint caret = TextView.Caret.Position.BufferPosition;
            var buffer = TextView.TextBuffer;
            var tagAggregator = Aggregator.CreateTagAggregator<IClassificationTag>(buffer);
            var tags = tagAggregator.GetTags(lineSpan);
            List<IMappingTagSpan<IClassificationTag>> tagList = new List<IMappingTagSpan<IClassificationTag>>();
            foreach (var tag in tags)
            {
                tagList.Add(tag);
            }
            // Keyword and Identifier Case
            foreach (var tag in tagList)
            {
                var name = tag.Tag.ClassificationType.Classification.ToLower();
                //
                if (name == "keyword")
                {
                    var spans = tag.Span.GetSpans(buffer);
                    if (spans.Count > 0)
                    {
                        SnapshotSpan kwSpan = spans[0];
                        string keyword = kwSpan.GetText();
                        string transform = null;
                        //
                        switch (kwCase)
                        {
                            case 1:
                                transform = keyword.ToUpper();
                                break;
                            case 2:
                                transform = keyword.ToLower();
                                break;
                            case 3:
                                System.Globalization.CultureInfo culture = new System.Globalization.CultureInfo("en-US", false);
                                System.Globalization.TextInfo txtInfo = culture.TextInfo;
                                transform = txtInfo.ToTitleCase(keyword.ToLower());
                                break;
                        }
                        // Not none, and the tranfsform is not the same as the original
                        if ((kwCase != 0) && (String.Compare(transform, keyword) != 0))
                            editSession.Replace(kwSpan, transform);
                    }
                }
                else if ((name == "identifier") && syncIdentifier)
                {
                    var spans = tag.Span.GetSpans(buffer);
                    if (spans.Count > 0)
                    {
                        SnapshotSpan idSpan = spans[0];
                        string identifier = idSpan.GetText();
                        //
                        XFile _file = buffer.GetFile();
                        XTypeMember currentMember = XSharpLanguage.XSharpTokenTools.FindMember(caret.Position, _file);
                        //
                        if (currentMember == null)
                            continue;
                        CompletionType cType = null;
                        XSharpLanguage.CompletionElement foundElement = null;
                        XVariable element = null;
                        // Search in Parameters
                        if (currentMember.Parameters != null)
                            element = currentMember.Parameters.Find(x => XSharpLanguage.XSharpTokenTools.StringEquals(x.Name, identifier));
                        if (element == null)
                        {
                            // then Locals
                            if (currentMember.Locals != null)
                                element = currentMember.Locals.Find(x => XSharpLanguage.XSharpTokenTools.StringEquals(x.Name, identifier));
                            if (element == null)
                            {
                                if (currentMember.Parent != null)
                                {
                                    // Context Type....
                                    cType = new CompletionType(currentMember.Parent.Clone);
                                    // We can have a Property/Field of the current CompletionType
                                    if (!cType.IsEmpty())
                                    {
                                        cType = XSharpLanguage.XSharpTokenTools.SearchPropertyOrFieldIn(cType, identifier, Modifiers.Private, out foundElement);
                                    }
                                    // Not found ? It might be a Global !?
                                    if (foundElement == null)
                                    {

                                    }
                                }
                            }
                        }
                        if (element != null)
                        {
                            cType = new CompletionType((XVariable)element, "");
                            foundElement = new XSharpLanguage.CompletionElement(element);
                        }
                        // got it !
                        if (foundElement != null)
                        {
                            if ((String.Compare(foundElement.Name, identifier) != 0))
                                editSession.Replace(idSpan, foundElement.Name);
                        }
                    }
                }
            }
            // Indentation
            if (desiredIndentation != null)
            {
                if (desiredIndentation >= 0)
                {
                    int tabSize = TextView.Options.GetTabSize();
                    bool useSpaces = TextView.Options.IsConvertTabsToSpacesEnabled();
                    String lineText = line.GetText();
                    int lineLength = line.Length;
                    String newText = lineText.TrimStart();
                    int newLength = newText.Length;
                    if (lineLength >= newLength)
                    {
                        //
                        Span indentSpan = new Span(line.Start.Position, lineLength - newLength);
                        String indentSpaces = "";
                        if (useSpaces)
                            indentSpaces = new String(' ', (int)desiredIndentation);
                        else
                            indentSpaces = new String('\t', (int)desiredIndentation / tabSize) + new String(' ', (int)desiredIndentation % tabSize);
                        editSession.Replace(indentSpan, indentSpaces);
                    }
                }
            }
        }

    }


}
