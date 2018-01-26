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
    internal sealed partial class CommandFilter : IOleCommandTarget
    {
        public ITextView TextView { get; private set; }
        public IOleCommandTarget Next { get; set; }

        ICompletionBroker _completionBroker;
        ICompletionSession _completionSession;


        ISignatureHelpBroker _signatureBroker;
        ISignatureHelpSession _signatureSession;

        ITextStructureNavigator m_navigator;
        IBufferTagAggregatorFactoryService _aggregator;




        public CommandFilter(IWpfTextView textView, ICompletionBroker completionBroker, ITextStructureNavigator nav, ISignatureHelpBroker signatureBroker, IBufferTagAggregatorFactoryService aggregator)
        {
            m_navigator = nav;

            _completionSession = null;
            _signatureSession = null;

            TextView = textView;
            _completionBroker = completionBroker;
            _signatureBroker = signatureBroker;
            _aggregator = aggregator;
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
                        //char ch = GetTypeChar(pvaIn);
                        //if (_completionSession != null)
                        //{
                        //    if (Char.IsLetterOrDigit(ch) || ch == '_')
                        //        Filter();
                        //    else
                        //        CancelCompletionSession();
                        //}
                        //// Comma starts signature session
                        //if (ch == ',')
                        //{
                        //    StartSignatureSession(true);
                        //}
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
                                if (Char.IsLetterOrDigit(ch) || ch == '_')
                                    Filter();
                                else
                                    CancelCompletionSession();
                                //
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
                                    case ',':
                                        StartSignatureSession(true);
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


        private void GotoDefn()
        {
            try
            {
                XSharpModel.ModelWalker.Suspend();
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
                bool done = false;
                XSharpModel.CompletionType cType = XSharpLanguage.XSharpTokenTools.RetrieveType(file, tokenList, member, currentNS, stopToken, out gotoElement, currentText);
                //
                if ((gotoElement != null) && (gotoElement.XSharpElement != null))
                {
                    // Ok, find it ! Let's go ;)
                    done = true;
                }
                //
                if (!done && tokenList.Count > 1)
                {
                    // try again with just the last element in the list
                    tokenList.RemoveRange(0, tokenList.Count - 1);
                    cType = XSharpLanguage.XSharpTokenTools.RetrieveType(file, tokenList, member, currentNS, stopToken, out gotoElement, currentText);
                }
                if ((gotoElement != null) && (gotoElement.XSharpElement != null))
                {
                    // Ok, find it ! Let's go ;)
                    gotoElement.XSharpElement.OpenEditor();
                }

            }
            catch (Exception ex)
            {
                Trace.WriteLine("Goto Definition Exception : " + ex.Message);
            }
            finally
            {
                XSharpModel.ModelWalker.Resume();
            }
        }

        #region Completion Session
        private void Filter()
        {
            if (_completionSession == null)
                return;
            //
            if (_completionSession.SelectedCompletionSet != null)
            {
                _completionSession.SelectedCompletionSet.Filter();
                if (_completionSession.SelectedCompletionSet.Completions.Count == 0)
                    CancelCompletionSession();
                else
                {
                    _completionSession.SelectedCompletionSet.SelectBestMatch();
                    _completionSession.SelectedCompletionSet.Recalculate();
                }
            }
            //_completionSession.Filter();
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
            if (_completionSession.SelectedCompletionSet != null)
            {
                if (_completionSession.SelectedCompletionSet.SelectionStatus.IsSelected || force)
                {
                    //
                    _completionSession.Commit();
                    return true;
                }
            }
            _completionSession.Dismiss();
            return false;
        }

        bool StartCompletionSession(uint nCmdId, char typedChar)
        {
            if (_completionSession != null)
            {
                if (!_completionSession.IsDismissed)
                    return false;
            }

            SnapshotPoint caret = TextView.Caret.Position.BufferPosition;
            ITextSnapshot snapshot = caret.Snapshot;

            if (!_completionBroker.IsCompletionActive(TextView))
            {
                _completionSession = _completionBroker.CreateCompletionSession(TextView, snapshot.CreateTrackingPoint(caret, PointTrackingMode.Positive), true);
            }
            else
            {
                _completionSession = _completionBroker.GetSessions(TextView)[0];
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
                    _completionSession.Dismiss();

                    StartSignatureSession(false, cType, method);
                }
            }
            //
        }

        private void OnCompletionSessionDismiss(object sender, EventArgs e)
        {
            if (_completionSession.SelectedCompletionSet != null)
            {
                _completionSession.SelectedCompletionSet.Filter();
            }
            //
            _completionSession.Dismissed -= OnCompletionSessionDismiss;
            _completionSession.Committed -= OnCompletionSessionCommitted;
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
                if (!_signatureBroker.IsSignatureHelpActive(TextView))
                {
                    _signatureSession = _signatureBroker.CreateSignatureHelpSession(TextView, snapshot.CreateTrackingPoint(caret, PointTrackingMode.Positive), true);
                }
                else
                {
                    _signatureSession = _signatureBroker.GetSessions(TextView)[0];
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




    }

    static class CommandFilterHelper
    {
        private static System.Globalization.TextInfo txtInfo = new System.Globalization.CultureInfo("en-US", false).TextInfo;
        /// <summary>
        /// Format the Keywords and Identifiers in the Line, using the EditSession
        /// </summary>
        /// <param name="editSession"></param>
        /// <param name="line"></param>

        static public void FormatLine(IBufferTagAggregatorFactoryService Aggregator, ITextView TextView, ITextEdit editSession, ITextSnapshotLine line, int? desiredIndentation)
        {
            CommandFilterHelper.FormatLineCase(Aggregator, TextView, editSession, line);
            CommandFilterHelper.FormatLineIndent(Aggregator, TextView, editSession, line, desiredIndentation);
        }

        static public void FormatLineCase(IBufferTagAggregatorFactoryService Aggregator, ITextView TextView, ITextEdit editSession, ITextSnapshotLine line)
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
                if ((name == "keyword") && (kwCase != 0))
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
                                transform = txtInfo.ToTitleCase(keyword.ToLower());
                                break;
                        }
                        // Not none, and the tranfsform is not the same as the original
                        if (String.Compare(transform, keyword) != 0)
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
        }

        static public void FormatLineIndent(IBufferTagAggregatorFactoryService Aggregator, ITextView TextView, ITextEdit editSession, ITextSnapshotLine line, int? desiredIndentation)
        {
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
                        //
                        if (lineLength - newLength == 0)
                            editSession.Insert(line.Start.Position, indentSpaces);
                        else
                            editSession.Replace(indentSpan, indentSpaces);
                    }
                }
            }
        }

    }


}
