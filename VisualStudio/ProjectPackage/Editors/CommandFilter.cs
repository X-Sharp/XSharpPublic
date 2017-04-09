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

namespace XSharp.Project
{
    internal sealed class CommandFilter : IOleCommandTarget
    {
        ICompletionSession _completionSession;
        public IWpfTextView TextView { get; private set; }
        public ICompletionBroker CompletionBroker { get; private set; }
        public IOleCommandTarget Next { get; set; }



        ISignatureHelpBroker SignatureBroker;
        ISignatureHelpSession _signatureSession;
        ITextStructureNavigator m_navigator;


        public CommandFilter(IWpfTextView textView, ICompletionBroker completionBroker, ITextStructureNavigator nav, ISignatureHelpBroker signatureBroker)
        {
            m_navigator = nav;

            _completionSession = null;
            _signatureSession = null;

            TextView = textView;
            CompletionBroker = completionBroker;
            SignatureBroker = signatureBroker;
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
                        handled = StartCompletionSession(nCmdID, '\0');
                        break;
                    case VSConstants.VSStd2KCmdID.RETURN:
                        handled = CompleteCompletionSession(false);
                        break;

                    case VSConstants.VSStd2KCmdID.TAB:
                        handled = CompleteCompletionSession(true);
                        break;
                    case VSConstants.VSStd2KCmdID.CANCEL:
                        handled = CancelCompletionSession();
                        break;
                    case VSConstants.VSStd2KCmdID.PARAMINFO:
                        StartSignatureSession();
                        break;
                    case VSConstants.VSStd2KCmdID.TYPECHAR:
                        char ch = GetTypeChar(pvaIn);
                        if (_completionSession != null)
                        {
                            switch (ch)
                            {
                                case ' ':
                                    CompleteCompletionSession(true);
                                    break;
                                case ':':
                                case '.':
                                    CompleteCompletionSession(true);
                                    completeAndStart = true;
                                    break;
                                case '=':
                                    CancelCompletionSession();
                                    break;
                                default:
                                    Filter();
                                    break;
                            }
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
                                        StartCompletionSession(nCmdID, ch);
                                        break;
                                    case '(':
                                    case '{':
                                        StartSignatureSession();
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
                                        //if (_signatureSession != null)
                                        //{
                                        //    SnapshotPoint current = this.TextView.Caret.Position.BufferPosition;
                                        //    int line = current.GetContainingLine().LineNumber;
                                        //    int pos = current.Position;
                                        //    //
                                        //    int startLine = (int)_signatureSession.Properties["Line"];
                                        //    int startPos = (int)_signatureSession.Properties["Start"];
                                        //    if ( !(( line == startLine ) && ( pos >= startPos )) )
                                        //    {
                                        //        CancelSignatureSession();
                                        //    }
                                        //}
                                        break;
                                }
                            }
                            break;
                        case VSConstants.VSStd2KCmdID.BACKSPACE:
                            Filter();
                            break;
                        case VSConstants.VSStd2KCmdID.COMPLETEWORD:

                            break;
                    }
                }
            }

            return hresult;
        }

        private void GotoDefn()
        {
            // First, where are we ?
            int caretPos = this.TextView.Caret.Position.BufferPosition.Position;
            int lineNumber = this.TextView.Caret.Position.BufferPosition.GetContainingLine().LineNumber;
            String currentText = this.TextView.TextBuffer.CurrentSnapshot.GetText();
            string fileName = EditorHelpers.GetDocumentFileName(this.TextView.TextBuffer);
            if (String.IsNullOrEmpty(fileName))
                return;
            // Then, the corresponding Type/Element if possible
            IToken stopToken;
            //ITokenStream tokenStream;
            List<String> tokenList = XSharpLanguage.XSharpTokenTools.GetTokenList(caretPos, lineNumber, currentText, out stopToken, true, fileName);
            // Check if we can get the member where we are
            XSharpModel.XTypeMember member = XSharpLanguage.XSharpTokenTools.FindMember(caretPos, fileName);
            XSharpModel.XType currentNamespace = XSharpLanguage.XSharpTokenTools.FindNamespace(caretPos, fileName);
            // LookUp for the BaseType, reading the TokenList (From left to right)
            XSharpModel.XElement gotoElement;
            MemberInfo dummyElement;
            XSharpModel.CompletionType cType = XSharpLanguage.XSharpTokenTools.RetrieveType(fileName, tokenList, member, stopToken, out gotoElement, out dummyElement);
            //
            if (gotoElement != null)
            {
                // Ok, find it ! Let's go ;)
                gotoElement.OpenEditor();
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

            _completionSession.Properties["Command"] = nCmdId;
            _completionSession.Properties["Char"] = typedChar;
            _completionSession.Start();

            return true;
        }

        private void OnCompletionSessionDismiss(object sender, EventArgs e)
        {
            _completionSession = null;
        }
        #endregion


        #region Signature Session
        bool StartSignatureSession()
        {
            if (_signatureSession != null)
                return false;

            // First, where are we ?
            int caretPos;
            int startLineNumber = this.TextView.Caret.Position.BufferPosition.GetContainingLine().LineNumber;
            SnapshotPoint ssp = this.TextView.Caret.Position.BufferPosition;
            int lineNumber = startLineNumber;
            //
            do
            {
                ssp = ssp - 1;
                char leftCh = ssp.GetChar();
                if ((leftCh == '(') || (leftCh == '{'))
                    break;
                lineNumber = ssp.GetContainingLine().LineNumber;
            } while (startLineNumber == lineNumber);
            //
            caretPos = ssp.Position;
            String currentText = this.TextView.TextBuffer.CurrentSnapshot.GetText();
            string fileName = EditorHelpers.GetDocumentFileName(this.TextView.TextBuffer);
            if (String.IsNullOrEmpty(fileName))
                return false;
            // Then, the corresponding Type/Element if possible
            IToken stopToken;
            //ITokenStream tokenStream;
            List<String> tokenList = XSharpLanguage.XSharpTokenTools.GetTokenList(caretPos, lineNumber, currentText, out stopToken, true, fileName);
            // Check if we can get the member where we are
            XSharpModel.XTypeMember member = XSharpLanguage.XSharpTokenTools.FindMember(caretPos, fileName);
            XSharpModel.XType currentNamespace = XSharpLanguage.XSharpTokenTools.FindNamespace(caretPos, fileName);
            // LookUp for the BaseType, reading the TokenList (From left to right)
            XSharpModel.XElement gotoElement;
            MemberInfo systemElement;
            XSharpModel.CompletionType cType = XSharpLanguage.XSharpTokenTools.RetrieveType(fileName, tokenList, member, stopToken, out gotoElement, out systemElement);
            //
            if ((gotoElement != null) || (systemElement != null))
            {
                //_signatureSession = SignatureBroker.TriggerSignatureHelp( TextView);
                //_signatureSession.Dismissed += OnSignatureSessionDismiss;
                //if (gotoElement != null)
                //{
                //    _signatureSession.Properties["Element"] = gotoElement;
                //}
                //else if (systemElement != null)
                //{
                //    _signatureSession.Properties["Element"] = systemElement;
                //}
                if (gotoElement.Kind == XSharpModel.Kind.Class)
                {
                    XSharpModel.XType xType = gotoElement as XSharpModel.XType;
                    if (xType != null)
                    {
                        foreach (XSharpModel.XTypeMember mbr in xType.Members)
                        {
                            if (String.Compare(mbr.Name, "constructor", true) == 0)
                            {
                                gotoElement = mbr;
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
                if (gotoElement != null)
                {
                    _signatureSession.Properties["Element"] = gotoElement;
                }
                else if (systemElement != null)
                {
                    _signatureSession.Properties["Element"] = systemElement;
                }
                _signatureSession.Properties["Line"] = startLineNumber;
                _signatureSession.Properties["Start"] = ssp.Position;
                _signatureSession.Properties["Length"] = TextView.Caret.Position.BufferPosition.Position - ssp.Position;
                _signatureSession.Start();
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


}
