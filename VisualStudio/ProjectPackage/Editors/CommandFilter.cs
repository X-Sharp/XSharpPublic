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

namespace XSharp.Project
{
    internal sealed class CommandFilter : IOleCommandTarget
    {
        ICompletionSession _currentSession;

        public CommandFilter(IWpfTextView textView, ICompletionBroker broker)
        {
            _currentSession = null;

            TextView = textView;
            Broker = broker;
        }

        public IWpfTextView TextView { get; private set; }
        public ICompletionBroker Broker { get; private set; }
        public IOleCommandTarget Next { get; set; }

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
                        handled = StartSession(nCmdID, '\0');
                        break;
                    case VSConstants.VSStd2KCmdID.RETURN:
                        handled = Complete(false);
                        break;

                    case VSConstants.VSStd2KCmdID.TAB:
                        handled = Complete(true);
                        break;
                    case VSConstants.VSStd2KCmdID.CANCEL:
                        handled = Cancel();
                        break;
                    case VSConstants.VSStd2KCmdID.TYPECHAR:
                        char ch = GetTypeChar(pvaIn);
                        if (_currentSession != null)
                        {
                            switch (ch)
                            {
                                case ' ':
                                    Complete(true);
                                    break;
                                case ':':
                                case '.':
                                    Complete(true);
                                    completeAndStart = true;
                                    break;
                                case '=':
                                    Cancel();
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
                            if (_currentSession != null)
                            {
                                if (completeAndStart)
                                {
                                    StartSession(nCmdID, ch);
                                }
                            }
                            else
                            {
                                switch (ch)
                                {
                                    case ':':
                                    case '.':
                                        StartSession(nCmdID, ch);
                                        break;
                                    default:
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
            string fileName = EditorHelpers.GetDocumentFileName( this.TextView.TextBuffer );
            if (String.IsNullOrEmpty(fileName))
                return;
            // Then, the corresponding Type/Element if possible
            IToken stopToken;
            //ITokenStream tokenStream;
            List<String> tokenList = XSharpLanguage.XSharpTokenTools.GetTokenList(caretPos, lineNumber, currentText, out stopToken, true , fileName);
            // Check if we can get the member where we are
            XSharpModel.XTypeMember member = XSharpLanguage.XSharpTokenTools.FindMember(caretPos, fileName);
            XSharpModel.XType currentNamespace = XSharpLanguage.XSharpTokenTools.FindNamespace(caretPos, fileName);
            // LookUp for the BaseType, reading the TokenList (From left to right)
            XSharpModel.XElement gotoElement;
            MemberInfo dummyElement;
            XSharpModel.CompletionType cType = XSharpLanguage.XSharpTokenTools.RetrieveType(fileName, tokenList, member, stopToken, out gotoElement, out dummyElement );
            //
            if (gotoElement != null )
            {
                // Ok, find it ! Let's go ;)
                gotoElement.OpenEditor();
            }
            //
        }

        private void Filter()
        {
            if (_currentSession == null)
                return;
            _currentSession.SelectedCompletionSet.Filter();
            _currentSession.SelectedCompletionSet.SelectBestMatch();
            //_currentSession.SelectedCompletionSet.Recalculate();
        }

        bool Cancel()
        {
            if (_currentSession == null)
                return false;

            _currentSession.Dismiss();

            return true;
        }

        bool Complete(bool force)
        {
            if (_currentSession == null)
                return false;

            if (!_currentSession.SelectedCompletionSet.SelectionStatus.IsSelected && !force)
            {
                _currentSession.Dismiss();
                return false;
            }
            else
            {
                _currentSession.Commit();
                return true;
            }
        }

        bool StartSession(uint nCmdId, char typedChar)
        {
            if (_currentSession != null)
                return false;

            SnapshotPoint caret = TextView.Caret.Position.BufferPosition;
            ITextSnapshot snapshot = caret.Snapshot;

            if (!Broker.IsCompletionActive(TextView))
            {
                _currentSession = Broker.CreateCompletionSession(TextView, snapshot.CreateTrackingPoint(caret, PointTrackingMode.Positive), true);
            }
            else
            {
                _currentSession = Broker.GetSessions(TextView)[0];
            }

            _currentSession.Dismissed += OnSessionDismiss;

            _currentSession.Properties["Command"] = nCmdId;
            _currentSession.Properties["Char"] = typedChar;
            _currentSession.Start();

            return true;
        }

        private void OnSessionDismiss(object sender, EventArgs e)
        {
            _currentSession = null;
        }

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
            //else if (pguidCmdGroup == VSConstants.CMDSETID.StandardCommandSet12_guid )
            //{
            //    switch ((VSConstants.VSStd12CmdID)prgCmds[0].cmdID)
            //    {
            //        case VSConstants.VSStd12CmdID.PeekDefinition:
            //            prgCmds[0].cmdf = (uint)OLECMDF.OLECMDF_ENABLED | (uint)OLECMDF.OLECMDF_SUPPORTED;
            //            return VSConstants.S_OK;
            //    }
            //}
            return Next.QueryStatus(pguidCmdGroup, cCmds, prgCmds, pCmdText);
        }




    }


}
