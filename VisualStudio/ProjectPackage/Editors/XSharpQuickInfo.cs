//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Operations;
using Microsoft.VisualStudio.Utilities;
using LanguageService.SyntaxTree;
using System.Reflection;
using System.Diagnostics;
using XSharpColorizer;

namespace XSharp.Project
{
    internal class XSharpQuickInfoSource : IQuickInfoSource
    {
        private XSharpQuickInfoSourceProvider _provider;
        private ITextBuffer _subjectBuffer;
        private XSharpModel.XFile _file;

        public XSharpQuickInfoSource(XSharpQuickInfoSourceProvider provider, ITextBuffer subjectBuffer)
        {
            _provider = provider;
            _subjectBuffer = subjectBuffer;
            _file = _subjectBuffer.GetFile();
        }
        private int lastTriggerPoint = -1;
        private string lastHelp = "";
        private ITrackingSpan lastSpan = null;
        public void AugmentQuickInfoSession(IQuickInfoSession session, IList<object> qiContent, out ITrackingSpan applicableToSpan)
        {
            applicableToSpan = null;
            if (!XSharpProjectPackage.Instance.DebuggerIsRunning)
            {
                try
                {
                    XSharpModel.ModelWalker.Suspend();
                    var package = XSharp.Project.XSharpProjectPackage.Instance;
                    var optionsPage = package.GetIntellisenseOptionsPage();
                    if (optionsPage.DisableQuickInfo)
                        return;


                    // Map the trigger point down to our buffer.
                    SnapshotPoint? subjectTriggerPoint = session.GetTriggerPoint(_subjectBuffer.CurrentSnapshot);
                    if (!subjectTriggerPoint.HasValue )
                    {
                        return;
                    }
                    ITextSnapshot currentSnapshot = subjectTriggerPoint.Value.Snapshot;
                    if (subjectTriggerPoint.Value.Position == lastTriggerPoint)
                    {
                        if (!string.IsNullOrEmpty(lastHelp))
                        {
                            qiContent.Add(lastHelp);
                        }
                        if (lastSpan != null)
                        {
                            applicableToSpan = currentSnapshot.CreateTrackingSpan(
                                lastSpan.GetSpan(currentSnapshot), SpanTrackingMode.EdgeInclusive);
                        }
                        return;
                    }

                    lastTriggerPoint = subjectTriggerPoint.Value.Position;

                    //look for occurrences of our QuickInfo words in the span
                    ITextStructureNavigator navigator = _provider.NavigatorService.GetTextStructureNavigator(_subjectBuffer);
                    TextExtent extent = navigator.GetExtentOfWord(subjectTriggerPoint.Value);
                    string searchText = extent.Span.GetText();

                    // First, where are we ?
                    int caretPos = subjectTriggerPoint.Value.Position;
                    int lineNumber = subjectTriggerPoint.Value.GetContainingLine().LineNumber;
                    var snapshot = session.TextView.TextBuffer.CurrentSnapshot;
                    if (_file == null)
                        return;
                    // Then, the corresponding Type/Element if possible
                    IToken stopToken;
                    //ITokenStream tokenStream;
                    XSharpModel.XTypeMember member = XSharpLanguage.XSharpTokenTools.FindMember(lineNumber, _file);
                    XSharpModel.XType currentNamespace = XSharpLanguage.XSharpTokenTools.FindNamespace(caretPos, _file);
                    // adjust caretpos, for other completions we need to stop before the caret. Now we include the caret
                    List<String> tokenList = XSharpLanguage.XSharpTokenTools.GetTokenList(caretPos+1, lineNumber, snapshot, out stopToken, true, _file, false, member);
                    // Check if we can get the member where we are
                    //if (tokenList.Count > 1)
                    //{
                    //    tokenList.RemoveRange(0, tokenList.Count - 1);
                    //}
                    // LookUp for the BaseType, reading the TokenList (From left to right)
                    XSharpLanguage.CompletionElement gotoElement;
                    String currentNS = "";
                    if (currentNamespace != null)
                    {
                        currentNS = currentNamespace.Name;
                    }

                    XSharpModel.CompletionType cType = XSharpLanguage.XSharpTokenTools.RetrieveType(_file, tokenList, member, currentNS, stopToken, out gotoElement, snapshot, lineNumber);
                    //
                    //
                    if ((gotoElement != null) && (gotoElement.IsInitialized))
                    {
                        // Ok, find it ! Let's go ;)
                        applicableToSpan = currentSnapshot.CreateTrackingSpan
                            (
                                                    extent.Span.Start, searchText.Length, SpanTrackingMode.EdgeInclusive
                            );

                        if (gotoElement.XSharpElement != null)
                        {
                            if (gotoElement.XSharpElement.Kind == XSharpModel.Kind.Constructor)
                            {
                                if (gotoElement.XSharpElement.Parent != null)
                                {
                                    qiContent.Add(gotoElement.XSharpElement.Parent.Description);
                                }
                            }
                            else
                                qiContent.Add(gotoElement.XSharpElement.Description);
                        }
                        else if (gotoElement.SystemElement is TypeInfo)
                        {
                            XSharpLanguage.TypeAnalysis analysis = new XSharpLanguage.TypeAnalysis((TypeInfo)gotoElement.SystemElement);
                            qiContent.Add(analysis.Description);
                        }
                        else
                        {
                            // This works with System.MemberInfo AND 
                            XSharpLanguage.MemberAnalysis analysis = null;
                            if (gotoElement.SystemElement is MemberInfo)
                            {
                                analysis = new XSharpLanguage.MemberAnalysis(gotoElement.SystemElement);
                                if (analysis.IsInitialized)
                                {
                                    if ((analysis.Kind == XSharpModel.Kind.Constructor) && (cType != null) && (cType.SType != null))
                                    {
                                        XSharpLanguage.TypeAnalysis typeAnalysis;
                                        typeAnalysis = new XSharpLanguage.TypeAnalysis(cType.SType.GetTypeInfo());
                                        if (typeAnalysis.IsInitialized)
                                        {
                                            qiContent.Add(typeAnalysis.Description);
                                        }
                                    }
                                    else
                                    {
                                        qiContent.Add(analysis.Description);
                                    }
                                }
                            }

                        }
                        if (qiContent.Count > 0)
                        {
                            lastHelp = (string)qiContent[0];
                            lastSpan = applicableToSpan;
                        }
                        return;
                    }
                }
                catch (Exception ex)
                {
                    XSharpProjectPackage.Instance.DisplayOutPutMessage("XSharpQuickInfo.AugmentQuickInfoSession failed : " );
                    XSharpProjectPackage.Instance.DisplayException(ex);
                }
                finally
                {
                    XSharpModel.ModelWalker.Resume();
                }
            }
        }

        private bool m_isDisposed;
        public void Dispose()
        {
            if (!m_isDisposed)
            {
                GC.SuppressFinalize(this);
                m_isDisposed = true;
            }
        }

        [Export(typeof(IQuickInfoSourceProvider))]
        [Name("XSharp QuickInfo Source")]
        [Order(Before = "Default Quick Info Presenter")]
        [ContentType("XSharp")]
        internal class XSharpQuickInfoSourceProvider : IQuickInfoSourceProvider
        {

            [Import]
            internal ITextStructureNavigatorSelectorService NavigatorService { get; set; }

            [Import]
            internal ITextBufferFactoryService TextBufferFactoryService { get; set; }

            public IQuickInfoSource TryCreateQuickInfoSource(ITextBuffer textBuffer)
            {
                return new XSharpQuickInfoSource(this, textBuffer);
            }

        }

        internal class XSharpQuickInfoController : IIntellisenseController
        {

            private ITextView m_textView;
            private IList<ITextBuffer> m_subjectBuffers;
            private XSharpQuickInfoControllerProvider m_provider;
            private IQuickInfoSession m_session;

            internal XSharpQuickInfoController(ITextView textView, IList<ITextBuffer> subjectBuffers, XSharpQuickInfoControllerProvider provider)
            {
                m_textView = textView;
                m_subjectBuffers = subjectBuffers;
                m_provider = provider;

                m_textView.MouseHover += this.OnTextViewMouseHover;
            }

            private int lastPointPosition = -1;
            private void OnTextViewMouseHover(object sender, MouseHoverEventArgs e)
            {
                if (!XSharpProjectPackage.Instance.DebuggerIsRunning)
                {
                    try
                    {
                        //find the mouse position by mapping down to the subject buffer
                        SnapshotPoint? point = m_textView.BufferGraph.MapDownToFirstMatch
                             (new SnapshotPoint(m_textView.TextSnapshot, e.Position),
                            PointTrackingMode.Positive,
                            snapshot => m_subjectBuffers.Contains(snapshot.TextBuffer),
                            PositionAffinity.Predecessor);

                        if (point.HasValue && point.Value.Position != lastPointPosition)
                        {
                            lastPointPosition = point.Value.Position;
                            if (!m_provider.QuickInfoBroker.IsQuickInfoActive(m_textView))
                            {
                                ITrackingPoint triggerPoint = point.Value.Snapshot.CreateTrackingPoint(point.Value.Position,
                                    PointTrackingMode.Positive);
                                m_session = m_provider.QuickInfoBroker.TriggerQuickInfo(m_textView, triggerPoint, true);
                            }
                        }
                    }
                    catch (Exception ex)
                    {
                        XSharpProjectPackage.Instance.DisplayOutPutMessage("XSharpQuickInfo.OnTextViewMouseHover failed" );
                        XSharpProjectPackage.Instance.DisplayException(ex);
                    }
                }
            }

            public void Detach(ITextView textView)
            {
                if (m_textView == textView)
                {
                    m_textView.MouseHover -= this.OnTextViewMouseHover;
                    m_textView = null;
                }
            }

            public void ConnectSubjectBuffer(ITextBuffer subjectBuffer)
            {
                ;
            }

            public void DisconnectSubjectBuffer(ITextBuffer subjectBuffer)
            {
                ;
            }


        }

        [Export(typeof(IIntellisenseControllerProvider))]
        [Name("XSharp QuickInfo Controller")]
        [ContentType("XSharp")]
        internal class XSharpQuickInfoControllerProvider : IIntellisenseControllerProvider
        {

            [Import]
            internal IQuickInfoBroker QuickInfoBroker { get; set; }

            public IIntellisenseController TryCreateIntellisenseController(ITextView textView, IList<ITextBuffer> subjectBuffers)
            {
                return new XSharpQuickInfoController(textView, subjectBuffers, this);
            }
        }

    }
}
