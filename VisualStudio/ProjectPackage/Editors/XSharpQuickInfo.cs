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


        public void AugmentQuickInfoSession(IQuickInfoSession session, IList<object> qiContent, out ITrackingSpan applicableToSpan)
        {
            try
            {
                applicableToSpan = null;
                var package = XSharp.Project.XSharpProjectPackage.Instance;
                var optionsPage = package.GetIntellisenseOptionsPage();
                if (optionsPage.DisableQuickInfo)
                    return;

                XSharpModel.ModelWalker.Suspend();

                // Map the trigger point down to our buffer.
                SnapshotPoint? subjectTriggerPoint = session.GetTriggerPoint(_subjectBuffer.CurrentSnapshot);
                if (!subjectTriggerPoint.HasValue)
                {
                    applicableToSpan = null;
                    return;
                }
                ITextSnapshot currentSnapshot = subjectTriggerPoint.Value.Snapshot;
                SnapshotSpan querySpan = new SnapshotSpan(subjectTriggerPoint.Value, 0);

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
                List<String> tokenList = XSharpLanguage.XSharpTokenTools.GetTokenList(caretPos, lineNumber, snapshot, out stopToken, true, _file, false, member);
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
                                                //querySpan.Start.Add(foundIndex).Position, 9, SpanTrackingMode.EdgeInclusive
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
                        else if (gotoElement.CodeElement != null)
                        {
                            analysis = new XSharpLanguage.MemberAnalysis(gotoElement.CodeElement);
                            if (analysis.IsInitialized)
                            {
                                if ((analysis.Kind == XSharpModel.Kind.Constructor) && (cType != null) && (cType.CodeElement != null))
                                {
                                    if (cType.CodeElement.IsCodeType)
                                    {
                                        XSharpLanguage.TypeAnalysis typeAnalysis;
                                        typeAnalysis = new XSharpLanguage.TypeAnalysis((EnvDTE.CodeType)cType.CodeElement);
                                        if (typeAnalysis.IsInitialized)
                                        {
                                            qiContent.Add(typeAnalysis.Description);
                                        }
                                    }
                                }
                                else
                                {
                                    qiContent.Add(analysis.Description);
                                }
                            }
                        }

                    }
                    return;
                }
            }
            catch ( Exception ex )
            {
                Trace.WriteLine("XSharpQuickInfo.AugmentQuickInfoSession Exception : " + ex.Message);
            }
            finally
            {
                XSharpModel.ModelWalker.Resume();
            }
            applicableToSpan = null;
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

            private void OnTextViewMouseHover(object sender, MouseHoverEventArgs e)
            {
                try
                {
                    //find the mouse position by mapping down to the subject buffer
                    SnapshotPoint? point = m_textView.BufferGraph.MapDownToFirstMatch
                         (new SnapshotPoint(m_textView.TextSnapshot, e.Position),
                        PointTrackingMode.Positive,
                        snapshot => m_subjectBuffers.Contains(snapshot.TextBuffer),
                        PositionAffinity.Predecessor);

                    if (point != null)
                    {
                        ITrackingPoint triggerPoint = point.Value.Snapshot.CreateTrackingPoint(point.Value.Position,
                        PointTrackingMode.Positive);

                        if (!m_provider.QuickInfoBroker.IsQuickInfoActive(m_textView))
                        {
                            m_session = m_provider.QuickInfoBroker.TriggerQuickInfo(m_textView, triggerPoint, true);
                        }
                    }
                }
                catch (Exception ex)
                {
                    Trace.WriteLine("XSharpQuickInfo.OnTextViewMouseHover Exception : " + ex.Message);
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
            }

            public void DisconnectSubjectBuffer(ITextBuffer subjectBuffer)
            {
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
