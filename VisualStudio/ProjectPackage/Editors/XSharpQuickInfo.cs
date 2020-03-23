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
using System.Windows.Documents;
using System.Windows.Media;
using XSharpModel;
using System.Windows.Controls;
using Microsoft.VisualStudio.Text.Classification;

namespace XSharp.Project
{
    internal class XSharpQuickInfoSource : IQuickInfoSource
    {
        private XSharpQuickInfoSourceProvider _provider;
        private ITextBuffer _subjectBuffer;
        private XSharpModel.XFile _file;
        private IClassificationFormatMapService _formatMap;
        private IClassificationTypeRegistryService _registry;
        private static OptionsPages.IntellisenseOptionsPage _optionsPage;

        public XSharpQuickInfoSource(XSharpQuickInfoSourceProvider provider, ITextBuffer subjectBuffer, IClassificationFormatMapService formatMap, IClassificationTypeRegistryService registry)
        {
            _provider = provider;
            _subjectBuffer = subjectBuffer;
            _file = _subjectBuffer.GetFile();
            _formatMap = formatMap;
            _registry = registry;
            var package = XSharpProjectPackage.Instance;
            _optionsPage = package.GetIntellisenseOptionsPage();
        }
        private int lastTriggerPoint = -1;
        private Inline[] lastHelp = null;
        private String lastxmldoc = null;
        private ITrackingSpan lastSpan = null;
        private int lastVersion = -1;

        static void WriteOutputMessage(string message)
        {
            XSharpProjectPackage.Instance.DisplayOutPutMessage("XSharp.QuickInfoSource :" + message);
        }
        public void AugmentQuickInfoSession(IQuickInfoSession session, IList<object> qiContent, out ITrackingSpan applicableToSpan)
        {
            applicableToSpan = null;
            if (XSharpProjectPackage.Instance.DebuggerIsRunning)
            {
                return;
            }
            try
            {
                XSharpModel.ModelWalker.Suspend();
                var package = XSharp.Project.XSharpProjectPackage.Instance;
                var optionsPage = package.GetIntellisenseOptionsPage();
                if (optionsPage.DisableQuickInfo)
                    return;


                // Map the trigger point down to our buffer.
                SnapshotPoint? subjectTriggerPoint = session.GetTriggerPoint(_subjectBuffer.CurrentSnapshot);
                if (!subjectTriggerPoint.HasValue)
                {
                    return;
                }
                ITextSnapshot currentSnapshot = subjectTriggerPoint.Value.Snapshot;
                WriteOutputMessage($"Triggerpoint: {subjectTriggerPoint.Value.Position}");

                if ((subjectTriggerPoint.Value.Position == lastTriggerPoint) && (lastVersion == currentSnapshot.Version.VersionNumber))
                {
                    if (lastHelp != null)
                    {
                        var description = new TextBlock();
                        description.Inlines.AddRange(lastHelp);
                        qiContent.Add(description);
                        if (lastxmldoc != null)
                            qiContent.Add(lastxmldoc);
                        WriteOutputMessage($"Return last help content: {lastHelp}");
                    }
                    if (lastSpan != null)
                    {
                        applicableToSpan = lastSpan;
                    }
                    return;
                }
                // We don't want to lex the buffer. So get the tokens from the last lex run
                // and when these are too old, then simply bail out
                var tokens = _subjectBuffer.GetTokens();
                if (tokens != null)
                {
                    if (tokens.SnapShot.Version != currentSnapshot.Version)
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
                List<String> tokenList = XSharpLanguage.XSharpTokenTools.GetTokenList(caretPos + 1, lineNumber, tokens.TokenStream, out stopToken, true, _file, false, member);
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

                XSharpModel.CompletionType cType = XSharpLanguage.XSharpTokenTools.RetrieveType(_file, tokenList, member, currentNS, stopToken, out gotoElement, snapshot, lineNumber, _file.Project.Dialect);
                //
                //
                if ((gotoElement != null) && (gotoElement.IsInitialized))
                {
                    IClassificationType kwType = _registry.GetClassificationType("keyword");
                    IClassificationFormatMap fmap = _formatMap.GetClassificationFormatMap(category: "text");
                    Microsoft.VisualStudio.Text.Formatting.TextFormattingRunProperties kwFormat = fmap.GetTextProperties(kwType);
                    kwType = _registry.GetClassificationType("text");
                    fmap = _formatMap.GetClassificationFormatMap(category: "text");
                    Microsoft.VisualStudio.Text.Formatting.TextFormattingRunProperties txtFormat = fmap.GetTextProperties(kwType);
                    //
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
                                var xtype = gotoElement.XSharpElement.Parent as XType;
                                var qitm = new QuickInfoTypeAnalysis(xtype, kwFormat.ForegroundBrush, txtFormat.ForegroundBrush);
                                var description = new TextBlock();
                                description.Inlines.AddRange(qitm.WPFDescription);
                                qiContent.Add(description);

                            }
                        }
                        else if (gotoElement.XSharpElement is XSharpModel.XTypeMember)
                        {
                            QuickInfoTypeMember qitm = new QuickInfoTypeMember((XSharpModel.XTypeMember)gotoElement.XSharpElement, kwFormat.ForegroundBrush, txtFormat.ForegroundBrush);
                            var description = new TextBlock();
                            description.Inlines.AddRange(qitm.WPFDescription);
                            qiContent.Add(description);
                        }
                        else if (gotoElement.XSharpElement is XSharpModel.XVariable)
                        {
                            QuickInfoVariable qitm = new QuickInfoVariable((XSharpModel.XVariable)gotoElement.XSharpElement, kwFormat.ForegroundBrush, txtFormat.ForegroundBrush);
                            var description = new TextBlock();
                            description.Inlines.AddRange(qitm.WPFDescription);
                            qiContent.Add(description);

                        }
                        else if (gotoElement.XSharpElement is XSharpModel.XType)
                        {
                            var xtype = gotoElement.XSharpElement as XType;
                            var qitm = new QuickInfoTypeAnalysis(xtype, kwFormat.ForegroundBrush, txtFormat.ForegroundBrush);
                            var description = new TextBlock();
                            description.Inlines.AddRange(qitm.WPFDescription);
                            qiContent.Add(description);

                        }
                        else
                        {
                            var description = new TextBlock();
                            Run temp;
                            temp = new Run(gotoElement.XSharpElement.Description);
                            temp.Foreground = txtFormat.ForegroundBrush;
                            //
                            description.Inlines.Add(temp);
                            qiContent.Add(description);
                        }

                    }
                    else if (gotoElement.SystemElement is TypeInfo)
                    {
                        var ti = gotoElement.SystemElement as TypeInfo;
                        string xmldoc = XSharpXMLDocMember.GetTypeSummary(ti, member.File.Project);
                        QuickInfoTypeAnalysis analysis = new QuickInfoTypeAnalysis(ti, kwFormat.ForegroundBrush, txtFormat.ForegroundBrush);
                        var description = new TextBlock();
                        description.Inlines.AddRange(analysis.WPFDescription);
                        qiContent.Add(description);
                        if (xmldoc != null)
                            qiContent.Add(xmldoc);
                    }
                    else
                    {
                        // This works with System.MemberInfo AND
                        QuickInfoMemberAnalysis analysis = null;
                        if (gotoElement.SystemElement is MemberInfo)
                        {
                            string xmldoc = XSharpXMLDocMember.GetMemberSummary(gotoElement.SystemElement, member.File.Project);

                            analysis = new QuickInfoMemberAnalysis(gotoElement.SystemElement, kwFormat.ForegroundBrush, txtFormat.ForegroundBrush);
                            if (analysis.IsInitialized)
                            {
                                if ((analysis.Kind == XSharpModel.Kind.Constructor) && (cType != null) && (cType.SType != null))
                                {
                                    QuickInfoTypeAnalysis typeAnalysis;
                                    typeAnalysis = new QuickInfoTypeAnalysis(cType.SType.GetTypeInfo(), kwFormat.ForegroundBrush, txtFormat.ForegroundBrush);
                                    if (typeAnalysis.IsInitialized)
                                    {
                                        var description = new TextBlock();
                                        description.Inlines.AddRange(typeAnalysis.WPFDescription);
                                        qiContent.Add(description);
                                    }
                                }
                                else
                                {
                                    var description = new TextBlock();
                                    description.Inlines.AddRange(analysis.WPFDescription);
                                    qiContent.Add(description);
                                }
                                if (xmldoc != null)
                                    qiContent.Add(xmldoc);
                            }
                        }

                    }
                    if (qiContent.Count > 0)
                    {
                        TextBlock description;
                        description = qiContent[0] as TextBlock;
                        if (qiContent.Count > 1)
                        {
                            lastxmldoc = qiContent[1] as String;
                        }
                        else
                        {
                            lastxmldoc = null;
                        }
                        if (description != null)
                        {
                            lastHelp = new Inline[description.Inlines.Count];
                            description.Inlines.CopyTo(lastHelp, 0);
                            lastSpan = applicableToSpan;
                            lastVersion = currentSnapshot.Version.VersionNumber;
                            WriteOutputMessage($"Found new help content: {lastHelp}");
                        }
                    }
                    return;
                }
            }
            catch (Exception ex)
            {
                XSharpProjectPackage.Instance.DisplayOutPutMessage("XSharpQuickInfo.AugmentQuickInfoSession failed : ");
                XSharpProjectPackage.Instance.DisplayException(ex);
            }
            finally
            {
                XSharpModel.ModelWalker.Resume();
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

            [Import]
            IClassificationFormatMapService formatMap = null;

            [Import]
            IClassificationTypeRegistryService registry = null;

            public IQuickInfoSource TryCreateQuickInfoSource(ITextBuffer textBuffer)
            {
                return new XSharpQuickInfoSource(this, textBuffer, formatMap, registry);
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
                if (XSharpProjectPackage.Instance.DebuggerIsRunning)
                {
                    return;
                }
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
                    XSharpProjectPackage.Instance.DisplayOutPutMessage("XSharpQuickInfo.OnTextViewMouseHover failed");
                    XSharpProjectPackage.Instance.DisplayException(ex);
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

        internal class QuickInfoTypeAnalysis : XSharpLanguage.TypeAnalysis
        {
            Brush kwBrush;
            Brush txtBrush;

            internal QuickInfoTypeAnalysis(TypeInfo typeInfo, Brush kw, Brush txt) : base(typeInfo)
            {
                this.kwBrush = kw;
                this.txtBrush = txt;
            }
            internal QuickInfoTypeAnalysis(XType type, Brush fg, Brush txt) : base(type)
            {
                this.kwBrush = fg;
                this.txtBrush = txt;
            }
            public List<Inline> WPFDescription
            {
                get
                {
                    List<Inline> content = new List<Inline>();

                    Run temp;

                    if (this.Modifiers != XSharpModel.Modifiers.None)
                    {
                        temp = new Run(_optionsPage.formatKeyword(this.Modifiers) + " ");
                        temp.Foreground = this.kwBrush;
                        content.Add(temp);
                    }
                    temp = new Run(_optionsPage.formatKeyword(this.Visibility) + " ");
                    temp.Foreground = this.kwBrush;
                    content.Add(temp);
                    //
                    if (this.IsStatic)
                    {
                        temp = new Run(_optionsPage.Static() + " ");
                        temp.Foreground = this.kwBrush;
                        content.Add(temp);
                    }
                    //
                    if (this.Kind != XSharpModel.Kind.Field)
                    {
                        temp = new Run(_optionsPage.formatKeyword(this.Kind) + " ");
                        temp.Foreground = this.kwBrush;
                        content.Add(temp);
                    }
                    //
                    temp = new Run(this.Prototype);
                    temp.Foreground = txtBrush;
                    content.Add(temp);
                    //
                    return content;
                }

            }
        }

        internal class QuickInfoMemberAnalysis : XSharpLanguage.MemberAnalysis
        {
            Brush kwBrush;
            Brush txtBrush;

            internal QuickInfoMemberAnalysis(MemberInfo member, Brush kw, Brush txt) : base(member)
            {
                this.kwBrush = kw;
                this.txtBrush = txt;
            }

            public List<Inline> WPFDescription
            {
                get
                {
                    List<Inline> content = new List<Inline>();

                    Run temp;
                    if (this.Modifiers != XSharpModel.Modifiers.None)
                    {
                        temp = new Run(_optionsPage.formatKeyword(this.Modifiers) + " ");
                        temp.Foreground = this.kwBrush;
                        content.Add(temp);
                    }
                    temp = new Run(_optionsPage.formatKeyword(this.Visibility) + " ");
                    temp.Foreground = this.kwBrush;
                    content.Add(temp);
                    //
                    if ((this.IsStatic) && ((this.Kind != Kind.Function) && (this.Kind != Kind.Procedure)))
                    {
                        temp = new Run(_optionsPage.Static()+ " ");
                        temp.Foreground = this.kwBrush;
                        content.Add(temp);
                    }
                    //
                    if ((this.Kind != XSharpModel.Kind.Field) && (this.Kind != XSharpModel.Kind.Constructor))
                    {
                        temp = new Run(_optionsPage.formatKeyword(this.Kind) + " ");
                        temp.Foreground = this.kwBrush;
                        content.Add(temp);
                    }
                    //
                    content.AddRange(this.WPFPrototype);
                    //
                    return content;
                }

            }

            public List<Inline> WPFPrototype
            {
                get
                {
                    List<Inline> content = new List<Inline>();
                    Run temp;
                    List<Inline> vars = new List<Inline>();
                    if (this.Kind.HasParameters())
                    {
                        temp = new Run(this.Kind == XSharpModel.Kind.Constructor ? "{" : "(");
                        temp.Foreground = this.kwBrush;
                        vars.Add(temp);

                        foreach (var var in this.Parameters)
                        {
                            if (vars.Count > 1)
                            {
                                temp = new Run(", ");
                                temp.Foreground = txtBrush;
                                vars.Add(temp);
                            }
                            temp = new Run(var.Name + " ");
                            temp.Foreground = txtBrush;
                            vars.Add(temp);
                            temp = new Run(var.Direction + " ");
                            temp.Foreground = this.kwBrush;
                            vars.Add(temp);
                            temp = new Run(var.TypeName);
                            temp.Foreground = this.kwBrush;
                            vars.Add(temp);
                        }
                        temp = new Run(this.Kind == XSharpModel.Kind.Constructor ? "}" : ")");
                        temp.Foreground = this.kwBrush;
                        vars.Add(temp);
                    }
                    //
                    temp = new Run(this.Name);
                    temp.Foreground = txtBrush;
                    content.Add(temp);
                    content.AddRange(vars);
                    //
                    if (this.Kind.HasReturnType())
                    {
                        temp = new Run(" "+ _optionsPage.As());
                        temp.Foreground = this.kwBrush;
                        content.Add(temp);
                        temp = new Run(this.TypeName);
                        temp.Foreground = this.txtBrush;
                        content.Add(temp);
                    }
                    if (!String.IsNullOrEmpty(this.Value))
                    {
                        temp = new Run(" := " + this.Value);
                        temp.Foreground = this.txtBrush;
                        content.Add(temp);
                    }

                    //
                    return content;
                }
            }
        }

        internal class QuickInfoBase
        {
            internal Brush kwBrush;
            internal Brush txtBrush;

            internal QuickInfoBase(Brush kw, Brush txt)
            {
                this.kwBrush = kw;
                this.txtBrush = txt;
            }

            internal void AddVarInfo(List<Inline> list, XVariable var)
            {
                Run temp;
                temp = new Run(var.Name + " ");
                temp.Foreground = txtBrush;
                list.Add(temp);
                temp = new Run(_optionsPage.formatKeyword(var.ParamTypeDesc) + " ");
                temp.Foreground = this.kwBrush;
                list.Add(temp);
                temp = new Run(var.TypeName);
                temp.Foreground = this.txtBrush;
                list.Add(temp);
                if (var.IsArray)
                {
                    temp = new Run("[] ");
                    temp.Foreground = this.txtBrush;
                    list.Add(temp);
                }
            }

        }

        internal class QuickInfoTypeMember : QuickInfoBase
        {
            XSharpModel.XTypeMember typeMember;
 
            internal QuickInfoTypeMember(XSharpModel.XTypeMember tm, Brush kw, Brush txt) : base(kw,txt)
            {
                this.typeMember = tm;
            }

            public List<Inline> WPFDescription
            {
                get
                {
                    List<Inline> content = new List<Inline>();

                    Run temp;
                    if (this.typeMember.Modifiers != XSharpModel.Modifiers.None)
                    {
                        temp = new Run(_optionsPage.formatKeyword(this.typeMember.Modifiers) + " ");
                        temp.Foreground = this.kwBrush;
                        content.Add(temp);
                    }
                    temp = new Run(_optionsPage.formatKeyword(this.typeMember.Visibility) + " ");
                    temp.Foreground = this.kwBrush;
                    content.Add(temp);
                    //
                    if ((this.typeMember.IsStatic) && ((this.typeMember.Kind != Kind.Function) && (this.typeMember.Kind != Kind.Procedure)))
                    {
                        temp = new Run(_optionsPage.Static() + " ");
                        temp.Foreground = this.kwBrush;
                        content.Add(temp);
                    }
                    //
                    if (this.typeMember.Kind != XSharpModel.Kind.Field)
                    {
                        string kind = this.typeMember.Kind.ToString();
                        if (kind.StartsWith("vo",StringComparison.OrdinalIgnoreCase))
                        {
                            kind = kind.Substring(2);
                        }
                        temp = new Run(_optionsPage.formatKeyword(kind) + " ");
                        temp.Foreground = this.kwBrush;
                        content.Add(temp);
                    }
                    //
                    content.AddRange(this.WPFPrototype);
                    //
                    return content;
                }

            }

            public List<Inline> WPFPrototype
            {
                get
                {
                    List<Inline> content = new List<Inline>();
                    Run temp;
                    List<Inline> vars = new List<Inline>();
                    if (this.typeMember.Kind.HasParameters())
                    {
                        temp = new Run(this.typeMember.Kind == XSharpModel.Kind.Constructor ? "{" : "(");
                        temp.Foreground = this.kwBrush;
                        vars.Add(temp);

                        foreach (var var in this.typeMember.Parameters)
                        {
                            if (vars.Count > 1)
                            {
                                temp = new Run(", ");
                                temp.Foreground = txtBrush;
                                vars.Add(temp);
                            }
                            AddVarInfo(vars, var);
                        }
                        temp = new Run(this.typeMember.Kind == XSharpModel.Kind.Constructor ? "}" : ")");
                        temp.Foreground = this.kwBrush;
                        vars.Add(temp);
                    }
                    //
                    temp = new Run(this.typeMember.Name);
                    temp.Foreground = txtBrush;
                    content.Add(temp);
                    content.AddRange(vars);
                    //
                    if (this.typeMember.Kind.HasReturnType() && !String.IsNullOrEmpty(this.typeMember.TypeName))
                    {
                        temp = new Run(" " + _optionsPage.As());
                        temp.Foreground = this.kwBrush;
                        content.Add(temp);
                        temp = new Run(this.typeMember.TypeName);
                        temp.Foreground = this.txtBrush;
                        content.Add(temp);
                    }
                    if (!String.IsNullOrEmpty(this.typeMember.Value))
                    {
                        temp = new Run(" := " + this.typeMember.Value);
                        temp.Foreground = this.txtBrush;
                        content.Add(temp);
                    }
                    return content;
                }
            }

        }


        internal class QuickInfoVariable : QuickInfoBase
        {
            XSharpModel.XVariable xVar;

            internal QuickInfoVariable(XSharpModel.XVariable var, Brush kw, Brush txt) : base(kw,txt)
            {
                this.xVar = var;
            }

            public List<Inline> WPFDescription
            {
                get
                {
                    List<Inline> content = new List<Inline>();

                    Run temp;
                    if (this.xVar.IsParameter)
                    {
                        temp = new Run(_optionsPage.Parameter() + " ");
                        temp.Foreground = this.kwBrush;
                        content.Add(temp);
                    }
                    else
                    {
                        temp = new Run(_optionsPage.Local() + " ");
                        temp.Foreground = this.kwBrush;
                        content.Add(temp);
                    }
                    //
                    AddVarInfo(content, xVar);
                    return content;
                }

            }

        }

        /*
        internal class QuickInfoType
        {
            XSharpModel.XType xtype;
            Brush kwBrush;
            Brush txtBrush;

            internal QuickInfoType(XSharpModel.XType tm, Brush kw, Brush txt)
            {
                this.xtype = tm;
                this.kwBrush = kw;
                this.txtBrush = txt;
            }

            public List<Inline> WPFDescription
            {
                get
                {
                    List<Inline> content = new List<Inline>();

                    Run temp;
                    if (this.xtype.Modifiers != XSharpModel.Modifiers.None)
                    {
                        temp = new Run(_optionsPage.formatKeyword(this.xtype.Modifiers) + " ");
                        temp.Foreground = this.kwBrush;
                        content.Add(temp);
                    }
                    temp = new Run(_optionsPage.formatKeyword(this.xtype.Visibility) + " ");
                    temp.Foreground = this.kwBrush;
                    content.Add(temp);
                    //
                    if (this.xtype.IsStatic)
                    {
                        temp = new Run(_optionsPage.Static() + " ");
                        temp.Foreground = this.kwBrush;
                        content.Add(temp);
                    }
                    //
                    if (this.xtype.Kind != XSharpModel.Kind.Field)
                    {
                        temp = new Run(_optionsPage.formatKeyword(this.xtype.Kind) + " ");
                        temp.Foreground = this.kwBrush;
                        content.Add(temp);
                    }
                    //
                    content.AddRange(this.WPFPrototype);
                    //
                    return content;
                }

            }

            public List<Inline> WPFPrototype
            {
                get
                {
                    List<Inline> content = new List<Inline>();
                    Run temp;
                    //
                    temp = new Run(this.xtype.Name);
                    temp.Foreground = txtBrush;
                    content.Add(temp);
                    //
                    return content;
                }
            }

        }
        */

    }
    static class KeywordExtensions
    {
        internal static string formatKeyword(this OptionsPages.IntellisenseOptionsPage page,  Modifiers keyword)
        {
            return page.formatKeyword(keyword.ToString());
        }
        internal static string formatKeyword(this OptionsPages.IntellisenseOptionsPage page,  Kind keyword)
        {
            switch (keyword)
            {
                case Kind.VODefine:
                    return page.formatKeyword("define");
                case Kind.VOGlobal:
                    return page.formatKeyword("global");
                case Kind.VODLL:
                    return page.formatKeyword("_dll function");
            }
            return page.formatKeyword(keyword.ToString());
        }
        internal static string formatKeyword(this OptionsPages.IntellisenseOptionsPage page,  string keyword)
        {
            return page.SyncKeyword(keyword);
        } 
        internal static string As(this OptionsPages.IntellisenseOptionsPage page) => page.formatKeyword("AS ");
        internal static string Static(this OptionsPages.IntellisenseOptionsPage page) => page.formatKeyword("STATIC");
        internal static string Local(this OptionsPages.IntellisenseOptionsPage page) => page.formatKeyword("LOCAL");
        internal static string Parameter(this OptionsPages.IntellisenseOptionsPage page) => page.formatKeyword("PARAMETER");
        internal static string Usual(this OptionsPages.IntellisenseOptionsPage page) => page.formatKeyword("USUAL");

    }
}

