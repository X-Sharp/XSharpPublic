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
using System.Windows.Documents;
using System.Windows.Media;
using XSharpModel;
using System.Windows.Controls;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Text.Adornments;
//using Microsoft.VisualStudio.Text.Adornments;
namespace XSharp.LanguageService
{
    internal class XSharpQuickInfoSource : IQuickInfoSource
    {
        private XSharpQuickInfoSourceProvider _provider;
        private ITextBuffer _subjectBuffer;
        private XSharpModel.XFile _file;
        private IClassificationFormatMapService _formatMap;
        private IClassificationTypeRegistryService _registry;

        public XSharpQuickInfoSource(XSharpQuickInfoSourceProvider provider, ITextBuffer subjectBuffer, IClassificationFormatMapService formatMap, IClassificationTypeRegistryService registry)
        {
            _provider = provider;
            _subjectBuffer = subjectBuffer;
            _file = _subjectBuffer.GetFile();
            _formatMap = formatMap;
            _registry = registry;
        }
        private int lastTriggerPoint = -1;
        private Inline[] lastHelp = null;
        private String lastxmldoc = null;
        private ITrackingSpan lastSpan = null;
        private int lastVersion = -1;





        internal void WriteOutputMessage(string message)
        {
            if (XSettings.EnableQuickInfoLog && XSettings.EnableLogging)
            {
                XSettings.DisplayOutputMessage("XSharp.QuickInfoSource :" + message);
            }
        }

        //static bool skipFirst = true;

        public void AugmentQuickInfoSession(IQuickInfoSession session, IList<object> qiContent, out ITrackingSpan applicableToSpan)
        {
            applicableToSpan = null;
            //if (skipFirst)
            //{
            //    skipFirst = false;
            //    return;
            //}
            //else
            //{
            //    skipFirst = true;
            //}
            if (XSettings.DebuggerIsRunning)
            {
                return;
            }
            try
            {
                XSharpModel.ModelWalker.Suspend();
                if (XSettings.DisableQuickInfo)
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

                // find next delimiter, so we will include the whole word in the search
                var ssp = XSharpTokenTools.FindEndOfCurrentToken(subjectTriggerPoint.Value, currentSnapshot);
                int caretPos = ssp.Position;

                int lineNumber = ssp.GetContainingLine().LineNumber;
                var snapshot = session.TextView.TextBuffer.CurrentSnapshot;
                if (_file == null)
                    return;
                // Then, the corresponding Type/Element if possible
                IToken stopToken;
                //ITokenStream tokenStream;
                XMemberDefinition member = XSharpTokenTools.FindMember(lineNumber, _file);
                XTypeDefinition currentNamespace = XSharpTokenTools.FindNamespace(caretPos, _file);
                // adjust caretpos, for other completions we need to stop before the caret. Now we include the caret
                var tokenList = XSharpTokenTools.GetTokenList(caretPos , lineNumber, tokens.TokenStream, out stopToken, _file, false, member);
                // LookUp for the BaseType, reading the TokenList (From left to right)
                CompletionElement gotoElement;
                string currentNS = "";
                if (currentNamespace != null)
                {
                    currentNS = currentNamespace.Name;
                }

                var cType = XSharpTokenTools.RetrieveType(_file, tokenList, member, currentNS, stopToken, out gotoElement, snapshot, lineNumber, _file.Project.Dialect);
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

                    if (gotoElement.Result != null)
                    {
                        if (gotoElement.Result.Kind == Kind.Constructor)
                        {
                            if (gotoElement.Result.Parent != null)
                            {
                                var xtype = gotoElement.Result.Parent as IXType;
                                var qitm = new QuickInfoTypeAnalysis(xtype, kwFormat.ForegroundBrush, txtFormat.ForegroundBrush);
                                var description = new TextBlock();
                                description.Inlines.AddRange(qitm.WPFDescription);
                                qiContent.Add(description);

                            }
                        }
                        else if (gotoElement.Result is IXMember)
                        {
                            QuickInfoTypeMember qitm = new QuickInfoTypeMember((IXMember)gotoElement.Result, kwFormat.ForegroundBrush, txtFormat.ForegroundBrush);
                            var description = new TextBlock();
                            description.Inlines.AddRange(qitm.WPFDescription);
                            qiContent.Add(description);
                        }
                        else if (gotoElement.Result is IXVariable)
                        {
                            QuickInfoVariable qitm = new QuickInfoVariable((IXVariable)gotoElement.Result, kwFormat.ForegroundBrush, txtFormat.ForegroundBrush);
                            var description = new TextBlock();
                            description.Inlines.AddRange(qitm.WPFDescription);
                            qiContent.Add(description);

                        }
                        else if (gotoElement.Result is IXType)
                        {
                            var xtype = gotoElement.Result as IXType;
                            var qitm = new QuickInfoTypeAnalysis(xtype, kwFormat.ForegroundBrush, txtFormat.ForegroundBrush);
                            var description = new TextBlock();
                            description.Inlines.AddRange(qitm.WPFDescription);
                            qiContent.Add(description);

                        }
                        else
                        {
                            var description = new TextBlock();
                            Run temp;
                            temp = new Run(gotoElement.Result.Description);
                            temp.Foreground = txtFormat.ForegroundBrush;
                            //
                            description.Inlines.Add(temp);
                            qiContent.Add(description);
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
                            WriteOutputMessage($"Found new help content: {description.Text}");
                        }
                    }
                    return;
                }
            }
            catch (Exception ex)
            {
                XSettings.DisplayOutputMessage("XSharpQuickInfo.AugmentQuickInfoSession failed : ");
                XSettings.DisplayException(ex);
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
        [ContentType(XSharpConstants.LanguageName)]
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
                if (XSettings.DebuggerIsRunning)
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
                    XSettings.DisplayOutputMessage("XSharpQuickInfo.OnTextViewMouseHover failed");
                    XSettings.DisplayException(ex);
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
        [ContentType(XSharpConstants.LanguageName)]
        internal class XSharpQuickInfoControllerProvider : IIntellisenseControllerProvider
        {

            [Import]
            internal IQuickInfoBroker QuickInfoBroker { get; set; }

            public IIntellisenseController TryCreateIntellisenseController(ITextView textView, IList<ITextBuffer> subjectBuffers)
            {
                return new XSharpQuickInfoController(textView, subjectBuffers, this);
            }
        }

        internal class QuickInfoTypeAnalysis : TypeAnalysis
        {
            IXType _type;

            internal QuickInfoTypeAnalysis(IXType type, Brush fg, Brush txt) : base(type)
            {
                QuickInfoHelpers.kwBrush = fg;
                QuickInfoHelpers.txtBrush = txt;
                _type = type;
            }
            public List<Inline> WPFDescription
            {
                get
                {
                    List<Inline> content = new List<Inline>();

                    if (this.Type.Modifiers != XSharpModel.Modifiers.None)
                    {
                        content.addKeyword(XSettings.FormatKeyword(this.Type.ModifiersKeyword) + " ");
                    }
                    content.addKeyword(XSettings.FormatKeyword(this.Type.VisibilityKeyword) + " ");
                    //
                    if (this.IsStatic)
                    {
                        content.addKeyword(XSettings.FormatKeyword("STATIC "));
                    }
                    //
                    if (this.Type.Kind != XSharpModel.Kind.Field)
                    {
                        content.addKeyword(XSettings.FormatKeyword(this.Type.KindKeyword) + " ");
                    }
                    //
                    content.addText(this.Prototype);

                    //
                    string returns;
                    string remarks;
                    var xmldesc = XSharpXMLDocMember.GetTypeSummary(this._type, null, out returns, out remarks);
                    content.addSummary(xmldesc);
                    content.addReturns(returns);
                    content.addRemarks(remarks);
                    content.addLocation( _type.Location);
                    return content;
                }

            }
        }

        internal class QuickInfoMemberAnalysis : MemberAnalysis
        {
            internal QuickInfoMemberAnalysis(IXMember member, Brush kw, Brush txt) : base(member)
            {
                QuickInfoHelpers.kwBrush = kw;
                QuickInfoHelpers.txtBrush = txt;
            }

            public List<Inline> WPFDescription
            {
                get
                {
                    List<Inline> content = new List<Inline>();

                    if (this.Member.Modifiers != XSharpModel.Modifiers.None)
                    {
                        content.addKeyword(XSettings.FormatKeyword(this.Member.ModifiersKeyword) + " ");
                    }
                    content.addKeyword(XSettings.FormatKeyword(this.Member.VisibilityKeyword) + " ");
                    //
                    if ((this.Member.IsStatic) && ((this.Member.Kind != Kind.Function) && (this.Member.Kind != Kind.Procedure)))
                    {
                        content.addKeyword(XSettings.FormatKeyword("STATIC "));
                    }
                    //
                    if ((this.Member.Kind != XSharpModel.Kind.Field) && (this.Member.Kind != XSharpModel.Kind.Constructor))
                    {
                        content.addKeyword(XSettings.FormatKeyword(this.Member.KindKeyword) + " ");
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
                    if (this.Member.Kind.HasParameters())
                    {
                        content.addText(this.Name);
                        content.addKeyword(this.Member.Kind == XSharpModel.Kind.Constructor ? "{" : "(");
                        bool first = true;
                        foreach (var var in this.Member.Parameters)
                        {
                            if (! first)
                            {
                                content.addText(", ");
                            }
                            first = false;
                            content.addText(var.Name + " ");
                            content.addKeyword(var.ParamTypeDesc + " ");
                            content.addKeyword(var.TypeName);
                        }
                        content.addKeyword(this.Member.Kind == XSharpModel.Kind.Constructor ? "}" : ")");
                    }
                    //
                    if (!String.IsNullOrEmpty(this.Value))
                    {
                        content.addText(" := " + this.Value);
                    }
                    if (this.Member.Kind.HasReturnType())
                    {
                        content.addReturnType(this.TypeName);
                    }
                    return content;
                }
            }
        }

        internal class QuickInfoBase
        {
            internal QuickInfoBase(Brush kw, Brush txt)
            {
                QuickInfoHelpers.kwBrush = kw;
                QuickInfoHelpers.txtBrush = txt;
            }

            protected void addVarInfo(List<Inline> list, IXVariable var, out int len)
            {
                var name = var.Name;
                len = 0;
                var hasValue = !string.IsNullOrEmpty(var.Value);
                if (var.Kind == Kind.DbField)
                {
                    if (hasValue)
                    {
                        name = var.Value + "->" + name;
                    }
                }
                len += name.Length + 1;
                list.addText(name + " ");
                if (hasValue && var.Kind != Kind.DbField) // default value
                {
                    var text = " :=  " + var.Value + " ";
                    list.addText(text);
                    len += text.Length;

                }
                list.addPair(var.ParamTypeDesc + " ", var.TypeName);
                len += var.ParamTypeDesc.Length + 1;
                len += var.TypeName.Length;
                if (var.IsArray)
                {
                    list.addText("[] ");
                    len += 2;
                }
            }

        }

        internal class QuickInfoTypeMember : QuickInfoBase
        {
            IXMember typeMember;

            internal QuickInfoTypeMember(IXMember tm, Brush kw, Brush txt) : base(kw, txt)
            {
                this.typeMember = tm;
            }

            public List<Inline> WPFDescription
            {
                get
                {
                    List<Inline> content = new List<Inline>();

                    string text;
                    int len = 0;
                    if (this.typeMember.Modifiers != Modifiers.None)
                    {
                        text = XSettings.FormatKeyword(this.typeMember.ModifiersKeyword) + " ";
                        content.addKeyword(text);
                        len += text.Length;
                    }
                    text = XSettings.FormatKeyword(this.typeMember.VisibilityKeyword) + " ";
                    len += text.Length;
                    content.addKeyword(text);
                    //
                    if (this.typeMember.Kind != XSharpModel.Kind.Field)
                    {
                        text = XSettings.FormatKeyword(this.typeMember.KindKeyword) + " ";
                        len += text.Length;
                        content.addKeyword(text);
                    }
                    //
                    content.AddRange(this.WPFPrototype(len));
                    //
                    return content;
                }

            }

            private void checkLen(List<Inline> elements, ref int len)
            {
                if (len > 80)
                {
                    // New line starts with indent
                    elements.addText("\r\t");
                    len = 0;
                }
            }


            public List<Inline> WPFPrototype(int len)
            {
                List<Inline> content = new List<Inline>();
                content.addText(this.typeMember.Name);
                if (this.typeMember.Kind.HasParameters())
                {
                    content.addKeyword(this.typeMember.Kind == XSharpModel.Kind.Constructor ? "{" : "(");
                    len += 1;
                    bool first = true;
                    foreach (var var in this.typeMember.Parameters)
                    {
                        if (!first)
                        {
                            content.addText(", ");
                            len += 2;
                        }
                        first = false;
                        checkLen(content, ref len);
                        int varlen;
                        addVarInfo(content, var, out varlen);
                        len += varlen;
                    }
                    content.addKeyword(this.typeMember.Kind == XSharpModel.Kind.Constructor ? "}" : ")");
                }
                //
                //
                if (!String.IsNullOrEmpty(this.typeMember.Value))
                {
                    var text = " := " + this.typeMember.Value;
                    len += text.Length;
                    checkLen(content, ref len);
                    if (len == 0)
                    {
                        len = text.Length;
                    }
                    content.addText(text);
                }
                if (this.typeMember.Kind.HasReturnType() && !String.IsNullOrEmpty(this.typeMember.TypeName))
                {
                    len += this.typeMember.TypeName.Length + 4;
                    checkLen(content, ref len);
                    content.addReturnType(typeMember.TypeName);
                }

                string returns;
                string remarks;
                var xmldesc = XSharpXMLDocMember.GetMemberSummary(this.typeMember, null, out returns, out remarks);
                content.addSummary(xmldesc);
                content.addReturns(returns);
                content.addRemarks(remarks);
                content.addLocation(typeMember.Location);
                return content;
            }

        }


        internal class QuickInfoVariable : QuickInfoBase
        {
            IXVariable xVar;

            internal QuickInfoVariable(IXVariable var, Brush kw, Brush txt) : base(kw, txt)
            {
                this.xVar = var;
            }

            public List<Inline> WPFDescription
            {
                get
                {
                    List<Inline> content = new List<Inline>();
                    int len;
                    var kind = xVar.Kind.ToString();
                    if (xVar.Kind == Kind.DbField)
                        kind = "Field";
                    content.addKeyword( XSettings.FormatKeyword(kind + " "));
                    addVarInfo(content, xVar, out len);
                    return content;
                }

            }

        }
    }
    static class QuickInfoHelpers
    {
        static internal Brush kwBrush { get; set; }
        static internal Brush txtBrush { get; set; }
        static string replaceCRLF(string text)
        {
            text = text.Replace("\r", "\r\t");
            return text;
        }
        static internal void addKeyword(this List<Inline> content, string kw)
        {
            var temp = new Run(kw);
            temp.Foreground = kwBrush;
            content.Add(temp);
        }
        static internal void addText(this List<Inline> content, string kw)
        {
            var temp = new Run(kw);
            temp.Foreground = txtBrush;
            content.Add(temp);
        }
        static internal void addPair(this List<Inline> content, string kw, string text)
        {
            // add pair of KW - Text
            addKeyword(content, kw);
            addText(content, text);
        }
        static internal void addRemarks(this List<Inline> content, string remarks)
        {
            if (!string.IsNullOrEmpty(remarks))
            {
                addPair(content, "\rRemarks:", " " + replaceCRLF(remarks));

            }
        }
        static internal void addReturns(this List<Inline> content, string returns)
        {
            if (!string.IsNullOrEmpty(returns))
            {
                addPair(content, "\rReturns:", " " + replaceCRLF(returns));
            }
        }
        static internal void addSummary(this List<Inline> content, string xmldesc)
        {
            if (!String.IsNullOrEmpty(xmldesc))
            {
                content.addPair("\rSummary:", " " + replaceCRLF(xmldesc));
            }
        }
        static internal void addLocation(this List<Inline> content, string location)
        {
            if (!String.IsNullOrEmpty(location))
            {
                content.addPair("\r\rLocation:", " " + location);
            }
        }
        static internal void addReturnType(this List<Inline> content, string typeName)
        {
            content.addPair(" " + XSettings.FormatKeyword("AS "), typeName);
        }
    }
}

