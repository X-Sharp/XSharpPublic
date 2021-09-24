//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Core.Imaging;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Adornments;
using Microsoft.VisualStudio.Utilities;
using Microsoft.VisualStudio.Imaging;
using System.Windows.Documents;
using System.Windows.Media;
using XSharpModel;
using System.Windows.Controls;
using System.Threading;
using Microsoft.VisualStudio.Language.StandardClassification;
using System.Threading.Tasks;
using LanguageService.CodeAnalysis.XSharp;
using Microsoft.VisualStudio.Imaging.Interop;

namespace XSharp.LanguageService
{
    internal class XSharpQuickInfoSource : IAsyncQuickInfoSource
    {
        private XSharpQuickInfoSourceProvider _provider;
        private ITextBuffer _textBuffer;
        private static readonly ImageId _icon = KnownMonikers.Class.ToImageId();
        private XFile _file;


        public XSharpQuickInfoSource(XSharpQuickInfoSourceProvider provider, ITextBuffer textBuffer, XFile file)
        {
            _provider = provider;
            _textBuffer = textBuffer;
            _file = file;
        }

        internal void WriteOutputMessage(string message)
        {
            if (XSettings.EnableQuickInfoLog && XSettings.EnableLogging)
            {
                XSettings.DisplayOutputMessage("XSharp.QuickInfoSource :" + message);
            }
        }

        //static bool skipFirst = true;

        public async Task<QuickInfoItem> GetQuickInfoItemAsync(IAsyncQuickInfoSession session, CancellationToken cancellationToken)
        {
            if (XSettings.DebuggerIsRunning || XSettings.DisableQuickInfo)
            {
                await session.DismissAsync();
                return null;
            }
            var triggerPoint = session.GetTriggerPoint(_textBuffer.CurrentSnapshot);
            if (triggerPoint == null)
            {
                await session.DismissAsync();
                return null;
            }
            try
            {
                ModelWalker.Suspend();
                var ssp = triggerPoint.Value;
                // Map the trigger point down to our buffer.
                ITextSnapshot currentSnapshot = ssp.Snapshot;
                bool abort = false;
                var tokens = _textBuffer.GetTokens();
                if (cancellationToken.IsCancellationRequested)
                    return null;
                if (! abort)
                {
                    WriteOutputMessage($"Triggerpoint: {triggerPoint.Value.Position}");
                    // We don't want to lex the buffer. So get the tokens from the last lex run
                    // and when these are too old, then simply bail out
                    abort = tokens == null || tokens.SnapShot.Version != currentSnapshot.Version;
                }
                if (abort)
                {
                    await session.DismissAsync();
                    return null;
                }
                if (cancellationToken.IsCancellationRequested)
                    return null;
                var location = _textBuffer.FindLocation(ssp);
                CompletionState state;
                var tokenList = XSharpTokenTools.GetTokensUnderCursor(location, tokens.TokenStream, out state);
                // LookUp for the BaseType, reading the TokenList (From left to right)
                if (cancellationToken.IsCancellationRequested)
                    return null;
                var lookupresult = new List<IXSymbol>();
                lookupresult.AddRange(XSharpLookup.RetrieveElement(location, tokenList, state,out var notProcessed,true));

                //
                if (lookupresult.Count > 0)
                {
                    var element = lookupresult[0];
                    var qiContent = new List<object>();

                        if (element.Kind == Kind.Constructor)
                        {
                            if (element.Parent != null)
                            {
                                var xtype = element.Parent as IXTypeSymbol;
                                var qitm = new XTypeAnalysis(xtype);
                                AddImage(qiContent, qitm.Image);
                                var description = new ClassifiedTextElement(qitm.WPFDescription);
                                qiContent.Add(description);

                            }
                        }
                        else if (element is IXMemberSymbol mem)
                        {
                            QuickInfoTypeMember qitm = new QuickInfoTypeMember(mem);
                            AddImage(qiContent, qitm.Image);
                            var description = new ClassifiedTextElement(qitm.WPFDescription);
                            qiContent.Add(description);
                        }
                        else if (element is IXVariableSymbol var)
                        {
                            QuickInfoVariable qitm = new QuickInfoVariable(var);
                            AddImage(qiContent, qitm.Image);
                            var description = new ClassifiedTextElement(qitm.WPFDescription);
                            qiContent.Add(description);

                        }
                        else if (element is IXTypeSymbol xtype)
                        {
                            var qitm = new XTypeAnalysis(xtype);
                            AddImage(qiContent, qitm.Image);
                            var description = new ClassifiedTextElement(qitm.WPFDescription);
                            qiContent.Add(description);
                        }
                        else
                        {
                            var qitm = new XAnalysis(element);
                            AddImage(qiContent, qitm.Image);
                            var description = new ClassifiedTextElement(qitm.WPFDescription);
                            qiContent.Add(description);
                    }
                    if (cancellationToken.IsCancellationRequested)
                        return null;
                    var result = new ContainerElement(ContainerElementStyle.Wrapped, qiContent);
                    var line = ssp.GetContainingLine();
                    var lineSpan = _textBuffer.CurrentSnapshot.CreateTrackingSpan(line.Extent, SpanTrackingMode.EdgeInclusive);

                    return new QuickInfoItem(lineSpan, result);
                }
            }
            catch (Exception ex)
            {
                XSettings.DisplayOutputMessage("XSharpQuickInfo.AugmentQuickInfoSession failed : ");
                XSettings.DisplayException(ex);
            }
            finally
            {
                ModelWalker.Resume();
            }
            await session.DismissAsync();
            return null;
        }

        private void AddImage( List<object> qiContent, ImageMoniker image)
        {
            if (image.Id != KnownMonikers.None.Id)
            {
                qiContent.Add(new ImageElement(image.ToImageId()));
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

        [Export(typeof(IAsyncQuickInfoSourceProvider))]
        [Name("XSharp QuickInfo Source")]
        [Order]
        [ContentType(XSharpConstants.LanguageName)]
        internal class XSharpQuickInfoSourceProvider : IAsyncQuickInfoSourceProvider
        {

            public IAsyncQuickInfoSource TryCreateQuickInfoSource(ITextBuffer textBuffer)
            {
                var file = textBuffer.GetFile();
                if (file == null || file.XFileType != XFileType.SourceCode)
                    return null;
                return new XSharpQuickInfoSource(this, textBuffer, file);
            }


        }
        internal class QuickInfoBase
        {
            internal QuickInfoBase()
            {
            }

            protected void addVarInfo(List<ClassifiedTextRun> list, IXVariableSymbol var, out int len)
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
                if (var is IXParameterSymbol xps)
                {
                    list.addPair(xps.ParamTypeDesc + " ", var.TypeName);
                    len += xps.ParamTypeDesc.Length + 1;
                }
                else if (var is XSourceVariableSymbol xsvs)
                {
                    list.addPair(xsvs.LocalTypeDesc + " ", var.TypeName);
                    len += xsvs.LocalTypeDesc.Length + 1;
                }
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
            IXMemberSymbol typeMember;

            internal ImageMoniker Image
            {
                get
                {
                    switch (typeMember.Kind)
                    {
                        case Kind.Constructor:
                        case Kind.Destructor:
                        case Kind.Method:
                        case Kind.Function:
                        case Kind.Procedure:
                        case Kind.LocalFunc:
                        case Kind.LocalProc:
                            return KnownMonikers.Method;
                        case Kind.Access:
                        case Kind.Assign:
                        case Kind.Property:
                            return KnownMonikers.Property;
                        case Kind.Event:
                            return KnownMonikers.Event;
                        case Kind.Operator:
                            return KnownMonikers.Operator;
                        case Kind.VOGlobal:
                            return KnownMonikers.GlobalVariable;
                        case Kind.Field:
                            return KnownMonikers.Field;
                        case Kind.EnumMember:
                            return KnownMonikers.EnumerationItemPublic;
                        case Kind.VODefine:
                            return KnownMonikers.Constant;
                        case Kind.Parameter:
                            return KnownMonikers.Parameter;
                        case Kind.Local:
                        case Kind.MemVar:
                            return KnownMonikers.LocalVariable;
                    }
                    return KnownMonikers.None;
                }
            }
            internal QuickInfoTypeMember(IXMemberSymbol tm) : base()
            {
                this.typeMember = tm;
            }

            public ClassifiedTextRun[] WPFDescription
            {
                get
                {
                    var content = new List<ClassifiedTextRun>();

                    string text;
                    if (this.typeMember.Modifiers != Modifiers.None )
                    {
                        text = XSettings.FormatKeyword(this.typeMember.ModifiersKeyword) + " ";
                        content.addKeyword(text);
                    }
                    if (!this.typeMember.Kind.IsPPSymbol())
                    {
                        text = XSettings.FormatKeyword(this.typeMember.VisibilityKeyword) + " ";
                        content.addKeyword(text);
                    }
                    //
                    if (this.typeMember.Kind != XSharpModel.Kind.Field)
                    {
                        text = XSettings.FormatKeyword(this.typeMember.KindKeyword) + " ";
                        content.addKeyword(text);
                    }
                    //
                    content.AddRange(this.WPFPrototype());
                    //
                    return content.ToArray();
                }

            }

            private void checkLen(List<ClassifiedTextRun> elements, ref int len)
            {
                //if (len > 80)
                //{
                //    // New line starts with indent
                //    elements.addText("\r\t");
                //    len = 0;
                //}
            }


            public ClassifiedTextRun[] WPFPrototype()
            {
                var content = new List<ClassifiedTextRun>();
                string name = "";
                if (!this.typeMember.Kind.IsGlobalTypeMember())
                {
                    name = this.typeMember.Parent.Name;
                    var pos = name.IndexOf("<");
                    {
                        if (pos > 0)
                        {
                            name = name.Substring(0, pos);
                        }
                    }
                    if (this.typeMember.IsStatic)
                        name += ".";
                    else
                        name += ":";
                }
                name += this.typeMember.Name;
                content.addText(name);
                if (this.typeMember.Kind.HasParameters())
                {
                    content.addKeyword(this.typeMember.Kind == XSharpModel.Kind.Constructor ? "{" : "(");
                    bool first = true;
                    foreach (var var in this.typeMember.Parameters)
                    {
                        if (!first)
                        {
                            content.addText(", ");
                        }
                        first = false;
                        int varlen;
                        addVarInfo(content, var, out varlen);
                    }
                    content.addKeyword(this.typeMember.Kind == XSharpModel.Kind.Constructor ? "}" : ")");
                }
                //
                //
                if (!String.IsNullOrEmpty(this.typeMember.Value))
                {
                    var text = " := " + this.typeMember.Value;
                    content.addText(text);
                }
                if (this.typeMember.Kind.HasReturnType() && !String.IsNullOrEmpty(this.typeMember.TypeName))
                {
                    content.addReturnType(typeMember.TypeName);
                }

                string returns;
                string remarks;
                var xmldesc = XSharpXMLDocMember.GetMemberSummary(this.typeMember, null, out returns, out remarks);
                content.addSummary(xmldesc);
                content.addReturns(returns);
                content.addRemarks(remarks);
                content.addLocation(typeMember.Location);
                return content.ToArray();
            }

        }
        internal class QuickInfoVariable : QuickInfoBase
        {
            IXVariableSymbol xVar;

            internal ImageMoniker Image
            {
                get
                {
                    switch (xVar.Kind)
                    {
                        case Kind.Parameter:
                            return KnownMonikers.Parameter;
                        case Kind.Local:
                        case Kind.MemVar:
                            return KnownMonikers.LocalVariable;
                    }
                    return KnownMonikers.None;
                }
            }
            internal QuickInfoVariable(IXVariableSymbol var) : base()
            {
                this.xVar = var;
            }

            public ClassifiedTextRun[] WPFDescription
            {
                get
                {
                    var content = new List<ClassifiedTextRun>();
                    var kind = xVar.Kind.ToString();
                    if (xVar is XSourceImpliedVariableSymbol impvar)
                    {
                        switch (impvar.ImpliedKind)
                        {
                            case ImpliedKind.InCollection:
                                kind = "ForEach VAR";
                                break;
                            case ImpliedKind.LoopCounter:
                                kind = "For VAR";
                                break;
                            case ImpliedKind.Using:
                                kind = "Using VAR";
                                break;
                            case ImpliedKind.OutParam:
                                kind = "Out VAR";
                                break;
                            case ImpliedKind.TypeCheck:
                                kind = "IS VAR";
                                break;
                            case ImpliedKind.None:
                            case ImpliedKind.Assignment:
                            default:
                                kind = "VAR";
                                break;
                        }
                    }
                    if (xVar.Kind == Kind.DbField)
                        kind = "Field";
                    content.addKeyword(XSettings.FormatKeyword(kind + " "));
                    addVarInfo(content, xVar, out _);
                    return content.ToArray();
                }

            }

        }

    }
    static class QuickInfoHelpers
    {
        static string replaceCRLF(string text)
        {
            text = text.Replace("\r", "\r\t");
            return text;
        }
        static internal void addKeyword(this List<ClassifiedTextRun> content, string kw)
        {
            var temp = new ClassifiedTextRun(PredefinedClassificationTypeNames.Keyword, kw);
            content.Add(temp);
        }
        static internal void addText(this List<ClassifiedTextRun> content, string kw)
        {
            var temp = new ClassifiedTextRun(PredefinedClassificationTypeNames.Identifier, kw);
            content.Add(temp);
        }
        static internal void addPair(this List<ClassifiedTextRun> content, string kw, string text)
        {
            // add pair of KW - Text
            addKeyword(content, kw);
            if (text.IsXSharpTypeName())
            {
                addKeyword(content, text);
            }
            else
            {
                addText(content, text);
            }
        }
        static internal void addRemarks(this List<ClassifiedTextRun> content, string remarks)
        {
            if (!string.IsNullOrEmpty(remarks))
            {
                addPair(content, "\rRemarks:", " " + replaceCRLF(remarks));

            }
        }
        static internal void addReturns(this List<ClassifiedTextRun> content, string returns)
        {
            if (!string.IsNullOrEmpty(returns))
            {
                addPair(content, "\rReturns:", " " + replaceCRLF(returns));
            }
        }
        static internal void addSummary(this List<ClassifiedTextRun> content, string xmldesc)
        {
            if (!String.IsNullOrEmpty(xmldesc))
            {
                content.addPair("\rSummary:", " " + replaceCRLF(xmldesc));
            }
        }
        static internal void addLocation(this List<ClassifiedTextRun> content, string location)
        {
            if (!String.IsNullOrEmpty(location))
            {
                content.addPair("\rLocation:", " " + location);
            }
        }
        static internal void addReturnType(this List<ClassifiedTextRun> content, string typeName)
        {
            content.addPair(" " + XSettings.FormatKeyword("AS "), typeName);
        }
    }

}


