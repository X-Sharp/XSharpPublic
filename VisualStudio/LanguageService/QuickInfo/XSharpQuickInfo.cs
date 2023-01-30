//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using LanguageService.CodeAnalysis;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using Microsoft.VisualStudio.Core.Imaging;
using Microsoft.VisualStudio.Imaging;
using Microsoft.VisualStudio.Imaging.Interop;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Language.StandardClassification;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Adornments;
using Microsoft.VisualStudio.Utilities;
using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using XSharpModel;

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
                XSettings.LogMessage("XSharp.QuickInfoSource :" + message);
            }
        }

        //static bool skipFirst = true;

        public async Task<QuickInfoItem> GetQuickInfoItemAsync(IAsyncQuickInfoSession session, CancellationToken cancellationToken)
        {
            if (XSettings.DebuggerIsRunning || XEditorSettings.DisableQuickInfo)
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
                var document = _textBuffer.GetDocument();
                if (document == null)
                    return null;
                if (cancellationToken.IsCancellationRequested)
                    return null;
                if (!abort)
                {
                    WriteOutputMessage($"Triggerpoint: {triggerPoint.Value.Position}");
                    // We don't want to lex the buffer. So get the tokens from the last lex run
                    // and when these are too old, then simply bail out
                    abort = document == null || document.SnapShot.Version != currentSnapshot.Version;
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
                var tokenList = XSharpTokenTools.GetTokensUnderCursor(location, out state);
                // LookUp for the BaseType, reading the TokenList (From left to right)
                if (cancellationToken.IsCancellationRequested)
                    return null;
                var lookupresult = new List<IXSymbol>();
                lookupresult.AddRange(XSharpLookup.RetrieveElement(location, tokenList, state));
                var lastToken = tokenList.LastOrDefault();
                //
                if (lookupresult.Count > 0)
                {
                    var element = lookupresult[0];
                    if (element.Name == location.Member.Name)
                    {
                        if (location.LineNumber == location.Member.FirstSourceLine(location.Document))
                            element = location.Member;
                    }
                    var lineTokens = document.GetTokensInLine(lastToken.Line - 1);
                    if (element is XSourceUndeclaredVariableSymbol ||
                        lineTokens.First()?.Type == XSharpLexer.UDC_KEYWORD)
                    {
                        // check to see if the line where the element comes from starts with a UDC keyword
                        var firstToken = lineTokens[0];
                        if (firstToken.Type == XSharpLexer.UDC_KEYWORD)
                        {
                            element = new XSourceEntity(firstToken.Text, Kind.Command);
                        }
                    }
                    if (state == CompletionState.Constructors && element is IXTypeSymbol ixtype)
                    {
                        // when the cursor is before a "{" then show the constructor and not the type
                        var ctors = ixtype.GetConstructors();
                        if (ctors.Length > 0)
                        {
                            element = ctors[0];
                        }
                    }
                    var qiContent = new List<object>();

                    if (element.Kind == Kind.Constructor && lastToken?.Type != XSharpLexer.LCURLY &&
                        lastToken?.Type != XSharpLexer.CONSTRUCTOR && lastToken?.Type != XSharpLexer.LPAREN)
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
                XSettings.LogException(ex, "XSharpQuickInfo.AugmentQuickInfoSession failed : ");
            }
            finally
            {
                ModelWalker.Resume();
            }
            await session.DismissAsync();
            return null;
        }

        private ImageElement GetImage(ImageMoniker image)
        {
            return new ImageElement(image.ToImageId());
        }

        private void AddImage(List<object> qiContent, ImageMoniker image)
        {
            if (image.Id != KnownMonikers.None.Id)
            {
                qiContent.Add(GetImage(image));
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
            readonly IXSymbol _symbol;
            internal QuickInfoBase(IXSymbol symbol)
            {
                _symbol = symbol;
            }

            internal ImageMoniker Image
            {
                get
                {
                    switch (_symbol.Kind)
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
                    return default;
                }
            }


            protected void addVarInfo(List<ClassifiedTextRun> list, IXVariableSymbol var)
            {
                var name = var.Name;
                var hasValue = !string.IsNullOrEmpty(var.Value);
                if (hasValue && var.Kind == Kind.DbField)
                {
                    name = var.Value + "->" + name;
                }
                list.addText(name + " ");
                if (hasValue && var.Kind != Kind.DbField) // default value
                {
                    var text = " :=  " + var.Value + " ";
                    list.addText(text);

                }
                if (var is IXParameterSymbol xps)
                {
                    if (var.Kind == Kind.Parameter)
                        list.addPair(xps.ParamTypeDesc + " ", var.TypeName.GetXSharpTypeName());
                }
                else if (var is XSourceVariableSymbol xsvs)
                {
                    if (var.Kind == Kind.Undeclared)
                    {

                    }
                    else
                    {
                        list.addPair(xsvs.LocalTypeDesc + " ", var.TypeName.GetXSharpTypeName());
                    }
                }
                if (var.IsArray && !var.TypeName.EndsWith("]"))
                {
                    list.addText("[] ");
                }
            }

        }

        internal class QuickInfoTypeMember : QuickInfoBase
        {
            readonly IXMemberSymbol typeMember;

            internal QuickInfoTypeMember(IXMemberSymbol tm) : base(tm)
            {
                this.typeMember = tm;
            }

            public ClassifiedTextRun[] WPFDescription
            {
                get
                {
                    var content = new List<ClassifiedTextRun>();

                    string text;
                    if (this.typeMember.Modifiers != Modifiers.None)
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

            public ClassifiedTextRun[] WPFPrototype()
            {
                var content = new List<ClassifiedTextRun>();
                string name = "";
                if (!this.typeMember.Kind.IsGlobalTypeMember())
                {
                    name = this.typeMember.Parent.Name;
                    if (typeMember.IsExtension && typeMember is XPEMemberSymbol pesym)
                    {
                        name = pesym.DeclaringTypeSym.Name;
                    }
                    var pos = name.IndexOfAny(new char[] { '`', '<' });
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
                if (this.typeMember.Kind.HasParameters() && !this.typeMember.Kind.IsProperty())
                {
                    var isExt = typeMember.IsExtension;
                    content.addKeyword(this.typeMember.Kind == XSharpModel.Kind.Constructor ? "{" : "(");
                    bool first = true;
                    foreach (var var in this.typeMember.Parameters)
                    {
                        if (!first)
                        {
                            content.addText(", ");
                        }
                        if (isExt)
                        {
                            content.addKeyword("SELF ");
                            isExt = false;
                        }
                        first = false;
                        addVarInfo(content, var);
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
                    content.addReturnType(typeMember.TypeName.GetXSharpTypeName());
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
            readonly IXVariableSymbol xVar;

            internal QuickInfoVariable(IXVariableSymbol var) : base(var)
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
                    addVarInfo(content, xVar);
                    content.addLocation(xVar.Location);
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
        static internal void addWs(this List<ClassifiedTextRun> content)
        {
            var temp = new ClassifiedTextRun(PredefinedClassificationTypeNames.WhiteSpace, " ");
            content.Add(temp);

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
                if (location == XSolution.BuiltInFunctions)
                    content.addPair("\rLocation:", " Built-in X# functions");
                else
                    content.addPair("\rLocation:", " " + location);
            }
        }
        static internal void addReturnType(this List<ClassifiedTextRun> content, string typeName)
        {
            content.addPair(" " + XSettings.FormatKeyword("AS "), typeName.GetXSharpTypeName());
        }

        static internal int FirstSourceLine(this XSourceSymbol member, XDocument doc)
        {
            var range = member.Range;
            var firstSource = range.StartLine - 1;
            for (int i = firstSource; i < range.EndLine; i++)
            {
                doc.LineState.Get(i, out var flags);
                if (flags.HasFlag(LineFlags.DocComments))
                    firstSource = i + 1;
                else
                    break;
            }
            return firstSource;
        }
    }

}


