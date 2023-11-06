//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Collections.Generic;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Package;
using Microsoft.VisualStudio.TextManager.Interop;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio.Editor;
using XSharpModel;
using LanguageService.SyntaxTree;
using Microsoft.VisualStudio.Text;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using System.Diagnostics;
using XSharp.Settings;

namespace XSharp.LanguageService
{
    [Guid(XSharpConstants.guidXSharpLanguageServicePkgString)]
    public class XSharpLanguageService: Microsoft.VisualStudio.Package.LanguageService, IVsLanguageDebugInfo, IVsLanguageTextOps
    {
        private readonly IVsEditorAdaptersFactoryService _editorAdaptersFactoryService;
        private LanguagePreferences m_preferences;

        public XSharpLanguageService(object serviceContainer) : base()
        {
            var componentModel = XSharpLanguagePackage.GetComponentModel();
            _editorAdaptersFactoryService = componentModel.GetService<IVsEditorAdaptersFactoryService>();
            base.SetSite(serviceContainer);
        }

        public override string GetFormatFilterList()
        {
            string[] files = { "Source Files (*.prg)\n*.prg\n",
                               "Alternative Source Files (*.xs)\n*.xs\n",
                               "Preprocessor Output (*.ppo)\n*.ppo\n",
                               "Header files (*.?h)\n*.?h\n"
            };

            return string.Concat(files);
        }

        public override int CurFileExtensionFormat(string fileName)
        {
            return base.CurFileExtensionFormat(fileName);
        }
        public override int GetFileExtensions(out string extensions)
        {
            extensions = ".prg;.vh;.ppo;.xs;.xh;.ch;";
            return VSConstants.S_OK;
        }
        public int GetFormatFilterList(out string pbstrFilterList)
        {
            pbstrFilterList = GetFormatFilterList();
            return VSConstants.S_OK;
        }
        /// <summary>
        /// Set the default preferences for this language.
        /// </summary>
        public override LanguagePreferences GetLanguagePreferences()
        {
            if (m_preferences == null)
            {
                m_preferences = new LanguagePreferences(this.Site,
                                                        typeof(XSharpLanguagePackage).GUID,
                                                        this.Name);
                m_preferences.Init();
            }
            return m_preferences;
        }

        public override IScanner GetScanner(IVsTextLines buffer) => null;
        public override AuthoringScope ParseSource(ParseRequest req) => null;
        public override string Name => XSharpConstants.LanguageName;
        public override ViewFilter CreateViewFilter(CodeWindowManager mgr, IVsTextView newView)
        {
            // still needed for snippets. Once that is moved to MEF then this can disappear
            return new XSharpViewFilter(mgr, newView);
        }

        public int UpdateLanguageContext(uint dwHint, Microsoft.VisualStudio.TextManager.Interop.IVsTextLines pBuffer, Microsoft.VisualStudio.TextManager.Interop.TextSpan[] ptsSelection, object pUC)
        {
            // This called for the online help
            return VSConstants.S_OK;
        }

        public int CurFileExtensionFormat(string bstrFileName, out uint pdwExtnIndex)
        {
            // may be called from the save as dialog
            string ext = System.IO.Path.GetExtension(bstrFileName).ToLower();
            if (ext == ".prg")
                pdwExtnIndex = 0;
            else
                pdwExtnIndex = 1;
            return VSConstants.S_OK;
        }
        public override ExpansionProvider CreateExpansionProvider(Source src)
        {
            // This is called from the New Project and new Item dialogs
            return base.CreateExpansionProvider(src);
        }

        int classcounter = 0;
        public override ExpansionFunction CreateExpansionFunction(ExpansionProvider provider, string functionName)
        {
            ExpansionFunction  function = null;

            if (functionName == "ClassName")
            {
                function = new ClassNameExpansionFunction(provider, ++classcounter);
            }
            else if (functionName == "GenerateSwitchCases")
            {
                function = new GenerateSwitchCasesExpansionFunction(provider);
            }
            else if (functionName == "SimpleTypeName")
            {
                function = new SimpleTypeNameExpansionFunction(provider);
            }
            else if (functionName == "InitProcType")
            {
                function = new InitProcTypeExpansionFunction(provider);
            }

            return function;
        }


        public int QueryInvalidEncoding(uint Format, out string pbstrMessage)
        {
            // may be called when the source is saved under a different codepage
            pbstrMessage = String.Empty;
            return VSConstants.S_OK;
        }

        public override TypeAndMemberDropdownBars CreateDropDownHelper(IVsTextView forView) => null;
        #region IVsLanguageDebugInfo Members

        Guid XSharpLanguage = new Guid(Constants.XSharpLanguageString);

        int IVsLanguageDebugInfo.GetLanguageID(IVsTextBuffer pBuffer, int iLine, int iCol, out Guid pguidLanguageID)
        {
            pguidLanguageID = XSharpLanguage;
            return VSConstants.S_OK;
        }

        int IVsLanguageDebugInfo.GetLocationOfName(string pszName, out string pbstrMkDoc, TextSpan[] pspanLocation)
        {
            pbstrMkDoc = null;
            return VSConstants.E_FAIL;
        }

         int IVsLanguageDebugInfo.GetNameOfLocation(IVsTextBuffer pBuffer, int iLine, int iCol, out string pbstrName, out int piLineOffset)
        {
            pbstrName = "";
            piLineOffset = iCol;
            var file = getFile(pBuffer);
            if (file != null)
            {
                var member = file.FindMemberAtRow(iLine);
                if (member != null)
                {
                    if (member.Kind.IsClassMember(file.Project.Dialect))
                    {
                        pbstrName = member.FullName;
                    }
                    else
                    {
                        pbstrName = member.Name;
                    }
                    piLineOffset = member.Interval.Start;
                    return VSConstants.S_OK;
                }
            }
            return VSConstants.E_FAIL;
        }

        public XFile getFile(IVsTextBuffer pBuffer)
        {
            var buffer = _editorAdaptersFactoryService.GetDataBuffer(pBuffer);
            if (buffer != null)
            {
                if (buffer.Properties.TryGetProperty<XFile>(typeof(XSharpModel.XFile), out var file))
                {
                    return file;
                }
            }
            return null;
        }

        int IVsLanguageDebugInfo.GetProximityExpressions(IVsTextBuffer pBuffer, int iLine, int iCol, int cLines, out IVsEnumBSTR ppEnum)
        {
            ppEnum = null;
            if (XDebuggerSettings.DebuggerMode == DebuggerMode.Running)
            {
                return VSConstants.S_OK;
            }
            var file = getFile(pBuffer);
            var list = new List<String>();
            XSourceEntity member;
            try
            {
                // We use our original syntax here (so SELF and ":"). The expression compiler
                // in the debugger takes care of translating SELF to this and ':' to '.'
                if (file != null)
                {
                    member = file.FindMemberAtRow(iLine);
                    if (member == null)
                    {
                        return VSConstants.S_OK;
                    }
                    if (member.Kind.IsClassMember(file.Project.ParseOptions.Dialect))
                    {
                        if (!member.Modifiers.HasFlag(Modifiers.Static))
                        {
                            list.Add(XLiterals.FormatKeyword("SELF"));
                        }
                    }

                    var buffer = _editorAdaptersFactoryService.GetDataBuffer(pBuffer);
                    var xDocument = buffer.GetDocument();
                    if (xDocument == null)
                        return VSConstants.S_OK;
                    Dictionary<string, IXVariableSymbol> locals = null;
                    if (member != null)
                    {
                        if (member is XSourceMemberSymbol tm)
                        {
                            locals = new Dictionary<string, IXVariableSymbol>(StringComparer.OrdinalIgnoreCase);
                            var location = new XSharpSearchLocation(xDocument, tm.File, tm, buffer.CurrentSnapshot, iLine);
                            var vars = tm.GetLocals(location);
                            foreach (var v in vars)
                            {
                                if (!locals.ContainsKey(v.Name))
                                    locals.Add(v.Name, v);
                            }
                            foreach (var p in tm.Parameters)
                            {
                                if (!locals.ContainsKey(p.Name))
                                    locals.Add(p.Name, p);
                            }
                        }
                        var tokens = new List<IToken>();
                        //iLine is 1 based here...
                        iLine -= 1;
                        for (int i = iLine; i < iLine+cLines;  i++)
                        {
                            tokens.AddRange(xDocument.GetTokensInLineAndFollowing(i));
                        }
                        addtokens(buffer, tokens, list, file, locals);
                    }
                }
            }
            catch (Exception ex)
            {
                Logger.Exception(ex, "Get auto expressions");
            }
            ppEnum = new VsEnumBSTR(list);
            return VSConstants.S_OK;
        }

        private void addtokens(ITextBuffer buffer, List<IToken> tokens, IList<string> list, XFile file, IDictionary<string, IXVariableSymbol> locals)
        {
            try
            {
                string expression = "";
                foreach (var token in tokens)
                {
                    var type = token.Type;
                    switch (type)
                    {
                        case XSharpLexer.ID:
                            if (expression.Length == 0)
                            {
                                expression = token.Text;
                                if (locals != null && locals.TryGetValue(expression, out var local))
                                {
                                    expression = local.Name;
                                }
                            }
                            else
                            {
                                expression += token.Text;
                            }
                            if (!list.Contains(expression))
                            {

                                list.Add(expression);
                            }
                            break;
                        case XSharpLexer.COLON:
                        case XSharpLexer.DOT:
                        case XSharpLexer.SELF:
                        case XSharpLexer.SUPER:
                            expression += token.Text;
                            break;
                        default:
                            if (XSharpLexer.IsLiteral(token.Type))
                            {
                                list.Add(token.Text);
                            }
                            expression = "";
                            break;
                    }
                }
            }
            catch (Exception e)
            {
                Logger.Exception(e, "Add Auto Tokens");
            }
        }

        int IVsLanguageDebugInfo.IsMappedLocation(IVsTextBuffer pBuffer, int iLine, int iCol)
        {
            return VSConstants.S_FALSE;
        }

        int IVsLanguageDebugInfo.ResolveName(string pszName, uint dwFlags, out IVsEnumDebugName ppNames)
        {
            /*if((((RESOLVENAMEFLAGS)dwFlags) & RESOLVENAMEFLAGS.RNF_BREAKPOINT) != 0) {
            // TODO: This should go through the project/analysis and see if we can
            // resolve the names...
            }*/
            ppNames = null;
            return VSConstants.E_FAIL;
        }

        int IVsLanguageDebugInfo.ValidateBreakpointLocation(IVsTextBuffer pBuffer, int iLine, int iCol, TextSpan[] pCodeSpan)
        {
            pCodeSpan[0] = default(TextSpan);
            pCodeSpan[0].iStartLine = iLine;
            pCodeSpan[0].iEndLine = iLine;
            //pBuffer.GetLengthOfLine(iLine, out var len);
            //pCodeSpan[0].iStartIndex = 0;
            //pCodeSpan[0].iEndIndex = len;
            // Returning E_NOTIMPL indicates that this language only supports entire-line breakpoints. Consequently,
            // VS debugger will highlight the entire line when the breakpoint is active and the corresponding option
            // ("Highlight entire source line for breakpoints and current statement") is set. If we returned S_OK,
            // we'd have to handle this ourselves.
            //return VSConstants.S_OK;
            return VSConstants.E_NOTIMPL;
        }
        #endregion
        #region  IVsLanguageTextOps      
        public int GetDataTip(IVsTextLayer pTextLayer, TextSpan[] ptsSel, TextSpan[] ptsTip, out string text)
        {
            text = null;
            return VSConstants.E_NOTIMPL;

        }

        public int GetPairExtent(IVsTextLayer pTextLayer, TextAddress ta, TextSpan[] pts)
        {
            return VSConstants.E_NOTIMPL;
        }

        bool IsWordChar(char c, bool stopAtDelimiter)
        {
            switch (c)
            {
                case ' ':
                case '\t':
                    return false;
                case ':':
                case '.':
                    if (stopAtDelimiter)
                        return false; 
                    break;
                case '_':
                    return true;
                default:
                    if (!char.IsLetterOrDigit(c))
                        return false;
                    break;
            }
            return true;
        }
        public int GetWordExtent(IVsTextLayer pTextLayer, TextAddress ta, WORDEXTFLAGS flags, TextSpan[] pts)
        {
            if (flags.HasFlag(WORDEXTFLAGS.WORDEXT_FINDTOKEN) && this.IsDebugging)
            {
                var callStack = Environment.StackTrace.ToString();
                // when inside quick info in the debugger then we want to return a complete 
                // expression like SELF:SomeProperty as a single word.
                // when not in quick info then we want to select SomeProperty without also selecting
                // SELF and ':'
                var quickinfo = callStack.IndexOf("quickinfo", StringComparison.OrdinalIgnoreCase) > 0;
                // this also works for the last line. Selecting ta.Line+1, 0 will not
                pts[0].iStartLine = pts[0].iEndLine = ta.line;
                var ires = pTextLayer.GetLengthOfLine(ta.line, out int length);
                Debug.Assert(ires == VSConstants.S_OK);
                ires = pTextLayer.GetLineText(ta.line, 0, ta.line , length, out var text);
                Debug.Assert(ires == VSConstants.S_OK);
                if (!string.IsNullOrEmpty(text))
                {
                    var start = Math.Min(ta.index, text.Length - 1);
                    var index = start;
                    // find start token
                    while (index >= 0)
                    {
                        char c = text[index];
                        if (!IsWordChar(c, !quickinfo))
                            break;
                        index -= 1;
                    }
                    pts[0].iStartIndex = index + 1;
                    index = start;
                    while (index < text.Length)
                    {
                        char c = text[index];
                        if (!IsWordChar(c, true))
                            break;
                        index += 1;
                    }
                    pts[0].iEndIndex = index;
                }
                return VSConstants.S_OK;
            }
            return VSConstants.S_FALSE;
        }

        public int Format(IVsTextLayer pTextLayer, TextSpan[] ptsSel)
        {
            return VSConstants.E_NOTIMPL;
        }

        #endregion
    }
    internal class VsEnumBSTR : IVsEnumBSTR
    {
        private readonly IList<string> _values;
        private int _currentIndex;

        public VsEnumBSTR(IList<string> values)
        {
            _values = values;
            _currentIndex = 0;
        }

        public int Clone(out IVsEnumBSTR ppEnum)
        {
            ppEnum = new VsEnumBSTR(_values);
            return VSConstants.S_OK;
        }

        public int GetCount(out uint pceltCount)
        {
            pceltCount = (uint)_values.Count;
            return VSConstants.S_OK;
        }

        public int Next(uint celt, string[] rgelt, out uint pceltFetched)
        {
            var i = 0;
            for (; i < celt && _currentIndex < _values.Count; i++, _currentIndex++)
            {
                rgelt[i] = _values[_currentIndex];
            }

            pceltFetched = (uint)i;
            return i < celt
                ? VSConstants.S_FALSE
                : VSConstants.S_OK;
        }

        public int Reset()
        {
            _currentIndex = 0;
            return VSConstants.S_OK;
        }

        public int Skip(uint celt)
        {
            _currentIndex += (int)celt;
            return _currentIndex < _values.Count
                ? VSConstants.S_OK
                : VSConstants.S_FALSE;
        }
    }

}
