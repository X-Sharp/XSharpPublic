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
using XSharp.Project;
using Microsoft.VisualStudio.Editor;
using System.ComponentModel.Design;
using XSharpModel;
using LanguageService.SyntaxTree;
using XSharpLanguage;
using Microsoft.VisualStudio.Text;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using Microsoft.VisualStudio.ComponentModelHost;

namespace XSharp.LanguageService
{
    [Guid(GuidStrings.guidXSharpLanguageServicePkgString)]
    public class XSharpLanguageService : Microsoft.VisualStudio.Package.LanguageService, IVsLanguageDebugInfo
    {
        private readonly IServiceContainer _serviceContainer;
        private readonly IComponentModel _componentModel;
        private readonly IVsEditorAdaptersFactoryService _editorAdaptersFactoryService;
        private LanguagePreferences m_preferences;
        private Project.OptionsPages.IntellisenseOptionsPage _optionsPage;
        //private XSharpScanner m_scanner;

        public XSharpLanguageService(IServiceContainer serviceContainer) : base()
        {
            _serviceContainer = serviceContainer;
            _componentModel = XSharpProjectPackage.GetComponentModel();
            _editorAdaptersFactoryService = _componentModel.GetService<IVsEditorAdaptersFactoryService>();
            base.SetSite(serviceContainer);
            _optionsPage = XSharpProjectPackage.Instance.GetIntellisenseOptionsPage();

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
        public override LanguagePreferences GetLanguagePreferences()
        {
            if (m_preferences == null)
            {
                m_preferences = new LanguagePreferences(this.Site,
                                                        typeof(XSharpLanguageService).GUID,
                                                        this.Name);
                m_preferences.Init();
            }
            return m_preferences;
        }

        public override IScanner GetScanner(IVsTextLines buffer)
        {
            // no needed since we use MEF for this
            return null;
        }

        public override string Name
        {
            get { return "XSharp"; }
        }

        public override AuthoringScope ParseSource(ParseRequest req)
        {
            // no needed since we use MEF for this
            return null;
        }
        public override ViewFilter CreateViewFilter(CodeWindowManager mgr, IVsTextView newView)
        {
            // still needed for snippets. Once that is moved to MEF then this can disappear
            return new XSharpViewFilter(mgr, newView);
        }

        public override Source CreateSource(IVsTextLines buffer)
        {
            // Used for commenting. Once that is moved to MEF then this can disappear
            XSharpSource src = new XSharpSource(this, buffer, GetColorizer(buffer));
            return src;
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

        public override TypeAndMemberDropdownBars CreateDropDownHelper(IVsTextView forView)
        {
            return new XSharpTypeAndMemberDropDownBars( this, forView );
        }
       #region IVsLanguageDebugInfo Members

        int IVsLanguageDebugInfo.GetLanguageID(IVsTextBuffer pBuffer, int iLine, int iCol, out Guid pguidLanguageID)
        {
            pguidLanguageID = Guid.Empty;
            return VSConstants.S_OK;
        }

        int IVsLanguageDebugInfo.GetLocationOfName(string pszName, out string pbstrMkDoc, TextSpan[] pspanLocation)
        {
            pbstrMkDoc = null;
            return VSConstants.E_FAIL;
        }

         int IVsLanguageDebugInfo.GetNameOfLocation(IVsTextBuffer pBuffer, int iLine, int iCol, out string pbstrName, out int piLineOffset)
        {
            var file = getFile(pBuffer);

            pbstrName = "";
            piLineOffset = iCol;
            if (file != null)
            {
                var member = file.FindMemberAtRow(iLine);
                if (member != null)
                {
                    pbstrName = member.Name;
                }
            }
            return VSConstants.E_FAIL;
        }

        public XFile getFile(IVsTextBuffer pBuffer)
        {
            var buffer = _editorAdaptersFactoryService.GetDataBuffer(pBuffer);
            XFile file;
            if (buffer.Properties.TryGetProperty(typeof(XSharpModel.XFile), out file))
            {
                return file;
            }
            return null;
        }

        int IVsLanguageDebugInfo.GetProximityExpressions(IVsTextBuffer pBuffer, int iLine, int iCol, int cLines, out IVsEnumBSTR ppEnum)
        {
            ppEnum = null;
            var file = getFile(pBuffer);
            var list = new List<String>();
            XElement member;
            // We use our orignal syntax here (so SELF and ":"). The expression compiler
            // in the debugger takes care of translating SELF to this and ':' to '.'
            if (file != null)
            {
                member = file.FindMemberAtRow(iLine);
                switch (member.Kind)
                {
                    case Kind.Method:
                    case Kind.Access:
                    case Kind.Assign:
                    case Kind.Property:
                    case Kind.Event:
                    case Kind.Field:
                    case Kind.Constructor:
                    case Kind.Destructor:
                    case Kind.Operator:
                        if (!member.Modifiers.HasFlag(Modifiers.Static))
                        {
                            list.Add(_optionsPage.formatKeyword("SELF"));
                        }
                        break;
                }
                var buffer = _editorAdaptersFactoryService.GetDataBuffer(pBuffer);
                Dictionary<string, XVariable> locals = null;
                if (member != null)
                {
                    if (member is XMemberDefinition)
                    {
                        locals = new Dictionary<string, XVariable>(StringComparer.OrdinalIgnoreCase);
                        var tm = member as XMemberDefinition;
                        var vars = tm.GetLocals(buffer.CurrentSnapshot, iLine, file.Project.ParseOptions.Dialect);
                        foreach (var v in vars)
                        {
                            locals.Add(v.Name, (XVariable) v);
                        }
                        foreach (var p in tm.Parameters)
                        {
                            locals.Add(p.Name, (XVariable)p);
                        }
                    }
                    addtokens(buffer, iLine , list, file, locals);
                }
            }


            ppEnum = new VsEnumBSTR(list);
            return VSConstants.S_OK;
        }

        private void addtokens(ITextBuffer buffer, int iLine, IList<string> list, XFile file, IDictionary<string, XVariable> locals)
        {
            if (iLine <= 0)
                return;
            iLine -= 1;
            string slines = "";
            // parse the expression on three lines.
            // we don't have to worry if it can be translated or not
            // the expression compiler in the debugger will simply ignore incorrect expressions
            // we use the locals array to make sure the case of the locals and parameters is correct
            int start = Math.Max(iLine - 1, 0);
            int end = Math.Min(iLine + 1, buffer.CurrentSnapshot.LineCount - 1);
            for (int i = start; i <= end; i++)
            {
                var line = buffer.CurrentSnapshot.GetLineFromLineNumber(i);
                slines += line.GetText() + "\r\n";
            }
            var reporter = new XSharpLanguage.ErrorIgnorer();
            ITokenStream tokenStream;
            bool ok = XSharp.Parser.VsParser.Lex(slines, file.FullPath, file.Project.ParseOptions, reporter, out tokenStream);
            var stream = tokenStream as BufferedTokenStream;
            var tokens = stream.GetTokens();
            string expression = "";
            foreach (var token in tokens)
            {
                var type = token.Type;
                if (type == XSharpLexer.ID)
                {
                    if (expression.Length == 0)
                    {
                        expression = token.Text;
                        if (locals != null && locals.ContainsKey(expression))
                        {
                            expression = locals[expression].Name;
                        }
                    }
                    else
                    {
                        expression += token.Text;
                    }
                    if (! list.Contains(expression))
                    {

                        list.Add(expression);
                    }
                }
                else if (type == XSharpLexer.COLON || type == XSharpLexer.DOT)
                {
                    expression += token.Text;
                }
                else if (type == XSharpLexer.SELF)
                {
                    expression += token.Text;
                }
                else if (type == XSharpLexer.SUPER)
                {
                    expression += token.Text;
                }
                else
                {
                    expression = "";
                }
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

            // Returning E_NOTIMPL indicates that this language only supports entire-line breakpoints. Consequently,
            // VS debugger will highlight the entire line when the breakpoint is active and the corresponding option
            // ("Highlight entire source line for breakpoints and current statement") is set. If we returned S_OK,
            // we'd have to handle this ourselves.
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
