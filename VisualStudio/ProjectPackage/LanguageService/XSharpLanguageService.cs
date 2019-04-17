//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Package;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.VisualStudio.OLE.Interop;
using System.Runtime.InteropServices;
using XSharp.Project;
namespace XSharp.LanguageService
{
    [Guid(GuidStrings.guidXSharpLanguageServicePkgString)]
    public class XSharpLanguageService : Microsoft.VisualStudio.Package.LanguageService
    {
        private LanguagePreferences m_preferences;
        //private XSharpScanner m_scanner;

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
            //if (m_scanner == null)
            //{
            //    m_scanner = new XSharpScanner(buffer);
            //}
            //m_scanner = null;
            //return m_scanner;
            return null;
        }

        public override string Name
        {
            get { return "XSharp"; }
        }

        public override AuthoringScope ParseSource(ParseRequest req)
        {
            XSharpAuthoringScope scope = new XSharpAuthoringScope();

            switch (req.Reason)
            {
                case Microsoft.VisualStudio.Package.ParseReason.None:
                    break;
                case Microsoft.VisualStudio.Package.ParseReason.MemberSelect:
                    break;
                case Microsoft.VisualStudio.Package.ParseReason.HighlightBraces:
                    break;
                case Microsoft.VisualStudio.Package.ParseReason.MemberSelectAndHighlightBraces:
                    break;
                case Microsoft.VisualStudio.Package.ParseReason.MatchBraces:
                    break;
                case Microsoft.VisualStudio.Package.ParseReason.Check:
                    break;
                case Microsoft.VisualStudio.Package.ParseReason.CompleteWord:
                    break;
                case Microsoft.VisualStudio.Package.ParseReason.DisplayMemberList:
                    break;
                case Microsoft.VisualStudio.Package.ParseReason.QuickInfo:
                    break;
                case Microsoft.VisualStudio.Package.ParseReason.MethodTip:
                    break;
                case Microsoft.VisualStudio.Package.ParseReason.Autos:
                    break;
                case Microsoft.VisualStudio.Package.ParseReason.CodeSpan:
                    break;
                case Microsoft.VisualStudio.Package.ParseReason.Goto:
                    break;
                default:
                    break;

            }
            return scope;
        }
        public override ViewFilter CreateViewFilter(CodeWindowManager mgr, IVsTextView newView)
        {
            return new XSharpViewFilter(mgr, newView);
        }

        public override Source CreateSource(IVsTextLines buffer)
        {
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
    }
}
