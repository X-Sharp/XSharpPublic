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
            return "XSharp Source Files (*.prg)\n*.prg\nAll Files (*.*)\n*.*\n";
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
            if (req.Reason == ParseReason.Check ||
                req.Reason == ParseReason.None)
            {
                // Parse the entire source as given in req.Text. Store results in the XSharpAuthoringScope object.
            }
            else if (req.Reason == ParseReason.DisplayMemberList)
            {
                // Parse the line specified in req.Line for the two tokens just before req.Col to get the identifier and the member connector symbol. 
                // Find members of the identifer in the parse tree and store the list of members in the Declarations class.
            }
            else if (req.Reason == ParseReason.MethodTip)
            {
                // Parse the line specified in req.Line for the token just before req.Col to obtain the name of the method. 
                // Find all method signatures with the same name in the existing parse tree and store the list of signatures in the Methods class.
            }
            // continue for the rest of the supported ParseReason values.
            return scope;
        }
    }
}
