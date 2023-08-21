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
using static Microsoft.VisualStudio.VSConstants;
using Microsoft.VisualStudio.Text;
using LanguageService.SyntaxTree;

namespace XSharp.LanguageService
{
    internal class XSharpViewFilter : ViewFilter
    {

        private String[] type1 = { "Expansion" };
        private String[] type2 = { "SurroundsWith" };
        internal XSharpViewFilter(CodeWindowManager mgr, IVsTextView view) : base(mgr, view)
        {

        }
        // This class is only used for snippet support.
        // Once that has been moved to MEF we can remove the class

        protected override int QueryCommandStatus(ref Guid guidCmdGroup, uint nCmdId)
        {
            if (Guid.Equals(guidCmdGroup, Microsoft.VisualStudio.Shell.VsMenus.guidStandardCommandSet2K))
            {
                switch (nCmdId)
                {
                    case (int) VSStd2KCmdID.INSERTSNIPPET:
                    case (int)VSStd2KCmdID.SURROUNDWITH:
                        return (int)(OLECMDF.OLECMDF_SUPPORTED | OLECMDF.OLECMDF_ENABLED);
                    case (int)VSStd2KCmdID.CodeDefView:
                        return (int)OLECMDF.OLECMDF_INVISIBLE;
                }
            }
            if (Guid.Equals(guidCmdGroup, Microsoft.VisualStudio.Shell.VsMenus.guidStandardCommandSet97))
            {
                switch (nCmdId)
                {
                    case (int)VSStd97CmdID.GotoDefn:
                        return (int)(OLECMDF.OLECMDF_SUPPORTED | OLECMDF.OLECMDF_ENABLED);
                    case (int)VSStd97CmdID.GotoDecl:
                    case (int)VSStd97CmdID.GotoRef:
                        return (int)(OLECMDF.OLECMDF_SUPPORTED | OLECMDF.OLECMDF_INVISIBLE);
                }
            }
            return base.QueryCommandStatus(ref guidCmdGroup, nCmdId);
        }


        public override bool HandlePreExec(ref Guid guidCmdGroup, uint nCmdId, uint nCmdexecopt, IntPtr pvaIn, IntPtr pvaOut)
        {
            if (Guid.Equals(guidCmdGroup, Microsoft.VisualStudio.Shell.VsMenus.guidStandardCommandSet2K))
            {
                switch (nCmdId)
                {
                    case (int)VSStd2KCmdID.INSERTSNIPPET:
                        {
                            ExpansionProvider ep = GetExpansionProvider();
                            if (this.TextView != null && ep != null)
                            {
                                ep.DisplayExpansionBrowser(TextView, "Insert snippet", type1, false, null, false);
                            }
                            return true;
                        }

                    case (int)VSStd2KCmdID.SURROUNDWITH:
                        {
                            ExpansionProvider ep = GetExpansionProvider();
                            if (this.TextView != null && ep != null)
                            {
                                ep.DisplayExpansionBrowser(TextView, "Surround with", type2, false, null, false);
                            }
                        }
                        break;
                    case (int)VSStd2KCmdID.ECMD_LEFTCLICK:
                        Source.OnCommand(TextView, (VSConstants.VSStd2KCmdID)nCmdId, '\0');
                        break;
                }
            }
            return base.HandlePreExec(ref guidCmdGroup, nCmdId, nCmdexecopt, pvaIn, pvaOut);
        }
    }
}
