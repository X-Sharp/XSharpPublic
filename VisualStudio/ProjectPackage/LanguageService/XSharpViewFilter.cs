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
        public override ExpansionProvider GetExpansionProvider()
        {
            return base.GetExpansionProvider();
        }
        public override void CommentSelection()
        {
            base.CommentSelection();
        }
        public override bool CanReformat()
        {
            return base.CanReformat();
        }
        public override TextTipData CreateTextTipData()
        {
            return base.CreateTextTipData();
        }


        public override void Close()
        {
            base.Close();
        }

        public override void RegisterTextViewEventHandlers(Microsoft.VisualStudio.TextManager.Interop.IVsTextView view)
        {
            base.RegisterTextViewEventHandlers(view);
        }

        public override void RegisterExpansionManagerEventHandlers(Microsoft.VisualStudio.TextManager.Interop.IVsExpansionManager emgr)
        {
            base.RegisterExpansionManagerEventHandlers(emgr);
        }

        public override int OnAfterSnippetsKeyBindingChange(uint dwCmdGuid, uint dwCmdId, int fBound)
        {
            return base.OnAfterSnippetsKeyBindingChange(dwCmdGuid, dwCmdId, fBound);
        }

        public override int OnAfterSnippetsUpdate()
        {
            return base.OnAfterSnippetsUpdate();
        }

        public override int GetWordExtent(int line, int index, uint flags, Microsoft.VisualStudio.TextManager.Interop.TextSpan[] span)
        {
            return base.GetWordExtent(line, index, flags, span);
        }

        public override int GetDataTipText(Microsoft.VisualStudio.TextManager.Interop.TextSpan[] aspan, out string textValue)
        {
            return base.GetDataTipText(aspan, out textValue);
        }

        public override int GetPairExtents(int line, int index, Microsoft.VisualStudio.TextManager.Interop.TextSpan[] span)
        {
            return base.GetPairExtents(line, index, span);
        }

        public override void OnChangeCaretLine(Microsoft.VisualStudio.TextManager.Interop.IVsTextView view, int line, int col)
        {
            base.OnChangeCaretLine(view, line, col);
        }

        public override void OnChangeScrollInfo(Microsoft.VisualStudio.TextManager.Interop.IVsTextView view, int iBar, int iMinUnit, int iMaxUnits, int iVisibleUnits, int iFirstVisibleUnit)
        {
            base.OnChangeScrollInfo(view, iBar, iMinUnit, iMaxUnits, iVisibleUnits, iFirstVisibleUnit);
        }

        public override void OnKillFocus(Microsoft.VisualStudio.TextManager.Interop.IVsTextView view)
        {
            base.OnKillFocus(view);
        }

        public override void OnSetBuffer(Microsoft.VisualStudio.TextManager.Interop.IVsTextView view, Microsoft.VisualStudio.TextManager.Interop.IVsTextLines buffer)
        {
            base.OnSetBuffer(view, buffer);
        }

        public override void OnSetFocus(Microsoft.VisualStudio.TextManager.Interop.IVsTextView view)
        {
            base.OnSetFocus(view);
        }

        protected override int QueryCommandStatus(ref Guid guidCmdGroup, uint nCmdId)
        {
            if (Guid.Equals(guidCmdGroup, Microsoft.VisualStudio.Shell.VsMenus.guidStandardCommandSet2K))
            {
                switch ((VSStd2KCmdID)nCmdId)
                {
                    case VSStd2KCmdID.INSERTSNIPPET:
                    case VSStd2KCmdID.SURROUNDWITH:
                        return (int)(OLECMDF.OLECMDF_SUPPORTED | OLECMDF.OLECMDF_ENABLED);
                    case VSStd2KCmdID.CodeDefView:
                        return (int)OLECMDF.OLECMDF_INVISIBLE;
                }
            }
            if (Guid.Equals(guidCmdGroup, Microsoft.VisualStudio.Shell.VsMenus.guidStandardCommandSet97))
            {
                switch ((VSStd97CmdID)nCmdId)
                {
                    case VSStd97CmdID.GotoDefn:
                        return (int)(OLECMDF.OLECMDF_SUPPORTED | OLECMDF.OLECMDF_LATCHED);
                    case VSStd97CmdID.GotoDecl:
                    case VSStd97CmdID.GotoRef:
                        return (int)(OLECMDF.OLECMDF_SUPPORTED | OLECMDF.OLECMDF_INVISIBLE);
                }
            }
            return base.QueryCommandStatus(ref guidCmdGroup, nCmdId);
        }

        protected override int QueryParameterList(ref Guid guidCmdGroup, uint nCmdId, uint nCmdexecopt, IntPtr pvaIn, IntPtr pvaOut)
        {
            return base.QueryParameterList(ref guidCmdGroup, nCmdId, nCmdexecopt, pvaIn, pvaOut);
        }

        public override bool HandlePreExec(ref Guid guidCmdGroup, uint nCmdId, uint nCmdexecopt, IntPtr pvaIn, IntPtr pvaOut)
        {
            if (Guid.Equals(guidCmdGroup, Microsoft.VisualStudio.Shell.VsMenus.guidStandardCommandSet2K))
            {
                switch ((VSConstants.VSStd2KCmdID)nCmdId)
                {
                    case VSStd2KCmdID.INSERTSNIPPET:
                        {
                            ExpansionProvider ep = GetExpansionProvider();
                            if (this.TextView != null && ep != null)
                            {
                                ep.DisplayExpansionBrowser(TextView, "Insert snippet", type1, false, null, false);
                            }
                            return true;
                        }

                    case VSStd2KCmdID.SURROUNDWITH:
                        {
                            ExpansionProvider ep = GetExpansionProvider();
                            if (this.TextView != null && ep != null)
                            {
                                ep.DisplayExpansionBrowser(TextView, "Surround with", type2, false, null, false);
                            }
                        }
                        break;
                    case VSStd2KCmdID.ECMD_LEFTCLICK:
                        Source.OnCommand(TextView, (VSConstants.VSStd2KCmdID)nCmdId, '\0');
                        break;
                }
            }
            return base.HandlePreExec(ref guidCmdGroup, nCmdId, nCmdexecopt, pvaIn, pvaOut);
        }

        public override void HandlePostExec(ref Guid guidCmdGroup, uint nCmdId, uint nCmdexecopt, IntPtr pvaIn, IntPtr pvaOut, bool bufferWasChanged)
        {
            base.HandlePostExec(ref guidCmdGroup, nCmdId, nCmdexecopt, pvaIn, pvaOut, bufferWasChanged);
        }

        protected override int ExecCommand(ref Guid guidCmdGroup, uint nCmdId, uint nCmdexecopt, IntPtr pvaIn, IntPtr pvaOut)
        {
            return base.ExecCommand(ref guidCmdGroup, nCmdId, nCmdexecopt, pvaIn, pvaOut);
        }

        protected override int InnerExec(ref Guid guidCmdGroup, uint nCmdId, uint nCmdexecopt, IntPtr pvaIn, IntPtr pvaOut)
        {
            return base.InnerExec(ref guidCmdGroup, nCmdId, nCmdexecopt, pvaIn, pvaOut);
        }

        public override void OnAutoComplete()
        {
            base.OnAutoComplete();
        }

        public override bool HandleSmartIndent()
        {
            return base.HandleSmartIndent();
        }

        public override void HandleQuickInfo()
        {
            base.HandleQuickInfo();
        }

        public override int GetFullDataTipText(string textValue, Microsoft.VisualStudio.TextManager.Interop.TextSpan ts, out string fullTipText)
        {
            return base.GetFullDataTipText(textValue, ts, out fullTipText);
        }

        public override void HandleGoto(VSStd97CmdID cmd)
        {
            base.HandleGoto(cmd);
        }

        public override void ShowContextMenu(int menuId, Guid groupGuid, Microsoft.VisualStudio.OLE.Interop.IOleCommandTarget target, int x, int y)
        {
            base.ShowContextMenu(menuId, groupGuid, target, x, y);
        }

        public override void UncommentSelection()
        {
            base.UncommentSelection();
        }

        public override void ReformatDocument()
        {
            base.ReformatDocument();
        }

        public override void ReformatSelection()
        {
            base.ReformatSelection();
        }

    }
}
