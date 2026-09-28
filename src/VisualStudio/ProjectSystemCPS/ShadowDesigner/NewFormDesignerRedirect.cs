//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Collections.Generic;
using System.IO;
using System.Runtime.InteropServices;
using System.Threading.Tasks;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Threading;
using XSharpModel;

namespace XSharp.ProjectSystem.ShadowDesigner
{
    /// <summary>
    /// Opens a form or user control that was just added with "Add New Item" in the shadow designer instead of the
    /// code editor.
    /// </summary>
    /// <remarks>
    /// The item template wizard opens the new .prg itself (primary view), so it does not go through the shadow
    /// designer commands (double click, View Designer); VS picks the default editor for .prg, the X# code editor.
    /// MPFproj redirected this in XSharpFileNode, which is gone together with the MPFproj SDK path.
    /// CPS' own mechanism for "forms open in the designer" (IProjectSpecificEditorProvider with IsDefaultEditor) is
    /// not usable: it routes every open of the file to the project specific editor factory, also the debugger and
    /// navigation (LOGVIEWID_Debugging/TextView), which the shadow designer cannot serve.
    /// Instead this watches the first window of documents of CPS X# projects: when it belongs to a form .prg (a
    /// .Designer.prg exists) whose file was created just now (by the template), the shadow designer is opened and,
    /// once that succeeded, the code window is closed. Every file is redirected at most once per session, so a later
    /// "View Code" is not affected. "Add Existing Item" opens no window, so it is not affected either.
    /// </remarks>
    internal sealed class NewFormDesignerRedirect : IVsRunningDocTableEvents
    {
        // A file created longer ago is not a new item from a template
        private static readonly TimeSpan NewFileAge = TimeSpan.FromSeconds(30);
        // How long to wait for the code model to know the new file (evaluation of the project)
        private static readonly TimeSpan CodeModelTimeout = TimeSpan.FromSeconds(10);

        private static NewFormDesignerRedirect _instance;

        private readonly IVsRunningDocumentTable _rdt;
        private readonly JoinableTaskFactory _joinableTaskFactory;
        private readonly HashSet<string> _handled = new HashSet<string>(StringComparer.OrdinalIgnoreCase);

        private NewFormDesignerRedirect(IVsRunningDocumentTable rdt, JoinableTaskFactory joinableTaskFactory)
        {
            _rdt = rdt;
            _joinableTaskFactory = joinableTaskFactory;
        }

        /// <summary>
        /// Starts watching the document windows, once per session. Called when the first X# CPS project loads, with
        /// the JoinableTaskFactory of the CPS threading service (runs the switch to the designer).
        /// </summary>
        public static void EnsureAdvised(JoinableTaskFactory joinableTaskFactory)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            if (_instance != null)
            {
                return;
            }
            if (!(ServiceProvider.GlobalProvider.GetService(typeof(SVsRunningDocumentTable)) is IVsRunningDocumentTable rdt))
            {
                return;
            }
            var instance = new NewFormDesignerRedirect(rdt, joinableTaskFactory);
            if (ErrorHandler.Succeeded(rdt.AdviseRunningDocTableEvents(instance, out uint _)))
            {
                // Process lifetime: the table outlives every solution, there is nothing to unadvise before shutdown
                _instance = instance;
            }
        }

        public int OnBeforeDocumentWindowShow(uint docCookie, int fFirstShow, IVsWindowFrame pFrame)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            if (fFirstShow == 0 || pFrame == null)
            {
                return VSConstants.S_OK;
            }
            try
            {
                if (ErrorHandler.Failed(_rdt.GetDocumentInfo(docCookie, out uint _, out uint _, out uint _, out string moniker,
                        out IVsHierarchy hierarchy, out uint _, out IntPtr docData)))
                {
                    return VSConstants.S_OK;
                }
                if (docData != IntPtr.Zero)
                {
                    Marshal.Release(docData);
                }
                string mainPrg = GetMainPrg(moniker);
                if (mainPrg == null || hierarchy == null || !hierarchy.IsCapabilityMatch(XSharpCapabilities.XSharpCps) ||
                    !IsNewFile(mainPrg) || !_handled.Add(mainPrg))
                {
                    return VSConstants.S_OK;
                }
                Logger.Information($"NewFormDesignerRedirect: new form {mainPrg} opened in the code editor, switching to the designer");
                _joinableTaskFactory.RunAsync(() => SwitchToDesignerAsync(mainPrg, pFrame))
                    .FileAndForget("XSharp/ProjectSystemCPS/NewFormDesignerRedirect");
            }
            catch (Exception ex)
            {
                Logger.Exception(ex, "NewFormDesignerRedirect.OnBeforeDocumentWindowShow");
            }
            return VSConstants.S_OK;
        }

        /// <summary>
        /// Form1.prg or Form1.Designer.prg -> Form1.prg, for files of an X# form (the template can open either).
        /// </summary>
        private static string GetMainPrg(string moniker)
        {
            if (string.IsNullOrEmpty(moniker) || !moniker.EndsWith(".prg", StringComparison.OrdinalIgnoreCase))
            {
                return null;
            }
            const string designerSuffix = ".designer.prg";
            if (moniker.EndsWith(designerSuffix, StringComparison.OrdinalIgnoreCase))
            {
                return moniker.Substring(0, moniker.Length - designerSuffix.Length) + ".prg";
            }
            return moniker;
        }

        private static bool IsNewFile(string path)
        {
            try
            {
                return File.Exists(path) && DateTime.UtcNow - File.GetCreationTimeUtc(path) < NewFileAge;
            }
            catch (Exception)
            {
                return false;
            }
        }

        private static async Task SwitchToDesignerAsync(string mainPrg, IVsWindowFrame codeFrame)
        {
            // Let the Add New Item operation finish; the .Designer.prg is written by the same template and the project
            // (and with it the code model) only knows the new files after its next evaluation.
            var deadline = DateTime.UtcNow + CodeModelTimeout;
            while (!(ShadowDesignerBridge.HasDesignerFile(mainPrg) && XSolution.FindFile(mainPrg)?.Project != null))
            {
                if (DateTime.UtcNow >= deadline)
                {
                    Logger.Information($"NewFormDesignerRedirect: {mainPrg} is not known to the X# code model, keeping the code editor");
                    return;
                }
                await Task.Delay(200);
            }
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            if (!await XSharpShadowDesignerCommands.TryOpenAsync(mainPrg))
            {
                // The designer could not be opened: the code editor stays, as without this redirect
                return;
            }
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            // A document from a template is not dirty; PromptSave only asks if the user already typed something
            int hr = codeFrame.CloseFrame((uint)__FRAMECLOSE.FRAMECLOSE_PromptSave);
            Logger.Information($"NewFormDesignerRedirect: closed the code editor of {mainPrg}, hr=0x{hr:X8}");
        }

        #region IVsRunningDocTableEvents members that are not used
        public int OnAfterFirstDocumentLock(uint docCookie, uint dwRDTLockType, uint dwReadLocksRemaining, uint dwEditLocksRemaining) => VSConstants.S_OK;
        public int OnBeforeLastDocumentUnlock(uint docCookie, uint dwRDTLockType, uint dwReadLocksRemaining, uint dwEditLocksRemaining) => VSConstants.S_OK;
        public int OnAfterSave(uint docCookie) => VSConstants.S_OK;
        public int OnAfterAttributeChange(uint docCookie, uint grfAttribs) => VSConstants.S_OK;
        public int OnAfterDocumentWindowHide(uint docCookie, IVsWindowFrame pFrame) => VSConstants.S_OK;
        #endregion
    }
}
