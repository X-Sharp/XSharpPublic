//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using Community.VisualStudio.Toolkit;
using EnvDTE80;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Threading;
using Microsoft.VisualStudio;
using System;
using System.Threading;
using System.Threading.Tasks;
using XSharpModel;
using System.Runtime.InteropServices;

//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

namespace XSharp.Debugger.UI
{
    [PackageRegistration(UseManagedResourcesOnly = true, AllowsBackgroundLoading = true)]
    [ProvideAutoLoad(VSConstants.UICONTEXT.SolutionExists_string, PackageAutoLoadFlags.BackgroundLoad)]
    [DefaultRegistryRoot("Software\\Microsoft\\VisualStudio\\14.0")]
    [ProvideToolWindow(typeof(MemvarsWindow.Pane), Style = VsDockStyle.Float, Window = WindowGuids.SolutionExplorer)]
    [ProvideToolWindowVisibility(typeof(MemvarsWindow.Pane), VSConstants.UICONTEXT.Debugging_string)]
    [ProvideToolWindow(typeof(SettingsWindow.Pane), Style = VsDockStyle.Float, Window = WindowGuids.SolutionExplorer)]
    [ProvideToolWindowVisibility(typeof(SettingsWindow.Pane), VSConstants.UICONTEXT.Debugging_string)]
    [ProvideToolWindow(typeof(GlobalsWindow.Pane), Style = VsDockStyle.Float, Window = WindowGuids.SolutionExplorer)]
    [ProvideToolWindowVisibility(typeof(GlobalsWindow.Pane), VSConstants.UICONTEXT.Debugging_string)]
    [ProvideToolWindow(typeof(WorkareasWindow.Pane), Style = VsDockStyle.Float, Window = WindowGuids.SolutionExplorer)]
    [ProvideToolWindowVisibility(typeof(WorkareasWindow.Pane), VSConstants.UICONTEXT.Debugging_string)]
    [Guid(XSharpConstants.guidXSharpDebuggerUIPkgString)]
    public sealed class XSharpDebuggerUIPackage : ToolkitPackage,  IVsDebuggerEvents, IDisposable
    {
        private static XSharpDebuggerUIPackage instance;
        DTE2 m_dte;
        internal DTE2 Dte => m_dte;

        // =========================================================================================
        // Properties
        // =========================================================================================
        public static XSharpDebuggerUIPackage XInstance = null;


        public XSharpDebuggerUIPackage() : base()
        {
            XInstance = this;
        }



        /// <summary>
        /// Initialization of the package; this method is called right after the package is sited, so this is the place
        /// where you can put all the initialization code that rely on services provided by VisualStudio.
        /// </summary>
        protected override async Task InitializeAsync(CancellationToken cancellationToken, IProgress<ServiceProgressData> progress)
        {

            this.RegisterToolWindows();
            instance = this;
            await base.InitializeAsync(cancellationToken, progress);
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();

            m_dte = (DTE2)ServiceProvider.GlobalProvider.GetService(typeof(EnvDTE.DTE));

            // make sure the debugger has the version from the main thread
            await this.RegisterDebuggerEventsAsync();
            await this.RegisterCommandsAsync();
        }


  
        private async Task<bool> RegisterDebuggerEventsAsync()
        {
            int hr;
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            m_debugger = await VS.Services.GetDebuggerAsync();
            if (m_debugger != null)
            {
                hr = m_debugger.AdviseDebuggerEvents(this, out m_Debuggercookie);
                ErrorHandler.ThrowOnFailure(hr);
                XDebuggerSettings.DebuggerMode = DebuggerMode.Design;
            }
            return true;
        }
        private void UnRegisterDebuggerEvents()
        {
            int hr;
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();

                if (m_debugger != null && m_Debuggercookie != 0)
                {
                    hr = m_debugger.UnadviseDebuggerEvents(m_Debuggercookie);
                    ErrorHandler.ThrowOnFailure(hr);
                }
            });
            m_Debuggercookie = 0;
            m_debugger = null;
        }
        private IVsDebugger m_debugger = null;
        private uint m_Debuggercookie = 0;

        public int OnModeChange(DBGMODE dbgmodeNew)
        {
            var changed = XDebuggerSettings.DebuggerMode != (DebuggerMode)dbgmodeNew;
            var oldMode = XDebuggerSettings.DebuggerMode;
            XDebuggerSettings.DebuggerMode = (DebuggerMode)dbgmodeNew;
            if (changed || dbgmodeNew == DBGMODE.DBGMODE_Break)
            {
                switch (dbgmodeNew)
                {
                    case DBGMODE.DBGMODE_Design:
                        XDebuggerSettings.DebuggingXSharpExe = false;
                        Support.ClearWindows();
                        break;
                    case DBGMODE.DBGMODE_Break:
                        Support.RefreshWindows();
                        break;
                    case DBGMODE.DBGMODE_Run:
                        // Just started? 
                        if (oldMode == DebuggerMode.Design)
                            Support.RefreshWindows();
                        break;
                    case DBGMODE.DBGMODE_Enc:
                        break;
                }
            }
            return VSConstants.S_OK;
        }

        public void Dispose()
        {
            this.UnRegisterDebuggerEvents();
        }
    }


}
