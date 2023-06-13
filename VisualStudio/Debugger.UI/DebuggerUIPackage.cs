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

/*
Substitution strings
String	Description
$=RegistryEntry$	The value of the RegistryEntry entry. If the registry entry string
ends in a backslash (\), then the default value of the registry subkey is used.
For example, the substitution string $=HKEY_CURRENT_USER\Environment\TEMP$ is expanded
to the temporary folder of the current user.
$AppName$	        The qualified name of the application that is passed to the AppEnv.dll entry points.
The qualified name consists of the application name, an underscore, and the class identifier
(CLSID) of the application automation object, which is also recorded as the value of the
ThisVersion CLSID setting in the project .pkgdef file.
$AppDataLocalFolder	The subfolder under %LOCALAPPDATA% for this application.
$BaseInstallDir$	The full path of the location where Visual Studio was installed.
$CommonFiles$	    The value of the %CommonProgramFiles% environment variable.
$MyDocuments$	    The full path of the My Documents folder of the current user.
$PackageFolder$	    The full path of the directory that contains the package assembly files for the application.
$ProgramFiles$	    The value of the %ProgramFiles% environment variable.
$RootFolder$	    The full path of the root directory of the application.
$RootKey$	        The root registry key for the application. By default the root is in
HKEY_CURRENT_USER\Software\CompanyName\ProjectName\VersionNumber (when the application
is running, _Config is appended to this key). It is set by the RegistryRoot value in
the SolutionName.pkgdef file.
The $RootKey$ string can be used to retrieve a registry value under the application subkey.
For example, the string "$=$RootKey$\AppIcon$" will return the value of the AppIcon entry
under the application root subkey.
The parser processes the .pkgdef file sequentially, and can access a registry entry under the application subkey only if the entry has been previously defined
$ShellFolder$	The full path of the location where Visual Studio was installed.
$System$	The Windows\system32 folder.
$WINDIR$	The Windows folder.
*/


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
                // Get initial value
                DBGMODE[] modeArray = new DBGMODE[1];
                hr = m_debugger.GetMode(modeArray);
                XDebuggerSettings.DebuggerMode = (DebuggerMode)modeArray[0];
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
            XDebuggerSettings.DebuggerMode = (DebuggerMode)dbgmodeNew;
            if (changed || dbgmodeNew == DBGMODE.DBGMODE_Break)
            {
                switch (dbgmodeNew)
                {
                    case DBGMODE.DBGMODE_Design:
                        XDebuggerSettings.DebuggingXSharpExe = false;
                        Support.InitializeWindows();
                        break;
                    case DBGMODE.DBGMODE_Break:
                        Support.RefreshWindows();
                        break;
                    case DBGMODE.DBGMODE_Run:
                        Support.InitializeWindows();
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
