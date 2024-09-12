﻿//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.Win32;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Runtime.InteropServices;
using System.Threading;
using System.Threading.Tasks;
using XSharp.Project.Options;
using XSharp.Project.WPF;
using XSharpModel;
using Task = System.Threading.Tasks.Task;
using XSharp.Settings;
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


namespace XSharp.Project
{

    /// <summary>
    /// This class implements the package exposed by this assembly.
    /// </summary>
    /// <remarks>
    /// <para>A Visual Studio component can be registered under different registry roots; for instance
    /// when you debug your package you want to register it in the experimental hive. This
    /// attribute specifies the registry root to use if no one is provided to regpkg.exe with
    /// the /root switch.</para>
    /// <para>A description of the different attributes used here is given below:</para>
    /// <para>DefaultRegistryRoot: This defines the default registry root for registering the package.
    /// We are currently using the experimental hive.</para>
    /// <para>ProvideObject: Declares that a package provides creatable objects of specified type.</para>
    /// <para>ProvideProjectFactory: Declares that a package provides a project factory.</para>
    /// <para>ProvideProjectItem: Declares that a package provides a project item.</para>
    /// </remarks>
    ///
    [InstalledProductRegistration("#110", "#112", XSharp.Constants.FileVersion, IconResourceID = 400)]
    [Description(XSharpConstants.ProjectSystemName)]
    [PackageRegistration(UseManagedResourcesOnly = true, AllowsBackgroundLoading = true)]
    [ProvideAutoLoad(VSConstants.UICONTEXT.SolutionExists_string, PackageAutoLoadFlags.BackgroundLoad)]
    // -- Async Package
    [DefaultRegistryRoot("Software\\Microsoft\\VisualStudio\\14.0")]
    [ProvideProjectFactory(typeof(XSharpProjectFactory),
        XSharpConstants.LanguageName, XSharpConstants.ProjectFileMask, XSharpConstants.ProjectExtension, XSharpConstants.ProjectExtensions,
        @".NullPath", LanguageVsTemplate = "XSharp", NewProjectRequireNewFolderVsTemplate = false)]


    [ProvideOptionPage(typeof(Options.DialogPageProvider.WindowEditor), "X# Custom Editors", "VO Window Editor",
        categoryResourceID: 200,
        pageNameResourceID: 201,
        keywordListResourceId: 301,
        supportsAutomation: true,
        Sort = 1)]
    [ProvideOptionPage(typeof(Options.DialogPageProvider.OtherEditor), "X# Custom Editors", "Other Editors",
        categoryResourceID: 200,
        pageNameResourceID: 202,
        keywordListResourceId: 302,
        supportsAutomation: true, Sort = 2)]
    [ProvideOptionPage(typeof(Options.DialogPageProvider.Debugger), "Debugger", "X# Debugger",
     categoryResourceID: 203,
     pageNameResourceID: 204,
     keywordListResourceId: 304,
     supportsAutomation: true)]

    [ProvideProjectFactory(typeof(XSharpWPFProjectFactory),
        null,
        null,
        null,
        null,
        null,
        LanguageVsTemplate = XSharpConstants.LanguageName,
        TemplateGroupIDsVsTemplate = "WPF",
        ShowOnlySpecifiedTemplatesVsTemplate = false, SortPriority = 100)]

    [ProvideProjectItem(typeof(XSharpProjectFactory), "XSharp Items", @"ItemTemplates\Class", 500)]
    [ProvideProjectItem(typeof(XSharpProjectFactory), "XSharp Items", @"ItemTemplates\Form", 500)]

    // Editors for VOBinaries
    [ProvideEditorExtension(typeof(VOFormEditorFactory), ".xsfrm", 0x42, DefaultName = "XSharp VO Form Editor", NameResourceID = 80110)]
    [ProvideEditorExtension(typeof(VOMenuEditorFactory), ".xsmnu", 0x42, DefaultName = "XSharp VO Menu Editor", NameResourceID = 80111)]
    [ProvideEditorExtension(typeof(VODBServerEditorFactory), ".xsdbs", 0x42, DefaultName = "XSharp VO DbServer Editor", NameResourceID = 80112)]
    [ProvideEditorExtension(typeof(VOFieldSpecEditorFactory), ".xsfs", 0x42, DefaultName = "XSharp VO FieldSpec Editor", NameResourceID = 80113)]
    //[ProvideEditorExtension(typeof(VOSQLServerEditorFactory), ".xssql", 0x42, DefaultName = "XSharp VO SQL Editor", NameResourceID = 80114)]
    //[ProvideEditorExtension(typeof(VOReportEditorFactory), ".xsrep", 0x42, DefaultName = "XSharp VO Report Editor", NameResourceID = 80115)]
    [ProvideEditorLogicalView(typeof(VOFormEditorFactory), VSConstants.LOGVIEWID.Designer_string, IsTrusted = true)]
    [ProvideEditorLogicalView(typeof(VOMenuEditorFactory), VSConstants.LOGVIEWID.Designer_string, IsTrusted = true)]
    [ProvideEditorLogicalView(typeof(VODBServerEditorFactory), VSConstants.LOGVIEWID.Designer_string, IsTrusted = true)]
    [ProvideEditorLogicalView(typeof(VOFieldSpecEditorFactory), VSConstants.LOGVIEWID.Designer_string, IsTrusted = true)]
    // Vulcan Binaries
    [ProvideEditorExtension(typeof(VOFormEditorFactory), ".vnfrm", 0x42, DefaultName = "XSharp VO Form Editor", NameResourceID = 80110)]
    [ProvideEditorExtension(typeof(VOMenuEditorFactory), ".vnmnu", 0x42, DefaultName = "XSharp VO Menu Editor", NameResourceID = 80111)]
    [ProvideEditorExtension(typeof(VODBServerEditorFactory), ".vndbs", 0x42, DefaultName = "XSharp VO DbServer Editor", NameResourceID = 80112)]
    [ProvideEditorExtension(typeof(VOFieldSpecEditorFactory), ".vnfs", 0x42, DefaultName = "XSharp VO FieldSpec Editor", NameResourceID = 80113)]
    //[ProvideEditorExtension(typeof(VOSQLServerEditorFactory), ".vnsqs", 0x42, DefaultName = "XSharp VO SQL Editor", NameResourceID = 80114)]
    //[ProvideEditorExtension(typeof(VOReportEditorFactory), ".vnrep", 0x42, DefaultName = "XSharp VO Report Editor", NameResourceID = 80115)]

    [SingleFileGeneratorSupportRegistrationAttribute(typeof(XSharpProjectFactory))]  // 5891B814-A2E0-4e64-9A2F-2C2ECAB940FE"
    [Guid(XSharpConstants.guidXSharpProjectPkgString)]
#if REPOWINDOW
    [ProvideToolWindow(typeof(RepositoryWindow.Pane), Style = VsDockStyle.Float, Window = WindowGuids.SolutionExplorer)]
    [ProvideToolWindowVisibility(typeof(RepositoryWindow.Pane), VSConstants.UICONTEXT.SolutionExistsAndFullyLoaded_string)]
#endif
    [ProvideMenuResource("Menus.ctmenu", 1)]
    public sealed class XSharpProjectPackage : AsyncProjectPackage, IVsShellPropertyEvents, IDisposable
    {
        private static XSharpProjectPackage instance = null;
        private XPackageSettings settings;
        private IErrorList _errorList = null;
        private ITaskList _taskList = null;
        //private XSharpProjectSelector _projectSelector = null;
        private uint shellCookie;
        IVsShell shell = null;
 
        public static XSharpProjectPackage XInstance => instance ;


        // =========================================================================================
        // Properties
        // =========================================================================================

        internal ITaskList TaskList => _taskList;
        internal IErrorList ErrorList => _errorList;


        public XSharpProjectPackage() : base()
        {
            instance = this;
        }


        XSharpShellEvents _shellEvents;
        #region Overridden Implementation
        /// <summary>
        /// Initialization of the package; this method is called right after the package is sited, so this is the place
        /// where you can put all the initialization code that rely on services provided by VisualStudio.
        /// </summary>
        protected override async Task InitializeAsync(CancellationToken cancellationToken, IProgress<ServiceProgressData> progress)
        {
            // Give the codemodel a way to talk to the VS Shell
            _shellEvents = new XSharpShellEvents();

            this.RegisterToolWindows();

            await base.InitializeAsync(cancellationToken, progress);
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();

 
            // The project selector helps to choose between MPF and CPS projects
            //_projectSelector = new XSharpProjectSelector();
            //await _projectSelector.InitAsync(this);

            this.settings = new XPackageSettings(this);

            VS.Events.SolutionEvents.OnAfterOpenSolution += SolutionEvents_OnAfterOpenSolution;
            VS.Events.BuildEvents.ProjectConfigurationChanged += BuildEvents_ProjectConfigurationChanged;

            this.RegisterProjectFactory(new XSharpProjectFactory(this));
            // Indicate how to open the different source files : SourceCode or Designer ??
            this.RegisterProjectFactory(new XSharpWPFProjectFactory(this));

            // editors for the binaries
            base.RegisterEditorFactory(new VOFormEditorFactory(this));
            base.RegisterEditorFactory(new VOMenuEditorFactory(this));
            base.RegisterEditorFactory(new VODBServerEditorFactory(this));
            base.RegisterEditorFactory(new VOFieldSpecEditorFactory(this));

            _errorList = await VS.GetRequiredServiceAsync<SVsErrorList, IErrorList>();
            _taskList = await VS.GetRequiredServiceAsync<SVsTaskList, ITaskList>();

            shell = await VS.GetRequiredServiceAsync<SVsShell, IVsShell>();
            if (shell != null)
            {
                shell.AdviseShellPropertyChanges(this, out shellCookie);
            }
            await this.RegisterCommandsAsync();
            await GetOptionsAsync();
            XSettings.CodeDomProviderClass  = typeof(XSharp.CodeDom.XSharpCodeDomProvider);

        }


        #region SolutionEvents

        private void SolutionEvents_OnAfterOpenSolution(Solution obj)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            // refresh the references for all projects
            foreach (var xnode in XSharpProjectNode.AllProjects)
            {
                xnode.LoadPackageReferences();
                xnode.UpdateReferencesInProjectModel();
            }
        }
        private void BuildEvents_ProjectConfigurationChanged(Community.VisualStudio.Toolkit.Project prj)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            if (!prj.IsXSharp())
                return;
            foreach (var project in XSharpProjectNode.AllProjects)
            {
                if (string.Compare(project.Url, prj?.FullPath, true) == 0)
                {
                    project.CreateProjectOptions();
                    project.ClearCache();
                    project.GetParseOptions();

                }
            }
        }


        #endregion
        public async Task<bool> GetOptionsAsync()
        {
            var options = ProjectSystemOptions.Load();
            if (options == null)
            {
                options = new ProjectSystemOptions();
                options.DebuggerOptions = await Options.DebuggerOptions.GetLiveInstanceAsync();
                options.WindowEditorOptions = await Options.WindowEditorOptions.GetLiveInstanceAsync();
                options.OtherEditorOptions = await Options.OtherEditorOptions.GetLiveInstanceAsync();
                options.Save();
            }
            else
            {
                // save values from disk to private registry 
                await options.DebuggerOptions.SaveAsync();
                await options.WindowEditorOptions.SaveAsync();
                await options.OtherEditorOptions.SaveAsync();
            }
            options.WriteToSettings();
            return true;
        }

        
        
		
        /// <summary>
        /// Read the comment tokens from the Tools/Options dialog and pass them to the CodeModel assembly
        /// </summary>
        public void SetCommentTokens()
        {
            var commentTokens = _taskList.CommentTokens;
            var tokens = new List<XCommentToken>();
            foreach (var token in commentTokens)
            {
                var cmttoken = new XCommentToken(token.Text, (int)token.Priority);
                tokens.Add(cmttoken);
            }
            XSolution.SetCommentTokens(tokens);
        }


        public override string ProductUserContext
        {
            get { return "XSharp"; }
        }


        #endregion
        public int OnShellPropertyChange(int propid, object var)
        {
            // A modal dialog has been opened. Editor Options ?
            if (propid == (int)__VSSPROPID4.VSSPROPID_IsModal && var is bool)
            {
                // when modal window closes
                if (!(bool)var)
                {
                    SetCommentTokens();
                    var options = new ProjectSystemOptions();
                    ThreadHelper.JoinableTaskFactory.Run(async delegate
                    {
                        options.DebuggerOptions = await DebuggerOptions.GetLiveInstanceAsync();
                        options.WindowEditorOptions = await WindowEditorOptions.GetLiveInstanceAsync();
                        options.OtherEditorOptions = await OtherEditorOptions.GetLiveInstanceAsync();
                    });
                    options.Save();
                }
            }
            return VSConstants.S_OK;
        }

        public void Dispose()
        {
            if (shell != null)
            {
                JoinableTaskFactory.Run(async delegate
                {
                    await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                    shell.UnadviseShellPropertyChanges(shellCookie);
                    shellCookie = 0;
                });
            }
        }
    }
    internal static class Logger
    {
        internal static void Exception(Exception e, string msg)
        {
            XSettings.Logger.Exception(e, msg);
        }
        internal static void Information(string msg)
        {
            XSettings.Logger.Information(msg);
        }
        internal static void Debug(string msg)
        {
            XSettings.Logger.Debug(msg);
        }
    }

}
