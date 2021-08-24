//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Package;
using Microsoft.VisualStudio.TextManager.Interop;

using XSharp.LanguageService;
using XSharp.Project.WPF;
using System.ComponentModel;
using System.ComponentModel.Design;
using System.Globalization;
using static XSharp.XSharpConstants;
using XSharp.VOEditors;
using System.Threading;
using System.Threading.Tasks;
using System.Collections.Generic;
using Microsoft;
using Microsoft.VisualStudio.ComponentModelHost;
using XSharpModel;
using Community.VisualStudio.Toolkit;
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

// The following lines ensure that the right versions of the various DLLs are loaded.
// They will be included in the generated PkgDef folder for the project system
[assembly: ProvideCodeBase(AssemblyName = "XSharp.CodeDom.XSharpCodeDomProvider", CodeBase = "XSharpCodeDomProvider.dll", Culture = "neutral", PublicKeyToken = XSharp.Constants.PublicKey, Version = XSharp.Constants.Version)]
[assembly: ProvideCodeBase(AssemblyName = "XSharp.VsParser", CodeBase = "XSharp.VsParser.dll", Culture = "neutral", PublicKeyToken = XSharp.Constants.PublicKey, Version = XSharp.Constants.Version)]
[assembly: ProvideCodeBase(AssemblyName = "XSharpModel", CodeBase = "XSharpModel.dll", Culture = "neutral", PublicKeyToken = XSharp.Constants.PublicKey, Version = XSharp.Constants.Version)]
[assembly: ProvideCodeBase(AssemblyName = "XSharpMonoCecil", CodeBase = "XSharpMonoCecil.dll", Culture = "neutral", PublicKeyToken = "50cebf1cceb9d05e", Version = "0.11.3.0")]
[assembly: ProvideCodeBase(AssemblyName = "System.Data.SQLite", CodeBase = "System.Data.SQLite.dll", Culture = "neutral", PublicKeyToken = "db937bc2d44ff139", Version = "1.0.113.0")]
[assembly: ProvideCodeBase(AssemblyName = "Community.VisualStudio.Toolkit")]
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
    [Description(ProjectSystemName)]
    // [PackageRegistration(UseManagedResourcesOnly = true)] <-- Standard Package loading
    // ++ Async Package
    [PackageRegistration(UseManagedResourcesOnly = true, AllowsBackgroundLoading = true)]
    [ProvideAutoLoad(VSConstants.UICONTEXT.SolutionExists_string, PackageAutoLoadFlags.BackgroundLoad)]
    // -- Async Package
    [DefaultRegistryRoot("Software\\Microsoft\\VisualStudio\\14.0")]
    [ProvideProjectFactory(typeof(XSharpProjectFactory),
        LanguageName, ProjectFileMask, ProjectExtension, ProjectExtensions,
        @".NullPath", LanguageVsTemplate = "XSharp", NewProjectRequireNewFolderVsTemplate = false)]


    [ProvideOptionPage(typeof(Options.DialogPageProvider.WindowEditor), "X# Custom Editors", "Window Editor", 0, 0, true,Sort = 1)]
    [ProvideOptionPage(typeof(Options.DialogPageProvider.OtherEditor), "X# Custom Editors", "Other Editors", 0, 0, true, Sort = 2)]

    [ProvideLanguageCodeExpansionAttribute(
         typeof(XSharpLanguageService),
         LanguageName,  // Name of language used as registry key.
         1,         // Resource ID of localized name of language service.
         LanguageName,  // language key used in snippet templates.
         @"%InstallRoot%\Common7\IDE\Extensions\XSharp\Snippets\%LCID%\SnippetsIndex.xml",  // Path to snippets index
         SearchPaths = @"%InstallRoot%\Common7\IDE\Extensions\XSharp\Snippets\%LCID%\Snippets;" +
                  @"\%MyDocs%\Code Snippets\XSharp\My Code Snippets"
         )]

    [ProvideProjectFactory(typeof(XSharpWPFProjectFactory),
        null,
        null,
        null,
        null,
        null,
        LanguageVsTemplate = LanguageName,
        TemplateGroupIDsVsTemplate = "WPF",
        ShowOnlySpecifiedTemplatesVsTemplate = false, SortPriority = 100)]

    [ProvideProjectItem(typeof(XSharpProjectFactory), "XSharp Items", @"ItemTemplates\Class", 500)]
    [ProvideProjectItem(typeof(XSharpProjectFactory), "XSharp Items", @"ItemTemplates\Form", 500)]
    // 109 in the next lines is the resource id of the editor (XSharp Source Code Editor)
    [ProvideEditorExtension(typeof(XSharpEditorFactory), ".prg", 0x42, DefaultName = EditorName, NameResourceID = 109)]
    [ProvideEditorExtension(typeof(XSharpEditorFactory), ".xs", 0x42, DefaultName = EditorName, NameResourceID = 109)]
    [ProvideEditorExtension(typeof(XSharpEditorFactory), ".ppo", 0x42, DefaultName = EditorName, NameResourceID = 109)]
    [ProvideEditorExtension(typeof(XSharpEditorFactory), ".vh", 0x42, DefaultName = EditorName, NameResourceID = 109)]
    [ProvideEditorExtension(typeof(XSharpEditorFactory), ".xh", 0x42, DefaultName = EditorName, NameResourceID = 109)]
    [ProvideEditorExtension(typeof(XSharpEditorFactory), ".ch", 0x42, DefaultName = EditorName, NameResourceID = 109)]
    [ProvideEditorExtension(typeof(XSharpEditorFactory), ".rc", 0x42, DefaultName = EditorName, NameResourceID = 109)]
    // This tells VS that we support Code and Designer view
    // The guids are VS specific and should not be changed
    [ProvideEditorLogicalView(typeof(XSharpEditorFactory), VSConstants.LOGVIEWID.Designer_string)]
    [ProvideEditorLogicalView(typeof(XSharpEditorFactory), VSConstants.LOGVIEWID.Code_string)]

    // Editors for VOBinaries
    [ProvideEditorExtension(typeof(VOFormEditorFactory), ".xsfrm", 0x42, DefaultName = "XSharp VO Form Editor", NameResourceID = 80110)]
    [ProvideEditorExtension(typeof(VOMenuEditorFactory), ".xsmnu", 0x42, DefaultName = "XSharp VO Menu Editor", NameResourceID = 80111)]
    [ProvideEditorExtension(typeof(VODBServerEditorFactory), ".xsdbs", 0x42, DefaultName = "XSharp VO DbServer Editor", NameResourceID = 80112)]
    [ProvideEditorExtension(typeof(VOFieldSpecEditorFactory), ".xsfs", 0x42, DefaultName = "XSharp VO FieldSpec Editor", NameResourceID = 80113)]
    [ProvideEditorLogicalView(typeof(VOFormEditorFactory), VSConstants.LOGVIEWID.Designer_string)]
    [ProvideEditorLogicalView(typeof(VOMenuEditorFactory), VSConstants.LOGVIEWID.Designer_string)]
    [ProvideEditorLogicalView(typeof(VODBServerEditorFactory), VSConstants.LOGVIEWID.Designer_string)]
    [ProvideEditorLogicalView(typeof(VOFieldSpecEditorFactory), VSConstants.LOGVIEWID.Designer_string)]
    // Vulcan Binaries
    [ProvideEditorExtension(typeof(VOFormEditorFactory), ".vnfrm", 0x42, DefaultName = "XSharp VO Form Editor", NameResourceID = 80110)]
    [ProvideEditorExtension(typeof(VOMenuEditorFactory), ".vnmnu", 0x42, DefaultName = "XSharp VO Menu Editor", NameResourceID = 80111)]
    [ProvideEditorExtension(typeof(VODBServerEditorFactory), ".vndbs", 0x42, DefaultName = "XSharp VO DbServer Editor", NameResourceID = 80112)]
    [ProvideEditorExtension(typeof(VOFieldSpecEditorFactory), ".vnfs", 0x42, DefaultName = "XSharp VO FieldSpec Editor", NameResourceID = 80113)]

    [SingleFileGeneratorSupportRegistrationAttribute(typeof(XSharpProjectFactory))]  // 5891B814-A2E0-4e64-9A2F-2C2ECAB940FE"
    [Guid(GuidStrings.guidXSharpProjectPkgString)]

    [ProvideToolWindow(typeof(RepositoryWindow.Pane), Style = VsDockStyle.Float, Window = WindowGuids.SolutionExplorer)]
    [ProvideToolWindowVisibility(typeof(RepositoryWindow.Pane), VSConstants.UICONTEXT.NoSolution_string)]

    [ProvideMenuResource("Menus.ctmenu", 1)]
    //[ProvideBindingPath]        // Tell VS to look in our path for assemblies
    public sealed class XSharpProjectPackage : ToolkitPackage, IVsShellPropertyEvents,IVsDebuggerEvents
    {
        private static XSharpProjectPackage instance;
        private XPackageSettings settings;
        //private XSharpDocumentWatcher _documentWatcher;
        private IErrorList _errorList = null;
        private ITaskList _taskList = null;
        //private XSharpProjectSelector _projectSelector = null;
        private uint shellCookie;

        public static XSharpProjectPackage XInstance = null;
        private XSharpLanguageService _langservice;

        // =========================================================================================
        // Properties
        // =========================================================================================

        internal ITaskList TaskList => _taskList;
        internal IErrorList ErrorList => _errorList;


        public XSharpProjectPackage() : base()
        {
            XInstance = this;
            ModelScannerEvents.Start();
        }

      

        // XSharpLanguageService _langService = null;
        #region Overridden Implementation
        /// <summary>
        /// Initialization of the package; this method is called right after the package is sited, so this is the place
        /// where you can put all the initialization code that rely on services provided by VisualStudio.
        /// </summary>
        protected override async System.Threading.Tasks.Task InitializeAsync(CancellationToken cancellationToken, IProgress<ServiceProgressData> progress)
        {
            XSettings.DisplayOutputMessage = XSharpOutputPane.DisplayOutputMessage;
            XSettings.DisplayException = XSharpOutputPane.DisplayException;
            XSettings.ShowMessageBox = ShowMessageBox;

            this.RegisterToolWindows();

            XSharpProjectPackage.instance = this;
            await base.InitializeAsync(cancellationToken, progress);
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            // The project selector helps to choose between MPF and CPS projects
            //_projectSelector = new XSharpProjectSelector();
            //await _projectSelector.InitAsync(this);


            this.settings = new XPackageSettings(this);

            this.RegisterProjectFactory(new XSharpProjectFactory(this));
            // Indicate how to open the different source files : SourceCode or Designer ??
            this.RegisterEditorFactory(new XSharpEditorFactory(this));
            this.RegisterProjectFactory(new XSharpWPFProjectFactory(this));

            // editors for the binaries
            base.RegisterEditorFactory(new VOFormEditorFactory(this));
            base.RegisterEditorFactory(new VOMenuEditorFactory(this));
            base.RegisterEditorFactory(new VODBServerEditorFactory(this));
            base.RegisterEditorFactory(new VOFieldSpecEditorFactory(this));

            //this._documentWatcher = new XSharpDocumentWatcher(this);
            _errorList = await VS.GetRequiredServiceAsync<SVsErrorList, IErrorList>();
            _taskList = await VS.GetRequiredServiceAsync<SVsTaskList, ITaskList>();

            var shell = await VS.GetRequiredServiceAsync<SVsShell, IVsShell>();
            if (shell != null)
            {
                shell.AdviseShellPropertyChanges(this, out shellCookie);
            }
            GetEditorOptions();
            _langservice = await GetServiceAsync(typeof(XSharpLanguageService)) as XSharpLanguageService;
            await this.RegisterCommandsAsync();
        }



        public void GetEditorOptions()
        {
            System.Threading.Tasks.Task.Run(async () =>
            {
                var woptions = await Options.WindowEditorOptions.GetLiveInstanceAsync();
                XEditorSettings.ShowGrid = woptions.ShowGrid;
                XEditorSettings.GridX = woptions.GridX;
                XEditorSettings.GridY = woptions.GridY;
                XEditorSettings.PasteOffSetX = woptions.PasteOffSetX;
                XEditorSettings.PasteOffSetY = woptions.PasteOffSetY;
                XEditorSettings.PartialLasso = woptions.PartialLasso;

                var options = await Options.OtherEditorOptions.GetLiveInstanceAsync();
                XEditorSettings.DbServerDefaultRDD = options.DbServerDefaultRDD;
                XEditorSettings.DbServerParentClass = options.DbServerParentClass;
                XEditorSettings.MenuParentClass = options.MenuParentClass;
                XEditorSettings.FieldSpecParentClass = options.FieldSpecParentClass;
                XEditorSettings.ToolbarParentClass = options.ToolbarParentClass;

            }).FireAndForget();

        }
        /// <summary>
        /// Read the comment tokens from the Tools/Options dialog and pass them to the CodeModel assembly
        /// </summary>
        public void SetCommentTokens()
        {
            var commentTokens = _taskList.CommentTokens;
            var tokens = new List<XSharpModel.XCommentToken>();
            foreach (var token in commentTokens)
            {
                var cmttoken = new XSharpModel.XCommentToken(token.Text, (int)token.Priority);
                tokens.Add(cmttoken);
            }
            XSharpModel.XSolution.SetCommentTokens(tokens);
        }


        public  string ProductUserContext
        {
            get { return "XSharp"; }
        }

        internal int ShowMessageBox(string message)
        {
            string title = string.Empty;
            return (int) VS.MessageBox.Show(title, message);

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
                    GetEditorOptions();
                }
            }
            return VSConstants.S_OK;
        }

        public int OnModeChange(DBGMODE dbgmodeNew)
        {
            throw new NotImplementedException();
        }
    }


}
