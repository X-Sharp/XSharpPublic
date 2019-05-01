﻿//
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
using static XSharp.Project.XSharpConstants;
using XSharp.Project.OptionsPages;
using XSharp.VOEditors;
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
                    ThisVersionDTECLSID setting in the project .pkgdef file.
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
[assembly: ProvideCodeBase(AssemblyName = "XSharp.CodeDom.XSharpCodeDomProvider", CodeBase = "XSharpCodeDomProvider.dll", Culture = "neutral", PublicKeyToken = "ed555a0467764586", Version = XSharp.Constants.Version)]
[assembly: ProvideCodeBase(AssemblyName = "XSharp.VsParser", CodeBase = "XSharp.VsParser.dll", Culture = "neutral", PublicKeyToken = "ed555a0467764586", Version = XSharp.Constants.Version)]
[assembly: ProvideCodeBase(AssemblyName = "XSharpColorizer", CodeBase = "XSharpColorizer.dll", Culture = "neutral", PublicKeyToken = "ed555a0467764586", Version = XSharp.Constants.Version)]
[assembly: ProvideCodeBase(AssemblyName = "XSharpModel", CodeBase = "XSharpModel.dll", Culture = "neutral", PublicKeyToken = "ed555a0467764586", Version = XSharp.Constants.Version)]
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
    [InstalledProductRegistration("#110", "#112", XSharp.Constants.Version, IconResourceID = 400)]
    [Description(ProjectSystemName)]
    [PackageRegistration(UseManagedResourcesOnly = true)]
    [DefaultRegistryRoot("Software\\Microsoft\\VisualStudio\\14.0")]
    [ProvideObject(typeof(XSharpGeneralPropertyPage))]
    [ProvideObject(typeof(XSharpLanguagePropertyPage))]
    [ProvideObject(typeof(XSharpDialectPropertyPage))]
    [ProvideObject(typeof(XSharpBuildPropertyPage))]
    [ProvideObject(typeof(XSharpBuildEventsPropertyPage))]
    [ProvideObject(typeof(XSharpDebugPropertyPage))]
    [ProvideProjectFactory(typeof(XSharpProjectFactory),
        LanguageName, ProjectFileMask, ProjectExtension, ProjectExtensions,
        @".NullPath", LanguageVsTemplate = "XSharp", NewProjectRequireNewFolderVsTemplate = false)]

    [ProvideService(typeof(XSharpLanguageService), ServiceName = LanguageServiceName)]
    [ProvideLanguageExtension(typeof(XSharpLanguageService), ".prg")]
    [ProvideLanguageExtension(typeof(XSharpLanguageService), ".xs")]
    [ProvideLanguageExtension(typeof(XSharpLanguageService), ".ppo")]
    [ProvideLanguageExtension(typeof(XSharpLanguageService), ".vh")]
    [ProvideLanguageExtension(typeof(XSharpLanguageService), ".xh")]
    [ProvideLanguageExtension(typeof(XSharpLanguageService), ".ch")]
    [ProvideLanguageService(typeof(XSharpLanguageService),
                         LanguageName,
                         1,                            // resource ID of localized language name
                         AutoOutlining = true,
                         CodeSense = true,             // Supports IntelliSense
                         CodeSenseDelay = 1000,        // Delay to wait
                         DefaultToInsertSpaces = true,
                         DefaultToNonHotURLs = true,
                         EnableAdvancedMembersOption = true,
                         EnableAsyncCompletion = true, // Supports background parsing
                         EnableCommenting = true,      // Supports commenting out code
                         EnableLineNumbers = true,
                         MatchBraces = true,
                         MatchBracesAtCaret = true,
                         MaxErrorMessages = 10,
                         QuickInfo = true,
                         RequestStockColors = false,   // Supplies custom colors
                         ShowCompletion = true,
                         ShowDropDownOptions = true,    // Supports NavigationBar
                         ShowMatchingBrace = true,

#if SMARTINDENT
                         ShowSmartIndent = true,
                         EnableFormatSelection = true,
#else
                         ShowSmartIndent = false,
                         EnableFormatSelection = false,
#endif
                         HideAdvancedMembersByDefault = true,
                         SingleCodeWindowOnly = false,
                         ShowHotURLs = true,
                         SupportCopyPasteOfHTML = true

                 )]
    [ProvideLanguageCodeExpansionAttribute(
         typeof(XSharpLanguageService),
         LanguageName,  // Name of language used as registry key.
         1,         // Resource ID of localized name of language service.
         LanguageName,  // language key used in snippet templates.
         @"%InstallRoot%\Common7\IDE\Extensions\XSharp\Snippets\%LCID%\SnippetsIndex.xml",  // Path to snippets index
         SearchPaths = @"%InstallRoot%\Common7\IDE\Extensions\XSharp\Snippets\%LCID%\Snippets;" +
                  @"\%MyDocs%\Code Snippets\XSharp\My Code Snippets"
         )]
    [ProvideLanguageEditorOptionPageAttribute(
                 typeof(IntellisenseOptionsPage),  // GUID of property page
                 LanguageName,  // Language Name
                 null,      // Page Category
                 "Intellisense",// Page name
                 "#201"         // Localized name of property page
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
    [ProvideMenuResource("Menus.ctmenu", 1)]
    //[ProvideBindingPath]        // Tell VS to look in our path for assemblies
    public sealed class XSharpProjectPackage : ProjectPackage, IOleComponent,
        IVsShellPropertyEvents, IVsDebuggerEvents, XSharpModel.IOutputWindow
    {
        private uint m_componentID;
        private static XSharpProjectPackage instance;
        private XPackageSettings settings;
        private uint shellCookie;
        private XSharpLibraryManager _libraryManager;
        private XSharpDocumentWatcher _documentWatcher;

        // =========================================================================================
        // Properties
        // =========================================================================================

        /// <summary>
        /// Gets the singleton XSharpProjectPackage instance.
        /// </summary>
        public static XSharpProjectPackage Instance
        {
            get { return XSharpProjectPackage.instance; }
        }

        public void OpenInBrowser(string url)
        {
            IVsWebBrowsingService service = (IVsWebBrowsingService)GetService(typeof(SVsWebBrowsingService));
            if (service != null)
            {
                IVsWindowFrame frame = null;
                service.Navigate(url, (uint)(__VSWBNAVIGATEFLAGS.VSNWB_WebURLOnly | __VSWBNAVIGATEFLAGS.VSNWB_ForceNew), out frame);
                frame.Show();
            }
        }
        internal IntellisenseOptionsPage GetIntellisenseOptionsPage()
        {
            var page = (IntellisenseOptionsPage)GetDialogPage(typeof(IntellisenseOptionsPage));
            return page;
        }

        internal IVsTextManager4 GetTextManager()
        {
            return (IVsTextManager4)GetService(typeof(SVsTextManager));
        }

        #region Overridden Implementation
        /// <summary>
        /// Initialization of the package; this method is called right after the package is sited, so this is the place
        /// where you can put all the initialization code that rely on services provided by VisualStudio.
        /// </summary>
        protected override void Initialize()
        {
            // Suspend walking until Solution is opened.
            base.SolutionListeners.Add(new ModelScannerEvents(this));
            base.Initialize();
            XSharpProjectPackage.instance = this;
            XSharpModel.XSolution.OutputWindow = this;
            this.RegisterProjectFactory(new XSharpProjectFactory(this));
            this.settings = new XPackageSettings(this);
            validateVulcanEditors();
            this.RegisterDebuggerEvents();
            // Indicate how to open the different source files : SourceCode or Designer ??
            this.RegisterEditorFactory(new XSharpEditorFactory(this));
            this.RegisterProjectFactory(new XSharpWPFProjectFactory(this));

            // editors for the binaries
            base.RegisterEditorFactory(new VOFormEditorFactory(this));
            base.RegisterEditorFactory(new VOMenuEditorFactory(this));
            base.RegisterEditorFactory(new VODBServerEditorFactory(this));
            base.RegisterEditorFactory(new VOFieldSpecEditorFactory(this));
            // Register the language service

            // Proffer the service.
            IServiceContainer serviceContainer = this as IServiceContainer;
            XSharpLanguageService langService = new XSharpLanguageService();
            langService.SetSite(this);
            serviceContainer.AddService(typeof(XSharpLanguageService),
                                        langService,
                                        true);

            // Register a timer to call our language service during
            // idle periods.
            IOleComponentManager mgr = GetService(typeof(SOleComponentManager))
                                       as IOleComponentManager;
            if (m_componentID == 0 && mgr != null)
            {
                OLECRINFO[] crinfo = new OLECRINFO[1];
                crinfo[0].cbSize = (uint)Marshal.SizeOf(typeof(OLECRINFO));
                crinfo[0].grfcrf = (uint)_OLECRF.olecrfNeedIdleTime |
                                              (uint)_OLECRF.olecrfNeedPeriodicIdleTime;
                crinfo[0].grfcadvf = (uint)_OLECADVF.olecadvfModal |
                                              (uint)_OLECADVF.olecadvfRedrawOff |
                                              (uint)_OLECADVF.olecadvfWarningsOff;
                crinfo[0].uIdleTimeInterval = 1000;
                int hr = mgr.FRegisterComponent(this, crinfo, out m_componentID);
            }
            // Initialize Custom Menu Items
            XSharp.Project.XSharpMenuItems.Initialize(this);
            // register property changed event handler
            var shell = this.GetService(typeof(SVsShell)) as IVsShell;
            shell.AdviseShellPropertyChanges(this, out shellCookie);
            //
            // LibraryManager : Offers Object Browser and ClassView
            // ObjectBrowser : Add the LibraryManager service as a Service provided by that container
            IServiceContainer container = this as IServiceContainer;
            ServiceCreatorCallback callback = new ServiceCreatorCallback(CreateService);
            //
            container.AddService(typeof(IXSharpLibraryManager), callback, true);
            this._documentWatcher = new XSharpDocumentWatcher(this);
        }


        private object CreateService(IServiceContainer container, Type serviceType)
        {
            if (typeof(IXSharpLibraryManager) == serviceType)
            {
                return _libraryManager = new XSharpLibraryManager(this);
            }
            return null;
        }


        protected override void Dispose(bool disposing)
        {
            try
            {
                this.UnRegisterDebuggerEvents();
                if (null != _libraryManager)
                {
                    _libraryManager.Dispose();
                    _libraryManager = null;
                }
            }
            finally
            {
                base.Dispose(disposing);
            }
        }


        private bool CheckKey(string editor, string extension)
        {
            var root = VSRegistry.RegistryRoot(__VsLocalRegistryType.RegType_Configuration);
            var reg = root.OpenSubKey("editors\\" + editor + "\\Extensions");
            bool Ok = true;
            if (reg != null)
            {
                object value = reg.GetValue(extension);
                if (value is int && (int)value >= 0x42)
                {
                    Ok = false;
                }
                reg.Close();
            }
            return Ok;
        }

        private void validateVulcanEditors()
        {
            // check Vulcan Source code editor keys
            bool Ok = true;
            // Source editor
            Ok = Ok && CheckKey(GuidStrings.guidVulcanSourceCodeEditor, "prg");
            Ok = Ok && CheckKey(GuidStrings.guidVulcanSourceCodeEditor, "ppo");
            Ok = Ok && CheckKey(GuidStrings.guidVulcanSourceCodeEditor, "vh");
            Ok = Ok && CheckKey(GuidStrings.guidVulcanFormEditor, "vnfrm");
            Ok = Ok && CheckKey(GuidStrings.guidVulcanMenuEditor, "vnmnu");
            Ok = Ok && CheckKey(GuidStrings.guidVulcanDbEditor, "vndbs");
            Ok = Ok && CheckKey(GuidStrings.guidVulcanFsEditor, "vnfs");
            if (!Ok)
            {
                int result = 0;
                Guid tempGuid = Guid.Empty;

                IVsUIShell VsUiShell = (IVsUIShell)GetService(typeof(SVsUIShell));
                ErrorHandler.ThrowOnFailure(VsUiShell.ShowMessageBox(0, ref tempGuid, "File Associations",
                    "The Vulcan file associations must be changed.\nPlease run setup again\n\n" +
                    "Failure to do so may result in unexpected behavior inside Visual Studio",
                    null, 0,
                    OLEMSGBUTTON.OLEMSGBUTTON_OK, OLEMSGDEFBUTTON.OLEMSGDEFBUTTON_FIRST,
                    OLEMSGICON.OLEMSGICON_CRITICAL, 0, out result));
            }







        }

        /// <summary>
        /// Gets the settings stored in the registry for this package.
        /// </summary>
        /// <value>The settings stored in the registry for this package.</value>
        public XPackageSettings Settings
        {
            get { return this.settings; }
        }
        public override string ProductUserContext
        {
            get { return "XSharp"; }
        }

        #endregion

        #region IOleComponent Members

        public int FDoIdle(uint grfidlef)
        {
            bool bPeriodic = (grfidlef & (uint)_OLEIDLEF.oleidlefPeriodic) != 0;
            // Use typeof(TestLanguageService) because we need to
            // reference the GUID for our language service.
            Microsoft.VisualStudio.Package.LanguageService service = GetService(typeof(XSharpLanguageService))
                                      as Microsoft.VisualStudio.Package.LanguageService;
            if (service != null)
            {
                service.OnIdle(bPeriodic);
            }
            if (_libraryManager != null)
                _libraryManager.OnIdle();

            var walker = XSharpModel.ModelWalker.GetWalker();
            if (walker != null && !walker.IsWalkerRunning && walker.HasWork)
            {
                walker.Walk();
            }
            return 0;
        }

        public int FContinueMessageLoop(uint uReason,
                                        IntPtr pvLoopData,
                                        MSG[] pMsgPeeked)
        {
            return 1;
        }

        public int FPreTranslateMessage(MSG[] pMsg)
        {
            return 0;
        }

        public int FQueryTerminate(int fPromptUser)
        {
            return 1;
        }

        public int FReserved1(uint dwReserved,
                              uint message,
                              IntPtr wParam,
                              IntPtr lParam)
        {
            return 1;
        }

        public IntPtr HwndGetWindow(uint dwWhich, uint dwReserved)
        {
            return IntPtr.Zero;
        }

        public void OnActivationChange(IOleComponent pic,
                                       int fSameComponent,
                                       OLECRINFO[] pcrinfo,
                                       int fHostIsActivating,
                                       OLECHOSTINFO[] pchostinfo,
                                       uint dwReserved)
        {
        }

        public void OnAppActivate(int fActive, uint dwOtherThreadID)
        {
            //System.Diagnostics.Debug.WriteLine($"OnAppActivate: {fActive} {dwOtherThreadID}");
        }

        public void OnEnterState(uint uStateID, int fEnter)
        {
            //    System.Diagnostics.Debug.WriteLine($"OnEnterState: {uStateID} {fEnter}");
        }

        public void OnLoseActivation()
        {
            //  System.Diagnostics.Debug.WriteLine($"OnLoseActivation");
        }

        public void Terminate()
        {
            var shell = this.GetService(typeof(SVsShell)) as IVsShell;
            if (shell != null)
            {
                shell.UnadviseShellPropertyChanges(shellCookie);
                shellCookie = 0;
            }
        }

        public int OnShellPropertyChange(int propid, object var)
        {
            // A modal dialog has been opened. Editor Options ?
            if (propid == (int)__VSSPROPID4.VSSPROPID_IsModal)
            {
                CommandFilter.InvalidateOptions();
            }
            return VSConstants.S_OK;
        }
        #endregion

        #region IVSDebuggerEvents
        private IVsDebugger m_debugger = null;
        private uint m_Debuggercookie = 0;
        DBGMODE[] modeArray = new DBGMODE[1];
        private void RegisterDebuggerEvents()
        {
            int hr;
            m_debugger = base.GetService(typeof(SVsShellDebugger)) as IVsDebugger;
            if (m_debugger != null)
            {
                hr = m_debugger.AdviseDebuggerEvents(this, out m_Debuggercookie);
                Microsoft.VisualStudio.ErrorHandler.ThrowOnFailure(hr);
                // Get initial value
                hr = m_debugger.GetMode(modeArray);

            }
        }
        private void UnRegisterDebuggerEvents()
        {
            int hr;
            if (m_debugger != null)
            {
                if (m_Debuggercookie != 0)
                {
                    hr = m_debugger.UnadviseDebuggerEvents(m_Debuggercookie);
                    Microsoft.VisualStudio.ErrorHandler.ThrowOnFailure(hr);
                    m_Debuggercookie = 0;
                }
                m_debugger = null;
            }

        }
        public int OnModeChange(DBGMODE dbgmodeNew)
        {
            modeArray[0] = dbgmodeNew;
            return VSConstants.S_OK;
        }
        internal bool DebuggerIsRunning => modeArray[0] != DBGMODE.DBGMODE_Design;
        #endregion

        public void DisplayException(Exception ex)
        {
            if (GetIntellisenseOptionsPage().EnableOutputPane)
            {
                string space = "";
                while (ex != null)
                {
                    XSharpOutputPane.DisplayOutPutMessage(space+"**** Exception *** " + ex.GetType().FullName);
                    XSharpOutputPane.DisplayOutPutMessage(space + ex.Message);
                    XSharpOutputPane.DisplayOutPutMessage(space + ex.StackTrace);
                    ex = ex.InnerException;
                    space += " ";
                }

            }

        }

        public void DisplayOutPutMessage(string message)
        {
            if (GetIntellisenseOptionsPage().EnableOutputPane)
            {
                XSharpOutputPane.DisplayOutPutMessage(message);
            }
        }

    }


}
