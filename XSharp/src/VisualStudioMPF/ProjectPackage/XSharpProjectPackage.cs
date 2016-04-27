
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
    [Description("XSharp Project System")]
    [PackageRegistration(UseManagedResourcesOnly = true)]
    [DefaultRegistryRoot("Software\\Microsoft\\VisualStudio\\14.0")]
    [ProvideObject(typeof(XSharpGeneralPropertyPage))]      // 53651BEA-799A-45EB-B58C-C884F5417219
    [ProvideObject(typeof(XSharpLanguagePropertyPage))]     // 2652FCA6-1C45-4D25-942D-4C5D5EDE9539
    [ProvideObject(typeof(XSharpBuildPropertyPage))]        // E994C210-9D6D-4CF4-A061-EBBEA2BC626B
    [ProvideObject(typeof(XSharpDebugPropertyPage))]        // 2955A638-C389-4675-BB1C-6B2BC173C1E7
    [ProvideProjectFactory(typeof(XSharpProjectFactory), 
        XSharpConstants.LanguageName, 
        XSharpConstants.LanguageName + " Project Files (*." + XSharpConstants.ProjectExtension + ");*." + XSharpConstants.ProjectExtension,
        XSharpConstants.ProjectExtension,
        XSharpConstants.ProjectExtensions,
        @".NullPath", LanguageVsTemplate = "XSharp", NewProjectRequireNewFolderVsTemplate = false)]

    [ProvideService(typeof(XSharpLanguageService), ServiceName = "XSharp Language Service")]
    [ProvideLanguageExtension(typeof(XSharpLanguageService), ".prg")]
    [ProvideLanguageExtension(typeof(XSharpLanguageService), ".ppo")]
    [ProvideLanguageExtension(typeof(XSharpLanguageService), ".xs")]
    [ProvideLanguageExtension(typeof(XSharpLanguageService), ".xh")]
    [ProvideLanguageExtension(typeof(XSharpLanguageService), ".vh")]
    [ProvideLanguageService(typeof(XSharpLanguageService),
                         "XSharp",
                         1,                            // resource ID of localized language name
                         CodeSense = true,             // Supports IntelliSense
                         RequestStockColors = false,   // Supplies custom colors
                         EnableCommenting = true,      // Supports commenting out code
                         EnableAsyncCompletion = true  // Supports background parsing
                         )]
    [ProvideLanguageCodeExpansionAttribute(
         typeof(XSharpLanguageService),
         "XSharp",  // Name of language used as registry key.
         1,         // Resource ID of localized name of language service.
         "XSharp",  // language key used in snippet templates.
         @"%InstallRoot%\XSharp Language\SnippetsIndex.xml",  // Path to snippets index
         SearchPaths = @"%InstallRoot%\XSharp Language\Snippets\%LCID%\Snippets\;" +
                       @"%TestDocs%\Code Snippets\XSharp Language\XSharp Code Snippets"

         )]
    /*
        [ProvideLanguageEditorOptionPageAttribute()
                 "{A2FE74E1-FFFF-3311-4342-123052450768}",  // GUID of property page
                 "XSharp",  // Registry key name for language
                 "Options",      // Registry key name for property page
                 "#242"         // Localized name of property page
                 )]
        [ProvideLanguageEditorOptionPageAttribute(
                 "XSharp",  // Registry key name for language
                 "Advanced",     // Registry key name for node
                 "#243"         // Localized name of node
                 )]
        [ProvideLanguageEditorOptionPageAttribute(
            "{A2FE74E2-FFFF-3311-4342-123052450768}",  // GUID of property page     
            "XSharp",  // Registry key name for language
                 @"Advanced\Indenting",     // Registry key name for property page
                 "#244"         // Localized name of property page

                 )]


    [ProvideLanguageEditorOptionPage(typeof(Options.AdvancedOptionPage), "CSharp", null, "Advanced", pageNameResourceId: "#102", keywordListResourceId: 306)]
        [ProvideLanguageEditorOptionPage(typeof(Options.Formatting.FormattingStylePage), "CSharp", null, @"Code Style", pageNameResourceId: "#114", keywordListResourceId: 313)]
        [ProvideLanguageEditorOptionPage(typeof(Options.IntelliSenseOptionPage), "CSharp", null, "IntelliSense", pageNameResourceId: "#103", keywordListResourceId: 312)]
        [ProvideLanguageEditorOptionPage(typeof(Options.Formatting.FormattingOptionPage), "CSharp", "Formatting", "General", pageNameResourceId: "#108", keywordListResourceId: 307)]
        [ProvideLanguageEditorOptionPage(typeof(Options.Formatting.FormattingIndentationOptionPage), "CSharp", "Formatting", "Indentation", pageNameResourceId: "#109", keywordListResourceId: 308)]
        [ProvideLanguageEditorOptionPage(typeof(Options.Formatting.FormattingWrappingPage), "CSharp", "Formatting", "Wrapping", pageNameResour
    */
    [ProvideProjectFactory(typeof(XSharpWPFProjectFactory),
        null,
        null,
        null,
        null,
        null,
        LanguageVsTemplate = XSharpConstants.LanguageName,
        TemplateGroupIDsVsTemplate = "WPF",
        ShowOnlySpecifiedTemplatesVsTemplate = false, SortPriority =100)]

    [ProvideProjectItem(typeof(XSharpProjectFactory), "XSharp Items", @"ItemTemplates\Class", 500)]
    [ProvideProjectItem(typeof(XSharpProjectFactory), "XSharp Items", @"ItemTemplates\Form", 500)]

    [ProvideEditorExtension(typeof(XSharpEditorFactory), ".prg", Int32.MaxValue,DefaultName = "XSharp Source Code Editor",NameResourceID =109)]
    [ProvideEditorExtension(typeof(XSharpEditorFactory), ".xs", Int32.MaxValue, DefaultName = "XSharp Source Code Editor", NameResourceID = 109)]
    [ProvideEditorExtension(typeof(XSharpEditorFactory), ".vh", Int32.MaxValue, DefaultName = "XSharp Source Code Editor", NameResourceID = 109)]
    [ProvideEditorExtension(typeof(XSharpEditorFactory), ".xh", Int32.MaxValue, DefaultName = "XSharp Source Code Editor", NameResourceID = 109)]
    [ProvideEditorExtension(typeof(XSharpEditorFactory), ".ppo", Int32.MaxValue, DefaultName = "XSharp Source Code Editor", NameResourceID = 109)]
    // Todo
    // Add extensions for VO Compatible editors here


    // Attention! The LOGVIEWID guids are magic numbers provided by Microsoft. Don't change them.
    [ProvideEditorLogicalView(typeof(XSharpEditorFactory), "{7651a702-06e5-11d1-8ebd-00a0c90f26ea}")]  //LOGVIEWID_Designer
    [ProvideEditorLogicalView(typeof(XSharpEditorFactory), "{7651a701-06e5-11d1-8ebd-00a0c90f26ea}")]  //LOGVIEWID_Code

    [Guid(GuidStrings.guidXSharpProjectPkgString)]
    public sealed class XSharpProjectPackage : ProjectPackage, IOleComponent
    {
        private uint m_componentID;


        #region Overridden Implementation
        /// <summary>
        /// Initialization of the package; this method is called right after the package is sited, so this is the place
        /// where you can put all the initialization code that rely on services provided by VisualStudio.
        /// </summary>
        protected override void Initialize()
        {
            base.Initialize();
            this.RegisterProjectFactory(new XSharpProjectFactory(this));

            // Indicate how to open the different source files : SourceCode or Designer ??
            this.RegisterEditorFactory(new XSharpEditorFactory(this));

            this.RegisterProjectFactory(new XSharpWPFProjectFactory(this));


            // Register the language service
            // Add our command handlers for menu (commands must exist in the .vsct file)
            OleMenuCommandService mcs = GetService(typeof(IMenuCommandService)) as OleMenuCommandService;
            if (null != mcs)
            {
                // Create the command for the menu item.
                CommandID menuCommandID = new CommandID(GuidList.guidXSharpLanguageServiceCmdSet, (int)PkgCmdIDList.cmdidInsertSnippet);
                MenuCommand menuItem = new MenuCommand(MenuItemCallback, menuCommandID);
                mcs.AddCommand(menuItem);
            }

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

         }

        public override string ProductUserContext
        {
            get { return "XSharp"; }
        }
        private void MenuItemCallback(object sender, EventArgs e)
        {
            // Show a Message Box to prove we were here
            IVsUIShell uiShell = (IVsUIShell)GetService(typeof(SVsUIShell));
            Guid clsid = Guid.Empty;
            int result;
            Microsoft.VisualStudio.ErrorHandler.ThrowOnFailure(uiShell.ShowMessageBox(
                       0,
                       ref clsid,
                       "XSharp Language Service",
                       string.Format(CultureInfo.CurrentCulture, "Inside {0}.MenuItemCallback()", this.ToString()),
                       string.Empty,
                       0,
                       OLEMSGBUTTON.OLEMSGBUTTON_OK,
                       OLEMSGDEFBUTTON.OLEMSGDEFBUTTON_FIRST,
                       OLEMSGICON.OLEMSGICON_INFO,
                       0,        // false
                       out result));
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
        }

        public void OnEnterState(uint uStateID, int fEnter)
        {
        }

        public void OnLoseActivation()
        {
        }

        public void Terminate()
        {
        }

        #endregion

    }

}
