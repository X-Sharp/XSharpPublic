#undef LIBRARYMANAGER
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using Community.VisualStudio.Toolkit;
using Microsoft;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.ComponentModelHost;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.Win32;
using System;
using System.ComponentModel.Design;
using System.Runtime.InteropServices;
using System.Threading;
using XSharp.LanguageService.OptionsPages;
using XSharpModel;
using XSharp.Settings;

// The following lines ensure that the right versions of the various DLLs are loaded.
// They will be included in the generated PkgDef folder for the project system
[assembly: ProvideCodeBase(AssemblyName = "Community.VisualStudio.Toolkit")]
[assembly: ProvideCodeBase(AssemblyName = "XSharp.VsParser")]
[assembly: ProvideCodeBase(AssemblyName = "XSharp.CodeModel")]
[assembly: ProvideCodeBase(AssemblyName = "XSharp.CodeAnalysis")]
[assembly: ProvideCodeBase(AssemblyName = "XSharp.MonoCecil")]

namespace XSharp.LanguageService
{

    /// <summary>
    /// This class implements the package exposed by this assembly.
    /// </summary>
    /// <remarks>
    ///

    [Guid(XSharpConstants.guidXSharpLanguageServicePkgString)]
    [PackageRegistration(UseManagedResourcesOnly = true, AllowsBackgroundLoading = true)]
    [ProvideAutoLoad(VSConstants.UICONTEXT.ShellInitialized_string, PackageAutoLoadFlags.BackgroundLoad)]
    [ProvideService(typeof(XSharpLanguagePackage), ServiceName = XSharpConstants.LanguageServiceName, IsAsyncQueryable = false)]//
    // 109 in the next lines is the resource id of the editor (XSharp Source Code Editor)
    [ProvideEditorExtension(typeof(XSharpEditorFactory), ".prg", 0x42, DefaultName = XSharpConstants.EditorName, NameResourceID = 109)]
    [ProvideEditorExtension(typeof(XSharpEditorFactory), ".xs", 0x42, DefaultName = XSharpConstants.EditorName, NameResourceID = 109)]
    [ProvideEditorExtension(typeof(XSharpEditorFactory), ".ppo", 0x42, DefaultName = XSharpConstants.EditorName, NameResourceID = 109)]
    [ProvideEditorExtension(typeof(XSharpEditorFactory), ".vh", 0x42, DefaultName = XSharpConstants.EditorName, NameResourceID = 109)]
    [ProvideEditorExtension(typeof(XSharpEditorFactory), ".xh", 0x42, DefaultName = XSharpConstants.EditorName, NameResourceID = 109)]
    [ProvideEditorExtension(typeof(XSharpEditorFactory), ".ch", 0x42, DefaultName = XSharpConstants.EditorName, NameResourceID = 109)]
    [ProvideEditorExtension(typeof(XSharpEditorFactory), ".rc", 0x42, DefaultName = XSharpConstants.EditorName, NameResourceID = 109)]
    // This tells VS that we support Code and Designer view
    // The guids are VS specific and should not be changed
    [ProvideEditorLogicalView(typeof(XSharpEditorFactory), VSConstants.LOGVIEWID.Designer_string, IsTrusted = true)]
    [ProvideEditorLogicalView(typeof(XSharpEditorFactory), VSConstants.LOGVIEWID.Code_string, IsTrusted = true)]
    [ProvideLanguageExtension(typeof(XSharpLanguagePackage), ".prg")]
    [ProvideLanguageExtension(typeof(XSharpLanguagePackage), ".xs")]
    [ProvideLanguageExtension(typeof(XSharpLanguagePackage), ".ppo")]
    [ProvideLanguageExtension(typeof(XSharpLanguagePackage), ".vh")]
    [ProvideLanguageExtension(typeof(XSharpLanguagePackage), ".xh")]
    [ProvideLanguageExtension(typeof(XSharpLanguagePackage), ".ch")]
    [ProvideLanguageService(typeof(XSharpLanguagePackage),
                         XSharpConstants.LanguageName,
                         1,                            // resource ID of localized language name
                         AutoOutlining = true,
                         CodeSense = true,             // Supports IntelliSense
                         CodeSenseDelay = 1000,        // Delay to wait
                         DefaultToInsertSpaces = true,
                         DefaultToNonHotURLs = true,
                         EnableAdvancedMembersOption = true,
                         EnableAsyncCompletion = true, // Supports background parsing
                         EnableCommenting = true,      // Supports commenting out code
                         EnableFormatSelection = true,
                         EnableLineNumbers = true,
                         HideAdvancedMembersByDefault = true,
                         MatchBraces = true,
                         MatchBracesAtCaret = true,
                         MaxErrorMessages = 10,
                         QuickInfo = true,
                         RequestStockColors = true,   // Supplies custom colors
                         ShowCompletion = true,
                         ShowDropDownOptions = true,    // Supports NavigationBar
                         ShowMatchingBrace = true,
                         ShowSmartIndent = true,
                         SingleCodeWindowOnly = false,
                         ShowHotURLs = true,
                         SupportCopyPasteOfHTML = true
                 )]
    [ProvideLanguageCodeExpansion(
         typeof(XSharpLanguagePackage),
         XSharpConstants.LanguageName,  // Name of language used as registry key.
         1,         // Resource ID of localized name of language service.
         XSharpConstants.LanguageName,  // language key used in snippet templates.
         @"%InstallRoot%\Common7\IDE\Extensions\XSharp\Snippets\SnippetsIndex.xml",  // Path to snippets index
         SearchPaths = @"%InstallRoot%\Common7\IDE\Extensions\XSharp\Snippets\Snippets;" +
                  @"\%MyDocs%\Code Snippets\XSharp\My Code Snippets"
         )]

    //Note that the name of the entry in Tools/Options/TextEditor is defined in VsPackage.Resx in item #1 as X#
    [ProvideLanguageEditorOptionPage(typeof(FormattingOptionsPage), XSharpConstants.LanguageName, null, "Formatting", pageNameResourceId: "202", keywordListResourceId: 302)]
    [ProvideLanguageEditorOptionPage(typeof(OtherOptionsPage), XSharpConstants.LanguageName, null, "Options", pageNameResourceId: "203", keywordListResourceId: 303)]
    [ProvideLanguageEditorOptionPage(typeof(CompletionOptionsPage), XSharpConstants.LanguageName, null, "Settings Completion", pageNameResourceId: "204", keywordListResourceId: 304)]
    [ProvideLanguageEditorOptionPage(typeof(IntellisenseOptionsPage), XSharpConstants.LanguageName, null, "Intellisense", pageNameResourceId: "205", keywordListResourceId: 305)]
    [ProvideLanguageEditorOptionPage(typeof(IndentingOptionsPage), XSharpConstants.LanguageName, null, "Indentation", pageNameResourceId: "206", keywordListResourceId: 306)]
    [ProvideLanguageEditorOptionPage(typeof(GeneratorOptionsPage), XSharpConstants.LanguageName, null, "Generator", pageNameResourceId: "207", keywordListResourceId: 307)]
    public sealed class XSharpLanguagePackage : AsyncPackage, IVsShellPropertyEvents, IOleComponent
    {
        private static XSharpLanguagePackage instance;
        private IVsTextManager4 _txtManager;
        private uint shellCookie;
#if LIBRARYMANAGER
        private XSharpLibraryManager _libraryManager;
#endif
        private uint m_componentID;
        private IOleComponentManager _oleComponentManager = null;


        public static XSharpLanguagePackage Instance
        {
            get { return XSharpLanguagePackage.instance; }
        }

#if LIBRARYMANAGER
        private object CreateLibraryService(IServiceContainer container, Type serviceType)
        {
            if (typeof(IXSharpLibraryManager) == serviceType)
            {
                return _libraryManager = new XSharpLibraryManager(this);
            }
            return null;
        }
#endif

        IntellisenseOptionsPage _intellisensePage;
        FormattingOptionsPage _formattingPage;
        IndentingOptionsPage _indentingPage;
        OtherOptionsPage _otherOptionsPage;
        GeneratorOptionsPage _generatorOptionsPage;
        CompletionOptionsPage _completionOptionsPage;
        private T GetDialogPage<T>() where T : DialogPage
        {
            return (T)base.GetDialogPage(typeof(T));
        }

        private LanguageServiceOptions GetOptions()
        {
            // pages in alphabetical order
            if (_completionOptionsPage == null)
            {
                _completionOptionsPage = GetDialogPage<CompletionOptionsPage>();
            }
            if (_formattingPage == null)
            {
                _formattingPage = GetDialogPage<FormattingOptionsPage>();
            }
            if (_generatorOptionsPage == null)
            {
                _generatorOptionsPage = GetDialogPage<GeneratorOptionsPage>();
            }
            if (_indentingPage == null)
            {
                _indentingPage = GetDialogPage<IndentingOptionsPage>();
            }
            if (_intellisensePage == null)
            {
                _intellisensePage = GetDialogPage<IntellisenseOptionsPage>();
            }
            if (_otherOptionsPage == null)
            {
                _otherOptionsPage = GetDialogPage<OtherOptionsPage>();
            }
            return GetOptionsFromPages();
        }
        private LanguageServiceOptions GetOptionsFromPages()
        {
            var options = new LanguageServiceOptions();
            options.CompletionOptions = _completionOptionsPage.Options;
            options.GeneratorOptions = _generatorOptionsPage.Options;
            options.FormattingOptions = _formattingPage.Options;
            options.IndentingOptions = _indentingPage.Options;
            options.IntellisenseOptions = _intellisensePage.Options;
            options.OtherOptions = _otherOptionsPage.Options;
            _txtManager.GetSettings(options.TabOptions);
            options.LogOptions.ReadFromRegistry();
            return options;

        }
        public void GetIntellisenseSettings(bool load)
        {
            LanguageServiceOptions options = GetOptions();
            if (load)
            {
                options = LanguageServiceOptions.Load();
                if (options == null)
                {
                    options = GetOptionsFromPages();
                }
                else
                {
                    _completionOptionsPage.SetOptions(options.CompletionOptions);
                    _generatorOptionsPage.SetOptions(options.GeneratorOptions);
                    _formattingPage.SetOptions(options.FormattingOptions);
                    _indentingPage.SetOptions(options.IndentingOptions);
                    _intellisensePage.SetOptions(options.IntellisenseOptions);
                    _otherOptionsPage.SetOptions(options.OtherOptions);
                    _txtManager.SetSettings(options.TabOptions);

                }
            }
            else
            {
                options.Save();
            }
            options.WriteToSettings();
            options.WriteToRegistry();
            return;
        }
        

        protected override async System.Threading.Tasks.Task InitializeAsync(CancellationToken cancellationToken, IProgress<ServiceProgressData> progress)
        {
            instance = this;
            LanguageServiceEvents.Start();
            await base.InitializeAsync(cancellationToken, progress);
            _txtManager = await GetServiceAsync(typeof(SVsTextManager)) as IVsTextManager4;
            Assumes.Present(_txtManager);

            // register property changed event handler
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            await this.RegisterCommandsAsync();
            XSharpXMLDocTools.Initialize();
            var shell = await this.GetServiceAsync(typeof(SVsShell)) as IVsShell;
            if (shell != null)
            {
                shell.AdviseShellPropertyChanges(this, out shellCookie);
            }
            XSettings.Version = await VS.Shell.GetVsVersionAsync();
            this.RegisterEditorFactory(new XSharpEditorFactory(this));
            IServiceContainer serviceContainer = this as IServiceContainer;
            XSharpLanguageService languageService = new XSharpLanguageService(serviceContainer);
            languageService.SetSite(this);

            serviceContainer.AddService(typeof(XSharpLanguageService),
                                        languageService,
                                        true);
#if LIBRARYMANAGER
            if (!XSettings.DisableClassViewObjectView)
            {
                ServiceCreatorCallback callback = new ServiceCreatorCallback(CreateLibraryService);
                serviceContainer.AddService(typeof(IXSharpLibraryManager), callback, true);
            }
#endif


            // Register a timer to call several services
            // idle periods.
            _oleComponentManager = await GetServiceAsync(typeof(SOleComponentManager)) as IOleComponentManager;
            if (m_componentID == 0 && _oleComponentManager != null)
            {
                OLECRINFO[] crinfo = new OLECRINFO[1];
                crinfo[0].cbSize = (uint)Marshal.SizeOf(typeof(OLECRINFO));
                crinfo[0].grfcrf = (uint)_OLECRF.olecrfNeedIdleTime |
                                              (uint)_OLECRF.olecrfNeedPeriodicIdleTime;
                crinfo[0].grfcadvf = (uint)_OLECADVF.olecadvfModal |
                                              (uint)_OLECADVF.olecadvfRedrawOff |
                                              (uint)_OLECADVF.olecadvfWarningsOff;
                crinfo[0].uIdleTimeInterval = 1000;

                int hr = _oleComponentManager.FRegisterComponent(this, crinfo, out m_componentID);
            }
            GetIntellisenseSettings(true);
            await Commenting.InitializeAsync();
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            XSettings.LanguageService = this;
        }

        protected override void Dispose(bool disposing)
        {
            try
            {
#if LIBRARYMANAGER
                if (null != _libraryManager)
                {
                    _libraryManager.Dispose();
                    _libraryManager = null;
                }
#endif
                if (_oleComponentManager != null)
                {
                    ThreadHelper.JoinableTaskFactory.Run(async ( )=>
                    {
                        await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                        _oleComponentManager.FRevokeComponent(m_componentID);
                    });
                    _oleComponentManager = null;
                }

            }
            finally
            {
                base.Dispose(disposing);

            }
        }

        public void Terminate()
        {
            ThreadHelper.ThrowIfNotOnUIThread();
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
            if (propid == (int)__VSSPROPID4.VSSPROPID_IsModal && var is bool lValue)
            {
                // when modal window closes
                if (!lValue)
                {
                    GetIntellisenseSettings(false);
                    var res = ThreadHelper.JoinableTaskFactory.RunAsync(RefreshAllDocumentWindowSettingsAsync);

                }
            }
            return VSConstants.S_OK;
        }

        /// <summary>
        /// Reload the source code editor settings for all open X# editor windows
        /// </summary>
        /// <returns></returns>
        private async System.Threading.Tasks.Task RefreshAllDocumentWindowSettingsAsync()
        {
            var docs = await VS.Windows.GetAllDocumentWindowsAsync();
            foreach (var doc in docs)
            {
                var view = await doc.GetDocumentViewAsync();
                if (view != null)
                {
                    var buffer = view.TextBuffer;
                    if (buffer.GetClassifier() != null)
                    {
                        EditorConfigReader.ReadSettings(buffer, view.FilePath);
                    }
                }
            }
            return;
        }

        internal static IComponentModel GetComponentModel()
        {
            return (IComponentModel)GetGlobalService(typeof(SComponentModel));
        }

        #region IOLEComponent
        public int FReserved1(uint dwReserved, uint message, IntPtr wParam, IntPtr lParam)
        {
            return 1;
        }

        public int FPreTranslateMessage(MSG[] pMsg)
        {
            return 0;
        }

        public void OnEnterState(uint uStateID, int fEnter)
        {
        }

        public void OnAppActivate(int fActive, uint dwOtherThreadID)
        {
        }

        public void OnLoseActivation()
        {
        }

        public void OnActivationChange(IOleComponent pic, int fSameComponent, OLECRINFO[] pcrinfo, int fHostIsActivating, OLECHOSTINFO[] pchostinfo, uint dwReserved)
        {
        }

        public int FDoIdle(uint grfidlef)
        {
            bool bPeriodic = (grfidlef & (uint)_OLEIDLEF.oleidlefPeriodic) != 0;
#if LIBRARYMANAGER
            if (_libraryManager != null)
                _libraryManager.OnIdle();
#endif

            if (!ModelWalker.IsRunning && ModelWalker.HasWork)
            {
                ModelWalker.Walk();
            }
            return 0;
        }

        public int FContinueMessageLoop(uint uReason, IntPtr pvLoopData, MSG[] pMsgPeeked)
        {
            return 1;
        }

        public int FQueryTerminate(int fPromptUser)
        {
            return 1;
        }

        public IntPtr HwndGetWindow(uint dwWhich, uint dwReserved)
        {
            return IntPtr.Zero;
        }
        #endregion
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
    }
}
