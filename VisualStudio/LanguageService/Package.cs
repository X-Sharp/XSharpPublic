//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio;
using System;
using System.Runtime.InteropServices;
using System.ComponentModel;
using static XSharp.XSharpConstants;
using XSharp.LanguageService.OptionsPages;
using System.Threading;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.VisualStudio.Shell.Interop;
using System.ComponentModel.Design;
using Microsoft.VisualStudio.ComponentModelHost;
using XSharpModel;
using Microsoft.Win32;
using Microsoft;
using Microsoft.VisualStudio.OLE.Interop;

// The following lines ensure that the right versions of the various DLLs are loaded.
// They will be included in the generated PkgDef folder for the project system
[assembly: ProvideCodeBase(AssemblyName = "XSharp.VsParser", CodeBase = "XSharp.VsParser.dll", Culture = "neutral", PublicKeyToken = XSharp.Constants.PublicKey, Version = XSharp.Constants.Version)]
[assembly: ProvideCodeBase(AssemblyName = "XSharpModel", CodeBase = "XSharpModel.dll", Culture = "neutral", PublicKeyToken = XSharp.Constants.PublicKey, Version = XSharp.Constants.Version)]
namespace XSharp.LanguageService
{

    /// <summary>
    /// This class implements the package exposed by this assembly.
    /// </summary>
    /// <remarks>
    ///
    [Guid(GuidStrings.guidXSharpLanguageServicePkgString)]
    [PackageRegistration(UseManagedResourcesOnly = true, AllowsBackgroundLoading = true)]
    [ProvideAutoLoad(VSConstants.UICONTEXT.SolutionExistsAndFullyLoaded_string, PackageAutoLoadFlags.BackgroundLoad)]
    [DefaultRegistryRoot("Software\\Microsoft\\VisualStudio\\14.0")]
    [ProvideService(typeof(XSharpLanguageService), ServiceName = LanguageServiceName, IsAsyncQueryable = false)]//
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
                         ShowSmartIndent = true,
                         EnableFormatSelection = true,
                         HideAdvancedMembersByDefault = true,
                         SingleCodeWindowOnly = false,
                         ShowHotURLs = true,
                         SupportCopyPasteOfHTML = true
                 )]
    [ProvideLanguageCodeExpansion(
         typeof(XSharpLanguageService),
         LanguageName,  // Name of language used as registry key.
         1,         // Resource ID of localized name of language service.
         LanguageName,  // language key used in snippet templates.
         @"%InstallRoot%\Common7\IDE\Extensions\XSharp\Snippets\%LCID%\SnippetsIndex.xml",  // Path to snippets index
         SearchPaths = @"%InstallRoot%\Common7\IDE\Extensions\XSharp\Snippets\%LCID%\Snippets;" +
                  @"\%MyDocs%\Code Snippets\XSharp\My Code Snippets"
         )]
    //Note that the name of the entry in Tools/Options/TextEditor is defined in VsPackage.Resx in item #1 as X#
    [ProvideLanguageEditorOptionPage(typeof(IntellisenseOptionsPage), LanguageName, null, "Intellisense", pageNameResourceId: "201")]  // keywordlistresourceid
    [ProvideLanguageEditorOptionPage(typeof(FormattingOptionsPage), LanguageName, null, "Formatting", pageNameResourceId: "202")]       // keywordlistresourceid
    public sealed class XSharpLanguageService : AsyncPackage, IVsShellPropertyEvents, IVsDebuggerEvents, IOleComponent
    {
        private static XSharpLanguageService instance;
        private IVsTextManager4 _txtManager;
        private uint shellCookie;
        private XSharpLibraryManager _libraryManager;
        private uint m_componentID;
        private IOleComponentManager _oleComponentManager = null;

        public static XSharpLanguageService Instance
        {
            get { return XSharpLanguageService.instance; }
        }

        private object CreateLibraryService(IServiceContainer container, Type serviceType)
        {
            if (typeof(IXSharpLibraryManager) == serviceType)
            {
                return _libraryManager = new XSharpLibraryManager(this);
            }
            return null;
        }

        IntellisenseOptionsPage _intellisensePage;
        FormattingOptionsPage _formattingPage;
        internal void GetIntellisenseSettings()
        {
            if (_intellisensePage == null)
            {
                _intellisensePage = (IntellisenseOptionsPage)GetDialogPage(typeof(IntellisenseOptionsPage));
            }
            if (_formattingPage == null)
            {
                _formattingPage = (FormattingOptionsPage)GetDialogPage(typeof(FormattingOptionsPage));
            }
            if (_intellisensePage.SettingsChanged)
            {
                XSettings.EnableLogging = _intellisensePage.EnableOutputPane;
                XSettings.EnableBraceMatchLog = _intellisensePage.EnableBraceMatchLog;
                XSettings.EnableCodeCompletionLog = _intellisensePage.EnableCodeCompletionLog;
                XSettings.EnableDatabaseLog = _intellisensePage.EnableDatabaseLog;
                XSettings.EnableParameterLog = _intellisensePage.EnableParameterLog;
                XSettings.EnableParseLog = _intellisensePage.EnableParserLog;
                XSettings.EnableQuickInfoLog = _intellisensePage.EnableQuickInfoLog;
                XSettings.EnableReferenceInfoLog = _intellisensePage.EnableReferenceInfoLog;
                XSettings.EnableTypelookupLog = _intellisensePage.EnableTypelookupLog;

                XSettings.DisableAssemblyReferences = _intellisensePage.DisableAssemblyReferences;
                XSettings.DisableBraceMatching = _intellisensePage.DisableBraceMatching;
                XSettings.DisableCaseSynchronization = _intellisensePage.DisableCaseSynchronization;
                XSettings.DisableClassViewObjectView = _intellisensePage.DisableClassViewObjectView;
                XSettings.DisableCodeCompletion = _intellisensePage.DisableCodeCompletion;
                XSettings.DisableEditorDropDowns = _intellisensePage.DisableEditorDropdowns;
                XSettings.DisableEntityParsing = _intellisensePage.DisableEntityParsing;
                XSettings.DisableForeignProjectReferences = _intellisensePage.DisableForeignProjectReferences;
                XSettings.DisableGotoDefinition = _intellisensePage.DisableGotoDefinition;
                XSettings.DisableHighLightWord = _intellisensePage.DisableHighLightWord;
                XSettings.DisableLightBulb = _intellisensePage.DisableLightBulb;
                XSettings.DisableParameterInfo = _intellisensePage.DisableParameterInfo;
                XSettings.DisablePeekDefinition = _intellisensePage.DisablePeekDefinition;
                XSettings.DisableQuickInfo = _intellisensePage.DisableQuickInfo;
                XSettings.DisableRegions = _intellisensePage.DisableRegions;
                XSettings.DisableSyntaxHighlighting = _intellisensePage.DisableSyntaxColorization;
                XSettings.DisableXSharpProjectReferences = _intellisensePage.DisableXSharpProjectReferences;

                //XSettings.EditorIndentSize
                XSettings.EditorCompletionListTabs = _intellisensePage.CompletionListTabs;
                XSettings.EditorCommitChars = _intellisensePage.CommitChars;
                XSettings.EditorCompletionAutoPairs = _intellisensePage.AutoPairs;
                XSettings.EditorCompletionListAfterEachChar = false; // _intellisensePage.ShowAfterChar;
                XSettings.EditorKeywordsInAll = _intellisensePage.KeywordsInAll;

                XSettings.EditorNavigationSorted = _intellisensePage.SortNavigationBars;
                XSettings.EditorNavigationIncludeFields = _intellisensePage.IncludeFieldsInNavigationBars;
                XSettings.EditorNavigationMembersOfCurrentTypeOnly = _intellisensePage.ShowMembersOfCurrentTypeOnly;
                XSettings.EditorNavigationExcludeMembersFromOtherFiles = _intellisensePage.ExcludeMembersFromOtherFiles;
                var languagePreferences = new LANGPREFERENCES3[1];
                languagePreferences[0].guidLang = GuidStrings.guidLanguageService;
                int result = VSConstants.S_FALSE;
                ThreadHelper.JoinableTaskFactory.Run(async delegate
                {
                    await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                    result =  _txtManager.GetUserPreferences4(pViewPrefs: null, pLangPrefs: languagePreferences, pColorPrefs: null);
                });
                if (result == VSConstants.S_OK)
                {
                    XSettings.EditorIndentStyle= (int) languagePreferences[0].IndentStyle;
                    XSettings.EditorHideAdvancedMembers = languagePreferences[0].fHideAdvancedAutoListMembers != 0;
                    XSettings.EditorTabSize = (int)languagePreferences[0].uTabSize;
                    XSettings.EditorIndentSize = (int) languagePreferences[0].uIndentSize;
                    XSettings.EditorTabsAsSpaces  = languagePreferences[0].fInsertTabs == 0;
                }

                XSettings.EditorIndentFactor = _formattingPage.MultiFactor;
                XSettings.EditorFormatAlignDoCase = _formattingPage.AlignDoCase;
                XSettings.EditorFormatAlignMethod = _formattingPage.AlignMethod;
                XSettings.IdentifierCase = _formattingPage.IdentifierCase;
                XSettings.UDCKeywordCase = _formattingPage.UdcCase;
                XSettings.EditorTrimTrailingWhiteSpace = _formattingPage.TrimTrailingWhiteSpace;
                XSettings.EditorInsertFinalNewline = _formattingPage.InsertFinalNewLine;
                XSettings.KeywordCase = _formattingPage.KeywordCase;

                _formattingPage.SettingsChanged = false;
                _intellisensePage.SettingsChanged = false;
            }
            return ;
        }

        protected override async System.Threading.Tasks.Task InitializeAsync(CancellationToken cancellationToken, IProgress<ServiceProgressData> progress)
        {
            instance = this;
            await base.InitializeAsync(cancellationToken, progress);
            _txtManager = await GetServiceAsync(typeof(SVsTextManager)) as IVsTextManager4;
            Assumes.Present(_txtManager);

            // register property changed event handler
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            XSharpXMLDocTools.Initialize();
            var shell = await this.GetServiceAsync(typeof(SVsShell)) as IVsShell;
            if (shell != null)
            {
                shell.AdviseShellPropertyChanges(this, out shellCookie);
            }

            IServiceContainer serviceContainer = this as IServiceContainer;
            XSharpLegacyLanguageService languageService = new XSharpLegacyLanguageService(serviceContainer);
            languageService.SetSite(this);

            serviceContainer.AddService(typeof(XSharpLegacyLanguageService),
                                        languageService,
                                        true);

            ServiceCreatorCallback callback = new ServiceCreatorCallback(CreateLibraryService);
            serviceContainer.AddService(typeof(IXSharpLibraryManager), callback, true);

            RegisterDebuggerEvents();
            addOurFileExtensionsForDiffAndPeek("Diff\\SupportedContentTypes");
            addOurFileExtensionsForDiffAndPeek("Peek\\SupportedContentTypes");


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
            GetIntellisenseSettings();
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
                if (_oleComponentManager != null)
                {
                    ThreadHelper.JoinableTaskFactory.Run(async delegate
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


        const string EXTENSIONS = ".prg;.ppo;.ch;.xh;.xs";
        private void addOurFileExtensionsForDiffAndPeek(string parent)
        {
            using (RegistryKey root = VSRegistry.RegistryRoot(__VsLocalRegistryType.RegType_Configuration, true))
            {
                if (root != null)
                {
                    using (RegistryKey key = root.OpenSubKey(parent, true))
                    {
                        key.SetValue(EXTENSIONS, "");
                    }
                }
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
                    GetIntellisenseSettings();
                }
            }
            return VSConstants.S_OK;
        }
        #region IVSDebuggerEvents
        private void RegisterDebuggerEvents()
        {
            int hr;
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                m_debugger = await this.GetServiceAsync(typeof(SVsShellDebugger)) as IVsDebugger;
                if (m_debugger != null)
                {
                    hr = m_debugger.AdviseDebuggerEvents(this, out m_Debuggercookie);
                    Microsoft.VisualStudio.ErrorHandler.ThrowOnFailure(hr);
                    // Get initial value
                    DBGMODE[] modeArray = new DBGMODE[1];
                    hr = m_debugger.GetMode(modeArray);
                    XSettings.DebuggerIsRunning = modeArray[0] == DBGMODE.DBGMODE_Run;
                }
            });
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
                    Microsoft.VisualStudio.ErrorHandler.ThrowOnFailure(hr);
                }
            });
            m_Debuggercookie = 0;
            m_debugger = null;
        }
        private IVsDebugger m_debugger = null;
        private uint m_Debuggercookie = 0;

        public int OnModeChange(DBGMODE dbgmodeNew)
        {
            XSettings.DebuggerIsRunning = dbgmodeNew != DBGMODE.DBGMODE_Design;
            return VSConstants.S_OK;
        }
#endregion
  
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
            if (_libraryManager != null)
                _libraryManager.OnIdle();

            var walker = XSharpModel.ModelWalker.GetWalker();
            if (walker != null && !walker.IsWalkerRunning && walker.HasWork)
            {
                walker.Walk();
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

}
