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
using static XSharp.LanguageService.XSharpConstants;
using XSharp.LanguageService.OptionsPages;
using System.Threading;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.VisualStudio.Shell.Interop;
using System.ComponentModel.Design;
using Microsoft.VisualStudio.ComponentModelHost;
using XSharpModel;
using Microsoft.Win32;
using Microsoft;

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
                 null,          // Page Category
                 "Intellisense",// Page name
                 "#201"         // Localized name of property page
                 )]
    public sealed class XSharpLanguageService : AsyncPackage, IVsShellPropertyEvents, IVsDebuggerEvents
    {
        private static XSharpLanguageService instance;
        private IVsTextManager4 _txtManager;
        private uint shellCookie;

        public static XSharpLanguageService Instance
        {
            get { return XSharpLanguageService.instance; }
        }


        IntellisenseOptionsPage _intellisensePage;
        internal IntellisenseOptionsPage GetIntellisenseOptionsPage()
        {
            if (_intellisensePage == null)
            {
                _intellisensePage = (IntellisenseOptionsPage)GetDialogPage(typeof(IntellisenseOptionsPage));
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
                XSettings.EditorCompletionListAfterEachChar = _intellisensePage.ShowAfterChar;
                XSettings.EditorIndentFactor = _intellisensePage.MultiFactor;
                XSettings.EditorKeywordsInAll = _intellisensePage.KeywordsInAll;
                XSettings.EditorUseDotAsUniversalSelector = _intellisensePage.UseDotAsUniversalSelector;

                XSettings.EditorNavigationSorted = _intellisensePage.SortNavigationBars;
                XSettings.EditorNavigationIncludeFields = _intellisensePage.IncludeFieldsInNavigationBars;
                XSettings.EditorNavigationMembersOfCurrentTypeOnly = _intellisensePage.ShowMembersOfCurrentTypeOnly;
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
                

                XSettings.KeywordCase = _intellisensePage.KeywordCase;
                _intellisensePage.SettingsChanged = false;
            }
            return _intellisensePage;
        }

        protected override async System.Threading.Tasks.Task InitializeAsync(CancellationToken cancellationToken, IProgress<ServiceProgressData> progress)
        {
            instance = this;
            await base.InitializeAsync(cancellationToken, progress);
            _txtManager = await GetServiceAsync(typeof(SVsTextManager)) as IVsTextManager4;
            Assumes.Present(_txtManager);

            // register property changed event handler
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();

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
            RegisterDebuggerEvents();
            addOurFileExtensionsForDiffAndPeek("Diff\\SupportedContentTypes");
            addOurFileExtensionsForDiffAndPeek("Peek\\SupportedContentTypes");

            _txtManager = await GetServiceAsync(typeof(SVsTextManager)) as IVsTextManager4;

        }

        protected override void Dispose(bool disposing)
        {
            try
            {

                this.UnRegisterDebuggerEvents();
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
                    GetIntellisenseOptionsPage();
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
                m_debugger = await this.GetServiceAsync(typeof(SVsShellDebugger)) as IVsDebugger;
                if (m_debugger != null)
                {
                    await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
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
            if (m_debugger != null && m_Debuggercookie != 0)
            {
                hr = m_debugger.UnadviseDebuggerEvents(m_Debuggercookie);
                Microsoft.VisualStudio.ErrorHandler.ThrowOnFailure(hr);
            }
            m_Debuggercookie = 0;
            m_debugger = null;
        }
        private IVsDebugger m_debugger = null;
        private uint m_Debuggercookie = 0;

        public int OnModeChange(DBGMODE dbgmodeNew)
        {
            XSettings.DebuggerIsRunning = dbgmodeNew == DBGMODE.DBGMODE_Run;
            return VSConstants.S_OK;
        }
#endregion
        internal IVsTextManager4 GetTextManager()
        {
            return this._txtManager;
        }

        internal static void DisplayOutputMessage(string message)
        {
            XSettings.DisplayOutputMessage(message);
        }
        internal static void DisplayException(Exception e)
        {
            XSettings.DisplayException(e);
        }
        internal static void ShowMessageBox(string message)
        {
            XSettings.ShowMessageBox(message);
        }
        internal static IComponentModel GetComponentModel()
        {
            return (IComponentModel)GetGlobalService(typeof(SComponentModel));
        }

      
    }

}
