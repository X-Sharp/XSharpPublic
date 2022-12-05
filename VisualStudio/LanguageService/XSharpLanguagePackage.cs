//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#undef COMPLETION
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
using Community.VisualStudio.Toolkit;
using System.Collections.Generic;

// The following lines ensure that the right versions of the various DLLs are loaded.
// They will be included in the generated PkgDef folder for the project system
[assembly: ProvideCodeBase(AssemblyName = "Community.VisualStudio.Toolkit")]
[assembly: ProvideCodeBase(AssemblyName = "XSharp.VsParser")]
[assembly: ProvideCodeBase(AssemblyName = "XSharpModel")]
namespace XSharp.LanguageService
{

    /// <summary>
    /// This class implements the package exposed by this assembly.
    /// </summary>
    /// <remarks>
    ///

    [Guid(GuidStrings.guidXSharpLanguageServicePkgString)]
    [PackageRegistration(UseManagedResourcesOnly = true, AllowsBackgroundLoading = true)]
    [ProvideAutoLoad(VSConstants.UICONTEXT.ShellInitialized_string,PackageAutoLoadFlags.BackgroundLoad)]
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
    [ProvideLanguageEditorOptionPage(typeof(FormattingOptionsPage), LanguageName, null, "Formatting", pageNameResourceId: "202", keywordListResourceId: 302)]
    [ProvideLanguageEditorOptionPage(typeof(OtherOptionsPage), LanguageName, null, "Options", pageNameResourceId: "203", keywordListResourceId: 303)]
#if COMPLETION
    [ProvideLanguageEditorOptionPage(typeof(CompletionOptionsPage), LanguageName, null, "Settings Completion", pageNameResourceId: "204",keywordListResourceId:304)]
#endif
    [ProvideLanguageEditorOptionPage(typeof(IntellisenseOptionsPage), LanguageName, null, "Intellisense", pageNameResourceId: "205", keywordListResourceId: 305)]
    [ProvideLanguageEditorOptionPage(typeof(IndentingOptionsPage), LanguageName, null, "Indentation", pageNameResourceId: "206", keywordListResourceId: 306)]
    [ProvideLanguageEditorOptionPage(typeof(GeneratorOptionsPage), LanguageName, null, "Generator", pageNameResourceId: "207", keywordListResourceId: 307)]
    public sealed class XSharpLanguageService : AsyncPackage, IVsShellPropertyEvents, IVsDebuggerEvents, IOleComponent
    {
        private static XSharpLanguageService instance;
        private IVsTextManager4 _txtManager;
        private uint shellCookie;
        private XSharpLibraryManager _libraryManager;
        private uint m_componentID;
        private IOleComponentManager _oleComponentManager = null;
        internal bool optionWasChanged = false;

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
        IndentingOptionsPage _indentingPage;
        OtherOptionsPage _otherOptionsPage;
        GeneratorOptionsPage _generatorOptionsPage;
#if COMPLETION
        CompletionOptionsPage _completionOptionsPage;
#endif
        public void GetIntellisenseSettings()
        {
            if (_intellisensePage == null)
            {
                _intellisensePage = (IntellisenseOptionsPage)GetDialogPage(typeof(IntellisenseOptionsPage));
            }
            if (_formattingPage == null)
            {
                _formattingPage = (FormattingOptionsPage)GetDialogPage(typeof(FormattingOptionsPage));
            }
            if (_indentingPage == null)
            {
                _indentingPage = (IndentingOptionsPage)GetDialogPage(typeof(IndentingOptionsPage));
            }
            if (_otherOptionsPage == null)
            {
                _otherOptionsPage = (OtherOptionsPage)GetDialogPage(typeof(OtherOptionsPage));
            }
            if (_otherOptionsPage == null)
            {
                _otherOptionsPage = (OtherOptionsPage)GetDialogPage(typeof(OtherOptionsPage));
            }
            if (_generatorOptionsPage == null)
            {
                _generatorOptionsPage = (GeneratorOptionsPage)GetDialogPage(typeof(GeneratorOptionsPage));
            }
#if COMPLETION
            if (_completionOptionsPage == null)
            {
                _completionOptionsPage = (CompletionOptionsPage)GetDialogPage(typeof(CompletionOptionsPage));
            }
#endif
            // Intellisense
            XSettings.EnableOutputWindowLogging = _intellisensePage.EnableOutputPane;
            XSettings.EnableBraceMatchLog = _intellisensePage.EnableBraceMatchLog;
            XSettings.EnableCodeCompletionLog = _intellisensePage.EnableCodeCompletionLog;
            XSettings.EnableDatabaseLog = _intellisensePage.EnableDatabaseLog;
            XSettings.EnableParameterLog = _intellisensePage.EnableParameterLog;
            XSettings.EnableParseLog = _intellisensePage.EnableParserLog;
            XSettings.EnableQuickInfoLog = _intellisensePage.EnableQuickInfoLog;
            XSettings.EnableReferenceInfoLog = _intellisensePage.EnableReferenceInfoLog;
            XSettings.EnableTypelookupLog = _intellisensePage.EnableTypelookupLog;


            XSettings.DisableAssemblyReferences = _intellisensePage.DisableAssemblyReferences;
            XSettings.DisableClassViewObjectView = _intellisensePage.DisableClassViewObjectView;
            XSettings.DisableEditorDropDowns = _intellisensePage.DisableEditorDropdowns;
            XSettings.DisableEntityParsing = _intellisensePage.DisableEntityParsing;
            XSettings.DisableForeignProjectReferences = _intellisensePage.DisableForeignProjectReferences;
            XEditorSettings.DisableSyntaxHighlighting = _intellisensePage.DisableSyntaxColorization;
            XSettings.DisableXSharpProjectReferences = _intellisensePage.DisableXSharpProjectReferences;

            //XEditorSettings.IndentSize
            XEditorSettings.CompletionListTabs = _intellisensePage.CompletionListTabs;
            XEditorSettings.CommitChars = _intellisensePage.CommitChars;
            XEditorSettings.CompletionListAfterEachChar = false; // _intellisensePage.ShowAfterChar;
            XEditorSettings.KeywordsInAll = _intellisensePage.KeywordsInAll;

            XEditorSettings.NavigationSorted = _intellisensePage.SortNavigationBars;
            XEditorSettings.NavigationIncludeFields = _intellisensePage.IncludeFieldsInNavigationBars;
            XEditorSettings.NavigationMembersOfCurrentTypeOnly = _intellisensePage.ShowMembersOfCurrentTypeOnly;
            XEditorSettings.NavigationExcludeMembersFromOtherFiles = _intellisensePage.ExcludeMembersFromOtherFiles;
            var languagePreferences = new LANGPREFERENCES3[1];
            languagePreferences[0].guidLang = GuidStrings.guidLanguageService;
            int result = VSConstants.S_FALSE;
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                result = _txtManager.GetUserPreferences4(pViewPrefs: null, pLangPrefs: languagePreferences, pColorPrefs: null);
            });
            if (result == VSConstants.S_OK)
            {
                XEditorSettings.IndentStyle = (int)languagePreferences[0].IndentStyle;
                XEditorSettings.HideAdvancedMembers = languagePreferences[0].fHideAdvancedAutoListMembers != 0;
                XEditorSettings.TabSize = (int)languagePreferences[0].uTabSize;
                XEditorSettings.IndentSize = (int)languagePreferences[0].uIndentSize;
                XEditorSettings.TabsAsSpaces = languagePreferences[0].fInsertTabs == 0;
            }
            // Formatting
            XEditorSettings.IndentFactor = _formattingPage.MultiFactor;
            XEditorSettings.IdentifierCase = _formattingPage.IdentifierCase;
            XEditorSettings.UDCKeywordCase = _formattingPage.UdcCase;
            XEditorSettings.TrimTrailingWhiteSpace = _formattingPage.TrimTrailingWhiteSpace;
            XEditorSettings.InsertFinalNewline = _formattingPage.InsertFinalNewLine;
            XEditorSettings.KeywordCase = _formattingPage.KeywordCase;
            // Indentation

            // validate indentation settings
            _indentingPage.ValidateSettings();
            XEditorSettings.IndentTypeMembers = _indentingPage.IndentEntityContent;
            XEditorSettings.IndentTypeFields = _indentingPage.IndentFieldContent;
            XEditorSettings.IndentStatements = _indentingPage.IndentBlockContent;
            XEditorSettings.IndentCaseContent = _indentingPage.IndentCaseContent;
            XEditorSettings.IndentCaseLabel = _indentingPage.IndentCaseLabel;
            XEditorSettings.IndentContinuedLines = _indentingPage.IndentMultiLines;
            XEditorSettings.IndentPreprocessorLines = _indentingPage.IndentPreprocessorLines;
            XEditorSettings.IndentNamespace = _indentingPage.IndentNamespace;

#if COMPLETION
            // Completion
            XSettings.CompleteLocals = _completionOptionsPage.CompleteLocals;
            XSettings.CompleteSelf = _completionOptionsPage.CompleteSelf;
            XSettings.CompleteParent = _completionOptionsPage.CompleteParent;
            XSettings.CompleteNamespaces = _completionOptionsPage.CompleteNamespaces;
            XSettings.CompleteTypes = _completionOptionsPage.CompleteTypes;
            XSettings.CompleteKeywords = _completionOptionsPage.CompleteKeywords;
            XSettings.CompleteSnippets = _completionOptionsPage.CompleteSnippets;
            XSettings.CompleteGlobals = _completionOptionsPage.CompleteGlobals;
            XSettings.CompleteGlobalsP = _completionOptionsPage.CompleteGlobalsP;
            XSettings.CompleteGlobalsA = _completionOptionsPage.CompleteGlobalsA;
            XSettings.CompleteFunctions = _completionOptionsPage.CompleteFunctions;
            XSettings.CompleteFunctionsP = _completionOptionsPage.CompleteFunctionsP;
            XSettings.CompleteFunctionsA = _completionOptionsPage.CompleteFunctionsA;
            XSettings.CompleteNumChars = _completionOptionsPage.CompleteNumChars;
            //XSettings.MaxCompletionEntries = _completionOptionsPage.MaxCompletionEntries;
#endif
            // Generator
            XSettings.CodeGeneratorPrivateStyle = (PrivateStyle)_generatorOptionsPage.PrivateStyle;
            XSettings.CodeGeneratorPublicStyle = (PublicStyle)_generatorOptionsPage.PublicStyle;
            XSettings.CodeGeneratorShowXmlComments = _generatorOptionsPage.ShowXmlComments;

            // Other
            XEditorSettings.ShowDividers = _otherOptionsPage.ShowDividers;
            XEditorSettings.CompletionAutoPairs = _otherOptionsPage.AutoPairs;
            XEditorSettings.ShowSingleLineDividers = _otherOptionsPage.ShowSingleLineDividers;
            XEditorSettings.DisableAutoOpen = !_otherOptionsPage.AutoOpen;
            XEditorSettings.DisableHighLightWord = !_otherOptionsPage.EnableHighlightWord;
            XEditorSettings.DisableBraceMatching = !_otherOptionsPage.EnableBraceMatching;
            XEditorSettings.DisableKeywordMatching = !_otherOptionsPage.EnableKeywordmatching;
            XEditorSettings.DisableCodeCompletion = !_otherOptionsPage.EnableCodeCompletion;
            XEditorSettings.DisableLightBulb = !_otherOptionsPage.EnableLightBulbs;
            XEditorSettings.DisableParameterInfo = !_otherOptionsPage.EnableParameterInfo;
            XEditorSettings.DisableQuickInfo = !_otherOptionsPage.EnableQuickInfo;
            XEditorSettings.DisableRegions = !_otherOptionsPage.EnableRegions;

            XSettings.EnableFileLogging = _otherOptionsPage.LanguageServiceLogging;

            // Persist in registry for CodeDomProvider code generation
            Constants.WriteSetting(Constants.RegistryKeywordCase, (int)XEditorSettings.KeywordCase);
            Constants.WriteSetting(Constants.RegistryPrivateKeyword, (int)XSettings.CodeGeneratorPrivateStyle);
            Constants.WriteSetting(Constants.RegistryPublicKeyword, (int)XSettings.CodeGeneratorPublicStyle);
            Constants.WriteSetting(Constants.RegistryUseTabs, XEditorSettings.TabsAsSpaces ? 0 : 1);
            Constants.WriteSetting(Constants.RegistryTabSize, XEditorSettings.TabSize);
            Constants.WriteSetting(Constants.RegistryIndentSize, XEditorSettings.IndentSize);
            optionWasChanged = false;
            return;
        }

        protected override async System.Threading.Tasks.Task InitializeAsync(CancellationToken cancellationToken, IProgress<ServiceProgressData> progress)
        {
            instance = this;
            ModelScannerEvents.Start();

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
            //if (!XSettings.DisableClassViewObjectView)
            //{
            //    ServiceCreatorCallback callback = new ServiceCreatorCallback(CreateLibraryService);
            //    serviceContainer.AddService(typeof(IXSharpLibraryManager), callback, true);
            //}

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
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            XSettings.LanguageService = this;
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
            /*
            // our user text editor settings are stored inside ApplicationPrivateSettings\\TextEditor\\XSharp
            using (RegistryKey root = VSRegistry.RegistryRoot(__VsLocalRegistryType.RegType_UserSettings))
            {
                var subkey = root.OpenSubKey("ApplicationPrivateSettings\\TextEditor\\XSharp");
                {
                    var names = subkey.GetValueNames();
                    var list = new List<string>();
                    foreach (var name in names)
                    {
                        list.Add(subkey.GetValue(name).ToString());
                    }
                }
            }
            */


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
                if (!lValue )
                {
                    GetIntellisenseSettings();
                    RefreshDocumentSettingsAsync().FireAndForget();
                }
            }
            return VSConstants.S_OK;
        }

        /// <summary>
        /// Reload the source code editor settings for all open X# editor windows
        /// </summary>
        /// <returns></returns>
        private async System.Threading.Tasks.Task RefreshDocumentSettingsAsync()
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

#region IVSDebuggerEvents
        private void RegisterDebuggerEvents()
        {
            int hr;
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                m_debugger = await VS.GetServiceAsync<SVsShellDebugger,IVsDebugger>();
                if (m_debugger != null)
                {
                    hr = m_debugger.AdviseDebuggerEvents(this, out m_Debuggercookie);
                    Microsoft.VisualStudio.ErrorHandler.ThrowOnFailure(hr);
                    // Get initial value
                    DBGMODE[] modeArray = new DBGMODE[1];
                    hr = m_debugger.GetMode(modeArray);
                    XSettings.DebuggerMode = (DebuggerMode)modeArray[0];
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
            XSettings.DebuggerMode = (DebuggerMode)dbgmodeNew;
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

}
