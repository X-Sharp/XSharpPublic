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
    public sealed class XSharpLanguageService : AsyncPackage, IVsShellPropertyEvents
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
                XSettings.EnableReferenceInfoLog = _intellisensePage.EnableReferenceInfoLog;
                XSettings.EnableTypelookupLog = _intellisensePage.EnableTypelookupLog;

                XSettings.DisableAssemblyReferences = _intellisensePage.DisableAssemblyReferences;
                XSettings.DisableBraceMatching = _intellisensePage.DisableBraceMatching;
                XSettings.DisableCaseSynchronization = _intellisensePage.DisableCaseSynchronization;
                XSettings.DisableClassViewObjectView = _intellisensePage.DisableClassViewObjectView;
                XSettings.DisableCodeCompletion = _intellisensePage.DisableCodeCompletion;
                XSettings.DisableEntityParsing = _intellisensePage.DisableEntityParsing;
                XSettings.DisableForeignProjectReferences = _intellisensePage.DisableForeignProjectReferences;
                XSettings.DisableGotoDefinition = _intellisensePage.DisableGotoDefinition;
                XSettings.DisableHighLightWord = _intellisensePage.DisableHighLightWord;
                XSettings.DisablePeekDefinition = _intellisensePage.DisablePeekDefinition;
                XSettings.DisableQuickInfo = _intellisensePage.DisableQuickInfo;
                XSettings.DisableRegions = _intellisensePage.DisableRegions;
                XSettings.DisableSyntaxHighlighting = _intellisensePage.DisableSyntaxColorization;
                XSettings.DisableXSharpProjectReferences = _intellisensePage.DisableXSharpProjectReferences;

                XSettings.KeywordCase = (int)_intellisensePage.KeywordCase;
                _intellisensePage.SettingsChanged = false;
            }
            return _intellisensePage;
        }

        protected override async System.Threading.Tasks.Task InitializeAsync(CancellationToken cancellationToken, IProgress<ServiceProgressData> progress)
        {
            instance = this;
            await base.InitializeAsync(cancellationToken, progress);
            _txtManager = await GetServiceAsync(typeof(SVsTextManager)) as IVsTextManager4;

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
                    CommandFilter.InvalidateOptions();
                    GetIntellisenseOptionsPage();
                }
            }
            return VSConstants.S_OK;
        }

        internal IVsTextManager4 GetTextManager()
        {
            return this._txtManager;
        }

        internal static void DisplayOutPutMessage(string message)
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

        internal bool DebuggerIsRunning
        {
            get
            {
                return false;
            }
        }
    }

}
