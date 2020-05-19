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

// The following lines ensure that the right versions of the various DLLs are loaded.
// They will be included in the generated PkgDef folder for the project system
[assembly: ProvideCodeBase(AssemblyName = "XSharp.VsParser", CodeBase = "XSharp.VsParser.dll", Culture = "neutral", PublicKeyToken = XSharp.Constants.PublicKey, Version = XSharp.Constants.Version)]
[assembly: ProvideCodeBase(AssemblyName = "XSharpColorizer", CodeBase = "XSharpColorizer.dll", Culture = "neutral", PublicKeyToken = XSharp.Constants.PublicKey, Version = XSharp.Constants.Version)]
[assembly: ProvideCodeBase(AssemblyName = "XSharpModel", CodeBase = "XSharpModel.dll", Culture = "neutral", PublicKeyToken = XSharp.Constants.PublicKey, Version = XSharp.Constants.Version)]
[assembly: ProvideCodeBase(AssemblyName = "System.Collections.Immutable", CodeBase = "System.Collections.Immutable.dll", Culture = "neutral", PublicKeyToken = "b03f5f7f11d50a3a", Version = "1.2.5.0")]
[assembly: ProvideCodeBase(AssemblyName = "System.Reflection.Metadata", CodeBase = "System.Reflection.Metadata.dll", Culture = "neutral", PublicKeyToken = "b03f5f7f11d50a3a", Version = "1.4.5.0")]
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
                _intellisensePage = (IntellisenseOptionsPage)GetDialogPage(typeof(IntellisenseOptionsPage));
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
                }
            }
            return VSConstants.S_OK;
        }

        internal IVsTextManager4 GetTextManager()
        {
            return this._txtManager;
        }

        public static void SetLogger(IDebugLogger dbglogger)
        {
            logger = dbglogger;
        }
        static IDebugLogger logger = null;

        internal static void DisplayOutPutMessage(string message)
        {
            if (logger != null)
            {
                logger.DisplayOutPutMessage(message);
            }
        }
        internal static void DisplayException(Exception e)
        {
            if (logger != null)
            {
                logger.DisplayException(e);
            }

        }
        internal static void ShowMessageBox(string message)
        {
            if (logger != null)
            {
                logger.ShowMessageBox(message);
            }

        }
        internal static IComponentModel GetComponentModel()
        {
            return (IComponentModel)GetGlobalService(typeof(SComponentModel));
        }

        internal bool DebuggerIsRunning
        {
            get
            {
                if (logger != null)
                {
                    return logger.DebuggerIsRunning;
                }

                return false;
            }
        }
    }

    public interface IDebugLogger
    {
        void DisplayOutPutMessage(string message);
        void DisplayException(Exception e);
        int ShowMessageBox(string message);
        bool DebuggerIsRunning { get; }
    }
}
