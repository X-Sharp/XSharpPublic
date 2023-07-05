using Microsoft.VisualStudio.Shell;
using System;
using System.Runtime.InteropServices;
using XSharpModel;
using XSharp.Settings;
namespace XSharp.LanguageService.OptionsPages
{

    [Guid(XSharpConstants.IntellisenseOptionsPageGuidString)]
    [SharedSettings("TextEditor.XSharp", false)]
    [ComVisible(true)]
    public class IntellisenseOptionsPage : XSDialogPage<IntellisenseOptionsControl, IntellisenseOptions>
    {
        // The base class exposes the AutomationObject that contains the values

        private void SetDefaultCommitChars()
        {
            if (Options.CommitChars == null || string.IsNullOrEmpty(Options.CommitChars))
            {
                Options.CommitChars = IntellisenseOptions.DefaultCommitChars;
            }
        }

        public override void LoadSettingsFromStorage()
        {
            base.LoadSettingsFromStorage();
            if (Options.CommitChars != "<Empty>")
            {
                SetDefaultCommitChars();
            }
            else
            {
                Options.CommitChars = "";
            }
        }
        public override void SaveSettingsToStorage()
        {
            if (Options.CommitChars != null && Options.CommitChars.Length == 0)
                Options.CommitChars = "<Empty>";
            SetDefaultCommitChars();
            base.SaveSettingsToStorage();
        }

    }
    public class IntellisenseOptions : OptionsBase
    {
        public const string DefaultCommitChars = "{}[]().,:;+-*/%&|^!~<>?@#\'\"\\";
        #region Properties
        public bool CompletionListTabs { get; set; }
        public String CommitChars { get; set; }
        public bool ExcludeMembersFromOtherFiles { get; set; }
        public bool KeywordsInAll { get; set; }
        public bool ShowAfterChar { get; set; }
        public XSharp.Settings.KeywordCase KeywordCase { get; set; }
        public bool IncludeFieldsInNavigationBars { get; set; }
        public bool SortNavigationBars { get; set; }
        public bool ShowMembersOfCurrentTypeOnly { get; set; }
        public bool DisableSyntaxColorization { get; set; }
        public bool DisableEntityParsing { get; set; }
        public bool DisableEditorDropdowns { get; set; }
        public bool DisableClassViewObjectView { get; set; }
        public bool DisableAssemblyReferences { get; set; }
        public bool DisableForeignProjectReferences { get; set; }
        public bool DisableXSharpProjectReferences { get; set; }
        public bool EnableOutputPane { get; set; }
        public bool EnableDatabaseLog { get; set; }
        public bool EnableParserLog { get; set; }
        public bool EnableCodeCompletionLog { get; set; }
        public bool EnableParameterLog { get; set; }
        public bool EnableBraceMatchLog { get; set; }
        public bool EnableQuickInfoLog { get; set; }
        public bool EnableTypelookupLog { get; set; }
        public bool EnableReferenceInfoLog { get; set; }
        #endregion
        public IntellisenseOptions()
        {
            CompletionListTabs = true;
            CommitChars = DefaultCommitChars;
            ExcludeMembersFromOtherFiles = true;
            KeywordsInAll = true;
            ShowAfterChar = true;
            KeywordCase = KeywordCase.Upper;
            IncludeFieldsInNavigationBars = true;
            SortNavigationBars = true;
            ShowMembersOfCurrentTypeOnly = false;
            DisableSyntaxColorization = false;
            DisableEntityParsing = false;
            DisableEditorDropdowns = false;
            DisableClassViewObjectView = false;
            DisableAssemblyReferences = false;
            DisableForeignProjectReferences = false;
            DisableXSharpProjectReferences = false;
            EnableOutputPane = false;
            EnableDatabaseLog = false;
            EnableParserLog = false;
            EnableCodeCompletionLog = false;
            EnableParameterLog = false;
            EnableBraceMatchLog = false;
            EnableQuickInfoLog = false;
            EnableTypelookupLog = false;
            EnableReferenceInfoLog = false;
        }

        public override void WriteToSettings()
        {
            XSettings.EnableOutputWindowLogging = EnableOutputPane;
            XSettings.EnableBraceMatchLog = EnableBraceMatchLog;
            XSettings.EnableCodeCompletionLog = EnableCodeCompletionLog;
            XSettings.EnableDatabaseLog = EnableDatabaseLog;
            XSettings.EnableParameterLog = EnableParameterLog;
            XSettings.EnableParseLog = EnableParserLog;
            XSettings.EnableQuickInfoLog = EnableQuickInfoLog;
            XSettings.EnableReferenceInfoLog = EnableReferenceInfoLog;
            XSettings.EnableTypelookupLog = EnableTypelookupLog;

            XSettings.DisableAssemblyReferences = DisableAssemblyReferences;
            XSettings.DisableClassViewObjectView = DisableClassViewObjectView;
            XSettings.DisableEditorDropDowns = DisableEditorDropdowns;
            XSettings.DisableEntityParsing = DisableEntityParsing;
            XSettings.DisableForeignProjectReferences = DisableForeignProjectReferences;
            XSettings.DisableXSharpProjectReferences = DisableXSharpProjectReferences;

            XEditorSettings.DisableSyntaxHighlighting = DisableSyntaxColorization;
            XEditorSettings.CompletionListTabs = CompletionListTabs;
            XEditorSettings.CommitChars = CommitChars;
            XEditorSettings.CompletionListAfterEachChar = false; // ShowAfterChar;
            XEditorSettings.KeywordsInAll = KeywordsInAll;
            XEditorSettings.NavigationSorted = SortNavigationBars;
            XEditorSettings.NavigationIncludeFields = IncludeFieldsInNavigationBars;
            XEditorSettings.NavigationMembersOfCurrentTypeOnly = ShowMembersOfCurrentTypeOnly;
            XEditorSettings.NavigationExcludeMembersFromOtherFiles = ExcludeMembersFromOtherFiles;

        }

    }
}
