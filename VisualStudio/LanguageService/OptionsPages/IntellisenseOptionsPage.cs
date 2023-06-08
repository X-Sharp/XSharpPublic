using Microsoft.VisualStudio.Shell;
using System;
using System.Runtime.InteropServices;

namespace XSharp.LanguageService.OptionsPages
{

    [Guid(XSharpConstants.IntellisenseOptionsPageGuidString)]
    [SharedSettings("TextEditor.XSharp", false)]
    [ComVisible(true)]
    public class IntellisenseOptionsPage : XSDialogPage<IntellisenseOptionsControl, IntellisenseOptions>
    {
        #region Properties that are delegated to the Options object
        public bool CompletionListTabs
        {
            get => Options.CompletionListTabs;
            set => Options.CompletionListTabs = value;
        }
        public String CommitChars
        {
            get => Options.CommitChars;
            set => Options.CommitChars = value;
        }
        public bool ExcludeMembersFromOtherFiles
        {
            get => Options.ExcludeMembersFromOtherFiles;
            set => Options.ExcludeMembersFromOtherFiles = value;
        }
        public bool KeywordsInAll
        {
            get => Options.KeywordsInAll;
            set => Options.KeywordsInAll = value;
        }
        public bool ShowAfterChar
        {
            get => Options.ShowAfterChar;
            set => Options.ShowAfterChar = value;
        }
        public XSharpModel.KeywordCase KeywordCase
        {
            get => Options.KeywordCase;
            set => Options.KeywordCase = value;
        }
        public bool IncludeFieldsInNavigationBars
        {
            get => Options.IncludeFieldsInNavigationBars;
            set => Options.IncludeFieldsInNavigationBars = value;
        }
        public bool SortNavigationBars
        {
            get => Options.SortNavigationBars;
            set => Options.SortNavigationBars = value;
        }
        public bool ShowMembersOfCurrentTypeOnly
        {
            get => Options.ShowMembersOfCurrentTypeOnly;
            set => Options.ShowMembersOfCurrentTypeOnly = value;
        }
        public bool DisableSyntaxColorization
        {
            get => Options.DisableSyntaxColorization;
            set => Options.DisableSyntaxColorization = value;
        }
        public bool DisableEntityParsing
        {
            get => Options.DisableEntityParsing;
            set => Options.DisableEntityParsing = value;
        }
        public bool DisableEditorDropdowns
        {
            get => Options.DisableEditorDropdowns;
            set => Options.DisableEditorDropdowns = value;
        }
        public bool DisableClassViewObjectView
        {
            get => Options.DisableClassViewObjectView;
            set => Options.DisableClassViewObjectView = value;
        }
        public bool DisableAssemblyReferences
        {
            get => Options.DisableAssemblyReferences;
            set => Options.DisableAssemblyReferences = value;
        }
        public bool DisableForeignProjectReferences
        {
            get => Options.DisableForeignProjectReferences;
            set => Options.DisableForeignProjectReferences = value;
        }
        public bool DisableXSharpProjectReferences
        {
            get => Options.DisableXSharpProjectReferences;
            set => Options.DisableXSharpProjectReferences = value;
        }
        public bool EnableOutputPane
        {
            get => Options.EnableOutputPane;
            set => Options.EnableOutputPane = value;
        }
        public bool EnableDatabaseLog
        {
            get => Options.EnableDatabaseLog;
            set => Options.EnableDatabaseLog = value;
        }
        public bool EnableParserLog
        {
            get => Options.EnableParserLog;
            set => Options.EnableParserLog = value;
        }
        public bool EnableCodeCompletionLog
        {
            get => Options.EnableCodeCompletionLog;
            set => Options.EnableCodeCompletionLog = value;
        }
        public bool EnableParameterLog
        {
            get => Options.EnableParameterLog;
            set => Options.EnableParameterLog = value;
        }
        public bool EnableBraceMatchLog
        {
            get => Options.EnableBraceMatchLog;
            set => Options.EnableBraceMatchLog = value;
        }
        public bool EnableQuickInfoLog
        {
            get => Options.EnableQuickInfoLog;
            set => Options.EnableQuickInfoLog = value;
        }
        public bool EnableTypelookupLog
        {
            get => Options.EnableTypelookupLog;
            set => Options.EnableTypelookupLog = value;
        }
        public bool EnableReferenceInfoLog
        {
            get => Options.EnableReferenceInfoLog;
            set => Options.EnableReferenceInfoLog = value;
        }
        #endregion


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
}
