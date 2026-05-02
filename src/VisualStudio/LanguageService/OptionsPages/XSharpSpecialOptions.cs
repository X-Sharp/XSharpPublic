// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
using System.Windows;

namespace XSharp.LanguageService.OptionsPages
{
    public partial class XSharpSpecialOptions : Window
    {
        private readonly IntellisenseOptionsPage _page;

        public XSharpSpecialOptions(IntellisenseOptionsPage page)
        {
            InitializeComponent();
            _page = page;
            LoadSettings();
        }

        private void LoadSettings()
        {
            chkDisableEditorDropdowns.IsChecked          = _page.Options.DisableEditorDropdowns;
            chkDisableClassViewObjectView.IsChecked       = _page.Options.DisableClassViewObjectView;
            chkDisableEntityParsing.IsChecked             = _page.Options.DisableEntityParsing;
            chkDisableSyntaxColorization.IsChecked        = _page.Options.DisableSyntaxColorization;
            chkDisableAssemblyReferences.IsChecked        = _page.Options.DisableAssemblyReferences;
            chkDisableForeignProjectReferences.IsChecked  = _page.Options.DisableForeignProjectReferences;
            chkDisableXSharpProjectReferences.IsChecked   = _page.Options.DisableXSharpProjectReferences;
            chkEnableOutputPane.IsChecked                 = _page.Options.EnableOutputPane;
            chkEnableDatabaseLog.IsChecked                = _page.Options.EnableDatabaseLog;
            chkEnableParserLog.IsChecked                  = _page.Options.EnableParserLog;
            chkEnableParameterTipsLog.IsChecked           = _page.Options.EnableParameterLog;
            chkEnableBraceMatchLog.IsChecked              = _page.Options.EnableBraceMatchLog;
            chkEnableCodeCompletionLog.IsChecked          = _page.Options.EnableCodeCompletionLog;
            chkEnableQuickInfoLog.IsChecked               = _page.Options.EnableQuickInfoLog;
            chkEnableTypeLookupLog.IsChecked              = _page.Options.EnableTypelookupLog;
            chkEnableReferenceLog.IsChecked               = _page.Options.EnableReferenceInfoLog;
            chkLogToDebug.IsChecked                       = LogOptions.Instance.LogToDebug;
            chkLogToFile.IsChecked                        = LogOptions.Instance.LogToFile;
        }

        private void SaveSettings()
        {
            _page.Options.DisableEditorDropdowns         = chkDisableEditorDropdowns.IsChecked == true;
            _page.Options.DisableClassViewObjectView      = chkDisableClassViewObjectView.IsChecked == true;
            _page.Options.DisableEntityParsing            = chkDisableEntityParsing.IsChecked == true;
            _page.Options.DisableSyntaxColorization       = chkDisableSyntaxColorization.IsChecked == true;
            _page.Options.DisableAssemblyReferences       = chkDisableAssemblyReferences.IsChecked == true;
            _page.Options.DisableForeignProjectReferences = chkDisableForeignProjectReferences.IsChecked == true;
            _page.Options.DisableXSharpProjectReferences  = chkDisableXSharpProjectReferences.IsChecked == true;
            _page.Options.EnableOutputPane                = chkEnableOutputPane.IsChecked == true;
            _page.Options.EnableDatabaseLog               = chkEnableDatabaseLog.IsChecked == true;
            _page.Options.EnableParserLog                 = chkEnableParserLog.IsChecked == true;
            _page.Options.EnableParameterLog              = chkEnableParameterTipsLog.IsChecked == true;
            _page.Options.EnableBraceMatchLog             = chkEnableBraceMatchLog.IsChecked == true;
            _page.Options.EnableCodeCompletionLog         = chkEnableCodeCompletionLog.IsChecked == true;
            _page.Options.EnableQuickInfoLog              = chkEnableQuickInfoLog.IsChecked == true;
            _page.Options.EnableTypelookupLog             = chkEnableTypeLookupLog.IsChecked == true;
            _page.Options.EnableReferenceInfoLog          = chkEnableReferenceLog.IsChecked == true;
            LogOptions.Instance.LogToFile                 = chkLogToFile.IsChecked == true;
            LogOptions.Instance.LogToDebug                = chkLogToDebug.IsChecked == true;
            LogOptions.Instance.WriteToSettings();
        }

        private void OnOk(object sender, RoutedEventArgs e)
        {
            SaveSettings();
            DialogResult = true;
        }
    }
}
