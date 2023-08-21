using System;
using System.Windows.Forms;

namespace XSharp.LanguageService.OptionsPages
{
    public partial class XSharpSpecialOptions : Form
    {
        private IntellisenseOptionsPage _page;
        public XSharpSpecialOptions(IntellisenseOptionsPage page)
        {
            InitializeComponent();
            _page = page;
            loadSettings();
        }

        private void btnOk_Click(object sender, EventArgs e)
        {
            this.DialogResult = DialogResult.OK;
            SaveSettings();
            this.Close();
        }
        internal void loadSettings()
        {
            this.chkDisableEditorDropdowns.Checked = _page.Options.DisableEditorDropdowns;
            this.chkDisableClassViewObjectView.Checked = _page.Options.DisableClassViewObjectView;
            this.chkDisableEntityParsing.Checked = _page.Options.DisableEntityParsing;
            this.chkDisableSyntaxColorization.Checked = _page.Options.DisableSyntaxColorization;
            this.chkDisableAssemblyReferences.Checked = _page.Options.DisableAssemblyReferences;
            this.chkDisableForeignProjectReferences.Checked = _page.Options.DisableForeignProjectReferences;
            this.chkDisableXSharpProjectReferences.Checked = _page.Options.DisableXSharpProjectReferences;
            this.chkEnableOutputPane.Checked = _page.Options.EnableOutputPane;
            this.chkEnableDatabaseLog.Checked = _page.Options.EnableDatabaseLog;
            this.chkEnableParserLog.Checked = _page.Options.EnableParserLog;
            this.chkEnableParameterTipsLog.Checked = _page.Options.EnableParameterLog;
            this.chkEnableBraceMatchLog.Checked = _page.Options.EnableBraceMatchLog;
            this.chkEnableCodeCompletionLog.Checked = _page.Options.EnableCodeCompletionLog;
            this.chkEnableQuickInfoLog.Checked = _page.Options.EnableQuickInfoLog;
            this.chkEnableTypeLookupLog.Checked = _page.Options.EnableTypelookupLog;
            this.chkEnableReferenceLog.Checked = _page.Options.EnableReferenceInfoLog;
            this.chkLogToDebug.Checked = LogOptions.Instance.LogToDebug;
            this.chkLogToFile.Checked = LogOptions.Instance.LogToFile;
        }
        internal void SaveSettings()
        {
            _page.Options.DisableEditorDropdowns = this.chkDisableEditorDropdowns.Checked;
            _page.Options.DisableClassViewObjectView = this.chkDisableClassViewObjectView.Checked;
            _page.Options.DisableEntityParsing = this.chkDisableEntityParsing.Checked;
            _page.Options.DisableSyntaxColorization = this.chkDisableSyntaxColorization.Checked;
            _page.Options.DisableAssemblyReferences = this.chkDisableAssemblyReferences.Checked;
            _page.Options.DisableForeignProjectReferences = this.chkDisableForeignProjectReferences.Checked;
            _page.Options.DisableXSharpProjectReferences = this.chkDisableXSharpProjectReferences.Checked;
            _page.Options.EnableOutputPane = this.chkEnableOutputPane.Checked;
            _page.Options.EnableDatabaseLog = this.chkEnableDatabaseLog.Checked;
            _page.Options.EnableParserLog = this.chkEnableParserLog.Checked;
            _page.Options.EnableParameterLog = this.chkEnableParameterTipsLog.Checked;
            _page.Options.EnableBraceMatchLog = this.chkEnableBraceMatchLog.Checked;
            _page.Options.EnableCodeCompletionLog = this.chkEnableCodeCompletionLog.Checked;
            _page.Options.EnableQuickInfoLog = this.chkEnableQuickInfoLog.Checked;
            _page.Options.EnableTypelookupLog = this.chkEnableTypeLookupLog.Checked;
            _page.Options.EnableReferenceInfoLog = this.chkEnableReferenceLog.Checked;
            LogOptions.Instance.LogToFile = this.chkLogToFile.Checked;
            LogOptions.Instance.LogToDebug = this.chkLogToDebug.Checked;
            LogOptions.Instance.WriteToSettings();

        }

    }
}
