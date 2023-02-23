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
            this.chkDisableEditorDropdowns.Checked = _page.DisableEditorDropdowns;
            this.chkDisableClassViewObjectView.Checked = _page.DisableClassViewObjectView;
            this.chkDisableEntityParsing.Checked = _page.DisableEntityParsing;
            this.chkDisableSyntaxColorization.Checked = _page.DisableSyntaxColorization;
            this.chkDisableAssemblyReferences.Checked = _page.DisableAssemblyReferences;
            this.chkDisableForeignProjectReferences.Checked = _page.DisableForeignProjectReferences;
            this.chkDisableXSharpProjectReferences.Checked = _page.DisableXSharpProjectReferences;
            this.chkEnableOutputPane.Checked = _page.EnableOutputPane;
            this.chkEnableDatabaseLog.Checked = _page.EnableDatabaseLog;
            this.chkEnableParserLog.Checked = _page.EnableParserLog;
            this.chkEnableParameterTipsLog.Checked = _page.EnableParameterLog;
            this.chkEnableBraceMatchLog.Checked = _page.EnableBraceMatchLog;
            this.chkEnableCodeCompletionLog.Checked = _page.EnableCodeCompletionLog;
            this.chkEnableQuickInfoLog.Checked = _page.EnableQuickInfoLog;
            this.chkEnableTypeLookupLog.Checked = _page.EnableTypelookupLog;
            this.chkEnableReferenceLog.Checked = _page.EnableReferenceInfoLog;
            this.chkLogToDebug.Checked = (int) Constants.GetSetting("Log2Debug", 0) != 0;
            this.chkLogToFile.Checked = (int) Constants.GetSetting("Log2File", 0) != 0;
        }
        internal void SaveSettings()
        {
            _page.DisableEditorDropdowns = this.chkDisableEditorDropdowns.Checked;
            _page.DisableClassViewObjectView = this.chkDisableClassViewObjectView.Checked;
            _page.DisableEntityParsing = this.chkDisableEntityParsing.Checked;
            _page.DisableSyntaxColorization = this.chkDisableSyntaxColorization.Checked;
            _page.DisableAssemblyReferences = this.chkDisableAssemblyReferences.Checked;
            _page.DisableForeignProjectReferences = this.chkDisableForeignProjectReferences.Checked;
            _page.DisableXSharpProjectReferences = this.chkDisableXSharpProjectReferences.Checked;
            _page.EnableOutputPane = this.chkEnableOutputPane.Checked;
            _page.EnableDatabaseLog = this.chkEnableDatabaseLog.Checked;
            _page.EnableParserLog = this.chkEnableParserLog.Checked;
            _page.EnableParameterLog = this.chkEnableParameterTipsLog.Checked;
            _page.EnableBraceMatchLog = this.chkEnableBraceMatchLog.Checked;
            _page.EnableCodeCompletionLog = this.chkEnableCodeCompletionLog.Checked;
            _page.EnableQuickInfoLog = this.chkEnableQuickInfoLog.Checked;
            _page.EnableTypelookupLog = this.chkEnableTypeLookupLog.Checked;
            _page.EnableReferenceInfoLog = this.chkEnableReferenceLog.Checked;
            XSharpLanguagePackage.Instance.optionWasChanged = true;
            Constants.WriteSetting("Log2Debug", chkLogToDebug.Checked ? 1 : 0);
            Constants.WriteSetting("Log2File", chkLogToFile.Checked ? 1 : 0);

        }

    }
}
