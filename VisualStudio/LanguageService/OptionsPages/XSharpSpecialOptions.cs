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
            
            this.chkDisableParameters.Checked = _page.DisableParameterInfo;
            this.chkDisableEditorDropdowns.Checked = _page.DisableEditorDropdowns;
            this.chkBraceMatching.Checked = _page.DisableBraceMatching;
            this.chkDisableClassViewObjectView.Checked = _page.DisableClassViewObjectView;
            this.chkDisableEntityParsing.Checked = _page.DisableEntityParsing;
            this.chkDisableHighlightWord.Checked = _page.DisableHighLightWord;
            this.chkDisableLigtBulb.Checked = _page.DisableLightBulb;
            this.chkDisablePeekDefinition.Checked = _page.DisablePeekDefinition;
            this.chkDisableQuickInfo.Checked = _page.DisableQuickInfo;
            this.chkDisableRegions.Checked = _page.DisableRegions;
            this.chkDisableSyntaxColorization.Checked = _page.DisableSyntaxColorization;
            this.chkDisableCaseSynchronization.Checked = _page.DisableCaseSynchronization;
            this.chkDisableCodeCompletion.Checked = _page.DisableCodeCompletion;
            this.chkDisableGotoDefinition.Checked = _page.DisableGotoDefinition;
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
        }
        internal void SaveSettings()
        {
            _page.DisableParameterInfo = this.chkDisableParameters.Checked;
            _page.DisableEditorDropdowns = this.chkDisableEditorDropdowns.Checked;
            _page.DisableBraceMatching = this.chkBraceMatching.Checked;
            _page.DisableClassViewObjectView = this.chkDisableClassViewObjectView.Checked;
            _page.DisableEntityParsing = this.chkDisableEntityParsing.Checked;
            _page.DisableHighLightWord = this.chkDisableHighlightWord.Checked;
            _page.DisableLightBulb = this.chkDisableLigtBulb.Checked;
            _page.DisablePeekDefinition = this.chkDisablePeekDefinition.Checked;
            _page.DisableQuickInfo = this.chkDisableQuickInfo.Checked;
            _page.DisableRegions = this.chkDisableRegions.Checked;
            _page.DisableSyntaxColorization = this.chkDisableSyntaxColorization.Checked;
            _page.DisableCaseSynchronization = this.chkDisableCaseSynchronization.Checked;
            _page.DisableCodeCompletion = this.chkDisableCodeCompletion.Checked;
            _page.DisableGotoDefinition = this.chkDisableGotoDefinition.Checked;
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

        }

    }
}
