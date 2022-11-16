using System;
using System.Windows.Forms;
using XSharpModel;
namespace XSharp.LanguageService.OptionsPages
{
    public partial class OtherOptionsControl : XSUserControl
    {
        public OtherOptionsControl()
        {
            InitializeComponent();
            chkShowDividers.Tag = nameof(OtherOptionsPage.ShowDividers);
            chkSingleLineDividers.Tag = nameof(OtherOptionsPage.ShowSingleLineDividers);
            chkBackupForms.Tag = nameof(OtherOptionsPage.FormEditorMakeBackupFiles);
            chkHighlightWord.Tag = nameof(OtherOptionsPage.EnableHighlightWord);
            chkBraceMatching.Tag = nameof(OtherOptionsPage.EnableBraceMatching);
            chkKeywordMatching.Tag = nameof(OtherOptionsPage.EnableKeywordmatching);

            chkLightBulbs.Tag = nameof(OtherOptionsPage.EnableLightBulbs);
            chkQuickInfo.Tag = nameof(OtherOptionsPage.EnableQuickInfo);
            chkParameters.Tag = nameof(OtherOptionsPage.EnableParameterInfo);
            chkCompletion.Tag = nameof(OtherOptionsPage.EnableCodeCompletion);
            chkRegions.Tag = nameof(OtherOptionsPage.EnableRegions);
        }
        internal OtherOptionsPage OurOptionsPage => (OtherOptionsPage) optionPage;

        internal override void ReadValues()
        {
            base.ReadValues();
            chkSingleLineDividers.Enabled = chkShowDividers.Checked;
        }

        private void chkShowDividers_CheckedChanged(object sender, EventArgs e)
        {
            chkSingleLineDividers.Enabled = chkShowDividers.Checked;
            if (!chkSingleLineDividers.Enabled)
            {
                chkSingleLineDividers.Checked = false;
            }
        }
    }
}
