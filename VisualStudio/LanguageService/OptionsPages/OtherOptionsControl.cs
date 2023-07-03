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
            chkShowDividers.Tag = nameof(OtherOptions.ShowDividers);
            chkSingleLineDividers.Tag = nameof(OtherOptions.ShowSingleLineDividers);
            chkHighlightWord.Tag = nameof(OtherOptions.EnableHighlightWord);
            chkBraceMatching.Tag = nameof(OtherOptions.EnableBraceMatching);
            chkKeywordMatching.Tag = nameof(OtherOptions.EnableKeywordmatching);

            chkLightBulbs.Tag = nameof(OtherOptions.EnableLightBulbs);
            chkQuickInfo.Tag = nameof(OtherOptions.EnableQuickInfo);
            chkParameters.Tag = nameof(OtherOptions.EnableParameterInfo);
            chkCompletion.Tag = nameof(OtherOptions.EnableCodeCompletion);
            chkRegions.Tag = nameof(OtherOptions.EnableRegions);
            chkAutoPairs.Tag = nameof(OtherOptions.AutoPairs);
            chkAutoOpen.Tag = nameof(OtherOptions.AutoOpen);
        }
        internal override void ReadValues(object options)
        {
            base.ReadValues(options);
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
