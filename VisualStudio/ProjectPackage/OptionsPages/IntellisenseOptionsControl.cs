using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace XSharp.Project.OptionsPages
{
    public partial class IntellisenseOptionsControl : UserControl
    {
        public IntellisenseOptionsControl()
        {
            InitializeComponent();
            if (String.IsNullOrEmpty(System.Environment.GetEnvironmentVariable("XSHARPDEV")))
            {
                this.groupBox1.Visible = false;
            }

        }
        internal IntellisenseOptionsPage optionsPage;
        private bool _loading = false;
        internal void Initialize()
        {
            _loading = true;
            chkCompletionListtabs.Checked = optionsPage.CompletionListTabs;
            chkKeywordsInAll.Checked = optionsPage.KeywordsInAll;
            chkDotAsUniversalSelector.Checked = optionsPage.UseDotAsUniversalSelector;
            chkShowAfterChar.Checked = optionsPage.ShowAfterChar;
            grpCase.Enabled = true;
            switch (optionsPage.KeywordCase)
            {
                case 1:
                    rbUpper.Checked = true;
                    break;
                case 2:
                    rbLower.Checked = true;
                    break;
                case 3:
                    rbTitle.Checked = true;
                    break;
                default:
                    rbNone.Checked = true;
                    break;
            }
            //
            chkIdentifierCase.Checked = optionsPage.IdentifierCase;
            chkAlignDoCase.Checked = optionsPage.AlignDoCase;
            chkAlignMethod.Checked = optionsPage.AlignMethod;
            chkIncludeFields.Checked = optionsPage.IncludeFieldsInNavigationBars;
            chkSortNavBar.Checked = optionsPage.SortNavigationBars;
            chkDisableParameters.Checked = optionsPage.DisableParameterInfo;
            chkDisableEditorDropdowns.Checked = optionsPage.DisableEditorDropdowns;
            chkBraceMatching.Checked = optionsPage.DisableBraceMatching;
            chkDisableClassViewObjectView.Checked = optionsPage.DisableClassViewObjectView;
            chkDisableEntityParsing.Checked = optionsPage.DisableEntityParsing;
            chkDisableHighlightWord.Checked = optionsPage.DisableHighLightWord;
            chkDisableLigtBulb.Checked = optionsPage.DisableLightBulb;
            chkDisablePeekDefinition.Checked = optionsPage.DisablePeekDefinition;
            chkDisableQuickInfo.Checked = optionsPage.DisableQuickInfo;
            chkDisableRegions.Checked = optionsPage.DisableRegions;
            chkDisableSyntaxColorization.Checked = optionsPage.DisableSyntaxColorization;
            chkDisableCaseSynchronization.Checked = optionsPage.DisableCaseSynchronization;
            chkDisableCodeCompletion.Checked = optionsPage.DisableCodeCompletion;
            _loading = false;

        }


        private void chkCompletionListtabs_CheckedChanged(object sender, EventArgs e)
        {
            optionsPage.CompletionListTabs = chkCompletionListtabs.Checked;
        }

        private void chkKeywordsInAll_CheckedChanged(object sender, EventArgs e)
        {
            optionsPage.KeywordsInAll = chkKeywordsInAll.Checked;
        }

        private void kwCaseChanged(object sender, EventArgs e)
        {
            if (rbNone.Checked)
            {
                optionsPage.KeywordCase = 0;
            }
            else if (rbUpper.Checked)
            {
                optionsPage.KeywordCase = 1;
            }
            else if (rbLower.Checked)
            {
                optionsPage.KeywordCase = 2;
            }
            else if (rbTitle.Checked)
            {
                optionsPage.KeywordCase = 3;
            }
        }

        private void chkIdentifierCase_CheckedChanged(object sender, EventArgs e)
        {
            //optionsPage.IdentifierCase = chkIdentifierCase.Checked;
        }

        private void chkAlignDoCase_CheckedChanged(object sender, EventArgs e)
        {
            optionsPage.AlignDoCase = chkAlignDoCase.Checked;
        }

        private void chkAlignMethod_CheckedChanged(object sender, EventArgs e)
        {
            optionsPage.AlignMethod = chkAlignMethod.Checked;
        }

        private void chkDotAsUniversalSelector_CheckedChanged(object sender, EventArgs e)
        {
            optionsPage.UseDotAsUniversalSelector = chkDotAsUniversalSelector.Checked;
        }

        private void chkIncludeFields_CheckedChanged(object sender, EventArgs e)
        {
            optionsPage.IncludeFieldsInNavigationBars = chkIncludeFields.Checked;
        }

        private void chkSortNavBar_CheckedChanged(object sender, EventArgs e)
        {
            optionsPage.SortNavigationBars = chkSortNavBar.Checked;
        }

        private void chkShowAfterChar_CheckedChanged(object sender, EventArgs e)
        {
            optionsPage.ShowAfterChar = chkShowAfterChar.Checked;
        }

        private void Control_CheckedChanged(object sender, EventArgs e)
        {
            if (! _loading )
            {
                optionsPage.DisableParameterInfo = chkDisableParameters.Checked;
                optionsPage.DisableEditorDropdowns = chkDisableEditorDropdowns.Checked;
                optionsPage.DisableBraceMatching = chkBraceMatching.Checked;
                optionsPage.DisableClassViewObjectView = chkDisableClassViewObjectView.Checked;
                optionsPage.DisableEntityParsing = chkDisableEntityParsing.Checked;
                optionsPage.DisableHighLightWord = chkDisableHighlightWord.Checked;
                optionsPage.DisableLightBulb = chkDisableLigtBulb.Checked;
                optionsPage.DisablePeekDefinition = chkDisablePeekDefinition.Checked;
                optionsPage.DisableQuickInfo = chkDisableQuickInfo.Checked;
                optionsPage.DisableRegions = chkDisableRegions.Checked;
                optionsPage.DisableSyntaxColorization = chkDisableSyntaxColorization.Checked;
                optionsPage.DisableCaseSynchronization = chkDisableCaseSynchronization.Checked;
                optionsPage.DisableCodeCompletion = chkDisableCodeCompletion.Checked;
            }

        }

    }
}
