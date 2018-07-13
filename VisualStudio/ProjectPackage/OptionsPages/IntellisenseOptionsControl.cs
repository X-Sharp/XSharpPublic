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
                this.btnShowMeTheMagic.Visible = false;
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
              }

        }

        private void btnShowMeTheMagic_Click(object sender, EventArgs e)
        {
            XSharpSpecialOptions form = new XSharpSpecialOptions();

            form.chkDisableParameters.Checked = optionsPage.DisableParameterInfo;
            form.chkDisableEditorDropdowns.Checked = optionsPage.DisableEditorDropdowns;
            form.chkBraceMatching.Checked = optionsPage.DisableBraceMatching;
            form.chkDisableClassViewObjectView.Checked = optionsPage.DisableClassViewObjectView;
            form.chkDisableEntityParsing.Checked = optionsPage.DisableEntityParsing;
            form.chkDisableHighlightWord.Checked = optionsPage.DisableHighLightWord;
            form.chkDisableLigtBulb.Checked = optionsPage.DisableLightBulb;
            form.chkDisablePeekDefinition.Checked = optionsPage.DisablePeekDefinition;
            form.chkDisableQuickInfo.Checked = optionsPage.DisableQuickInfo;
            form.chkDisableRegions.Checked = optionsPage.DisableRegions;
            form.chkDisableSyntaxColorization.Checked = optionsPage.DisableSyntaxColorization;
            form.chkDisableCaseSynchronization.Checked = optionsPage.DisableCaseSynchronization;
            form.chkDisableCodeCompletion.Checked = optionsPage.DisableCodeCompletion;
            form.chkDisableGotoDefinition.Checked = optionsPage.DisableGotoDefinition;
            form.chkDisableAssemblyReferences.Checked = optionsPage.DisableAssemblyReferences;
            form.chkDisableForeignProjectReferences.Checked = optionsPage.DisableForeignProjectReferences;
            form.chkDisableXSharpProjectReferences.Checked = optionsPage.DisableXSharpProjectReferences;
            form.chkEnableOutputPane.Checked = optionsPage.EnableOutputPane ;
            form.ShowDialog();
            if (form.DialogResult == DialogResult.OK)
            {
                optionsPage.DisableParameterInfo = form.chkDisableParameters.Checked;
                optionsPage.DisableEditorDropdowns = form.chkDisableEditorDropdowns.Checked;
                optionsPage.DisableBraceMatching = form.chkBraceMatching.Checked;
                optionsPage.DisableClassViewObjectView = form.chkDisableClassViewObjectView.Checked;
                optionsPage.DisableEntityParsing = form.chkDisableEntityParsing.Checked;
                optionsPage.DisableHighLightWord = form.chkDisableHighlightWord.Checked;
                optionsPage.DisableLightBulb = form.chkDisableLigtBulb.Checked;
                optionsPage.DisablePeekDefinition = form.chkDisablePeekDefinition.Checked;
                optionsPage.DisableQuickInfo = form.chkDisableQuickInfo.Checked;
                optionsPage.DisableRegions = form.chkDisableRegions.Checked;
                optionsPage.DisableSyntaxColorization = form.chkDisableSyntaxColorization.Checked;
                optionsPage.DisableCaseSynchronization = form.chkDisableCaseSynchronization.Checked;
                optionsPage.DisableCodeCompletion = form.chkDisableCodeCompletion.Checked;
                optionsPage.DisableGotoDefinition = form.chkDisableGotoDefinition.Checked ;
                optionsPage.DisableAssemblyReferences = form.chkDisableAssemblyReferences.Checked;
                optionsPage.DisableForeignProjectReferences = form.chkDisableForeignProjectReferences.Checked  ;
                optionsPage.DisableXSharpProjectReferences = form.chkDisableXSharpProjectReferences.Checked ;
                optionsPage.EnableOutputPane = form.chkEnableOutputPane.Checked;
            }

        }
    }
}
