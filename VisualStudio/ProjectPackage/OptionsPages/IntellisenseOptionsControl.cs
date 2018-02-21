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
        }
        internal IntellisenseOptionsPage optionsPage;

        internal void Initialize()
        {
            chkCompletionListtabs.Checked = optionsPage.CompletionListTabs;
            chkDotAsUniversalSelector.Checked = optionsPage.UseDotAsUniversalSelector;
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
        }


        private void chkCompletionListtabs_CheckedChanged(object sender, EventArgs e)
        {
            optionsPage.CompletionListTabs = chkCompletionListtabs.Checked;
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
    }
}
