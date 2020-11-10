using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using XSharpModel;
namespace XSharp.LanguageService.OptionsPages
{
    public partial class FormattingOptionsControl : UserControl
    {
        public FormattingOptionsControl()
        {
            InitializeComponent();

        }
        internal FormattingOptionsPage formattingPage;
        private bool _loading = false;
        internal void Initialize()
        {
            _loading = true;
            grpCase.Enabled = true;
            switch (formattingPage.KeywordCase)
            {
                case KeywordCase.Upper:
                    rbUpper.Checked = true;
                    break;
                case KeywordCase.Lower:
                    rbLower.Checked = true;
                    break;
                case KeywordCase.Title:
                    rbTitle.Checked = true;
                    break;
                default:
                    rbNone.Checked = true;
                    break;
            }
            //
            chkIdentifierCase.Checked = formattingPage.IdentifierCase;
            chkAlignDoCase.Checked = formattingPage.AlignDoCase;
            chkAlignMethod.Checked = formattingPage.AlignMethod;
            multiFactor.Text = formattingPage.MultiFactor.ToString();
            _loading = false;

        }


        private void kwCaseChanged(object sender, EventArgs e)
        {
            if (rbNone.Checked)
            {
                formattingPage.KeywordCase = KeywordCase.None;
            }
            else if (rbUpper.Checked)
            {
                formattingPage.KeywordCase = KeywordCase.Upper;
            }
            else if (rbLower.Checked)
            {
                formattingPage.KeywordCase = KeywordCase.Lower;
            }
            else if (rbTitle.Checked)
            {
                formattingPage.KeywordCase = KeywordCase.Title;
            }
        }

        private void chkIdentifierCase_CheckedChanged(object sender, EventArgs e)
        {
            //optionsPage.IdentifierCase = chkIdentifierCase.Checked;
        }

        private void chkAlignDoCase_CheckedChanged(object sender, EventArgs e)
        {
            formattingPage.AlignDoCase = chkAlignDoCase.Checked;
        }

        private void chkAlignMethod_CheckedChanged(object sender, EventArgs e)
        {
            formattingPage.AlignMethod = chkAlignMethod.Checked;
        }

        private void Control_CheckedChanged(object sender, EventArgs e)
        {
            if (!_loading)
            {
            }

        }


        private void multiFactor_TextChanged(object sender, EventArgs e)
        {
            int factor = 0;
            int.TryParse(multiFactor.Text, out factor);
            formattingPage.MultiFactor = factor;
        }

    }
}
