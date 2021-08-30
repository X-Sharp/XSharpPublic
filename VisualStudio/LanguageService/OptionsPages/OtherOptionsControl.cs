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
    public partial class OtherOptionsControl : UserControl
    {
        public OtherOptionsControl()
        {
            InitializeComponent();
        }
        internal OtherOptionsPage optionsPage;
        private bool _loading = false;
        internal void Initialize()
        {
            _loading = true;
            chkShowDividers.Checked = optionsPage.ShowDividers;
            chkSingleLineDividers.Checked = optionsPage.ShowSingleLineDividers;
            chkSingleLineDividers.Enabled = chkShowDividers.Checked;
            chkShowXMLComments.Checked = optionsPage.ShowXmlComments;
            switch (optionsPage.PublicStyle)
            {
                case 1:
                    rbExport.Checked = true;
                    break;
                case 2:
                    rbNone.Checked = true;
                    break;
                default:
                    rbPublic.Checked = true;
                    break;
            }
            switch (optionsPage.PrivateStyle)
            {
                case 1:
                    rbHidden.Checked = true;
                    break;
                default:
                    rbPrivate.Checked = true;
                    break;
            }
            _loading = false;

        }

        private void chkShowDividers_CheckedChanged(object sender, EventArgs e)
        {
            if (! _loading)
                chkSingleLineDividers.Enabled = chkShowDividers.Checked;
            optionsPage.ShowDividers = chkShowDividers.Checked;
        }

        private void chkSingleLineDividers_CheckedChanged(object sender, EventArgs e)
        {
            optionsPage.ShowSingleLineDividers = chkSingleLineDividers.Checked;
        }

        private void chkShowXMLComments_CheckedChanged(object sender, EventArgs e)
        {
            optionsPage.ShowXmlComments = chkShowXMLComments.Checked;
        }

        private void groupBox1_Enter(object sender, EventArgs e)
        {

        }

        private void rbPublic_CheckedChanged(object sender, EventArgs e)
        {
            optionsPage.PublicStyle = 0;
        }

        private void rbExport_CheckedChanged(object sender, EventArgs e)
        {
            optionsPage.PublicStyle = 1;
        }

        private void rbNone_CheckedChanged(object sender, EventArgs e)
        {
            optionsPage.PublicStyle = 2;
        }

        private void rbPrivate_CheckedChanged(object sender, EventArgs e)
        {
            optionsPage.PrivateStyle = 0;
        }

        private void rbHidden_CheckedChanged(object sender, EventArgs e)
        {
            optionsPage.PrivateStyle = 1;
        }
    }
}
