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
    }
}
