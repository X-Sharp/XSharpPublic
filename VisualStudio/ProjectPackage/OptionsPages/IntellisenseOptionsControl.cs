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
            grpCase.Enabled = false;
        }


        private void chkCompletionListtabs_CheckedChanged(object sender, EventArgs e)
        {
            optionsPage.CompletionListTabs = chkCompletionListtabs.Checked;
        }
    }
}
