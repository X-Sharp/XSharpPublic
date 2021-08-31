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
    public partial class CompletionOptionsControl : UserControl
    {
        public CompletionOptionsControl()
        {
            InitializeComponent();
            this.rtfDescription.Text = String.Join(Environment.NewLine,
                new string[] {
                "Code Completion is triggered by special characters such as '.' and ':'",
                "The editor can also perform code completion at other locations such as:",
                "•\tafter certain keywords, such as AS, INHERIT, IMPLEMENTS, USING ",
                "•\tat the start of an expression, such as at the start of a line or after an",
                "\topening '(', '{' or '[' or after :=",
                "The filling of these last \"generic\" completion lists will be started after",
                "you have typed a few characters, but these lists could still be long",
                "and filling the list could be time consuming.",
                "",
                "On this page you can control where the editor will look for completion." });
        }
        internal CompletionOptionsPage optionsPage;
        internal void Initialize()
        {
            this.tbChars.Value = optionsPage.CompleteNumChars > 0 ? optionsPage.CompleteNumChars : 1 ;
            chkFields.Checked = optionsPage.CompleteSelf;
            chkInherited.Checked = optionsPage.CompleteParent;
            chkLocals.Checked = optionsPage.CompleteLocals;
            chkKeywords.Checked = optionsPage.CompleteKeywords;
            chkSnippets.Checked = optionsPage.CompleteSnippets;
            chkGlobalsProject.Checked = optionsPage.CompleteGlobals;
            chkGlobalsSource.Checked = optionsPage.CompleteGlobalsP;
            chkGlobalsExtern.Checked = optionsPage.CompleteGlobalsA;
            chkFunctions.Checked = optionsPage.CompleteFunctions;
            chkFunctionsSource.Checked = optionsPage.CompleteGlobalsP;
            chkFunctionsExternal.Checked = optionsPage.CompleteGlobalsA;
        }

        private void btnAll_Click(object sender, EventArgs e)
        {
            checkbuttons(true);
        }

        private void btnNothing_Click(object sender, EventArgs e)
        {
            checkbuttons(false);
        }
        private void checkbuttons(bool check)
        {
            foreach (var control in this.Controls)
            {
                if (control is CheckBox cb)
                {
                    cb.Checked = check;
                }
            }

        }

        private void Control_CheckedChanged(object sender, EventArgs e)
        {
            var isChecked = ((CheckBox)sender).Checked;
            if (sender == chkLocals)
            {
                optionsPage.CompleteLocals = isChecked;
            }
            else if (sender == chkFields)
            {
                optionsPage.CompleteSelf = isChecked;
            }
            else if (sender == chkInherited)
            {
                optionsPage.CompleteParent = isChecked;
            }
            else if (sender == chkKeywords)
            {
                optionsPage.CompleteKeywords = isChecked;
            }
            else if (sender == chkSnippets)
            {
                optionsPage.CompleteSnippets = isChecked;
            }
            else if (sender == chkFunctions)
            {
                optionsPage.CompleteFunctions = isChecked;
            }
            else if (sender == chkFunctionsExternal)
            {
                optionsPage.CompleteFunctionsA = isChecked;
            }
            else if (sender == chkFunctionsSource)
            {
                optionsPage.CompleteFunctionsP = isChecked;
            }
            else if (sender == chkGlobalsProject)
            {
                optionsPage.CompleteGlobals = isChecked;
            }
            else if (sender == chkGlobalsSource)
            {
                optionsPage.CompleteGlobalsP = isChecked;
            }
            else if (sender == chkGlobalsExtern)
            {
                optionsPage.CompleteGlobalsA = isChecked;
            }

        }

        private void tbChars_ValueChanged(object sender, EventArgs e)
        {
            optionsPage.CompleteNumChars = (int)tbChars.Value;

        }
    }
}
