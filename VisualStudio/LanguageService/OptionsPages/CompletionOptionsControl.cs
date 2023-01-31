using System;
using System.Windows.Controls;
using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using VSRegistry = Microsoft.VisualStudio.Shell.VSRegistry;
namespace XSharp.LanguageService.OptionsPages
{
    public partial class CompletionOptionsControl : XSUserControl
    {
        public CompletionOptionsControl()
        {
            InitializeComponent();
            chkSnippets.Visible = false;
            chkFields.Tag = nameof(CompletionOptionsPage.CompleteSelf);
            chkLocals.Tag = nameof(CompletionOptionsPage.CompleteLocals);
            chkInherited.Tag = nameof(CompletionOptionsPage.CompleteParent);
            chkNamespaces.Tag = nameof(CompletionOptionsPage.CompleteNamespaces);
            chkTypes.Tag = nameof(CompletionOptionsPage.CompleteTypes);
            chkKeywords.Tag = nameof(CompletionOptionsPage.CompleteKeywords);
            chkSnippets.Tag = nameof(CompletionOptionsPage.CompleteSnippets);
            chkGlobalsProject.Tag = nameof(CompletionOptionsPage.CompleteGlobals);
            chkGlobalsSource.Tag = nameof(CompletionOptionsPage.CompleteGlobalsP);
            chkGlobalsExtern.Tag = nameof(CompletionOptionsPage.CompleteGlobalsA);
            chkFunctions.Tag = nameof(CompletionOptionsPage.CompleteFunctions);
            chkFunctionsSource.Tag = nameof(CompletionOptionsPage.CompleteFunctionsP);
            chkFunctionsExternal.Tag = nameof(CompletionOptionsPage.CompleteFunctionsA);
            tbChars.Tag = nameof(CompletionOptionsPage.CompleteNumChars);
            this.rtfDescription.Text = String.Join(Environment.NewLine,
                new string[] {
                "Code Completion is triggered by special characters such as '.' and ':'",
                "The editor can also perform code completion at other locations such as:",
                "•\tafter certain keywords, such as AS, INHERIT, IMPLEMENTS, USING ",
                "•\tat the start of an expression, such as at the start of a line or after an",
                "\topening '(', '{' or '[' or after :=",
                "The filling of these second \"generic\" completion lists will be NOT be automatically ",
                "started, but you have to press the \"Complete Word\" key, which is usually assigned ",
                "to the Ctrl-Space key.",
                "",
                "On this page you can control where the editor will look for completion." });
            this.tbChars.Enabled = this.lblChars.Enabled = false;
            this.chkSnippets.Enabled = false;
        }


        internal override void ReadValues()
        {
            base.ReadValues();
            object value = null;
            // Set default values
            var reg = VSRegistry.RegistryRoot(__VsLocalRegistryType.RegType_UserSettings);
            if (reg != null)
            {
                var key = reg.OpenSubKey("ApplicationPrivateSettings\\TextEditor\\XSharp");
                if (key != null)
                {
                    value = key.GetValue("CompleteFunctions");
                }
            }
            if (value == null)
            {
                tbChars.Value = 4;
                checkbuttons(true);
            }
        }


        private void btnAll_Click(object sender, EventArgs e)
        {
            checkbuttons(true);
        }

        private void btnNothing_Click(object sender, EventArgs e)
        {
            checkbuttons(false);
        }
    }
}
