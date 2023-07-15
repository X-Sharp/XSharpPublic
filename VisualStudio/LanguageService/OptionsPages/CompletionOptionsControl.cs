using System;
namespace XSharp.LanguageService.OptionsPages
{
    public partial class CompletionOptionsControl : XSUserControl
    {
        public CompletionOptionsControl()
        {
            InitializeComponent();
            chkSnippets.Visible = false;
            chkFields.Tag = nameof(CompletionOptions.CompleteSelf);
            chkLocals.Tag = nameof(CompletionOptions.CompleteLocals);
            chkInherited.Tag = nameof(CompletionOptions.CompleteParent);
            chkNamespaces.Tag = nameof(CompletionOptions.CompleteNamespaces);
            chkTypes.Tag = nameof(CompletionOptions.CompleteTypes);
            chkKeywords.Tag = nameof(CompletionOptions.CompleteKeywords);
            chkSnippets.Tag = nameof(CompletionOptions.CompleteSnippets);
            chkGlobalsProject.Tag = nameof(CompletionOptions.CompleteGlobals);
            chkGlobalsSource.Tag = nameof(CompletionOptions.CompleteGlobalsP);
            chkGlobalsExtern.Tag = nameof(CompletionOptions.CompleteGlobalsA);
            chkFunctions.Tag = nameof(CompletionOptions.CompleteFunctions);
            chkFunctionsSource.Tag = nameof(CompletionOptions.CompleteFunctionsP);
            chkFunctionsExternal.Tag = nameof(CompletionOptions.CompleteFunctionsA);
            tbChars.Tag = nameof(CompletionOptions.CompleteNumChars);
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
