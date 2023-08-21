using System;
namespace XSharp.LanguageService.OptionsPages
{
    public partial class IntellisenseOptionsControl : XSUserControl
    {
        string defaultCommitChars = "{}[]().,:;+-*/%&|^!~<>?@#\'\"\\";
        public IntellisenseOptionsControl()
        {
            InitializeComponent();
            this.commitChars.Text = defaultCommitChars;
            chkCompletionListtabs.Tag = nameof(IntellisenseOptions.CompletionListTabs);
            chkKeywordsInAll.Tag = nameof(IntellisenseOptions.KeywordsInAll);
            chkIncludeFields.Tag = nameof(IntellisenseOptions.IncludeFieldsInNavigationBars);
            chkSortNavBar.Tag = nameof(IntellisenseOptions.SortNavigationBars);
            chkShowMembersOfCurrentType.Tag = nameof(IntellisenseOptions.ShowMembersOfCurrentTypeOnly);
            commitChars.Tag = nameof(IntellisenseOptions.CommitChars);
            chkExcludeMembersFromOtherfiles.Tag = nameof(IntellisenseOptions.ExcludeMembersFromOtherFiles);

            if (String.IsNullOrEmpty(Environment.GetEnvironmentVariable(Constants.EnvironmentXSharpDev)))
            {
                this.btnShowMeTheMagic.Visible = false;
            }

        }

        private void btnShowMeTheMagic_Click(object sender, EventArgs e)
        {
            XSharpSpecialOptions form = new XSharpSpecialOptions((IntellisenseOptionsPage)optionPage);
            form.ShowDialog();
        }
      
        private void btnReset_Click(object sender, EventArgs e)
        {
            this.commitChars.Text = defaultCommitChars;
        }
    }
}
