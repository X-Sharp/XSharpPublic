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
            chkCompletionListtabs.Tag = nameof(IntellisenseOptionsPage.CompletionListTabs);
            chkKeywordsInAll.Tag = nameof(IntellisenseOptionsPage.KeywordsInAll);
            chkIncludeFields.Tag = nameof(IntellisenseOptionsPage.IncludeFieldsInNavigationBars);
            chkSortNavBar.Tag = nameof(IntellisenseOptionsPage.SortNavigationBars);
            chkShowMembersOfCurrentType.Tag = nameof(IntellisenseOptionsPage.ShowMembersOfCurrentTypeOnly);
            commitChars.Tag = nameof(IntellisenseOptionsPage.CommitChars);
            chkExcludeMembersFromOtherfiles.Tag = nameof(IntellisenseOptionsPage.ExcludeMembersFromOtherFiles);

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
