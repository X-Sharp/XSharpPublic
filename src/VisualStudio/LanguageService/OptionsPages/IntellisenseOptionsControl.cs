using Community.VisualStudio.Toolkit;
using System;
using XSharp.Settings;
using XSharpModel;
namespace XSharp.LanguageService.OptionsPages
{
    public partial class IntellisenseOptionsControl : XSUserControl
    {
        string defaultCommitChars = "{}[]().,:;+-*/%&|^!~<>?@#\'\"\\";
        bool Isx86 = false;
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
            chkUseMicrosoftSQLite.Tag = nameof(IntellisenseOptions.UseMicrosoftSQLite);
            if (String.IsNullOrEmpty(Environment.GetEnvironmentVariable(Constants.EnvironmentXSharpDev)))
            {
                this.btnShowMeTheMagic.Visible = false;
            }
            Isx86 = IntPtr.Size == 4;
            if (Isx86)
            {
                chkUseMicrosoftSQLite.Checked = false;
                chkUseMicrosoftSQLite.Visible = false;
            }
            else if (XSettings.IsArm)
            {
                chkUseMicrosoftSQLite.Checked = true;
                chkUseMicrosoftSQLite.Visible = false;
            }

        }
        internal override void ReadValues(object options)
        {
            base.ReadValues(options);
            if (Isx86)
            {
                chkUseMicrosoftSQLite.Checked = false;
            }
            else if (XSettings.IsArm)
            {
                chkUseMicrosoftSQLite.Checked = true;
            }
        }
        internal override void SaveValues(object options)
        {
            base.SaveValues(options);
            if (!Isx86 && chkUseMicrosoftSQLite.Checked != XSettings.UseMicrosoftSQLite && !XSettings.IsArm)
            {
                VS.MessageBox.ShowWarning("You have changed the setting for the SQLite provider. This change will only take effect after you restart Visual Studio");
                XDatabase.DeleteOnClose = true;
                XSettings.UseMicrosoftSQLite = chkUseMicrosoftSQLite.Checked;
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
