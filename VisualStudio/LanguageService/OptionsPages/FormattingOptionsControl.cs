using System;
using System.Windows.Forms;
using XSharpModel;
using XSharp.Settings;
namespace XSharp.LanguageService.OptionsPages
{
    public partial class FormattingOptionsControl : XSUserControl
    {
        
        public FormattingOptionsControl()
        {
            InitializeComponent();
            grpCase.Enabled = true;
            rbUpper.Tag = nameof(KeywordCase.Upper);
            rbLower.Tag = nameof(KeywordCase.Lower);
            rbTitle.Tag = nameof(KeywordCase.Title);
            rbNone.Tag = nameof(KeywordCase.None);

            chkSynchronizeUDCKeywords.Tag = nameof(FormattingOptions.UdcCase);
            chkIdentifierCase.Tag = nameof(FormattingOptions.IdentifierCase);
            chkInsertFinalNewLine.Tag = nameof(FormattingOptions.InsertFinalNewLine);
            chkTrimTrailngWhiteSpace.Tag = nameof(FormattingOptions.TrimTrailingWhiteSpace);

        }


        internal override void ReadValues(object options)
        {
            base.ReadValues(options);
            switch (((FormattingOptions)options).KeywordCase)
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
            showExample();
        }
        internal override void SaveValues(object options)
        {
            base.SaveValues(options);
            var controls = new RadioButton[] { rbLower, rbUpper, rbNone, rbTitle };
            foreach (var rb in controls)
            {
                var tag = rb.Tag;
                if (tag is string strTag && rb.Checked)
                {
                    switch (strTag)
                    {
                        case nameof(KeywordCase.None):
                        case nameof(KeywordCase.Upper):
                        case nameof(KeywordCase.Lower):
                        case nameof(KeywordCase.Title):
                            ((FormattingOptions) options).KeywordCase = (KeywordCase)Enum.Parse(typeof(KeywordCase), strTag);
                            break;
                    }
                }
            }
        }

  
        private void caseChanged(object sender, EventArgs e)
        {
            showExample();
        }
        void showExample()
        {
            if (rbUpper.Checked)
            {
                tbExample.Text = "FUNCTION";
            }
            else if (rbLower.Checked)
            {
                tbExample.Text = "function";
            }
            else if (rbNone.Checked)
            {
                tbExample.Text = "FuNcTiOn";
            }
            else if (rbTitle.Checked)
            {
                tbExample.Text = "Function";
            }

        }
    }
}
