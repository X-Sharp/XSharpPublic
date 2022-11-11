using System;
using System.Windows.Forms;
using XSharpModel;
namespace XSharp.LanguageService.OptionsPages
{
    public partial class GeneratorOptionsControl : XSUserControl
    {
        public GeneratorOptionsControl()
        {
            InitializeComponent();
            rbExport.Tag = nameof(PublicStyle.Export);
            rbPublic.Tag = nameof(PublicStyle.Public);
            rbNone.Tag = nameof(PublicStyle.None);
            rbPrivate.Tag = nameof(PrivateStyle.Private);
            rbHidden.Tag = nameof(PrivateStyle.Hidden);
        }
        internal GeneratorOptionsPage OurOptionsPage => (GeneratorOptionsPage) optionPage;

        internal override void ReadValues()
        {
            base.ReadValues();

            switch (OurOptionsPage.PublicStyle)
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
            switch (OurOptionsPage.PrivateStyle)
            {
                case 1:
                    rbHidden.Checked = true;
                    break;
                default:
                    rbPrivate.Checked = true;
                    break;
            }
        }
        internal override void SaveValues()
        {
            base.SaveValues();
            var controls = new RadioButton[] { rbExport, rbNone, rbPublic, rbPrivate, rbHidden };
            foreach (var rb in controls)
            {
                var tag = rb.Tag;
                if (tag is string strTag && rb.Checked)
                {
                    switch (strTag)
                    {
                        case nameof(PublicStyle.Export):
                        case nameof(PublicStyle.Public):
                        case nameof(PublicStyle.None):
                            OurOptionsPage.PublicStyle = (int)(PublicStyle)Enum.Parse(typeof(PublicStyle), strTag);
                            break;
                        case nameof(PrivateStyle.Private):
                        case nameof(PrivateStyle.Hidden):
                            OurOptionsPage.PrivateStyle = (int)(PrivateStyle)Enum.Parse(typeof(PrivateStyle), strTag);
                            break;
                    }
                }
            }

        }
    }
}
