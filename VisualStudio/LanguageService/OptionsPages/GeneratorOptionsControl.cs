using System;
using System.Windows.Forms;
using XSharpModel;
using XSharp.Settings;
namespace XSharp.LanguageService.OptionsPages
{
    public partial class GeneratorOptionsControl : XSUserControl
    {
        public GeneratorOptionsControl()
        {
            InitializeComponent();
            chkShowXMLComments.Tag = nameof(GeneratorOptions.ShowXmlComments); 
            rbExport.Tag = nameof(PublicStyle.Export);
            rbPublic.Tag = nameof(PublicStyle.Public);
            rbNone.Tag = nameof(PublicStyle.None);
            rbPrivate.Tag = nameof(PrivateStyle.Private);
            rbHidden.Tag = nameof(PrivateStyle.Hidden);
        }

        internal override void ReadValues(object options)
        {
            GeneratorOptionsPage OurOptionsPage = (GeneratorOptionsPage)optionPage;
            base.ReadValues(options);

            switch (OurOptionsPage.Options.PublicStyle)
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
            switch (OurOptionsPage.Options.PrivateStyle)
            {
                case 1:
                    rbHidden.Checked = true;
                    break;
                default:
                    rbPrivate.Checked = true;
                    break;
            }
        }
        internal override void SaveValues(object options)
        {
            base.SaveValues(options);
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
                            ((GeneratorOptions) options).PublicStyle = (int)(PublicStyle)Enum.Parse(typeof(PublicStyle), strTag);
                            break;
                        case nameof(PrivateStyle.Private):
                        case nameof(PrivateStyle.Hidden):
                            ((GeneratorOptions)options).PrivateStyle = (int)(PrivateStyle)Enum.Parse(typeof(PrivateStyle), strTag);
                            break;
                    }
                }
            }

        }
    }
}
