using Microsoft.VisualStudio.Shell;
using System;
using System.Runtime.InteropServices;
using XSharpModel;
using XSharp.Settings;
namespace XSharp.LanguageService.OptionsPages
{

    [Guid(XSharpConstants.GeneratorOptionsPageGuidString)]
    [SharedSettings("TextEditor.XSharp",false)]
    [ComVisible(true)]
    class GeneratorOptionsPage : XSDialogPage<GeneratorOptionsControl, GeneratorOptions>
    {
        // The base class exposes the AutomationObject that contains the values
    }
    public class GeneratorOptions : OptionsBase
    {
        #region Properties
        public bool ShowXmlComments { get; set; }
        public int PublicStyle { get; set; }
        public int PrivateStyle { get; set; }
        #endregion
        public GeneratorOptions()
        {
            ShowXmlComments = true;
            PublicStyle = 0;
            PrivateStyle = 0;
        }
        public override void WriteToSettings()
        {
            XSettings.CodeGeneratorPrivateStyle = (PrivateStyle)PrivateStyle;
            XSettings.CodeGeneratorPublicStyle = (PublicStyle)PublicStyle;
            XSettings.CodeGeneratorShowXmlComments = ShowXmlComments;

        }
    }
}
