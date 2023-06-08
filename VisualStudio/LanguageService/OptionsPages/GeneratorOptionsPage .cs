using Microsoft.VisualStudio.Shell;
using System;
using System.Runtime.InteropServices;

namespace XSharp.LanguageService.OptionsPages
{

    [Guid(XSharpConstants.GeneratorOptionsPageGuidString)]
    [SharedSettings("TextEditor.XSharp",false)]
    [ComVisible(true)]
    class GeneratorOptionsPage : XSDialogPage<GeneratorOptionsControl, GeneratorOptions>
    {
        #region Properties that are delegated to the Options object
        public bool ShowXmlComments
        {
            get => Options.ShowXmlComments;
            set => Options.ShowXmlComments = value;
        }
        public int PublicStyle
        {
            get => Options.PublicStyle;
            set => Options.PublicStyle = value;
        }
        public int PrivateStyle
        {
            get => Options.PrivateStyle;
            set => Options.PrivateStyle = value;
        }

        #endregion
    }
}
