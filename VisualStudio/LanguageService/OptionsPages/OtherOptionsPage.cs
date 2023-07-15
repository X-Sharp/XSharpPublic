using Microsoft.VisualStudio.Shell;
using System;
using System.ComponentModel;
using System.Runtime.InteropServices;
using XSharpModel;
using XSharp.Settings;
namespace XSharp.LanguageService.OptionsPages
{

    [Guid(XSharpConstants.OtherOptionsPageGuidString)]
    [SharedSettings("TextEditor.XSharp", false)]
    [ComVisible(true)]
    class OtherOptionsPage : XSDialogPage<OtherOptionsControl, OtherOptions>
    {
        // The base class exposes the AutomationObject that contains the values
    }
 
}
