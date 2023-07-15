using Microsoft.VisualStudio.Shell;
using System;
using System.Runtime.InteropServices;
namespace XSharp.LanguageService.OptionsPages
{
    [ComVisible(true)]
    [Guid(XSharpConstants.CompletionOptionsPageGuidString)]
    [SharedSettings("TextEditor.XSharp", false)]
    public class CompletionOptionsPage : XSDialogPage<CompletionOptionsControl, CompletionOptions>
    {
        // The base class exposes the AutomationObject that contains the values
    }
  

}
