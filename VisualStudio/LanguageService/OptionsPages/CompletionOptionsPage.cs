using Microsoft.VisualStudio.Shell;
using System;
using System.ComponentModel;
using System.Runtime.InteropServices;

namespace XSharp.LanguageService.OptionsPages
{
    [ComVisible(true)]
    [Guid(XSharpConstants.CompletionOptionsPageGuidString)]
    [SharedSettings("TextEditor.XSharp", false)]
    public class CompletionOptionsPage : XSDialogPage<CompletionOptionsControl, CompletionOptions>
    {
        public int CompleteNumChars{ get ; set; }
        public bool CompleteLocals { get; set; }
        public bool CompleteSelf { get; set; }
        public bool CompleteParent { get; set; }
        public bool CompleteNamespaces { get; set; }
        public bool CompleteTypes { get; set; }
        public bool CompleteKeywords { get; set; }
        public bool CompleteSnippets { get; set; }
        public bool CompleteGlobals{ get; set; }
        public bool CompleteGlobalsP { get; set; }
        public bool CompleteGlobalsA { get; set; }
        public bool CompleteFunctions { get; set; }
        public bool CompleteFunctionsP { get; set; }
        public bool CompleteFunctionsA { get; set; }

    }
}
