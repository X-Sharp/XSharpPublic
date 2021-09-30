using Microsoft.VisualStudio.Shell;
using System;
using System.ComponentModel;
using System.Runtime.InteropServices;

namespace XSharp.LanguageService.OptionsPages
{

    [Guid(XSharpConstants.CompletionOptionsPageGuidString)]
    [SharedSettings("TextEditor.XSharp", false)]
    public class CompletionOptionsPage : XSDialogPage<CompletionOptionsControl>
    {
        [DefaultValue(4)]
        public int CompleteNumChars{ get; set; }
        [DefaultValue(true)]
        public bool CompleteLocals { get; set; }
        [DefaultValue(true)]
        public bool CompleteSelf { get; set; }
        [DefaultValue(true)]
        public bool CompleteParent { get; set; }
        [DefaultValue(true)]
        public bool CompleteNamespaces { get; set; }
        [DefaultValue(true)]
        public bool CompleteTypes { get; set; }
        [DefaultValue(true)]
        public bool CompleteKeywords { get; set; }
        [DefaultValue(true)]
        public bool CompleteSnippets { get; set; }
        [DefaultValue(true)]
        public bool CompleteGlobals{ get; set; }
        [DefaultValue(true)]
        public bool CompleteGlobalsP { get; set; }
        [DefaultValue(true)]
        public bool CompleteGlobalsA { get; set; }
        [DefaultValue(true)]
        public bool CompleteFunctions { get; set; }
        [DefaultValue(true)]
        public bool CompleteFunctionsP { get; set; }
        [DefaultValue(true)]
        public bool CompleteFunctionsA { get; set; }
        [DefaultValue(10000)]
        public int MaxCompletionEntries { get; set; }
        

    }
}
