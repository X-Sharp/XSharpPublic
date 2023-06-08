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
        #region Properties that are delegated to the Options object
        public int CompleteNumChars
        {
            get => Options.CompleteNumChars;
            set => Options.CompleteNumChars = value;    
        }

        public bool CompleteLocals
        {
            get => Options.CompleteLocals;
            set => Options.CompleteLocals = value;
        }

        public bool CompleteSelf
        {
            get => Options.CompleteSelf;
            set => Options.CompleteSelf = value;
        }
        public bool CompleteParent
        {
            get => Options.CompleteParent;
            set => Options.CompleteParent = value;
        }
        public bool CompleteNamespaces
        {
            get => Options.CompleteNamespaces;
            set => Options.CompleteNamespaces = value;
        }
        public bool CompleteTypes
        {
            get => Options.CompleteTypes;
            set => Options.CompleteTypes = value;
        }
        public bool CompleteKeywords
        {
            get => Options.CompleteKeywords;
            set => Options.CompleteKeywords = value;
        }
        public bool CompleteSnippets
        {
            get => Options.CompleteSnippets;
            set => Options.CompleteSnippets = value;
        }
        public bool CompleteGlobals
        {
            get => Options.CompleteGlobals;
            set => Options.CompleteGlobals = value;
        }
        public bool CompleteGlobalsP
        {
            get => Options.CompleteGlobalsP;
            set => Options.CompleteGlobalsP = value;
        }
        public bool CompleteGlobalsA
        {
            get => Options.CompleteGlobalsA;
            set => Options.CompleteGlobalsA = value;
        }
        public bool CompleteFunctions
        {
            get => Options.CompleteFunctions;
            set => Options.CompleteFunctions = value;
        }
        public bool CompleteFunctionsP
        {
            get => Options.CompleteFunctionsP;
            set => Options.CompleteFunctionsP = value;
        }
        public bool CompleteFunctionsA
        {
            get => Options.CompleteFunctionsA;
            set => Options.CompleteFunctionsA = value;
        }
        #endregion
    }
}
