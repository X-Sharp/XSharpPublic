using Microsoft.VisualStudio.Shell;
using System;
using System.Runtime.InteropServices;

namespace XSharp.LanguageService.OptionsPages
{

    [SharedSettings("TextEditor.XSharp", false)]
    [Guid(XSharpConstants.IndentingOptionsPageGuidString)]
    [ComVisible(true)]
    public class IndentingOptionsPage : XSDialogPage<IndentingOptionsControl, IndentingOptions>
    {
        #region Properties that are delegated to the Options object
        public bool IndentEntityContent
        {
            get => Options.IndentEntityContent;
            set => Options.IndentEntityContent = value;
        }

        public bool IndentFieldContent
        {
            get => Options.IndentFieldContent;
            set => Options.IndentFieldContent = value;
        }
        public bool IndentBlockContent
        {
            get => Options.IndentBlockContent;
            set => Options.IndentBlockContent = value;
        }
        public bool IndentCaseContent
        {
            get => Options.IndentCaseContent;
            set => Options.IndentCaseContent = value;
        }
        public bool IndentMultiLines
        {
            get => Options.IndentMultiLines;
            set => Options.IndentMultiLines = value;
        }
        public bool IndentCaseLabel
        {
            get => Options.IndentCaseLabel;
            set => Options.IndentCaseLabel = value;
        }
        public bool IndentPreprocessorLines
        {
            get => Options.IndentPreprocessorLines;
            set => Options.IndentPreprocessorLines = value;
        }
        public bool IndentNamespace
        {
            get => Options.IndentNamespace;
            set => Options.IndentNamespace = value;
        }
        #endregion
    }
}
