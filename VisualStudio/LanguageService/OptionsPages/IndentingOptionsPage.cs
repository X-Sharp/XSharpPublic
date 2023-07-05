using Microsoft.VisualStudio.Shell;
using System;
using System.ComponentModel;
using System.Runtime.InteropServices;
using XSharpModel;
using XSharp.Settings;
namespace XSharp.LanguageService.OptionsPages
{

    [SharedSettings("TextEditor.XSharp", false)]
    [Guid(XSharpConstants.IndentingOptionsPageGuidString)]
    [ComVisible(true)]
    public class IndentingOptionsPage : XSDialogPage<IndentingOptionsControl, IndentingOptions>
    {
        // The base class exposes the AutomationObject that contains the values
    }
    public class IndentingOptions : OptionsBase
    {
        #region Properties
        [DefaultValue(true)]
        public bool IndentEntityContent { get; set; }
        [DefaultValue(true)]
        public bool IndentFieldContent { get; set; }
        [DefaultValue(true)]
        public bool IndentBlockContent { get; set; }
        [DefaultValue(true)]
        public bool IndentCaseContent { get; set; }
        [DefaultValue(true)]
        public bool IndentMultiLines { get; set; }
        [DefaultValue(false)]

        public bool IndentCaseLabel { get; set; }
        [DefaultValue(false)]
        public bool IndentPreprocessorLines { get; set; }
        [DefaultValue(false)]
        public bool IndentNamespace { get; set; }
        #endregion
        public IndentingOptions()
        {
            IndentEntityContent = true; // class members
            IndentFieldContent = true;  // class fields
            IndentBlockContent = true;  // statements
            IndentCaseContent = true;   // statement block inside case
            IndentMultiLines = true;    // Multi line statements
            IndentCaseLabel = false;
            IndentPreprocessorLines = false;
            IndentNamespace = false;
        }
        public override void WriteToSettings()
        {
            XEditorSettings.IndentTypeMembers = IndentEntityContent;
            XEditorSettings.IndentTypeFields = IndentFieldContent;
            XEditorSettings.IndentStatements = IndentBlockContent;
            XEditorSettings.IndentCaseContent = IndentCaseContent;
            XEditorSettings.IndentCaseLabel = IndentCaseLabel;
            XEditorSettings.IndentContinuedLines = IndentMultiLines;
            XEditorSettings.IndentPreprocessorLines = IndentPreprocessorLines;
            XEditorSettings.IndentNamespace = IndentNamespace;
        }
    }

}
