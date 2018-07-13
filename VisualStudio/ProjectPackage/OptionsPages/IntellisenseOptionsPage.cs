using Microsoft.VisualStudio.Shell;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace XSharp.Project.OptionsPages
{
    [Guid(XSharpConstants.IntellisenseOptionsPageGuidString)]
    [SharedSettings("TextEditor.XSharp",false)]
    class IntellisenseOptionsPage : DialogPage
    {
        public bool CompletionListTabs { get; set; }

        public bool KeywordsInAll { get; set; }

        public bool UseDotAsUniversalSelector { get; set; }
        
        public bool ShowAfterChar { get; set; }
        
        // 0 : none; 1 : UPPER; 2 : lower; 3 : TitleCase
        public int KeywordCase { get; set; }
        public bool IdentifierCase => false;
        public bool AlignDoCase { get; set; }
        public bool AlignMethod { get; set; }
        public bool IncludeFieldsInNavigationBars { get; set; }
        public bool SortNavigationBars { get; set; }
        public bool DisableBraceMatching { get; set; }
        public bool DisableLightBulb { get; set; }
        public bool DisableHighLightWord { get; set; }
        public bool DisableParameterInfo { get; set; }
        public bool DisablePeekDefinition { get; set; }
        public bool DisableQuickInfo { get; set; }
        public bool DisableSyntaxColorization { get; set; }
        public bool DisableEntityParsing { get; set; }
        public bool DisableRegions { get; set; }
        public bool DisableCaseSynchronization { get; set; }
        public bool DisableEditorDropdowns { get; set; }
        public bool DisableClassViewObjectView { get; set; }
        public bool DisableCodeCompletion { get; set; }
        public bool DisableGotoDefinition { get; set; }
        public bool DisableAssemblyReferences { get; set; }
        public bool DisableForeignProjectReferences { get; set; }
        public bool DisableXSharpProjectReferences { get; set; }
        public bool EnableOutputPane { get; set; }
        protected override IWin32Window Window
        {
            get
            {
                IntellisenseOptionsControl page = new IntellisenseOptionsControl();
                page.optionsPage = this;
                page.Initialize();
                return page;
            }
        }
        public override void LoadSettingsFromStorage()
        {
            base.LoadSettingsFromStorage();
        }
        public override void SaveSettingsToStorage()
        {
            base.SaveSettingsToStorage();
        }
    }
}
