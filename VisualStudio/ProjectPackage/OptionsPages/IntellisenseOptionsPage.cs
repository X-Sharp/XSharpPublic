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
        private bool completionListTabs = true;
        public bool CompletionListTabs
        {
            get { return completionListTabs; }
            set { completionListTabs = value; }
        }

        private bool useDotAsUniversalSelector = false;
        public bool UseDotAsUniversalSelector
        {
            get { return useDotAsUniversalSelector; }
            set { useDotAsUniversalSelector = value; }
        }

        private bool showAfterChar = false;
        public bool ShowAfterChar
        {
            get { return showAfterChar; }
            set { showAfterChar = value; }
        }

        // 0 : none; 1 : UPPER; 2 : lower; 3 : TitleCase
        private int keywordCase = 1;
        public int KeywordCase
        {
            get { return keywordCase; }
            set { keywordCase = value; }
        }

        //private bool identifierCase = false;
        public bool IdentifierCase
        {
            get { return false /*identifierCase */; }
            //set { identifierCase = value; }
        }

        private bool alignDoCase = false;
        public bool AlignDoCase
        {
            get { return alignDoCase; }
            set { alignDoCase = value; }
        }

        private bool alignMethod = false;
        public bool AlignMethod
        {
            get { return alignMethod; }
            set { alignMethod = value; }
        }

        private bool includeFieldsInNavigationBars;
        public bool IncludeFieldsInNavigationBars
        {
            get { return includeFieldsInNavigationBars; }
            set { includeFieldsInNavigationBars = value; }

        }
        private bool sortNavigationBars;
        public bool SortNavigationBars
        {
            get { return sortNavigationBars; }
            set { sortNavigationBars = value; }

        }

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
