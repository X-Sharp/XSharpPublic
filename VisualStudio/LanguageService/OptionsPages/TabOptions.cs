using Microsoft.VisualStudio.TextManager.Interop;
using XSharpModel;
using XSharp.Settings;
namespace XSharp.LanguageService
{
    public class TabOptions : OptionsBase
    {
        public TabOptions()
        {
            IndentStyle = (int)vsIndentStyle.vsIndentStyleSmart;
            TabSize = IndentSize = 3;
            HideAdvancedMembers = true;
            TabsAsSpaces = true;
        }
        public int IndentStyle { get; set; }
        public bool HideAdvancedMembers { get; set; }
        public int TabSize { get; set; }
        public int IndentSize { get; set; }
        public bool TabsAsSpaces { get; set; }

        public override void WriteToSettings()
        {
            XEditorSettings.IndentStyle = IndentStyle;
            XEditorSettings.HideAdvancedMembers = HideAdvancedMembers;
            XEditorSettings.TabSize = TabSize;
            XEditorSettings.IndentSize = IndentSize;
            XEditorSettings.TabsAsSpaces = TabsAsSpaces;
        }
    }
}
