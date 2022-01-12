using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using System.ComponentModel.Composition;
using static XSharp.XSharpConstants;

// This now uses the HightLightWord code in the Community Toolkit



namespace XSharp.LanguageService.Editors.HighlightWord
{


    [Export(typeof(IViewTaggerProvider))]
    [ContentType(LanguageName)]
    [TagType(typeof(TextMarkerTag))]
    internal class HighlightWordTaggerProvider : SameWordHighlighterBase
    {

    }
}
