using XSharpModel;
using Microsoft.VisualStudio.Text;

namespace XSharp.LanguageService
{

    /// <summary>
    /// This class keeps the first keyword or first two keywords for special lines in the editor
    /// These keywords are used for formatting the code
    /// </summary>
    internal class XSharpLineKeywords : XSharpLineInfo<XKeyword>
    {
        internal XSharpLineKeywords(ITextSnapshot snapshot) : base(snapshot)
        {
        }
        
    }
}
