using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.VisualStudio.Package;

namespace XSharp.LanguageService
{
    internal class XSharpTypeAndMemberDropDownBars : TypeAndMemberDropdownBars
    {
        XSharpLanguageService langservice = null;
        internal XSharpTypeAndMemberDropDownBars(XSharpLanguageService lang) : base(lang)
        {
            langservice = lang;
        }

        public override bool OnSynchronizeDropdowns(Microsoft.VisualStudio.Package.LanguageService languageService, Microsoft.VisualStudio.TextManager.Interop.IVsTextView textView, int line, int col, System.Collections.ArrayList dropDownTypes, System.Collections.ArrayList dropDownMembers, ref int selectedType, ref int selectedMember)
        {
            throw new NotImplementedException();
        }
    }
}
