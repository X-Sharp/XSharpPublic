using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.VisualStudio.Package;
using System.Collections;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.VisualStudio;
using XSharpModel;

namespace XSharp.LanguageService
{

    public sealed class XSharpTypeAndMemberDropDownBars : TypeAndMemberDropdownBars
    {
        XSharpLanguageService langservice = null;

        public XSharpTypeAndMemberDropDownBars(
            XSharpLanguageService lang,
            IVsTextView view)
            : base(lang)
        {
            langservice = lang;
            //
        }

        public override bool OnSynchronizeDropdowns(
            Microsoft.VisualStudio.Package.LanguageService languageService,
            IVsTextView textView,
            int line,
            int col,
            ArrayList dropDownTypes,
            ArrayList dropDownMembers,
            ref int selectedType,
            ref int selectedMember)
        {
            //
            bool bModification = false;
            Source src = languageService.GetSource(textView);
            String srcFile = src.GetFilePath();
            //
            XFile file = XSharpModel.XSolution.FindFullPath(srcFile);
            if (file == null)
            {
                // Uhh !??, Something went wrong
                return false;
            }
            // we may have to wait for the file to be parsed at least once...
            file.WaitParsing();
            List<XType> typeAtPos = new List<XType>();
            //
            dropDownTypes.Clear();
            //dropDownTypes.Capacity = file.TypeList.Count;
            //
            int nSelType = -1;
            int nSelMbr = -1;
            bool bInSel;
            int nTemp;
            int distance = int.MaxValue;
            int distanceM = int.MaxValue;
            int nCount = 0;
            //
            if (file.TypeList.Count > 0)
                nSelType = 0;
            foreach (XType eltType in file.TypeList)
            {
                //
                if (eltType.Kind != Kind.Class)
                    continue;
                //
                TextSpan sp = this.TextRangeToTextSpan(eltType.Range);
                //
                DROPDOWNFONTATTR ft;
                bModification = true;
                bInSel = false;
                //
                if (TextSpanHelper.ContainsInclusive(sp, line, col))
                {
                    //
                    if (line - sp.iStartLine < distance)
                    {
                        distance = line - sp.iStartLine;
                        bInSel = true;
                    }
                }
                if (bInSel)
                {
                    ft = DROPDOWNFONTATTR.FONTATTR_PLAIN;
                }
                else
                {
                    ft = DROPDOWNFONTATTR.FONTATTR_GRAY;
                }
                DropDownMember elt = new DropDownMember(eltType.Name, sp, eltType.Glyph, ft);
                nTemp = dropDownTypes.Add(elt);
                if (bInSel)
                {
                    nSelType = nTemp;
                }
                //
                dropDownMembers.Clear();
                if (nSelType == nCount)
                {
                    if (eltType.Members.Count > 0)
                        nSelMbr = 0;
                    foreach (XTypeMember member in eltType.Members)
                    {
                        TextSpan spM = this.TextRangeToTextSpan(member.Range);
                        //
                        DropDownMember eltM = new DropDownMember(member.Prototype, spM, member.Glyph, ft);
                        nTemp = dropDownMembers.Add(eltM);
                        //
                        if (TextSpanHelper.ContainsInclusive(spM, line, col))
                        {
                            //
                            if (line - sp.iStartLine < distanceM)
                            {
                                distanceM = line - sp.iStartLine;
                                nSelMbr = nTemp;
                            }
                        }
                    }
                }
                nCount++;
            }
            //
            if (nSelType > -1)
            {
                selectedType = nSelType;
            }
            if (nSelMbr > -1)
            {
                selectedMember = nSelMbr;
            }
            //
            return bModification;
        }

        private TextSpan TextRangeToTextSpan(TextRange tr)
        {
            // It seems that TextSpan is Zero-based
            // where our TextRange is One-Based.
            // --> Make the move
            TextSpan ts = new TextSpan();
            ts.iStartLine = tr.StartLine-1;
            ts.iStartIndex = tr.StartColumn-1;
            ts.iEndLine = tr.EndLine-1;
            ts.iEndIndex = tr.EndColumn-1;
            return ts;
        }

    }

}
