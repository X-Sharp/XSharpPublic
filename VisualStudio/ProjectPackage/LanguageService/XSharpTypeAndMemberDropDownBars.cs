//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
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

        internal class sortXElement : IComparer
        {
            // Calls CaseInsensitiveComparer.Compare with the parameters reversed.
            int IComparer.Compare(Object x, Object y)
            {
                XElement elt1 = x as XElement;
                XElement elt2 = y as XElement;
                //
                return elt1.Name.CompareTo(elt2.Name);
            }

        }

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
            var package = XSharp.Project.XSharpProjectPackage.Instance;
            var optionsPage = package.GetIntellisenseOptionsPage();
            if (optionsPage.DisableEditorDropdowns)
            {
                dropDownTypes.Clear();
                dropDownMembers.Clear();
                selectedType = selectedMember = -1;
                return true;
            }

            var sortItems = optionsPage.SortNavigationBars;
            var includeFields = optionsPage.IncludeFieldsInNavigationBars;
            bool bModification = false;
            Source src = languageService.GetSource(textView);
            String srcFile = src.GetFilePath();
            //
            XFile file = XSharpModel.XSolution.FindFullPath(srcFile);
            if (file == null || file.TypeList == null)
            {
                return false;
            }

            //
            dropDownTypes.Clear();
            dropDownMembers.Clear();
            int nSelType = -1;
            int nSelMbr = -1;
            //int distanceM = int.MaxValue;
            DROPDOWNFONTATTR ft;
            //
            //
            if (file.TypeList.Count > 0)
            {
                nSelType = 0;
            }

            List<XType> xList = file.TypeList.Values.ToList<XType>();
            if (sortItems)
            {
                xList.Sort(delegate (XType elt1, XType elt2)
                {
                    return elt1.Name.CompareTo(elt2.Name);
                });
            }
            XType typeGlobal = null;
            int nSelect  = 0;

            XElement selectedElement = file.FindMemberAtRow(line);
            XTypeMember currentMember = null;
            XType currentType = null;

            if (selectedElement is XTypeMember)
            {
                currentMember = selectedElement as XTypeMember;
                currentType = currentMember.Parent;
            }
            else if (selectedElement is XType)
            {
                currentType = selectedElement as XType;
                currentMember = null;
            }
            nSelect = 0;
            DropDownMember elt;
            foreach (XType eltType in xList)
            {
                if (eltType.Kind == Kind.Namespace)
                    continue;
                //
                if (XType.IsGlobalType(eltType))
                {
                    typeGlobal = eltType;
                }
                TextSpan sp = this.TextRangeToTextSpan(eltType.Range);
                bModification = true;
                if (eltType == currentType)
                {
                    ft = DROPDOWNFONTATTR.FONTATTR_PLAIN;
                }
                else
                {
                    ft = DROPDOWNFONTATTR.FONTATTR_GRAY;
                }
                string name = eltType.Name ;
                if (string.IsNullOrEmpty(name))
                {
                    name = "?";
                }
                elt = new DropDownMember(name, sp, eltType.Glyph, ft);
                nSelect = dropDownTypes.Add(elt);
                if (eltType?.FullName == currentType?.FullName)
                {
                    nSelType = nSelect;
                }
                //
            }

            if (currentType == null)
                currentType = typeGlobal;
            if (currentType != null)    // should not happen since all files have a global type
            {
                nSelMbr = 0;
                IEnumerable<XTypeMember> members = currentType.Members;
                if (sortItems)
                {
                    members = members.OrderBy(x => x.Name);
                }
                // Add member for class declaration
                TextSpan spM = this.TextRangeToTextSpan(currentType.Range);
                ft = DROPDOWNFONTATTR.FONTATTR_GRAY;
                if (currentType != typeGlobal)
                {
                    elt = new DropDownMember("("+currentType.Name+")", spM, currentType.Glyph, ft);
                    dropDownMembers.Add(elt);
                }
                foreach (XTypeMember member in members)
                {
                    if (includeFields || member.Kind != Kind.Field)
                    {
                        spM = this.TextRangeToTextSpan(member.Range);

                        if (member == currentMember)
                        {
                            ft = DROPDOWNFONTATTR.FONTATTR_PLAIN;
                        }
                        else
                        {
                            ft = DROPDOWNFONTATTR.FONTATTR_GRAY;
                        }
                        string prototype = member.Prototype;
                        if (prototype.Length > 80)
                        {
                            prototype = prototype.Substring(0, 80) + "...";
                        }
                        elt = new DropDownMember(prototype, spM, member.Glyph, ft);
                        nSelect = dropDownMembers.Add(elt);
                        if (member == currentMember)
                        {
                            nSelMbr = nSelect;
                        }
                    }
                }
            }
            selectedType    = nSelType;
            selectedMember  = nSelMbr;
            return bModification;
        }

        private TextSpan TextRangeToTextSpan(TextRange tr)
        {
            // It seems that TextSpan is Zero-based
            // where our TextRange is One-Based.
            // --> Make the move
            TextSpan ts = new TextSpan();
            ts.iStartLine = tr.StartLine - 1;
            ts.iStartIndex = tr.StartColumn - 1;
            ts.iEndLine = tr.EndLine - 1;
            ts.iEndIndex = tr.EndColumn - 1;
            // validate values
            if (ts.iStartLine < 0)
                ts.iStartLine = 0;
            if (ts.iStartIndex < 0)
                ts.iStartIndex = 0;
            if (ts.iEndLine < ts.iStartLine)
                ts.iEndLine = ts.iStartLine;
            if (ts.iEndIndex < ts.iStartLine && ts.iStartLine == ts.iEndLine)
                ts.iEndIndex = ts.iStartIndex;
            return ts;
        }

    }

}
