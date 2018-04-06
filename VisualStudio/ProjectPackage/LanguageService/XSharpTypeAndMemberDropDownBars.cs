//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System.Collections.Generic;
using System.Linq;
using Microsoft.VisualStudio.Package;
using System.Collections;
using Microsoft.VisualStudio.TextManager.Interop;
using XSharpModel;

namespace XSharp.LanguageService
{

    public sealed class XSharpTypeAndMemberDropDownBars : TypeAndMemberDropdownBars
    {
        int lastLine = -1;
        XElement _lastSelected;
        ArrayList _members = null;
        XFile _file = null;
        uint _lastHashCode = 0;

        public XSharpTypeAndMemberDropDownBars(
            XSharpLanguageService lang,
            IVsTextView view)
            : base(lang)
        {
        }
        public override int OnItemChosen(int combo, int entry)
        {
            bool handled = false;
            if (combo == 1 && entry < _members?.Count && entry >= 0)
            {
                var member = _members[entry] as XDropDownMember;
                if (member.Element.File != _file)
                { 
                    member.Element.OpenEditor();
                    handled = true;
                }
            }
            if (! handled)
                return base.OnItemChosen(combo, entry);
            return 0;
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
            // when the line did not change, we do nothing for performance reasons
            // this speeds up typing  for buffers with lots of entities
            if (line == lastLine)
                return false;
            lastLine = line;

            Source src = languageService.GetSource(textView);
            string srcFile = src.GetFilePath();
            //
            XFile file = XSolution.FindFullPath(srcFile);
            if (file == null || file.TypeList == null)
            {
                return false;
            }
            XElement selectedElement = file.FindMemberAtRow(line);
            if (selectedElement == _lastSelected)
            {
                return false;
            }
            if (file.ContentHashCode == _lastHashCode)
            {
                // no need to rebuild the list. Just focus to another element
                // locate item in members collection
                selectedMember = 0;
                selectedType = 0;
                _lastSelected = selectedElement;
                bool selectedIsType = false;
                for (int i = 0; i < dropDownMembers.Count; i++)
                {
                    var member = (XDropDownMember) dropDownMembers[i];
                    if (member.Element.Prototype == selectedElement.Prototype)
                    {
                        selectedMember = i;
                        selectedIsType = (i == 0 && member.Label.StartsWith("("));
                        break;
                    }
                }
                for (int i = 0; i < dropDownTypes.Count; i++)
                {
                    var member = (XDropDownMember)dropDownTypes[i];
                    if (selectedIsType && member.Element.Name == selectedElement.Name)
                    {
                        selectedType = i;
                        break;
                    }
                    else
                    if (member.Element.Name == selectedElement.Parent.Name)
                    {
                        selectedType = i;
                        break;
                    }
                }
                return true;
            }
            var sortItems = optionsPage.SortNavigationBars;
            var includeFields = optionsPage.IncludeFieldsInNavigationBars;
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
            XDropDownMember elt;
            // C# shows all items PLAIN
            // but when the selection is not really on an item, but for example on a comment
            // between methods, or on the comments between the namespace and the first class
            // then the next method/class is selected and displayed in GRAY
            // C# also includes members (for partial classes) that are defined in other source files
            // these are colored GRAY
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
                if (eltType == currentType)
                {
                    if (currentType.Range.EndLine >= line + 1 || currentType.Range.StartLine <= line + 1)
                        ft = DROPDOWNFONTATTR.FONTATTR_PLAIN;
                    else
                        ft = DROPDOWNFONTATTR.FONTATTR_GRAY;
                }
                else
                {
                    ft = DROPDOWNFONTATTR.FONTATTR_PLAIN;
                }
                string name = eltType.Name ;
                if (string.IsNullOrEmpty(name))
                {
                    name = "?";
                }
                elt = new XDropDownMember(name, sp, eltType.Glyph, ft);
                elt.Element = eltType;
                nSelect = dropDownTypes.Add(elt);
                if (eltType?.FullName == currentType?.FullName)
                {
                    nSelType = nSelect;
                }
            }

            if (currentType == null)
            { 
                currentType = typeGlobal;
            }
            bool hasPartial = false;
            if (currentType != null)    // should not happen since all files have a global type
            {
                nSelMbr = 0;
                IList<XTypeMember> members;
                if (currentType != typeGlobal && currentType.IsPartial)
                {
                    // retrieve members from other files ?
                    var fullType = file.Project.LookupFullName(currentType.FullName, true);
                    hasPartial = true;
                    members = fullType.Members;
                }
                else
                {
                    members = currentType.Members;
                }
                if (sortItems)
                {
                    members = members.OrderBy(x => x.Name).ToList();
                }
                // Add member for class declaration
                TextSpan spM = this.TextRangeToTextSpan(currentType.Range);
                ft = DROPDOWNFONTATTR.FONTATTR_PLAIN;
                if (currentType != typeGlobal)
                {
                    elt = new XDropDownMember("("+currentType.Name+")", spM, currentType.Glyph, ft);
                    elt.Element = currentType;
                    dropDownMembers.Add(elt);
                }
                if (currentMember == null)
                { 
                    currentMember = currentType.Members.FirstOrDefault();
                }
                foreach (XTypeMember member in members)
                {
                    if (includeFields || member.Kind != Kind.Field)
                    {
                        spM = this.TextRangeToTextSpan(member.Range);

                        if (member == currentMember)
                        {
                            if (member.Range.EndLine >= line+1 &&  member.Range.StartLine <= line+1)
                                ft = DROPDOWNFONTATTR.FONTATTR_PLAIN;
                            else
                                ft = DROPDOWNFONTATTR.FONTATTR_GRAY;
                        }
                        else
                        {
                            if (hasPartial)
                            { 
                                if (member.File == file)
                                    ft = DROPDOWNFONTATTR.FONTATTR_PLAIN;
                                else
                                    ft = DROPDOWNFONTATTR.FONTATTR_GRAY;
                            }
                        }
                        string prototype = member.Prototype;
                        if (prototype.Length > 80)
                        {
                            prototype = prototype.Substring(0, 80) + "...";
                        }
                        elt = new XDropDownMember(prototype, spM, member.Glyph, ft);
                        nSelect = dropDownMembers.Add(elt);
                        elt.Element = member;
                        if (member == currentMember)
                        {
                            nSelMbr = nSelect;
                        }
                    }
                }
            }
            _members = dropDownMembers;
            _file = file;
            _lastSelected = selectedElement;
            _lastHashCode = file.ContentHashCode;
            selectedType    = nSelType;
            selectedMember  = nSelMbr;
            return true;
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
    internal class XDropDownMember : DropDownMember
    {
            internal XDropDownMember(string label, TextSpan span, int glyph, DROPDOWNFONTATTR fontAttribute) :
            base(label, span, glyph, fontAttribute)
        {

        }
        internal XElement Element { get; set; }
    }
}
