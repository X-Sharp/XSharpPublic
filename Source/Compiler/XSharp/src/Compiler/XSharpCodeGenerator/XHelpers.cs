//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System.CodeDom;
using System.Collections.Generic;


namespace XSharp.CodeDom
{
    internal static class Helpers
    {
        /// <summary>
        /// Sort members on the line/column in which they are declared
        /// New members (without line/column) are sorted at the end of the list
        /// </summary>
        /// <param name="members"></param>
        /// <returns></returns>
        internal static CodeTypeMemberCollection SortMembers(CodeTypeMemberCollection members)
        {
            CodeTypeMemberCollection result = new CodeTypeMemberCollection();
            var items = new System.Collections.SortedList();
            var processed = new List<string>();
            var newfields = new List<CodeMemberField>();
            var hasfields = false;
            for (int i = 0; i < members.Count; i++)
            {
                var member = members[i];
                if (member is CodeMemberField && member.HasSourceCode())
                {
                    hasfields = true;
                }
                // HACK: prevent duplicate items: there is an error in their code
                // or our code that adds duplicates. This 
                if (member is IXCodeObject)
                {
                    var source = member.GetSourceCode().ToLower().Trim();
                    if (processed.Contains(source))
                        continue;
                    processed.Add(source);
                }
                else
                {
                    if (member is CodeMemberField field)
                    {
                        newfields.Add(field);
                    }
                }
                int line, col;
                var data = member.GetDesignerData();
                if (data != null)
                {
                    line = data.CaretPosition.Y;
                    col = data.CaretPosition.X;
                }
                else if (member is CodeMemberField field)
                {
                    line = col = -1;
                }
                else
                {
                    line = col = 999_999_999;
                }
                if (line != -1)
                {
                    var key = line.ToString("D10") + col.ToString("D10") + i.ToString("D10");
                    items.Add(key, member);
                }
            }
            var fieldStart = false;
            if (!hasfields)
            {
                foreach (var field in newfields)
                {
                    result.Add(field);
                }
            }
            foreach (System.Collections.DictionaryEntry item in items)
            {
                if (item.Value is CodeMemberField)
                {
                    if (!fieldStart)
                    {
                        fieldStart = true;
                    }
                }
                else
                {
                    if (fieldStart)
                    {
                        foreach (var field in newfields)
                        {
                            result.Add(field);
                        }
                        newfields.Clear();
                    }
                }
                result.Add((CodeTypeMember)item.Value);
            }
            return result;
        }
    }
    public class CodeDomDesignerData
    {
        public virtual System.Drawing.Point CaretPosition { get; set; }
        public virtual string FileName { get; set; }

    }

}
