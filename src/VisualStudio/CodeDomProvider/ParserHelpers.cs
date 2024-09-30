using System;
using System.CodeDom;
using System.Collections;
using System.Collections.Generic;
using System.Drawing;

namespace XSharp.CodeDom
{
    internal static class Helpers
    {
        public static Point GetDesignerData(this CodeObject e)
        {
            foreach (DictionaryEntry obj in e.UserData)
            {
                if (obj.Key is System.Type type && type.Name == "CodeDomDesignerData")
                {
                    dynamic designerData = obj.Value;
                    var caretPos = designerData.CaretPosition;
                    return caretPos;
                }
            }
            return new Point(-1, -1);
        }
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

                var caretPos = member.GetDesignerData();
                int line = caretPos.Y;
                int col = caretPos.X;
                if (line == -1)
                {
                    // New Member
                    if (member is CodeMemberField field)
                    {
                        newfields.Add(field);
                    }
                    else
                    {
                        line = col = 999_999_999;
                    }
                }
                if (line != -1)
                {
                    var key = line.ToString("D10") + col.ToString("D10") + i.ToString("D10");
                    items.Add(key, member);
                }
            }
            if (!hasfields)
            {
                foreach (var field in newfields)
                {
                    result.Add(field);
                }
                newfields.Clear();
            }
            foreach (System.Collections.DictionaryEntry item in items)
            {
                if (!(item.Value is CodeMemberField))
                {
                    // Insert new fields before the methods in the source file.
                    if (newfields.Count > 0)
                    {
                        result.AddRange(newfields.ToArray());
                        newfields.Clear();
                    }
                }
                result.Add((CodeTypeMember)item.Value);
            }
            return result;
        }
    }
}
