using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using XSharpModel;

namespace XSharp.LanguageService
{
    internal enum LineFlags
    {
        None = 0,
        Continued = 1 << 0,
        EntityStart = 1 << 1,
        SingleLineComments = 1 << 2,
        MultiLineComments = 1 << 3,
        DocComments = 1 << 4,
        Inactive = 1 << 5,
        SingleLineEntity = 1 << 6,
    }
    internal class XSharpLineState
    {
        readonly private Dictionary<int, LineFlags> dict;
        internal XSharpLineState()
        {
            dict = new Dictionary<int, LineFlags>();
        }
        internal void SetFlags(int line, LineFlags flags)
        {
            if (dict.ContainsKey(line))
            {
                dict[line] |= flags;
            }
            else
            {
                dict.Add(line, flags);
            }
        }
        internal void RemoveFlags(int line, LineFlags flags)
        {
            if (dict.ContainsKey(line))
            {
                dict[line] &= ~flags;
            }
        }
        internal void Clear()
        {
            dict.Clear();
        }
        internal LineFlags GetFlags(int line)
        {
            if (dict.TryGetValue(line, out var flags))
            {
                return flags;
            }
            return LineFlags.None;
        }
    }
}
