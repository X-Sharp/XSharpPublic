using System;
using Microsoft.VisualStudio.Text;

namespace XSharp.LanguageService
{
    [Flags]
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
    /// <summary>
    /// This class keeps the state for 0 based line numbers
    /// </summary>
    internal class XSharpLineState: XSharpLineInfo<LineFlags>
    {
        
        internal XSharpLineState(ITextSnapshot snapshot) : base(snapshot)
        {
            
        }
        internal void SetFlags(int line, LineFlags flags)
        {
            lock (dict)
            {
                Set(line, Get(line) | flags);
            }
        }

        internal bool IsComment(int line)
        {
            var flags = Get(line);
            return flags.HasFlag(LineFlags.SingleLineComments) ||
                flags.HasFlag(LineFlags.MultiLineComments) ||
                flags.HasFlag(LineFlags.DocComments);
        }
        internal void RemoveFlags(int line, LineFlags flags)
        {
            lock (dict)
            {
                if (dict.ContainsKey(line))
                {
                    var oldflags = this.Get(line);
                    Set(line, oldflags &= ~flags);
                }
            }
        }

    }

   

   
}
