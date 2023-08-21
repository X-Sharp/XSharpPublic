using System;
using Microsoft.VisualStudio.Text;

namespace XSharp.LanguageService
{
    [Flags]
    internal enum LineFlags
    {
        None = 0,
        /// <summary>
        /// This line is a continuation of the previous line
        /// </summary>
        IsContinued = 1 << 0,
        /// <summary>
        /// This line is the start of an entity
        /// </summary>
        EntityStart = 1 << 1,
        /// <summary>
        /// This line is a single line comment
        /// </summary>
        SingleLineComments = 1 << 2,
        /// <summary>
        ///  This line is a multi line comment
        /// </summary>
        MultiLineComments = 1 << 3,
        /// <summary>
        ///  This line is a documentation comment
        /// </summary>
        DocComments = 1 << 4,
        /// <summary>
        /// This line is in an inactive preprocessor region
        /// </summary>
        Inactive = 1 << 5,
        /// <summary>
        /// This line is a single line entity
        /// </summary>
        SingleLineEntity = 1 << 6,
        /// <summary>
        /// This line contains a preprocessor instruction
        /// </summary>
        Preprocessor = 1 << 7,
        /// <summary>
        /// This line starts with an attribute
        /// </summary>
        StartsWithAttribute = 1 << 8,
    }
    /// <summary>
    /// This class keeps the state for 0 based line numbers
    /// </summary>
    internal class XSharpLineState: XSharpLineInfo<LineFlags>
    {
        
        internal XSharpLineState() : base()
        {
            
        }
        internal void Clear()
        {
            lock (dict)
            {
                dict.Clear();
            }
        }
        internal void SetFlags(int line, LineFlags flags)
        {
            lock (dict)
            {
                Get(line, out var oldFlags);
                Set(line, oldFlags | flags);
            }
        }

        internal bool IsComment(int line)
        {
            Get(line, out var flags);
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
                    this.Get(line, out var oldflags);
                    Set(line, oldflags &= ~flags);
                }
            }
        }

    }

   

   
}
