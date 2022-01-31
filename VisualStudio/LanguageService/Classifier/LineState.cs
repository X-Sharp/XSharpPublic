﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using XSharpModel;
using System.Collections.Immutable;
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
    internal class XSharpLineState
    {
        private Dictionary<int, LineFlags> dict;
        internal ITextSnapshot Snapshot { get; set; }
        internal XSharpLineState(ITextSnapshot snapshot )
        {
            dict = new Dictionary<int, LineFlags>();
            Snapshot = snapshot;
        }
        internal void SetFlags(int line, LineFlags flags)
        {
            lock (dict)
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
        }
        internal bool IsComment(int line)
        {
            var flags = GetFlags(line);
            return flags.HasFlag(LineFlags.SingleLineComments) ||
                flags.HasFlag(LineFlags.MultiLineComments) ||
                flags.HasFlag(LineFlags.DocComments);
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
            dict = new Dictionary<int, LineFlags>();
        }
        internal LineFlags GetFlags(int line)
        {
            if (dict.TryGetValue(line, out var flags))
            {
                return flags;
            }
            return LineFlags.None;
        }
        /// <summary>
        /// Return a clone of the Lines array.
        /// </summary>
        internal IDictionary<int, LineFlags> Lines
        {
            get
            {
                var result = new Dictionary<int, LineFlags>();
                lock (dict)
                {
                    foreach (var item in dict)
                    {
                        result.Add(item.Key, item.Value);
                    }
                }
                return result;
            }
        }
    }
}
