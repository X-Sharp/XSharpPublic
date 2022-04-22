using System.Collections.Generic;
using System.Linq;
using Microsoft.VisualStudio.Text;
using System.Collections.Concurrent;

namespace XSharp.LanguageService
{
    internal class XSharpLineInfo<T> where T : struct
    {
        protected ConcurrentDictionary<int, T> dict;
        internal ITextSnapshot Snapshot { get; private set; }
        internal XSharpLineInfo(ITextSnapshot snapshot)
        {
            dict = new ConcurrentDictionary<int, T>();
            Snapshot = snapshot;
        }
       
        internal void Clear()
        {
            lock (dict)
            {
                dict.Clear();
            }
        }
        internal T Get(int line)
        {
            lock (dict)
            {
                if (dict.TryGetValue(line, out var flags))
                {
                    return flags;
                }
            }
            return default;
        }
        internal void Set(int line, T value)
        {
            lock (dict)
            {
                dict[line] = value;
            }
            return;
        }
        /// <summary>
        /// Return a clone of the Lines array.
        /// </summary>
        internal IDictionary<int, T> Lines
        {
            get
            {
                lock (dict)
                {
                    return dict.ToDictionary(entry => entry.Key, entry => entry.Value);
                }
            }
        }
    }
}
