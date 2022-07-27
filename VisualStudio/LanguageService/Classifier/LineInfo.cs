using System.Collections.Generic;
using System.Linq;
using Microsoft.VisualStudio.Text;
using System.Collections.Concurrent;

namespace XSharp.LanguageService
{
    internal class XSharpLineInfo<T> where T : struct
    {
        protected ConcurrentDictionary<int, T> dict;
        internal XSharpLineInfo()
        {
            dict = new ConcurrentDictionary<int, T>();
        }
        internal bool ContainsKey(int line)
        {
            return dict.ContainsKey(line);
        }
        internal bool Get(int line, out T value)
        {
            return dict.TryGetValue(line, out value);
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
