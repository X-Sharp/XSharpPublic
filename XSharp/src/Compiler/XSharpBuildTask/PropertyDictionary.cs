using System;
using System.Collections.Generic;

namespace XSharp.Build
{
    internal class PropertyDictionary : Dictionary<string, object>
    {

        internal PropertyDictionary() : base(StringComparer.OrdinalIgnoreCase)
        {

        }
        public T GetOrDefault<T>(string name, T @default)
        {
            object value;
            if (this.TryGetValue(name, out value))
            {
                return (T)value;
            }
            return @default;
        }

        public new object this[string name]
        {
            get
            {
                object value;
                return this.TryGetValue(name, out value)
                    ? value : null;
            }
            set { base[name] = value; }
        }
    }
}
