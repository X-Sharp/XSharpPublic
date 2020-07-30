using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharp.Build
{
    internal class BuildPropertyCollection : Dictionary<string, object>
    {

        internal BuildPropertyCollection() : base(StringComparer.OrdinalIgnoreCase)
        {

        }

        public new object this[string key]
        {
            get
            {
                if (ContainsKey(key))
                    return base[key];
                return null;
            }
            set
            {
                base[key] = value;
            }
        }

    }
}
