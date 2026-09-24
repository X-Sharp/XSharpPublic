using System;
using System.Collections.Generic;
using System.Globalization;

using Microsoft.VisualStudio.Shell.Interop;

namespace Microsoft.VisualStudio.Project
{
    /// <summary>
    /// Debugging helper that translates a numeric VSHPROPID value into the name of the
    /// corresponding member of the __VSHPROPID .. __VSHPROPID11 enumerations.
    /// </summary>
    internal static class VsHierarchyPropIdNames
    {
        /// <summary>
        /// The property id enumerations, in ascending order of the values they define.
        /// </summary>
        private static readonly Type[] PropIdEnums = new Type[]
        {
            typeof(__VSHPROPID),
            typeof(__VSHPROPID2),
            typeof(__VSHPROPID3),
            typeof(__VSHPROPID4),
            typeof(__VSHPROPID5),
            typeof(__VSHPROPID6),
            typeof(__VSHPROPID7),
            typeof(__VSHPROPID8),
            typeof(__VSHPROPID9),
            typeof(__VSHPROPID10),
            typeof(__VSHPROPID11),
            typeof(__VSSPROPID),
            typeof(__VSSPROPID2),
            typeof(__VSSPROPID3),
            typeof(__VSSPROPID4),
            typeof(__VSSPROPID5),
            typeof(__VSSPROPID6),
            typeof(__VSSPROPID7),
            typeof(__VSSPROPID8),
            typeof(__VSSPROPID9),
            typeof(__VSSPROPID10),
            typeof(__VSSPROPID11),
            typeof(__VSSPROPID12),
            typeof(__VSSPROPID13),
            typeof(__VSSPROPID14),
        };

        /// <summary>
        /// Cache of property id to name, so repeated lookups while stepping in the debugger are cheap.
        /// </summary>
        private static readonly Dictionary<int, string> NameCache = new Dictionary<int, string>();

        private static readonly object SyncRoot = new object();

        /// <summary>
        /// Returns the name of the __VSHPROPIDxx member matching <paramref name="propId"/>,
        /// or a "&lt;unknown&gt;" placeholder including the numeric value when there is no match.
        /// </summary>
        public static string GetName(int propId)
        {
            string name;

            lock (SyncRoot)
            {
                if (NameCache.TryGetValue(propId, out name))
                {
                    return name;
                }
            }

            name = Lookup(propId);

            lock (SyncRoot)
            {
                NameCache[propId] = name;
            }

            return name;
        }

        /// <summary>
        /// Convenience overload for the enum type most call sites already have in hand.
        /// </summary>
        public static string GetName(__VSHPROPID propId)
        {
            return GetName((int)propId);
        }

        private static string Lookup(int propId)
        {
            foreach (Type enumType in PropIdEnums)
            {
                // All the __VSHPROPIDxx enums have an underlying type of int.
                if (Enum.IsDefined(enumType, propId))
                {
                    string name = Enum.GetName(enumType, propId);

                    if (!String.IsNullOrEmpty(name))
                    {
                        return String.Concat(enumType.Name, ".", name);
                    }
                }
            }

            return String.Format(CultureInfo.InvariantCulture, "<unknown VSHPROPID> ({0})", propId);
        }
    }
}
