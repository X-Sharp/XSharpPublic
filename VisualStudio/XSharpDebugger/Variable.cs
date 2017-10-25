// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace XSharpDebugger
{
    /// <summary>
    /// This class represents a 'variable' in the Iris language.  A variable consists of a name
    /// and a type.  If the type is an array, the variable also tracks the subrange of the array.
    /// </summary>
    public sealed class Variable
    {
        public readonly string Name;
        public readonly XSharpType Type;
        public readonly SubRange SubRange; // Only used for arrays
        public bool In { get; set; }
        public bool Out { get; set; }
        public Variable(XSharpType type, string name)
        {
            Type = type;
            Name = name;
        }

        public Variable(XSharpType type, string name, SubRange subRange) : this(type, name)
        {
            SubRange = subRange;
        }

        public override string ToString()
        {
            string subRangeStr = string.Empty;
            if (SubRange != null)
                subRangeStr = "[" + SubRange.ToString() + "]";

            return string.Format("{0}{1} AS {2}", Name, subRangeStr, Type);
        }
    }
}
