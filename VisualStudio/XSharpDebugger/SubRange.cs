// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace XSharpDebugger
{
    /// <summary>
    /// A subrange consisting of 'from' and 'to' values.  Iris only uses subranges for arrays.
    /// </summary>
    public class SubRange
    {
        public readonly int From;
        public readonly int To;

        public SubRange(int from, int to)
        {
            From = from;
            To = to;
        }

        public override string ToString()
        {
            return string.Format("{0}..{1}", From, To);
        }
    }
}
