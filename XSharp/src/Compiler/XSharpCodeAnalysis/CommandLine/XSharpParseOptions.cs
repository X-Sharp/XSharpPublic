using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp
{
    public sealed partial class CSharpParseOptions
    { 
        public bool VirtualInstanceMethods { get; private set; }

        public void SetXSharpSpecificOptions(XSharpSpecificCompilationOptions opt)
        {
            if (opt != null)
            {
                VirtualInstanceMethods = opt.Vo3;
            }
        }

        public void SetXSharpSpecificOptions(CSharpParseOptions opt)
        {
            VirtualInstanceMethods = opt.VirtualInstanceMethods;
        }
    }
}