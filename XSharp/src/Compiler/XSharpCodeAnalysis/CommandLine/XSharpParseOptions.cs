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

        public string DefaultNamespace { get; private set; }

        public ImmutableArray<string> IncludePaths { get; private set; } = ImmutableArray.Create<string>();

        public void SetXSharpSpecificOptions(XSharpSpecificCompilationOptions opt)
        {
            if (opt != null)
            {
                VirtualInstanceMethods = opt.Vo3;
                DefaultNamespace = opt.NameSpace;
                IncludePaths = opt.IncludePaths.Split(new[] { ';' }, StringSplitOptions.RemoveEmptyEntries).ToImmutableArray();
            }
        }

        public void SetXSharpSpecificOptions(CSharpParseOptions opt)
        {
            VirtualInstanceMethods = opt.VirtualInstanceMethods;
            DefaultNamespace = opt.DefaultNamespace;
            IncludePaths = opt.IncludePaths;
        }
    }
}