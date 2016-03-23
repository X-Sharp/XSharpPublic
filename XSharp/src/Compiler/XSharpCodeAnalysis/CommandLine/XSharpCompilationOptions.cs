using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Roslyn.Utilities;
using System.Diagnostics;

namespace Microsoft.CodeAnalysis.CSharp
{
    public sealed partial class CSharpCompilationOptions
    {
        public bool ArrayZero { get; private set; }

        public bool InitStringVarsToEmpty { get; private set; }

        public void SetXSharpSpecificOptions(XSharpSpecificCompilationOptions opt)
        {
            if (opt != null)
            {
                ArrayZero = opt.ArrayZero;
                InitStringVarsToEmpty = opt.Vo2;
            }
        }

        public void SetOptions(CSharpCommandLineArguments opt)
        {

        }

        public void SetXSharpSpecificOptions(CSharpCompilationOptions opt)
        {
            ArrayZero = opt.ArrayZero;
            InitStringVarsToEmpty = opt.InitStringVarsToEmpty;
        }
    }
}