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

        public string DefaultIncludeDir { get; set; }
        public string WindowsDir { get; set; }
        public string SystemDir { get; set; }
        public bool InitStringVarsToEmpty { get; private set; }
        public bool VOClipperCallingConvention { get; private set; }
        public bool VOCompatibleIIF { get; private set; }
        public bool VOFloatConstants { get; private set; }
        public bool VOUntypedAllowed { get; private set; }
        public XSharpDialect Dialect { get; private set; }

        public bool IsDialectVO { get { return this.Dialect == XSharpDialect.VO || this.Dialect == XSharpDialect.Vulcan; } }
        public bool SupportsMemvars { get { return this.Dialect != XSharpDialect.Vulcan; } }
        public void SetXSharpSpecificOptions(XSharpSpecificCompilationOptions opt)
        {
            if (opt != null)
            {
                ArrayZero = opt.ArrayZero;
                InitStringVarsToEmpty = opt.Vo2;
                Dialect = opt.Dialect;
                VOClipperCallingConvention = opt.Vo5;
                VOCompatibleIIF = opt.Vo10;
                VOFloatConstants = opt.Vo14;
                VOUntypedAllowed = opt.Vo15;
            }
        }

        public void SetOptions(CSharpCommandLineArguments opt)
        {
        }

        public void SetXSharpSpecificOptions(CSharpCompilationOptions opt)
        {
            ArrayZero = opt.ArrayZero;
            InitStringVarsToEmpty = opt.InitStringVarsToEmpty;
            DefaultIncludeDir = opt.DefaultIncludeDir;
            WindowsDir = opt.WindowsDir;
            SystemDir = opt.SystemDir;
            Dialect = opt.Dialect;
            VOUntypedAllowed = opt.VOUntypedAllowed;
            VOCompatibleIIF = opt.VOCompatibleIIF;
            VOFloatConstants = opt.VOFloatConstants;
        }
    }
}