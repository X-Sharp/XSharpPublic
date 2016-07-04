/*
   Copyright 2016 XSharp B.V.

Licensed under the X# compiler source code License, Version 1.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.xsharp.info/licenses

Unless required by applicable law or agreed to in writing, software
Distributed under the License is distributed on an "as is" basis,
without warranties or conditions of any kind, either express or implied.
See the License for the specific language governing permissions and   
limitations under the License.
*/
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
        public bool VOCompatibleArtithmeticConversions { get; private set; }
        public bool VOCompatibleIIF { get; private set; }
        public bool VOFloatConstants { get; private set; }
        public bool VOImplicitCasts { get; private set; }
        public bool VOSignedUnsignedConversion { get; private set; }
        public bool VOStringComparisons { get; private set; }
        public bool VOUntypedAllowed { get; private set; }
        public XSharpDialect Dialect { get; private set; }
        public bool LateBinding { get; private set; }
        public bool HasDefaultTree { get; set; } = false;
        public bool CreatingRuntime { get; private set; }

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
                VOCompatibleArtithmeticConversions = opt.Vo11;
                VOCompatibleIIF = opt.Vo10;
                VOFloatConstants = opt.Vo14;
                VOImplicitCasts = opt.Vo7;
                VOSignedUnsignedConversion = opt.Vo4;
                VOStringComparisons = opt.Vo13;
                VOUntypedAllowed = opt.Vo15;
                LateBinding = opt.LateBinding;
                CreatingRuntime = opt.CreatingRuntime;
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
            VOCompatibleArtithmeticConversions = opt.VOCompatibleArtithmeticConversions;
            VOCompatibleIIF = opt.VOCompatibleIIF;
            VOFloatConstants = opt.VOFloatConstants;
            VOImplicitCasts = opt.VOImplicitCasts;
            VOSignedUnsignedConversion = opt.VOSignedUnsignedConversion;
            VOStringComparisons = opt.VOStringComparisons;
            VOUntypedAllowed = opt.VOUntypedAllowed;
            LateBinding = opt.LateBinding;
            CreatingRuntime = opt.CreatingRuntime;
        }
    }
}