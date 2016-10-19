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
        public bool VONullStrings { get; private set; }
        //public bool VirtualInstanceMethods { get; private set; }
        //public bool VOAllowMissingReturns { get; private set; }
        public bool VOArithmeticConversions { get; private set; }
        //public bool VOClipperCallingConvention { get; private set; }
        //public bool VOClipperIntegerDivisions { get; private set; }
        //public bool VOCompatibleIIF { get; private set; }
        //public bool VOFloatConstants { get; private set; }
        public bool VOImplicitCastsAndConversions { get; private set; }
        public bool VOImplicitSignedUnsignedConversions { get; private set; }
        //public bool VOInitializeVariables { get; private set; }
        //public bool VoInitAxitMethods { get; private set; }
        //public bool VOPreprocessorBehaviour { get; private set; }
        public bool VOResolveTypedFunctionPointersToPtr { get; private set; }
        public bool VOSignedUnsignedConversion { get; private set; }
        public bool VOStringComparisons { get; private set; }
        //public bool VOUntypedAllowed { get; private set; }
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
                //VoInitAxitMethods = opt.Vo1;
                VONullStrings = opt.Vo2;
                //VirtualInstanceMethods = opt.Vo3;
                VOSignedUnsignedConversion = opt.Vo4;
                //VOClipperCallingConvention = opt.Vo5;
                VOResolveTypedFunctionPointersToPtr = opt.Vo6;
                VOImplicitCastsAndConversions = opt.Vo7;
                //VOPreprocessorBehaviour = opt.Vo8;
                //VOAllowMissingReturns = opt.Vo9;
                //VOCompatibleIIF = opt.Vo10;
                VOArithmeticConversions = opt.Vo11;
                //VOClipperIntegerDivisions = opt.Vo12;
                VOStringComparisons = opt.Vo13;
                //VOFloatConstants = opt.Vo14;
                //VOUntypedAllowed = opt.Vo15;
                //VOInitializeVariables = opt.Vo16;
                Dialect = opt.Dialect;
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
            DefaultIncludeDir = opt.DefaultIncludeDir;
            WindowsDir = opt.WindowsDir;
            SystemDir = opt.SystemDir;
            Dialect = opt.Dialect;
            LateBinding = opt.LateBinding;
            CreatingRuntime = opt.CreatingRuntime;

            //VoInitAxitMethods = opt.VoInitAxitMethods; // vo1
            VONullStrings = opt.VONullStrings; // vo2
            //VirtualInstanceMethods = opt.VirtualInstanceMethods; // vo3
            VOSignedUnsignedConversion = opt.VOSignedUnsignedConversion; // vo4
            //VOClipperCallingConvention = opt.VOClipperCallingConvention;  // vo5
            VOResolveTypedFunctionPointersToPtr = opt.VOResolveTypedFunctionPointersToPtr; // vo6
            VOImplicitCastsAndConversions = opt.VOImplicitCastsAndConversions; // vo7
            //VOPreprocessorBehaviour = opt.VOPreprocessorBehaviour; // vo8
            //VOAllowMissingReturns = opt.VOAllowMissingReturns; // vo9
            //VOCompatibleIIF = opt.VOCompatibleIIF; // vo10
            VOArithmeticConversions = opt.VOArithmeticConversions; // vo11
            //VOClipperIntegerDivisions = opt.VOClipperIntegerDivisions; // vo12
            VOStringComparisons = opt.VOStringComparisons; // vo13
            //VOFloatConstants = opt.VOFloatConstants; // vo14
            //VOUntypedAllowed = opt.VOUntypedAllowed; // vo15
            //VOInitializeVariables = opt.VOInitializeVariables; // vo16
        }
    }
}