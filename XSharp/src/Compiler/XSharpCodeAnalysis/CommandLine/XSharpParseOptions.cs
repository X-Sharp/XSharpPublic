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
using System.Diagnostics;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp
{

    [Flags]
    public enum VulcanAssemblies : byte
    {
        None = 0,
        VulcanRT = 1,
        VulcanRTFuncs = 2
    }


    public sealed partial class CSharpParseOptions
    {

        // Options that can be set from the outside
        // Some options are also handled by the parser
        // Other options have flags, for the preprocessor macros, such as __VO1__

        public bool ArrayZero { get; private set; }

        public bool DebugEnabled { get; private set; }
        public XSharpDialect Dialect { get; private set; }
        public string DefaultIncludeDir { get; private set; }
        public string WindowsDir { get; private set; }
        public string SystemDir { get; private set; }
        public bool NoStdDef { get; private set; }
        public bool ShowIncludes { get; private set; }
        public bool PreprocessorOutput { get; private set; }
        public bool Verbose { get; private set; }
        public bool VirtualInstanceMethods { get; private set; }
        public bool VOAllowMissingReturns { get; private set; }
        public bool VOArithmeticConversions { get; private set; }
        public bool VOClipperIntegerDivisions { get; private set; }

        public bool VOFloatConstants { get; private set; }
        public bool VoInitAxitMethods { get; private set; }
        public bool VONullStrings { get; private set; }
        public bool VOClipperCallingConvention { get; private set; }
        public bool VOCompatibleIIF { get; private set; }
        public bool VOImplicitCastsAndConversions { get; private set; }
        public bool VOInitializeVariables { get; private set; } = false;
        public bool VOPreprocessorBehaviour { get; private set; }
        public bool VOResolveTypedFunctionPointersToPtr { get; private set; }
        public bool VOSignedUnsignedConversion { get; private set; }
        public bool VOStringComparisons { get; private set; }
        public string DefaultNamespace { get; private set; }
        public bool IsDialectVO { get { return this.Dialect == XSharpDialect.VO || this.Dialect == XSharpDialect.Vulcan; } }
        public bool SupportsMemvars { get { return this.Dialect != XSharpDialect.Vulcan; } }
        public ImmutableArray<string> IncludePaths { get; private set; } = ImmutableArray.Create<string>();
        public bool VulcanRTFuncsIncluded { get; private set; } = false;
        public bool VulcanRTIncluded { get; private set; } = false;
        public bool VOUntypedAllowed { get; private set; } = true;
        public VulcanAssemblies VulcanAssemblies { get; private set; } = VulcanAssemblies.None;
        public bool Overflow { get; private set; }
        public CSharpCommandLineArguments CommandLineArguments { get; private set; }

        public void SetXSharpSpecificOptions(XSharpSpecificCompilationOptions opt)
        {
            if (opt != null)
            {
                ArrayZero = opt.ArrayZero;
                Dialect = opt.Dialect;
                DefaultNamespace = opt.NameSpace;
                DefaultIncludeDir = opt.DefaultIncludeDir;
                WindowsDir = opt.WindowsDir;
                SystemDir = opt.SystemDir;
                NoStdDef = opt.NoStdDef;
                ShowIncludes = opt.ShowIncludes;
                Verbose = opt.Verbose;
                PreprocessorOutput = opt.PreProcessorOutput;
                IncludePaths = opt.IncludePaths.Split(new[] { ';' }, StringSplitOptions.RemoveEmptyEntries).ToImmutableArray();
                VulcanRTFuncsIncluded = opt.VulcanRTFuncsIncluded;
                VulcanRTIncluded = opt.VulcanRTIncluded;

                VoInitAxitMethods = opt.Vo1;
                VONullStrings = opt.Vo2;
                VirtualInstanceMethods = opt.Vo3;
                VOSignedUnsignedConversion = opt.Vo4;
                VOClipperCallingConvention = opt.Vo5;
                VOResolveTypedFunctionPointersToPtr = opt.Vo6;
                VOImplicitCastsAndConversions = opt.Vo7;
                VOPreprocessorBehaviour = opt.Vo8;
                VOAllowMissingReturns = opt.Vo9;
                VOCompatibleIIF = opt.Vo10;
                VOArithmeticConversions = opt.Vo11;
                VOClipperIntegerDivisions = opt.Vo12;
                VOStringComparisons = opt.Vo13;
                VOFloatConstants = opt.Vo14;
                VOUntypedAllowed = opt.Vo15;
                //VOInitializeVariables = opt.Vo16;
                VulcanAssemblies = opt.VulcanAssemblies;
                Overflow = opt.Overflow;

            }
        }

        public void SetOptions(CSharpCommandLineArguments opt)
        {
            if (opt != null)
            {
                DebugEnabled = opt.EmitPdb;
                CommandLineArguments = opt;
            }
        }

        public void SetXSharpSpecificOptions(CSharpParseOptions opt)
        {
            ArrayZero = opt.ArrayZero;
            DebugEnabled = opt.DebugEnabled;
            DefaultIncludeDir = opt.DefaultIncludeDir;
            WindowsDir = opt.WindowsDir;
            SystemDir = opt.SystemDir;
            DefaultNamespace = opt.DefaultNamespace;
            IncludePaths = opt.IncludePaths;
            ShowIncludes = opt.ShowIncludes;
            NoStdDef = opt.NoStdDef;
            PreprocessorOutput = opt.PreprocessorOutput;
            Verbose = opt.Verbose;

            VoInitAxitMethods = opt.VoInitAxitMethods; // vo1
            VONullStrings = opt.VONullStrings; // vo2
            VirtualInstanceMethods = opt.VirtualInstanceMethods; // vo3
            VOSignedUnsignedConversion = opt.VOSignedUnsignedConversion; // vo4
            VOClipperCallingConvention = opt.VOClipperCallingConvention;  // vo5
            VOResolveTypedFunctionPointersToPtr = opt.VOResolveTypedFunctionPointersToPtr; // vo6
            VOImplicitCastsAndConversions = opt.VOImplicitCastsAndConversions; // vo7
            VOPreprocessorBehaviour = opt.VOPreprocessorBehaviour; // vo8
            VOAllowMissingReturns = opt.VOAllowMissingReturns; // vo9
            VOCompatibleIIF = opt.VOCompatibleIIF; // vo10
            VOArithmeticConversions = opt.VOArithmeticConversions; // vo11
            VOClipperIntegerDivisions = opt.VOClipperIntegerDivisions; // vo12
            VOStringComparisons = opt.VOStringComparisons; // vo13
            VOFloatConstants = opt.VOFloatConstants; // vo14
            VOUntypedAllowed = opt.VOUntypedAllowed; // vo15
            //VOInitializeVariables = opt.VOInitializeVariables; // vo16
            VulcanAssemblies = opt.VulcanAssemblies;
            Overflow = opt.Overflow;
        }
    }
}