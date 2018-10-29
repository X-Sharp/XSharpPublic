/*
   Copyright 2016-2017 XSharp B.V.

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
using System.Collections.Immutable;
using System.IO;

namespace Microsoft.CodeAnalysis.CSharp
{

    [Flags]
    public enum RuntimeAssemblies : byte
    {
        None = 0,
        VulcanRT = 1,
        VulcanRTFuncs = 2,
        XSharpCore = 8,
        XSharpVO = 16
    }

    [Flags]
    public enum ParseLevel : byte
    {
        Lex = 1,
        Parse = 2,
        SyntaxCheck = 3,
        Complete = 4
    }

    public enum XSharpTargetDLL : Byte
    {
        Other =0,
        Core = 1,
        RDD = 2,
        VO = 3
    }

    public sealed partial class CSharpParseOptions
    {

        // Options that can be set from the outside
        // Some options are also handled by the parser
        // Other options have flags, for the preprocessor macros, such as __VO1__

        public bool ArrayZero { get; private set; }
        public int ClrVersion { get; private set; }
        public bool MacroScript { get; private set; }

        public XSharpTargetDLL TargetDLL { get; private set; }
        public bool DebugEnabled { get; private set; }
        public XSharpDialect Dialect { get; private set; }
        public string DefaultIncludeDir { get; private set; }
        public string WindowsDir { get; private set; }
        public string SystemDir { get; private set; }
        public bool NoStdDef { get; private set; }
        public bool ShowDefs { get; private set; }
        public bool ShowIncludes { get; private set; }
        public bool NoClipCall { get; internal set; } 
        public ParseLevel ParseLevel { get; set; } = ParseLevel.Complete;
        public bool AllowNamedArguments { get; private set; }
        public bool PreprocessorOutput { get; private set; }
        public bool SaveAsCSharp { get; private set; }
        public bool Verbose { get; private set; }
        public bool VirtualInstanceMethods { get; private set; }
        public bool VOAllowMissingReturns { get; private set; }
        public bool VOArithmeticConversions { get; private set; }
        public bool VOClipperIntegerDivisions { get; private set; }
        public bool VOClipperConstructors{ get; private set; }

        public bool VOFloatConstants { get; private set; }
        public bool VoInitAxitMethods { get; private set; }
        public bool VONullStrings { get; private set; }
        public bool VOClipperCallingConvention { get; private set; }
        public bool VOCompatibleIIF { get; private set; }
        public bool VOImplicitCastsAndConversions { get; private set; }
        public bool VOPreprocessorBehaviour { get; private set; }
        public bool VOResolveTypedFunctionPointersToPtr { get; private set; }
        public bool VOSignedUnsignedConversion { get; private set; }
        public bool VOStringComparisons { get; private set; }
        public string DefaultNamespace { get; private set; }
        public bool IsDialectVO { get { return this.Dialect.IsDialectVO(); } }
        public bool SupportsMemvars { get { return this.Dialect.SupportsMemvars(); } }
        public ImmutableArray<string> IncludePaths { get; private set; } = ImmutableArray.Create<string>();
        public bool VulcanRTFuncsIncluded => RuntimeAssemblies.HasFlag(RuntimeAssemblies.VulcanRTFuncs);
        public bool VulcanRTIncluded => RuntimeAssemblies.HasFlag(RuntimeAssemblies.VulcanRT);
        public bool XSharpRuntime => RuntimeAssemblies.HasFlag(RuntimeAssemblies.XSharpVO) |
            RuntimeAssemblies.HasFlag(RuntimeAssemblies.XSharpCore);
        public bool VOUntypedAllowed { get; private set; } = true;
        public RuntimeAssemblies RuntimeAssemblies { get; private set; } = RuntimeAssemblies.None;
        public bool Overflow { get; private set; }
        public CSharpCommandLineArguments CommandLineArguments { get; private set; }
        public TextWriter ConsoleOutput { get; private set; }

        public bool vo1 => VoInitAxitMethods;
        public bool vo2 => VONullStrings;
        public bool vo3 => VirtualInstanceMethods;
        public bool vo4 => VOSignedUnsignedConversion;
        public bool vo5 => VOClipperCallingConvention;
        public bool vo6 => VOResolveTypedFunctionPointersToPtr;
        public bool vo7 => VOImplicitCastsAndConversions;
        public bool vo8 => VOPreprocessorBehaviour;
        public bool vo9 => VOAllowMissingReturns;
        public bool vo10 => VOCompatibleIIF;
        public bool vo11 => VOArithmeticConversions;
        public bool vo12 => VOClipperIntegerDivisions;
        public bool vo13 => VOStringComparisons;
        public bool vo14 => VOFloatConstants;
        public bool vo15 => VOUntypedAllowed;
        public bool vo16 => VOClipperConstructors;
        public void SetXSharpSpecificOptions(XSharpSpecificCompilationOptions opt)
        {
            if (opt != null)
            {
                ArrayZero = opt.ArrayZero;
                ClrVersion = opt.ClrVersion;
                TargetDLL = opt.TargetDLL;
                Dialect = opt.Dialect;
                DefaultNamespace = opt.NameSpace;
                DefaultIncludeDir = opt.DefaultIncludeDir;
                WindowsDir = opt.WindowsDir;
                SystemDir = opt.SystemDir;
                NoStdDef = opt.NoStdDef;
                NoClipCall = opt.NoClipCall;
                ShowDefs = opt.ShowDefs;
                ShowIncludes = opt.ShowIncludes;
                Verbose = opt.Verbose;
                PreprocessorOutput = opt.PreProcessorOutput;
                ParseLevel = opt.ParseLevel;
                IncludePaths = opt.IncludePaths.Split(new[] { ';' }, StringSplitOptions.RemoveEmptyEntries).ToImmutableArray();
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
                VOClipperConstructors = opt.Vo16;

                RuntimeAssemblies = opt.RuntimeAssemblies;
                Overflow = opt.Overflow;
                ConsoleOutput = opt.ConsoleOutput;
                ParseLevel = opt.ParseLevel;
                AllowNamedArguments = opt.AllowNamedArguments;
                SaveAsCSharp = opt.SaveAsCSharp;
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
            ClrVersion = opt.ClrVersion;
            TargetDLL = opt.TargetDLL;
            MacroScript = opt.MacroScript;
            DebugEnabled = opt.DebugEnabled;
            DefaultIncludeDir = opt.DefaultIncludeDir;
            Dialect = opt.Dialect;
            WindowsDir = opt.WindowsDir;
            SystemDir = opt.SystemDir;
            DefaultNamespace = opt.DefaultNamespace;
            IncludePaths = opt.IncludePaths;
            ShowDefs = opt.ShowDefs;
            ShowIncludes = opt.ShowIncludes;
            NoStdDef = opt.NoStdDef;
            NoClipCall = opt.NoClipCall;
            PreprocessorOutput = opt.PreprocessorOutput;
            ParseLevel = opt.ParseLevel;
            SaveAsCSharp = opt.SaveAsCSharp;
            Verbose = opt.Verbose;
            AllowNamedArguments = opt.AllowNamedArguments;
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
            VOClipperConstructors = opt.VOClipperConstructors; // vo16
            RuntimeAssemblies = opt.RuntimeAssemblies;
            Overflow = opt.Overflow;
            ConsoleOutput = opt.ConsoleOutput;
            CommandLineArguments = opt.CommandLineArguments;
            ParseLevel = opt.ParseLevel;
        }

        public CSharpParseOptions WithXSharpSpecificOptions(XSharpSpecificCompilationOptions opt)
        {
            var result = new CSharpParseOptions(this);
            result.SetXSharpSpecificOptions(opt);
            return result;
        }

        public CSharpParseOptions WithOutput(TextWriter consoleOutput)
        {
            if (consoleOutput == this.ConsoleOutput)
            {
                return this;
            }
            var result = new CSharpParseOptions(this);
            result.SetXSharpSpecificOptions(this);
            result.ConsoleOutput = consoleOutput;
            return result;
        }
        public CSharpParseOptions WithMacroScript(bool macroScript)
        {
            if (macroScript == this.MacroScript)
            {
                return this;
            }
            var result = new CSharpParseOptions(this);
            result.SetXSharpSpecificOptions(this);
            result.MacroScript = macroScript;
            return result;
        }
    }
}
