//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Immutable;
using System.Collections.Generic;
using System.IO;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
#if !VSPARSER
using Microsoft.CodeAnalysis.CodeGen;
#endif

namespace Microsoft.CodeAnalysis.CSharp
{

    [Flags]
    public enum RuntimeAssemblies : int
    {
        None = 0,
        VulcanRT = 0x01,
        VulcanRTFuncs = 0x02,
        XSharpCore = 0x04,
        XSharpData = 0x08,
        XSharpRT = 0x10,
        XSharpVO = 0x20,
        XSharpXPP = 0x40,
        XSharpVFP = 0x80,
        SdkDefines = 0x100,
        VoSystem = 0x200,
        VoGui = 0x400,
        VoRdd = 0x800,
        VoSql = 0x1000,
        VoInet = 0x2000,
        VoConsole = 0x4000,
        VoReport = 0x8000,
        VoWin32 = 0x10000
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
        Other = 0,
        Core = 1,
        Data = 2,
        RDD = 3,
        RT = 4,
        VO = 5,
        XPP = 6,
        VFP = 7,
        VulcanRT = 8,    // strictly not a target but we use this in the OverloadResolution
        VulcanRTFuncs = 9,  // strictly not a target but we use this in the OverloadResolution
        VOWin32Api = 10,
        VOSystemClasses = 11,
        VORDDClasses = 12,
        VOSQLClasses = 13,
        VOGuiClasses = 14,
        VOInternetClasses = 15,
        VOConsoleClasses = 16,
    }

    public sealed partial class CSharpParseOptions
    {

        // Options that can be set from the outside
        // Some options are also handled by the parser
        // Other options have flags, for the preprocessor macros, such as __VO1__
        const LanguageVersion defaultLanguageVersion = LanguageVersion.CSharp9;
        #region private fields (need to be access with HasOption)
        private bool ArrayZero = false;
        private bool FoxInheritUnknown = false;
        private bool InitLocals = false;
        private bool VOAllowMissingReturns = false;
        private bool VOClipperIntegerDivisions = false;
        private bool VOClipperCallingConvention = false;
        private bool VOImplicitCastsAndConversions = false;
        private bool VOFloatConstants = false;
        private bool VONullStrings = false;
        private bool MemVars = false;
        private bool UndeclaredMemVars = false;
        private bool VOStringComparisons = false;
        private bool XPPInheritFromAbstract = false;
        private bool FoxArraySupport = false;
        private bool LateBinding = false;

        #endregion
        public bool AllowDotForInstanceMembers { get; private set; }
        public bool AllowUnsafe { get; private set; }
        public bool CaseSensitive { get; private set; }
        public int ClrVersion { get; private set; }
        public bool MacroScript { get; private set; }

        public XSharpTargetDLL TargetDLL { get; private set; }
        public bool DebugEnabled { get; private set; }
        public XSharpDialect Dialect { get; private set; }
        public string DefaultIncludeDir { get; private set; } = string.Empty;
        public string WindowsDir { get; private set; } = "";
        public string SystemDir { get; private set; } = "";
        public bool NoStdDef { get; private set; }
        public bool DumpAST { get; private set; }
        public bool ShowDefs { get; private set; }
        public bool ShowIncludes { get; private set; }
        public bool NoClipCall { get; internal set; }
        public ParseLevel ParseLevel { get; set; } = ParseLevel.Complete;
        public bool AllowNamedArguments { get; private set; }
        public bool PreprocessorOutput { get; private set; }
        public bool SaveAsCSharp { get; private set; }
        public bool EnforceOverride { get; private set; }
        public bool EnforceSelf { get; private set; }
        public string StdDefs { get; private set; } = string.Empty;
        public bool Verbose { get; private set; }
        public bool VirtualInstanceMethods { get; private set; }
        public bool VOArithmeticConversions { get; private set; }
        public bool VOClipperConstructors { get; private set; }

        public bool VoInitAxitMethods { get; private set; }
        public bool VOCompatibleIIF { get; private set; }
        public bool VOPreprocessorBehaviour { get; private set; }
        public bool VOResolveTypedFunctionPointersToPtr { get; private set; }
        public bool VOSignedUnsignedConversion { get; private set; }
        public string DefaultNamespace { get; private set; } = "";
        public bool ImplicitNamespace { get; private set; }
        public bool HasRuntime { get { return this.Dialect.NeedsRuntime(); } }
        public bool SupportsMemvars { get { return this.Dialect.SupportsMemvars() && MemVars; } }
        public bool SuppressInit1 { get; set; } = false;
#if !VSPARSER
        public ImmutableArray<string> IncludePaths { get; private set; } = ImmutableArray.Create<string>();
#else
        public IList<string> IncludePaths { get; private set; } = new List<string>();
#endif
        public bool VulcanRTFuncsIncluded => RuntimeAssemblies.HasFlag(RuntimeAssemblies.VulcanRTFuncs);
        public bool VulcanRTIncluded => RuntimeAssemblies.HasFlag(RuntimeAssemblies.VulcanRT);
        public bool XSharpRuntime => RuntimeAssemblies.HasFlag(RuntimeAssemblies.XSharpRT) |
            RuntimeAssemblies.HasFlag(RuntimeAssemblies.XSharpCore);
        public bool VOUntypedAllowed { get; private set; } = true;

        public RuntimeAssemblies RuntimeAssemblies { get; private set; } = RuntimeAssemblies.None;
        public bool Overflow { get; private set; }
        public CSharpCommandLineArguments? CommandLineArguments { get; private set; }
        public TextWriter? ConsoleOutput { get; private set; }
        public bool cs => CaseSensitive;
        public bool fox1 => FoxInheritUnknown;
        public bool fox2 => FoxArraySupport;
        public bool lb => LateBinding;
        public bool vo1 => VoInitAxitMethods;
        public bool vo10 => VOCompatibleIIF;
        public bool vo11 => VOArithmeticConversions;
        public bool vo12 => VOClipperIntegerDivisions;
        public bool vo13 => VOStringComparisons;
        public bool vo14 => VOFloatConstants;
        public bool vo15 => VOUntypedAllowed;
        public bool vo16 => VOClipperConstructors;
        public bool vo2 => VONullStrings;
        public bool vo3 => VirtualInstanceMethods;
        public bool vo4 => VOSignedUnsignedConversion;
        public bool vo5 => VOClipperCallingConvention;
        public bool vo6 => VOResolveTypedFunctionPointersToPtr;
        public bool vo7 => VOImplicitCastsAndConversions;
        public bool vo8 => VOPreprocessorBehaviour;
        public bool vo9 => VOAllowMissingReturns;
        public bool xpp1 => XPPInheritFromAbstract;
        public void SetXSharpSpecificOptions(XSharpSpecificCompilationOptions opt)
        {
            if (opt != null)
            {
                AllowDotForInstanceMembers = opt.AllowDotForInstanceMembers;
                AllowNamedArguments = opt.AllowNamedArguments;
                AllowUnsafe = opt.AllowUnsafe;
                ArrayZero = opt.ArrayZero;
                CaseSensitive = opt.CaseSensitive;
                ClrVersion = opt.ClrVersion;
                ConsoleOutput = opt.ConsoleOutput;
                DefaultIncludeDir = opt.DefaultIncludeDir;
                DefaultNamespace = opt.NameSpace;
                Dialect = opt.Dialect;
                DumpAST = opt.DumpAST;
                EnforceOverride = opt.EnforceOverride;
                EnforceSelf = opt.EnforceSelf;
                FoxArraySupport = opt.Fox2;
                FoxInheritUnknown = opt.Fox1;
                ImplicitNamespace = opt.ImplicitNameSpace;
#if !VSPARSER
                IncludePaths = opt.IncludePaths.Split(new[] { ';' }, StringSplitOptions.RemoveEmptyEntries).ToImmutableArray();
#else
                IncludePaths = opt.IncludePaths.Split(new[] { ';' }, StringSplitOptions.RemoveEmptyEntries);
#endif
                InitLocals = opt.InitLocals;
                LateBinding = opt.LateBinding;
                MemVars = opt.MemVars;
                NoClipCall = opt.NoClipCall;
                NoStdDef = opt.NoStdDef;
                Overflow = opt.Overflow;
                ParseLevel = opt.ParseLevel;
                ParseLevel = opt.ParseLevel;
                PreprocessorOutput = opt.PreProcessorOutput;
                RuntimeAssemblies = opt.RuntimeAssemblies;
                SaveAsCSharp = opt.SaveAsCSharp;
                ShowDefs = opt.ShowDefs;
                ShowIncludes = opt.ShowIncludes;
                StdDefs = opt.StdDefs;
                SuppressInit1 = opt.SuppressInit1;
                SystemDir = opt.SystemDir;
                TargetDLL = opt.TargetDLL;
                UndeclaredMemVars = opt.UndeclaredMemVars;
                Verbose = opt.Verbose;
                VirtualInstanceMethods = opt.Vo3;
                VOAllowMissingReturns = opt.Vo9;
                VOArithmeticConversions = opt.Vo11;
                VOClipperCallingConvention = opt.Vo5;
                VOClipperConstructors = opt.Vo16;
                VOClipperIntegerDivisions = opt.Vo12;
                VOCompatibleIIF = opt.Vo10;
                VOFloatConstants = opt.Vo14;
                VOImplicitCastsAndConversions = opt.Vo7;
                VoInitAxitMethods = opt.Vo1;
                VONullStrings = opt.Vo2;
                VOPreprocessorBehaviour = opt.Vo8;
                VOResolveTypedFunctionPointersToPtr = opt.Vo6;
                VOSignedUnsignedConversion = opt.Vo4;
                VOStringComparisons = opt.Vo13;
                VOUntypedAllowed = opt.Vo15;
                WindowsDir = opt.WindowsDir;
                XPPInheritFromAbstract = opt.Xpp1;
            }
            LanguageVersion = defaultLanguageVersion;
        }

        public void SetOptions(CSharpCommandLineArguments opt)
        {
            if (opt != null)
            {
                DebugEnabled = opt.EmitPdb;
                CommandLineArguments = opt;
            }
            LanguageVersion = defaultLanguageVersion;
        }
        internal CSharpParseOptions WithNoStdDef(bool nostddef)
        {
            if (this.NoStdDef == nostddef)
                return this;
            var result = new CSharpParseOptions(this);
            result.NoStdDef = nostddef;
            return result;

        }
        public void SetXSharpSpecificOptions(CSharpParseOptions opt)
        {
            AllowDotForInstanceMembers = opt.AllowDotForInstanceMembers;
            AllowNamedArguments = opt.AllowNamedArguments;
            AllowUnsafe = opt.AllowUnsafe;
            ArrayZero = opt.ArrayZero;
            CaseSensitive = opt.CaseSensitive;
            ClrVersion = opt.ClrVersion;
#if !VSPARSER
            CommandLineArguments = opt.CommandLineArguments;
#endif
            ConsoleOutput = opt.ConsoleOutput;
            DebugEnabled = opt.DebugEnabled;
            DefaultIncludeDir = opt.DefaultIncludeDir;
            DefaultNamespace = opt.DefaultNamespace;
            Dialect = opt.Dialect;
            DumpAST = opt.DumpAST;
            EnforceOverride = opt.EnforceOverride;
            EnforceSelf = opt.EnforceSelf;
            FoxArraySupport = opt.FoxArraySupport;      // fox2
            FoxInheritUnknown = opt.FoxInheritUnknown;  // fox1
            ImplicitNamespace = opt.ImplicitNamespace;
            IncludePaths = opt.IncludePaths;
            InitLocals = opt.InitLocals;
            LanguageVersion = defaultLanguageVersion;
            LateBinding = opt.LateBinding;
            MacroScript = opt.MacroScript;
            MemVars = opt.MemVars;
            NoClipCall = opt.NoClipCall;
            NoStdDef = opt.NoStdDef;
            Overflow = opt.Overflow;
            ParseLevel = opt.ParseLevel;
            ParseLevel = opt.ParseLevel;
            PreprocessorOutput = opt.PreprocessorOutput;
            RuntimeAssemblies = opt.RuntimeAssemblies;
            SaveAsCSharp = opt.SaveAsCSharp;
            ShowDefs = opt.ShowDefs;
            ShowIncludes = opt.ShowIncludes;
            StdDefs = opt.StdDefs;
            SuppressInit1 = opt.SuppressInit1;
            SystemDir = opt.SystemDir;
            TargetDLL = opt.TargetDLL;
            UndeclaredMemVars = opt.UndeclaredMemVars;
            Verbose = opt.Verbose;
            VirtualInstanceMethods = opt.VirtualInstanceMethods; // vo3
            VOAllowMissingReturns = opt.VOAllowMissingReturns; // vo9
            VOArithmeticConversions = opt.VOArithmeticConversions; // vo11
            VOClipperCallingConvention = opt.VOClipperCallingConvention;  // vo5
            VOClipperConstructors = opt.VOClipperConstructors; // vo16
            VOClipperIntegerDivisions = opt.VOClipperIntegerDivisions; // vo12
            VOCompatibleIIF = opt.VOCompatibleIIF; // vo10
            VOFloatConstants = opt.VOFloatConstants; // vo14
            VOImplicitCastsAndConversions = opt.VOImplicitCastsAndConversions; // vo7
            VoInitAxitMethods = opt.VoInitAxitMethods; // vo1
            VONullStrings = opt.VONullStrings; // vo2
            VOPreprocessorBehaviour = opt.VOPreprocessorBehaviour; // vo8
            VOResolveTypedFunctionPointersToPtr = opt.VOResolveTypedFunctionPointersToPtr; // vo6
            VOSignedUnsignedConversion = opt.VOSignedUnsignedConversion; // vo4
            VOStringComparisons = opt.VOStringComparisons; // vo13
            VOUntypedAllowed = opt.VOUntypedAllowed; // vo15
            WindowsDir = opt.WindowsDir;
            XPPInheritFromAbstract = opt.XPPInheritFromAbstract; // xpp1
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
            result.LanguageVersion = defaultLanguageVersion;
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
            result.LanguageVersion = LanguageVersion.CSharp9;
            return result;
        }

        public bool HasOption(CompilerOption option, XSharpParserRuleContext context, IList<PragmaOption> options)
        {
            switch (option)
            {
                case CompilerOption.AllowDotForInstanceMembers: // allowdot
                    return CheckOption(option, AllowDotForInstanceMembers, context, options);

                case CompilerOption.ArrayZero: // az
                    return CheckOption(option, ArrayZero, context, options);

                case CompilerOption.InitLocals: // initlocals
                    return CheckOption(option, InitLocals, context, options);

                case CompilerOption.MemVars: // memvar
                    return CheckOption(option, MemVars, context, options);

                case CompilerOption.Overflow: // ovf
                    return CheckOption(option, Overflow, context, options);

                case CompilerOption.UndeclaredMemVars: // undeclared
                    return CheckOption(option, UndeclaredMemVars, context, options);

                case CompilerOption.NullStrings: // vo2
                    return CheckOption(option, VONullStrings, context, options);

                case CompilerOption.VirtualInstanceMethods: // /vo3
                    return CheckOption(option, VirtualInstanceMethods, context, options);

                case CompilerOption.ClipperCallingConvention: // vo5
                    return CheckOption(option, VOClipperCallingConvention, context, options);

                case CompilerOption.ImplicitCastsAndConversions: // vo7
                    return CheckOption(option, VOImplicitCastsAndConversions, context, options);

                case CompilerOption.AllowMissingReturns: // vo9
                    return CheckOption(option, VOAllowMissingReturns, context, options);

                case CompilerOption.ClipperIntegerDivisions: // vo12
                    return CheckOption(option, VOClipperIntegerDivisions, context, options);

                case CompilerOption.FloatConstants: // vo14
                    return CheckOption(option, VOFloatConstants, context, options);

                case CompilerOption.UntypedAllowed: // vo15
                    return CheckOption(option, VOUntypedAllowed, context, options);

                case CompilerOption.DefaultClipperContructors: // vo16
                    return CheckOption(option, VOClipperConstructors, context, options);

                case CompilerOption.FoxArraySupport: // fox2
                    return CheckOption(option, FoxArraySupport, context, options);


                case CompilerOption.LateBinding:  // lb
                    return CheckOption(option, LateBinding, context, options);

                case CompilerOption.EnforceOverride:  // EnforceOverride
                    return CheckOption(option, EnforceOverride, context, options);

                case CompilerOption.EnforceSelf:  // enforceself
                    return CheckOption(option, EnforceSelf, context, options);

                case CompilerOption.SignedUnsignedConversion: // vo4
                case CompilerOption.ResolveTypedFunctionPointersToPtr: // vo6
                case CompilerOption.CompatibleIIF:  // vo10
                case CompilerOption.ArithmeticConversions: // vo11
                case CompilerOption.StringComparisons: // vo13
                    return false; // not handled during parsing
                case CompilerOption.Vo1: // Init/Axit => Constructor / Destruction
                case CompilerOption.Vo8: // Compatible Preprocessor
                case CompilerOption.Xpp1: // Inherit from Custom
                //case CompilerOption.Xpp2:
                case CompilerOption.Fox1: // Inherit from Custom
                case CompilerOption.AllowNamedArgs: // AllowNamedArguments: used in Antlr rules
                case CompilerOption.ImplicitNamespace:
                case CompilerOption.ClrVersion:
                case CompilerOption.All:
                    break;
            }
            return false;
        }

        public bool CheckOption(CompilerOption option, bool defaultValue, XSharpParserRuleContext context, IList<PragmaOption> options)
        {
            bool result = defaultValue;
            if (context != null && options != null && options.Count > 0)
            {
                int line = context.Start.Line;
                foreach (var pragmaoption in options)
                {
                    if (pragmaoption.Line > line)
                        break;
                    if (pragmaoption.Option == option || pragmaoption.Option == CompilerOption.All)
                    {
                        switch (pragmaoption.State)
                        {
                            case Pragmastate.On:
                                result = true;
                                break;
                            case Pragmastate.Off:
                                result = false;
                                break;
                            case Pragmastate.Default:
                                result = defaultValue;
                                break;
                        }
                    }
                }
            }
            return result;
        }
    }
}
