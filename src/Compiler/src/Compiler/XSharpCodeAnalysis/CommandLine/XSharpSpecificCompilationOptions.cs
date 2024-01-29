//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#nullable disable
using System;
using System.Collections.Generic;
using System.IO;
using Antlr4.Runtime;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
namespace Microsoft.CodeAnalysis.CSharp
{
    /// <summary>
    /// Represents various options that affect compilation, such as
    /// whether to emit an executable or a library, whether to optimize
    /// generated code, and so on.
    /// </summary>
    public sealed class XSharpSpecificCompilationOptions
    {
        public static readonly XSharpSpecificCompilationOptions Default = new();

        static string _defaultIncludeDir;
        static string _windir;
        static string _sysdir;
        public static void SetDefaultIncludeDir(string dir)
        {
            _defaultIncludeDir = dir;
        }
        public static void SetWinDir(string dir)
        {
            _windir = dir;
        }
        public static void SetSysDir(string dir)
        {
            _sysdir = dir;
        }
        public XSharpSpecificCompilationOptions()
        {
            // All defaults are set at property level
        }
        public bool AllowDotForInstanceMembers { get; internal set; } = false;
        public bool AllowOldStyleAssignments { get; internal set; } = false;
        public bool ArrayZero { get; internal set; } = false;
        public bool CaseSensitive { get; internal set; } = false;
        public int ClrVersion { get; internal set; } = 4;
        public string DefaultIncludeDir { get; internal set; } = _defaultIncludeDir;
        public XSharpDialect Dialect { get; internal set; } = XSharpDialect.Core;
        public string WindowsDir { get; internal set; } = _windir;
        public string SystemDir { get; internal set; } = _sysdir;
        public string IncludePaths { get; internal set; } = string.Empty;
        public bool ImplicitNameSpace { get; internal set; } = false;
        public bool InitLocals { get; internal set; } = false;
        public bool LateBinding { get; internal set; } = false;
        public bool AllowNamedArguments { get; internal set; } = false;
        public bool ModernSyntax { get; internal set; } = false;
        public bool NoClipCall { get; internal set; } = false;
        public bool NoStdDef { get; internal set; } = false;
        public string NameSpace { get; set; } = string.Empty;
        public ParseLevel ParseLevel { get; set; } = ParseLevel.Complete;
        public bool PreProcessorOutput { get; internal set; } = false;
        public bool SaveAsCSharp { get; internal set; } = false;
        public bool EnforceOverride { get; internal set; } = false;
        public bool DumpAST { get; internal set; } = false;
        public bool ShowDefs { get; internal set; } = false;
        public bool EnforceSelf { get; internal set; } = false;
        public bool ShowIncludes { get; internal set; } = false;
        public string StdDefs { get; internal set; } = string.Empty;
        public bool SuppressInit1 { get; internal set; } = false;
        public XSharpTargetDLL TargetDLL { get; internal set; } = XSharpTargetDLL.Other;
        public bool Verbose { get; internal set; } = false;
        public bool Vo1 { get; internal set; } = false;
        public bool Vo2 { get; internal set; } = false;
        public bool Vo3 { get; internal set; } = false;
        public bool Vo4 { get; internal set; } = false;
        public bool Vo5 { get; internal set; } = false;
        public bool Vo6 { get; internal set; } = false;
        public bool Vo7 { get; internal set; } = false;
        public bool Vo8 { get; internal set; } = false;
        public bool Vo9 { get; internal set; } = false;
        public bool Vo10 { get; internal set; } = false;
        public bool Vo11 { get; internal set; } = false;
        public bool Vo12 { get; internal set; } = false;
        public bool Vo13 { get; internal set; } = false;
        public bool Vo14 { get; internal set; } = false;
        public bool Vo15 { get; internal set; } = false;
        public bool Vo16 { get; internal set; } = false;
        public bool Vo17 { get; internal set; } = false;
        public bool Xpp1 { get; internal set; } = false;
        public bool Fox1 { get; internal set; } = false;
        public bool Fox2 { get; internal set; } = false;
        public bool VulcanRTFuncsIncluded => RuntimeAssemblies.HasFlag(RuntimeAssemblies.VulcanRTFuncs);
        public bool VulcanRTIncluded => RuntimeAssemblies.HasFlag(RuntimeAssemblies.VulcanRT);
        public bool XSharpRTIncluded => RuntimeAssemblies.HasFlag(RuntimeAssemblies.XSharpRT);
        public bool XSharpVOIncluded => RuntimeAssemblies.HasFlag(RuntimeAssemblies.XSharpVO);
        public bool XSharpVFPIncluded => RuntimeAssemblies.HasFlag(RuntimeAssemblies.XSharpVFP);
        public bool XSharpCoreIncluded => RuntimeAssemblies.HasFlag(RuntimeAssemblies.XSharpCore);
        public bool XSharpXPPIncluded => RuntimeAssemblies.HasFlag(RuntimeAssemblies.XSharpXPP);
        public bool XSharpHarbourIncluded => RuntimeAssemblies.HasFlag(RuntimeAssemblies.XSharpHarbour);
        internal RuntimeAssemblies RuntimeAssemblies { get; set; } = RuntimeAssemblies.None;
        public bool Overflow { get; internal set; } = false;
        public bool MemVars { get; internal set; } = false;
        public bool AllowUnsafe { get; internal set; } = false;
        public bool UndeclaredMemVars { get; internal set; } = false;
        public CompilerOption ExplicitOptions { get; internal set; } = CompilerOption.None;
        public bool UseNativeVersion { get; internal set; } = false;
        public string PreviousArgument { get; internal set; } = string.Empty;
        public TextWriter ConsoleOutput { get; internal set; }

        public void SetOption(CompilerOption option, bool value)
        {
            // options in alphabetical order
            switch (option)
            {
                case CompilerOption.AllowOldStyleAssignments:
                    AllowOldStyleAssignments = value;
                    break;
                case CompilerOption.AllowDotForInstanceMembers:
                    AllowDotForInstanceMembers = value;
                    break;
                case CompilerOption.AllowNamedArgs:
                    AllowNamedArguments = value;
                    break;
                case CompilerOption.ArrayZero:
                    ArrayZero = value;
                    break;
                case CompilerOption.EnforceSelf:
                    EnforceSelf = value;
                    break;
                case CompilerOption.EnforceOverride:
                    EnforceOverride = value;
                    break;
                case CompilerOption.Fox1:
                    Fox1 = value;
                    break;
                case CompilerOption.Fox2:
                    Fox2 = value;
                    break;
                case CompilerOption.ImplicitNamespace:
                    ImplicitNameSpace = value;
                    break;
                case CompilerOption.InitLocals:
                    InitLocals = value;
                    break;
                case CompilerOption.LateBinding:
                    LateBinding = value;
                    break;
                case CompilerOption.MemVars:
                    MemVars = value;
                    break;
                case CompilerOption.Overflow:
                    Overflow = value;
                    break;
                case CompilerOption.UndeclaredMemVars:
                    UndeclaredMemVars = value;
                    break;
                case CompilerOption.Vo1:
                    Vo1 = value;
                    break;
                case CompilerOption.Vo2:
                    Vo2 = value;
                    break;
                case CompilerOption.Vo3:
                    Vo3 = value;
                    break;
                case CompilerOption.Vo4:
                    Vo4 = value;
                    break;
                case CompilerOption.Vo5:
                    Vo5 = value;
                    break;
                case CompilerOption.Vo6:
                    Vo6 = value;
                    break;
                case CompilerOption.Vo7:
                    Vo7 = value;
                    break;
                case CompilerOption.Vo8:
                    Vo8 = value;
                    break;
                case CompilerOption.Vo9:
                    Vo9 = value;
                    break;
                case CompilerOption.Vo10:
                    Vo10 = value;
                    break;
                case CompilerOption.Vo11:
                    Vo11 = value;
                    break;
                case CompilerOption.Vo12:
                    Vo12 = value;
                    break;
                case CompilerOption.Vo13:
                    Vo13 = value;
                    break;
                case CompilerOption.Vo14:
                    Vo14 = value;
                    break;
                case CompilerOption.Vo15:
                    Vo15 = value;
                    break;
                case CompilerOption.Vo16:
                    Vo16 = value;
                    break;
                case CompilerOption.Vo17:
                    Vo17 = value;
                    break;
                case CompilerOption.Xpp1:
                    Xpp1 = value;
                    break;
                case CompilerOption.ModernSyntax:
                    ModernSyntax = value;
                    break;
                case CompilerOption.ClrVersion:
                case CompilerOption.None:
                default:
                    // n/a
                    break;
            }
        }

        public void SetOptionFromReference(string filename)
        {
            switch (System.IO.Path.GetFileNameWithoutExtension(filename).ToLower())
            {
                case VulcanAssemblyNames.VulcanRTFuncs:
                    this.RuntimeAssemblies |= RuntimeAssemblies.VulcanRTFuncs;
                    break;
                case VulcanAssemblyNames.VulcanRT:
                    this.RuntimeAssemblies |= RuntimeAssemblies.VulcanRT;
                    break;
                case XSharpAssemblyNames.SdkDefines:
                    this.RuntimeAssemblies |= RuntimeAssemblies.SdkDefines;
                    break;
                case XSharpAssemblyNames.XSharpCore:
                    this.RuntimeAssemblies |= RuntimeAssemblies.XSharpCore;
                    break;
                case XSharpAssemblyNames.XSharpData:
                    this.RuntimeAssemblies |= RuntimeAssemblies.XSharpData;
                    break;
                case XSharpAssemblyNames.XSharpRT:
                    this.RuntimeAssemblies |= RuntimeAssemblies.XSharpRT;
                    break;
                case XSharpAssemblyNames.XSharpVO:
                    this.RuntimeAssemblies |= RuntimeAssemblies.XSharpVO;
                    break;
                case XSharpAssemblyNames.XSharpXPP:
                    this.RuntimeAssemblies |= RuntimeAssemblies.XSharpXPP;
                    break;
                case XSharpAssemblyNames.XSharpVFP:
                    this.RuntimeAssemblies |= RuntimeAssemblies.XSharpVFP;
                    break;
                case XSharpAssemblyNames.XSharpHarbour:
                    this.RuntimeAssemblies |= RuntimeAssemblies.XSharpHarbour;
                    break;
                case XSharpAssemblyNames.VoSystem:
                    this.RuntimeAssemblies |= RuntimeAssemblies.VoSystem;
                    break;
                case XSharpAssemblyNames.VoGui:
                    this.RuntimeAssemblies |= RuntimeAssemblies.VoGui;
                    break;
                case XSharpAssemblyNames.VoRdd:
                    this.RuntimeAssemblies |= RuntimeAssemblies.VoRdd;
                    break;
                case XSharpAssemblyNames.VoSql:
                    this.RuntimeAssemblies |= RuntimeAssemblies.VoSql;
                    break;
                case XSharpAssemblyNames.VoInet:
                    this.RuntimeAssemblies |= RuntimeAssemblies.VoInet;
                    break;
                case XSharpAssemblyNames.VoConsole:
                    this.RuntimeAssemblies |= RuntimeAssemblies.VoConsole;
                    break;
                case XSharpAssemblyNames.VoReport:
                    this.RuntimeAssemblies |= RuntimeAssemblies.VoReport;
                    break;
                case XSharpAssemblyNames.VoWin32:
                    this.RuntimeAssemblies |= RuntimeAssemblies.VoWin32;
                    break;
                case "mscorlib":
                case "system":
                    if (!this.ExplicitOptions.HasFlag(CompilerOption.ClrVersion))
                    {
                        if (filename.ToLower().Contains("\\v2") || filename.ToLower().Contains("\\2."))
                        {
                            this.ExplicitOptions |= CompilerOption.ClrVersion;
                            this.ClrVersion = 2;
                        }
                        else if (filename.ToLower().Contains("\\v3") || filename.ToLower().Contains("\\3."))
                        {
                            this.ExplicitOptions |= CompilerOption.ClrVersion;
                            this.ClrVersion = 2;
                        }
                        else if (filename.ToLower().Contains("\\v4") || filename.ToLower().Contains("\\4."))
                        {
                            this.ExplicitOptions |= CompilerOption.ClrVersion;
                            this.ClrVersion = 4;
                        }
                    }
                    break;
            }
        }
        public void SetDialect(XSharpDialect dialect)
        {
            Dialect = dialect;
        }
        public void SetNoStdDef(bool noStdDef)
        {
            NoStdDef = noStdDef;
        }
    }

    public class PragmaBase
    {
        public Pragmastate State { get; private set; }
        public XSharpToken Token { get; private set; }
        public PragmaBase(XSharpToken token, Pragmastate state)
        {
            Token = token;
            State = state;
        }
        public int Line
        {
            get
            {
                if (Token == null)
                    return -1;
                return Token.Line;
            }
        }
    }
    public class PragmaOption : PragmaBase
    {
        public CompilerOption Option { get; private set; }
        public PragmaOption(XSharpToken token, Pragmastate state, CompilerOption option) : base(token, state)
        {
            Option = option;
        }
    }
    public class PragmaWarning : PragmaBase
    {
        public IList<IToken> Numbers { get; private set; }
        public IToken Warning { get; private set; }
        public IToken Switch { get; private set; }
        public PragmaWarning(XSharpToken token, Pragmastate state, IList<IToken> tokens, IToken warning, IToken switch_) : base(token, state)
        {
            Numbers = tokens;
            Warning = warning;
            Switch = switch_;
        }
    }

    [Flags]
    public enum CompilerOption : long
    {
        None = 0,
        Overflow = 1 << 0,
        Vo1 = 1 << 1,
        InitAxitConstructorDestructor = Vo1,
        Vo2 = 1 << 2,
        NullStrings = Vo2,
        Vo3 = 1 << 3,
        VirtualInstanceMethods = Vo3,
        Vo4 = 1 << 4,
        Vo5 = 1 << 5,
        ClipperCallingConvention = Vo5,
        Vo6 = 1 << 6,
        ResolveTypedFunctionPointersToPtr = Vo6,
        Vo7 = 1 << 7,
        ImplicitCastsAndConversions = Vo7,
        Vo8 = 1 << 8,
        CompatiblePreprocessor = Vo8,
        Vo9 = 1 << 9,
        AllowMissingReturns = Vo9,
        Vo10 = 1 << 10,
        CompatibleIIF = Vo10,
        Vo11 = 1 << 11,
        Vo12 = 1 << 12,
        ClipperIntegerDivisions = Vo12,
        Vo13 = 1 << 13,
        StringComparisons = Vo13,
        Vo14 = 1 << 14,
        FloatConstants = Vo14,
        Vo15 = 1 << 15,
        UntypedAllowed = Vo15,
        Vo16 = 1 << 16,
        DefaultClipperConstructors = Vo16,
        Vo17 = 1 << 17,
        CompatibleBeginSequence = Vo17,
        Xpp1 = 1 << 18,
        //Xpp2 = 1 << 18,
        Fox1 = 1 << 19,
        Fox2 = 1 << 20,
        FoxArraySupport = Fox2,
        InitLocals = 1 << 21,
        AllowNamedArgs = 1 << 22,
        ArrayZero = 1 << 23,
        LateBinding = 1 << 24,
        ImplicitNamespace = 1 << 25,
        MemVars = 1 << 26,
        UndeclaredMemVars = 1 << 27,
        ClrVersion = 1 << 28,
        EnforceSelf = 1 << 29,
        AllowDotForInstanceMembers = 1 << 30,
        EnforceOverride = 1L << 31,
        AllowOldStyleAssignments = 1L << 32,
        ModernSyntax = 1L << 33,
        All = -1,       // used for Push/Pop
    }

    internal static class CompilerOptionDecoder
    {
        public static bool NeedsRuntime(this CompilerOption option)
        {
            // sorted alphabetically
            switch (option)
            {
                case CompilerOption.Vo1: // InitAxit
                case CompilerOption.Vo2: // NullStrings
                case CompilerOption.Vo3: // VirtualInstanceMethods
                case CompilerOption.Vo4: // Integral conversions
                    return false;
                case CompilerOption.Vo5: //  ClipperCallingConvention:
                case CompilerOption.Vo6: //  ResolveTypedFunctionPointersToPtr:
                case CompilerOption.Vo7: //  ImplicitCastsAndConversions:
                    return true;
                case CompilerOption.Vo8: //  CompatiblePreprocessor:
                case CompilerOption.Vo9: // AllowMissingReturns
                case CompilerOption.Vo10: // CompatibleIIF:
                    return false;
                case CompilerOption.Vo11: // Arithmetic Conversions
                case CompilerOption.Vo12: // ClipperIntegerDivisions:
                case CompilerOption.Vo13: // StringComparisons:
                case CompilerOption.Vo14: // FloatConstants:
                case CompilerOption.Vo15: // UntypedAllowed:
                case CompilerOption.Vo16: // DefaultClipperContructors:
                case CompilerOption.Vo17: // CompatibleBeginSequence:
                case CompilerOption.Fox1:
                case CompilerOption.Fox2:
                case CompilerOption.MemVars:
                case CompilerOption.UndeclaredMemVars:
                case CompilerOption.Xpp1:
                    //case CompilerOption.Xpp2:
                    return true;
                case CompilerOption.AllowDotForInstanceMembers:
                case CompilerOption.AllowOldStyleAssignments:
                case CompilerOption.AllowNamedArgs:
                case CompilerOption.ArrayZero:
                case CompilerOption.ClrVersion:
                case CompilerOption.EnforceOverride:
                case CompilerOption.EnforceSelf:
                case CompilerOption.ImplicitNamespace:
                case CompilerOption.InitLocals:
                case CompilerOption.LateBinding:
                case CompilerOption.None:
                case CompilerOption.Overflow:
                case CompilerOption.ModernSyntax:
                    return false;
                default:
                    break;
            }
            return false;
        }

        internal static string Description(this CompilerOption option)
        {
            // options sorted in alphabetical order
            switch (option)
            {
                case CompilerOption.AllowDotForInstanceMembers:
                    return "Instance Dot operator for instance members"; ;
                case CompilerOption.ArrayZero:
                    return "Use Zero Based Arrays";
                case CompilerOption.AllowOldStyleAssignments:
                    return "Allow Old Style Assignments";
                case CompilerOption.EnforceOverride:
                    return "OVERRIDE must be explicitly used in the source code"; ;
                case CompilerOption.EnforceSelf:
                    return "Instance Method calls inside a class require a SELF prefix";
                case CompilerOption.Fox1:
                    return "All Classes inherit from unknown";
                case CompilerOption.Fox2:
                    break;
                case CompilerOption.ImplicitNamespace:
                    return "Enable Implicit Namespace lookups";
                case CompilerOption.InitLocals:
                    return "Initialize all local variables and fields";
                case CompilerOption.LateBinding:
                    return "Enable Late Binding";
                case CompilerOption.MemVars:
                    return "PRIVATE and or PUBLIC variables";
                case CompilerOption.ModernSyntax:
                    return "Modern XBase Syntax";
                case CompilerOption.AllowNamedArgs:
                    return "Allow Named Arguments";
                case CompilerOption.Overflow:
                    return "Overflow Exceptions";
                case CompilerOption.UndeclaredMemVars:
                    return "Undeclared Memory Variables";
                case CompilerOption.Vo1:
                    return "Init() & Axit() methods mapped to constructor and destructor";
                case CompilerOption.Vo2:
                    return "Initialize Strings";
                case CompilerOption.Vo3:
                    return "All methods Virtual";
                case CompilerOption.Vo4:
                    return "Implicit integer conversions";
                case CompilerOption.Vo5:
                    return "Implicit CLIPPER calling convention";
                case CompilerOption.Vo6:
                    return "Implicit pointer conversions";
                case CompilerOption.Vo7:
                    return "Implicit casts and Conversions";
                case CompilerOption.Vo8:
                    return "Compatible preprocessor";
                case CompilerOption.Vo9:
                    return "Allow missing return statements or missing return values";
                case CompilerOption.Vo10:
                    return "Compatible IIF expressions";
                case CompilerOption.Vo11:
                    return "Compatible fractional -> integer conversions";
                case CompilerOption.Vo12:
                    return "Clipper Integer divisions";
                case CompilerOption.Vo13:
                    return "VO Compatible string comparisons";
                case CompilerOption.Vo14:
                    return "Float literal Values";
                case CompilerOption.Vo15:
                    return "Allow untyped Locals and return types";
                case CompilerOption.Vo16:
                    return "Generate Clipper calling convention constructors for classes without constructor";
                case CompilerOption.Vo17:
                    return "VO Compatible BEGIN SEQUENCE .. END ";
                case CompilerOption.Xpp1:
                    return "All classes inherit from XPP.Abstract";
                //case CompilerOption.Xpp2:
                //    return "n/a";
                case CompilerOption.None:
                    return "";
            }
            return "";
        }
        internal static CompilerOption Decode(string option)
        {
            // options sorted in alphabetical order
            switch (option.ToLower())
            {
                case "allowoldstyleassignments":
                    return CompilerOption.AllowOldStyleAssignments;
                case "allowdot":
                    return CompilerOption.AllowDotForInstanceMembers;
                case "az":
                    return CompilerOption.ArrayZero;
                case "enforceoverride":
                    return CompilerOption.EnforceOverride;
                case "enforceself":
                    return CompilerOption.EnforceSelf;
                case "fovf":
                    return CompilerOption.Overflow;
                case "fox1":
                    return CompilerOption.Fox1;
                case "fox2":
                    return CompilerOption.Fox2;
                case "initlocals":
                    return CompilerOption.InitLocals;
                case "ins":
                    return CompilerOption.ImplicitNamespace;
                case "lb":
                    return CompilerOption.LateBinding;
                case "memvar":
                case "memvars":
                    return CompilerOption.MemVars;
                case "modernsyntax":
                    return CompilerOption.ModernSyntax;
                case "namedargs":
                case "namedarguments":
                    return CompilerOption.AllowNamedArgs;
                case "ovf":
                    return CompilerOption.Overflow;
                case "undeclared":
                    return CompilerOption.UndeclaredMemVars;
                case "vo1":
                    return CompilerOption.Vo1;
                case "vo2":
                    return CompilerOption.Vo2;
                case "vo3":
                    return CompilerOption.Vo3;
                case "vo4":
                    return CompilerOption.Vo4;
                case "vo5":
                    return CompilerOption.Vo5;
                case "vo6":
                    return CompilerOption.Vo6;
                case "vo7":
                    return CompilerOption.Vo7;
                case "vo8":
                    return CompilerOption.Vo8;
                case "vo9":
                    return CompilerOption.Vo9;
                case "vo10":
                    return CompilerOption.Vo10;
                case "vo11":
                    return CompilerOption.Vo11;
                case "vo12":
                    return CompilerOption.Vo12;
                case "vo13":
                    return CompilerOption.Vo13;
                case "vo14":
                    return CompilerOption.Vo14;
                case "vo15":
                    return CompilerOption.Vo15;
                case "vo16":
                    return CompilerOption.Vo16;
                case "vo17":
                    return CompilerOption.Vo17;
                case "xpp1":
                    return CompilerOption.Xpp1;
                //case "xpp2":
                //    return CompilerOption.Xpp2;
                default:
                    break;
            }
            return CompilerOption.None;
        }
    }

    public enum Pragmastate : byte
    {
        Default = 0,
        On = 1,
        Off = 2,
    }
}
