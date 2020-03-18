//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Linq;
namespace Microsoft.CodeAnalysis.CSharp
{

    public partial class CSharpCommandLineParser : CommandLineParser
    {
        private XSharpSpecificCompilationOptions options;
        // Vulcan Assembly Names

        public XSharpSpecificCompilationOptions XSharpSpecificCompilationOptions
        {
            get
            {
                return options;
            }
        }
        internal void ResetXSharpCommandlineOptions()
        {
            options = new XSharpSpecificCompilationOptions();
        }
        
        internal bool ParseXSharpArgument(ref string name, ref string value, string arg, List<Diagnostic> diagnostics)
        {
            if (options == null)
                options = new XSharpSpecificCompilationOptions();

            bool handled = true;
            bool positive = !name.EndsWith("-");
            bool encode = false;
            string oldname = name;
            if (name.EndsWith("+") || name.EndsWith("-"))
            {
                name = name.Substring(0, name.Length - 1);
            }
            switch (name)
            {
                case "az":
                    options.ArrayZero = positive;
                    encode = true;
                    break;
                case "cf":
                    OptionNotImplemented(diagnostics, oldname, "Compiling for Compact Framework");
                    break;
                case "dialect":
                    XSharpDialect dialect = XSharpDialect.Core;
                    if (string.IsNullOrEmpty(value))
                    {
                        AddDiagnostic(diagnostics, ErrorCode.ERR_SwitchNeedsString, MessageID.IDS_Text.Localize(), "/dialect:");
                    }
                    else if (!TryParseDialect(value, XSharpDialect.Core, out dialect))
                    {
                        AddDiagnostic(diagnostics, ErrorCode.ERR_InvalidDialect, value);
                    }
                    options.Dialect = dialect;
                    break;
                case "clr": // CLR
                    OptionNotImplemented(diagnostics, oldname, "Specify CLR version");
                    encode = true;
                    break;
                case "cs":
                    options.CaseSensitive = positive;
                    XSharpString.CaseSensitive = positive;
                    break;
                case "i":
                    if (value == null)
                    {
                        AddDiagnostic(diagnostics, ErrorCode.ERR_SwitchNeedsString, MessageID.IDS_Text.Localize(), "/i:");
                    }
                    else
                    {
                        if(value.StartsWith("\"") && value.EndsWith("\""))
                            value = value.Substring(1, value.Length - 2);
                        options.IncludePaths = string.IsNullOrEmpty(options.IncludePaths) ? value : options.IncludePaths +';' + value;
                    }
                    break;

                case "initlocals":
                    options.InitLocals = positive;
                    encode = true;
                    break;

                case "ins":
                    options.ImplicitNameSpace = positive;
                    encode = true;
                    break;

                case "lb":
                    options.LateBinding = positive;
                    encode = true;
                    break;

                case "namedarguments":
                    options.AllowNamedArguments = positive;
                    encode = true;
                    break;

                case "noclipcall":
                    options.NoClipCall = positive; 
                    break;

                case "norun":
                    OptionNotImplemented(diagnostics, oldname, "NoRun compiler option. To achieve the same result in X# simply remove the references to the X# and/or Vulcan runtime DLLs.");
                    break;

                case "nostddefs":
                    options.NoStdDef = positive;
                    break;

                case "ns":
                    if (value == null)
                    {
                        AddDiagnostic(diagnostics, ErrorCode.ERR_SwitchNeedsString, MessageID.IDS_Text.Localize(), "/ns:");
                    }
                    else
                    {
                        options.NameSpace = value;
                    }
                    break;
                case "fovf":    // synonym for checked
                case "ovf":     // synonym for checked
                    if (options.ExplicitOptions.HasFlag(CompilerOption.Overflow)  && positive != options.Overflow)
                    {
                        AddDiagnostic(diagnostics, ErrorCode.ERR_ConflictingCommandLineOptions, arg, options.PreviousArgument);
                    }
                    options.PreviousArgument = arg;
                    encode = true;

                    options.Overflow = positive;
                    if (positive)
                        name = "checked+";
                    else
                        name = "checked-";
                    handled = false;
                    break;
                case "languageversion":
                    handled = true;
                    break;
                case "lexonly":
                    options.ParseLevel = ParseLevel.Lex;
                    break;
                case "memvar":
                    options.MemVars = positive;
                    encode = true;
                    break;
                case "undeclared":
                    options.UndeclaredMemVars = positive;
                    encode = true;
                    break;
                case "parseonly":
                    options.ParseLevel = ParseLevel.Parse;
                    break;
                case "out":
                    if (!string.IsNullOrEmpty(value))
                    {
                        value = value.Trim();
                        if (value.StartsWith("\"") && value.EndsWith("\""))
                            value = value.Substring(1, value.Length - 2);
                        string fn = System.IO.Path.GetFileName(value).ToLower();
                        switch (fn)
                        {
                            case "xsharp.core.dll":
                                options.TargetDLL = XSharpTargetDLL.Core;
                                break;
                            case "xsharp.data.dll":
                                options.TargetDLL = XSharpTargetDLL.Data;
                                break;
                            case "xsharp.rt.dll":
                                options.TargetDLL = XSharpTargetDLL.RT;
                                break;
                            case "xsharp.vo.dll":
                                options.TargetDLL = XSharpTargetDLL.VO;
                                break;
                            case "xsharp.rdd.dll":
                                options.TargetDLL = XSharpTargetDLL.RDD;
                                break;
                            case "xsharp.xpp.dll":
                                options.TargetDLL = XSharpTargetDLL.XPP;
                                break;
                            case "xsharp.vfp.dll":
                                options.TargetDLL = XSharpTargetDLL.VFP;
                                break;
                            case "vowin32apilibrary.dll":
                                options.TargetDLL = XSharpTargetDLL.VOWin32Api;
                                break;
                            case "vosystemclasses.dll":
                                options.TargetDLL = XSharpTargetDLL.VOSystemClasses;
                                break;
                            case "vorddclasses.dll":
                                options.TargetDLL = XSharpTargetDLL.VORDDClasses;
                                break;
                            case "vosqlclasses.dll":
                                options.TargetDLL = XSharpTargetDLL.VOSQLClasses;
                                break;
                            case "voguiclasses.dll":
                                options.TargetDLL = XSharpTargetDLL.VOGuiClasses;
                                break;
                            case "vointernetclasses.dll":
                                options.TargetDLL = XSharpTargetDLL.VOInternetClasses;
                                break;
                            case "voconsoleclasses.dll":
                                options.TargetDLL = XSharpTargetDLL.VOConsoleClasses;
                                break;
                            default:
                                options.TargetDLL = XSharpTargetDLL.Other;
                                break;
                        }
                    }
                    handled = false;
                    break; 
                case "ppo":
                    options.PreProcessorOutput = positive;
                    break;
                case "r":
                case "reference":
                    if (!string.IsNullOrEmpty(value))
                    {
                        // /r:"reference"
                        // /r:alias=reference
                        // /r:alias="reference"
                        // /r:reference;reference
                        // /r:"path;containing;semicolons"
                        // /r:"unterminated_quotes
                        // /r:"quotes"in"the"middle
                        // /r:alias=reference;reference      ... error 2034
                        // /r:nonidf=reference               ... error 1679
                        var pos = value.IndexOf('=');
                        if (pos >= 0 && value[pos] == '=')
                        {
                            value = value.Substring(pos + 1);
                        }
                        if (value.StartsWith("\"") && value.EndsWith("\""))
                        {
                            value = value.Substring(1, value.Length - 2);
                        }
                        string filename = value;
                        if (value.IndexOf(";")!=-1)
                        {
                            foreach (var fname in  ParseSeparatedPaths(value).Where((path) => !string.IsNullOrWhiteSpace(path)))
                            {
                                SetOptionFromReference(fname);
                            }
                        }
                        else
                        {
                            SetOptionFromReference(filename);
                        }
                    }
                    handled = false;
                    break;
                case "s":
                    options.ParseLevel = ParseLevel.SyntaxCheck;
                    break;
                case "showdefs":
                case "showdefines":
                    options.ShowDefs = positive;
                    break;
                case "showincludes":
                    options.ShowIncludes = positive;
                    break;
                case "stddefs":
                    if (value == null)
                    {
                        AddDiagnostic(diagnostics, ErrorCode.ERR_SwitchNeedsString, MessageID.IDS_Text.Localize(), "/stddefs:");
                    }
                    else
                    {
                        if (value.StartsWith("\"") && value.EndsWith("\""))
                            value = value.Substring(1, value.Length - 2);
                        options.StdDefs = value;
                    }
                    break;


                case "tocs":
                    options.SaveAsCSharp = positive;
                    break;
                case "ast":
                    options.DumpAST = positive;
                    break;
                case "usenativeversion":
                    options.UseNativeVersion = true;
                    break;
                case "verbose":
                    options.Verbose = true;
                    options.ShowIncludes = true;
                    break;
                case "vo1":     // Init & Axit mapped to .ctor and .dtor
                    options.Vo1 = positive;
                    encode = true;
                    break;
                case "vo2":     // Initialize Strings to Empty string
                    options.Vo2 = positive;
                    encode = true;
                    break;
                case "vo3":     // All methods Virtual
                    options.Vo3 = positive;
                    encode = true;
                    break;
                case "vo4":     // Implicit signed/unsigned integer conversions
                    options.Vo4 = positive;
                    encode = true;
                    break;
                case "vo5":     // Implicit CLIPPER calling convention
                    options.Vo5 = positive;
                    encode = true;
                    break;
                case "vo6":     // Resolve typed function PTR to PTR
                    options.Vo6 = positive;
                    encode = true;
                    break;
                case "vo7":     // Compatible implicit cast & conversion
                    options.Vo7 = positive;
                    encode = true;
                    break;
                case "vo8":     // Compatible preprocessor
                    options.Vo8 = positive;
                    encode = true;
                    break;
                case "vo9":     // Allow missing RETURN
                    options.Vo9 = positive;
                    encode = true;
                    break;
                case "vo10":    // Compatible IIF
                    options.Vo10 = positive;
                    encode = true;
                    break;
                case "vo11":    // VO arithmetic conversions
                    options.Vo11 = positive;
                    encode = true;
                    break;
                case "vo12":    // Clipper integer divisions
                    options.Vo12 = positive;
                    encode = true;
                    break;
                case "vo13":    // VO String comparisons
                    options.Vo13 = positive;
                    encode = true;
                    break;
                case "vo14":    // VO FLoat Literals
                    options.Vo14 = positive;
                    encode = true;
                    break;
                case "vo15":    // VO Untyped allowed
                    options.Vo15 = positive;
                    encode = true;
                    break;
                case "vo16":    // VO Add Clipper CC Missing constructors
                    options.Vo16 = positive;
                    encode = true;
                    break;
                case "wx":       // disable warning
                    name = "warnaserror+";
                    handled = false;
                    break;
                case "xpp1":       // classes inherit from XPP.Abstract
                    options.Xpp1 = positive;
                    encode = true;
                    break;
                case "xpp2":       // untyped main instead of Start
                    options.Xpp2 = positive;
                    encode = true;
                    break;
                case "fox1":       // Classes inherit from unknown
                    options.Fox1 = positive;
                    encode = true;
                    break;
                case "unsafe":
                    options.AllowUnsafe = positive;
                    handled = false;    // there is also an 'unsafe' option in Roslyn
                    name = oldname;
                    break;

                default:
                    name = oldname;
                    handled = false;
                    break;

            }
            if (encode)
            {
                options.ExplicitOptions |= CompilerOptionDecoder.Decode(name);
            }
            return handled;
        }
        private void SetOptionFromReference(string filename)
        {
            switch (System.IO.Path.GetFileNameWithoutExtension(filename).ToLower())
            {
                case VulcanAssemblyNames.VulcanRTFuncs:
                    options.RuntimeAssemblies |= RuntimeAssemblies.VulcanRTFuncs;
                    break;
                case VulcanAssemblyNames.VulcanRT:
                    options.RuntimeAssemblies |= RuntimeAssemblies.VulcanRT;
                    break;
                case XSharpAssemblyNames.SdkDefines:
                    options.RuntimeAssemblies |= RuntimeAssemblies.SdkDefines;
                    break;
                case XSharpAssemblyNames.XSharpCore:
                    options.RuntimeAssemblies |= RuntimeAssemblies.XSharpCore;
                    break;
                case XSharpAssemblyNames.XSharpData:
                    options.RuntimeAssemblies |= RuntimeAssemblies.XSharpData;
                    break;
                case XSharpAssemblyNames.XSharpRT:
                    options.RuntimeAssemblies |= RuntimeAssemblies.XSharpRT;
                    break;
                case XSharpAssemblyNames.XSharpVO:
                    options.RuntimeAssemblies |= RuntimeAssemblies.XSharpVO;
                    break;
                case XSharpAssemblyNames.XSharpXPP:
                    options.RuntimeAssemblies |= RuntimeAssemblies.XSharpXPP;
                    break;
                case XSharpAssemblyNames.XSharpVFP:
                    options.RuntimeAssemblies |= RuntimeAssemblies.XSharpVFP;
                    break;
                case XSharpAssemblyNames.VoSystem:
                    options.RuntimeAssemblies |= RuntimeAssemblies.VoSystem;
                    break;
                case XSharpAssemblyNames.VoGui:
                    options.RuntimeAssemblies |= RuntimeAssemblies.VoGui;
                    break;
                case XSharpAssemblyNames.VoRdd:
                    options.RuntimeAssemblies |= RuntimeAssemblies.VoRdd;
                    break;
                case XSharpAssemblyNames.VoSql:
                    options.RuntimeAssemblies |= RuntimeAssemblies.VoSql;
                    break;
                case XSharpAssemblyNames.VoInet:
                    options.RuntimeAssemblies |= RuntimeAssemblies.VoInet;
                    break;
                case XSharpAssemblyNames.VoConsole:
                    options.RuntimeAssemblies |= RuntimeAssemblies.VoConsole;
                    break;
                case XSharpAssemblyNames.VoReport:
                    options.RuntimeAssemblies |= RuntimeAssemblies.VoReport;
                    break;
                case XSharpAssemblyNames.VoWin32:
                    options.RuntimeAssemblies |= RuntimeAssemblies.VoWin32;
                    break;
               case "mscorlib":
                case "system":
                    if (! options.ExplicitOptions.HasFlag(CompilerOption.ClrVersion))
                    {
                        if (filename.ToLower().Contains("\\v2") || filename.ToLower().Contains("\\2."))
                        {
                            options.ExplicitOptions |= CompilerOption.ClrVersion;
                            options.ClrVersion = 2;
                        }
                        else if (filename.ToLower().Contains("\\v3") || filename.ToLower().Contains("\\3."))
                        {
                            options.ExplicitOptions |= CompilerOption.ClrVersion;
                            options.ClrVersion = 2;
                        }
                        else if (filename.ToLower().Contains("\\v4") || filename.ToLower().Contains("\\4."))
                        {
                            options.ExplicitOptions |= CompilerOption.ClrVersion;
                            options.ClrVersion = 4;
                        }

                    }
                    break;
            }

        }
        private static bool TryParseDialect(string str, XSharpDialect defaultDialect, out XSharpDialect dialect)
        {
            if (str == null)
            {
                dialect = defaultDialect;
                return true;
            }

            switch (str.ToLowerInvariant())
            {
                case "core":
                    dialect = XSharpDialect.Core;
                    return true;

                case "vo":
                    dialect = XSharpDialect.VO;
                    return true;

                case "vulcan":
                case "vulcan.net":
                    dialect = XSharpDialect.Vulcan;
                    return true;

                case "dbase":
                    dialect = XSharpDialect.dBase;
                    return true;

                case "foxpro":
                case "foxbase":
                case "fox":
                case "vfp":
                    dialect = XSharpDialect.FoxPro;
                    return true;

                case "harbour":
                case "xharbour":
                    dialect = XSharpDialect.Harbour;
                    return true;
                case "xbase++":
                case "xbasepp":
                case "xpp":
                    dialect = XSharpDialect.XPP;
                    return true;
                default:
                    dialect = XSharpDialect.Core;
                    return false;
            }
        }

        private void ValidateXSharpSettings(List<Diagnostic> diagnostics) {
            bool withRT = false;
            var newDialect = options.Dialect;
            if (options.Dialect == XSharpDialect.Core)
            {
                if (!options.ExplicitOptions.HasFlag(CompilerOption.NamedArgs))
                    options.AllowNamedArguments = true;
            }
            else
            {
                if (!options.ExplicitOptions.HasFlag(CompilerOption.NamedArgs))
                    options.AllowNamedArguments = false;
            }
            if (newDialect == XSharpDialect.XPP && options.TargetDLL == XSharpTargetDLL.XPP)
            { 
                newDialect = XSharpDialect.VO;  // the runtime uses the VO syntax for classes
            }
            if (newDialect.HasRuntime()) {
                if (options.VulcanRTFuncsIncluded && options.VulcanRTIncluded) {
                    // Ok;
                    withRT = true;
                }
                else if (options.XSharpRTIncluded  && options.XSharpCoreIncluded ) {
                    // Ok;
                    withRT = true;
                }
                else if(options.TargetDLL == XSharpTargetDLL.VO || options.TargetDLL == XSharpTargetDLL.RDD ||
                    options.TargetDLL == XSharpTargetDLL.XPP || options.TargetDLL == XSharpTargetDLL.RT ||
                    options.TargetDLL == XSharpTargetDLL.VFP) {
                    // Ok
                    withRT = true;
                }
                else {
                    AddDiagnostic(diagnostics, ErrorCode.ERR_DialectRequiresReferenceToRuntime, options.Dialect.ToString(),
                        "XSharp.Core.DLL and XSharp.RT.DLL or VulcanRT.DLL and VulcanRTFuncs.DLL");
                    newDialect = XSharpDialect.Core;
                }
            }
            if (! withRT)
            {
                if (options.Vo5 && options.ExplicitOptions.HasFlag(CompilerOption.Vo5)) 
                {
                    AddDiagnostic(diagnostics, ErrorCode.ERR_CompilerOptionNotSupportedForDialect, "vo5", "Implicit CLIPPER calling convention", options.Dialect.ToString());
                    options.Vo5 = false;
                }
                if (options.Vo6 && options.ExplicitOptions.HasFlag(CompilerOption.Vo6))
                {
                    AddDiagnostic(diagnostics, ErrorCode.ERR_CompilerOptionNotSupportedForDialect, "vo6", "Implicit pointer conversions", options.Dialect.ToString());
                    options.Vo6 = false;
                }
                if (options.Vo7 && options.ExplicitOptions.HasFlag(CompilerOption.Vo7))
                {
                    AddDiagnostic(diagnostics, ErrorCode.ERR_CompilerOptionNotSupportedForDialect, "vo7", "Implicit casts and Conversions", options.Dialect.ToString());
                    options.Vo7 = false;
                }
                if (options.Vo11 && options.ExplicitOptions.HasFlag(CompilerOption.Vo11))
                {
                    AddDiagnostic(diagnostics, ErrorCode.ERR_CompilerOptionNotSupportedForDialect, "vo11", "Compatible numeric conversions", options.Dialect.ToString());
                    options.Vo11 = false;
                }
                if (options.Vo12 && options.ExplicitOptions.HasFlag(CompilerOption.Vo12))
                {
                    AddDiagnostic(diagnostics, ErrorCode.ERR_CompilerOptionNotSupportedForDialect, "vo12", "Clipper Integer divisions", options.Dialect.ToString());
                    options.Vo12 = false;
                }
                if (options.Vo13 && options.ExplicitOptions.HasFlag(CompilerOption.Vo13))
                {
                    AddDiagnostic(diagnostics, ErrorCode.ERR_CompilerOptionNotSupportedForDialect, "vo13", "VO Compatible string comparisons", options.Dialect.ToString());
                    options.Vo13 = false;
                }
                if (options.Vo14 && options.ExplicitOptions.HasFlag(CompilerOption.Vo14))
                {
                    AddDiagnostic(diagnostics, ErrorCode.ERR_CompilerOptionNotSupportedForDialect, "vo14", "Float literal Values", options.Dialect.ToString());
                    options.Vo14 = false;
                }
                if (options.Vo15 && options.ExplicitOptions.HasFlag(CompilerOption.Vo15))
                {
                    AddDiagnostic(diagnostics, ErrorCode.ERR_CompilerOptionNotSupportedForDialect, "vo15", "Allow untyped Locals and return types", options.Dialect.ToString());
                    options.Vo15 = false;
                }
                if (options.Vo16 && options.ExplicitOptions.HasFlag(CompilerOption.Vo16))
                {
                    AddDiagnostic(diagnostics, ErrorCode.ERR_CompilerOptionNotSupportedForDialect, "vo16", "Generate Clipper calling convention constructors for classes without constructor", options.Dialect.ToString());
                    options.Vo16 = false;
                }
            }
            else
            {
                if (!options.ExplicitOptions.HasFlag(CompilerOption.Vo15))
                {
                    options.Vo15 = true;            // Untyped allowed
                }
                if (options.Dialect == XSharpDialect.FoxPro)
                {
                    if (!options.ExplicitOptions.HasFlag(CompilerOption.Vo9))
                    {
                        options.Vo9 = true;             // generate default return values
                    }
                    if (!options.ExplicitOptions.HasFlag(CompilerOption.InitLocals))
                    {
                        options.InitLocals = true;
                    }
                    if (!options.ExplicitOptions.HasFlag(CompilerOption.Fox1))
                    {
                        options.Fox1 = true;             // inherit from Custom
                    }
                }
            }
            options.Dialect = newDialect;
           
        }

        private void OptionNotImplemented(List<Diagnostic> diagnostics, string option, string description )
        {
            AddDiagnostic(diagnostics, ErrorCode.WRN_CompilerOptionNotImplementedYet, option, description);
        }

    }
}
