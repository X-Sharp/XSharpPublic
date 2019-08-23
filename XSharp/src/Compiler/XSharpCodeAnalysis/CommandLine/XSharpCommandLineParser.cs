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
            string oldname = name;
            if (name.EndsWith("+"))
            {
                name = name.Substring(0, name.Length - 1);
            }
            else if (name.EndsWith("-"))
            {
                name = name.Substring(0, name.Length - 1);
            }
            switch (name)
            {
                case "az":
                    options.ArrayZero = positive;
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
                    break;
                case "cs":
                    options.CaseSensitive = positive;
                    OptionNotImplemented(diagnostics, oldname, "Case Sensitivity");
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

                case "ins":
                    options.ImplicitNameSpace = positive;
                    break;

                case "lb":
                    options.LateBinding = positive;
                    break;

                case "namedarguments":
                    options.AllowNamedArguments = positive;
                    options.NamedArgsHasBeenSet = true;
                    break;

                case "noclipcall":
                    options.NoClipCall = positive; 
                    break;

                case "norun":
                    OptionNotImplemented(diagnostics, oldname, "NoRun compiler option");
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
                    if (options.OverflowHasBeenSet && positive != options.Overflow)
                    {
                        AddDiagnostic(diagnostics, ErrorCode.ERR_ConflictingCommandLineOptions, arg, options.PreviousArgument);
                    }
                    options.PreviousArgument = arg;
                    options.OverflowHasBeenSet = true;

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
                    break;
                case "undeclared":
                    options.UndeclaredLocalVars = positive;
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
                case "verbose":
                    options.Verbose = true;
                    options.ShowIncludes = true;
                    break;
                case "vo1":     // Init & Axit mapped to .ctor and .dtor
                    options.Vo1 = positive;
                    break;
                case "vo2":     // Initialize Strings to Empty string
                    options.Vo2 = positive;
                    break;
                case "vo3":     // All methods Virtual
                    options.Vo3 = positive;
                    break;
                case "vo4":     // Implicit signed/unsigned integer conversions
                    options.Vo4 = positive;
                    break;
                case "vo5":     // Implicit CLIPPER calling convention
                    options.Vo5 = positive;
                    break;
                case "vo6":     // Resolve typed function PTR to PTR
                    options.Vo6 = positive;
                    break;
                case "vo7":     // Compatible implicit cast & conversion
                    options.Vo7 = positive;
                    break;
                case "vo8":     // Compatible preprocessor
                    options.Vo8 = positive;
                    break;
                case "vo9":     // Allow missing RETURN
                    options.Vo9 = positive;
                    options.ExplicitVO9 = true;
                    break;
                case "vo10":    // Compatible IIF
                    options.Vo10 = positive;
                    break;
                case "vo11":    // VO arithmetic conversions
                    options.Vo11 = positive;
                    OptionNotImplemented(diagnostics, oldname, "VO compatible arithmetic conversions");
                    break;
                case "vo12":    // Clipper integer divisions
                    options.Vo12 = positive;
                    break;
                case "vo13":    // VO String comparisons
                    options.Vo13 = positive;
                    break;
                case "vo14":    // VO FLoat Literals
                    options.Vo14 = positive;
                    break;
                case "vo15":    // VO Untyped allowed
                    options.Vo15 = positive;
                    options.ExplicitVO15 = true;
                    break;
                case "vo16":    // VO Add Clipper CC Missing constructors
                    options.Vo16 = positive;
                    break;
                case "wx":       // disable warning
                    name = "warnaserror+";
                    handled = false;
                    break;
                case "xpp1":       // classes inherit from XPP.Abstract
                    options.Xpp1 = positive;
                    break;
                case "xpp2":       // untyped main instead of Start
                    options.Xpp2 = positive;
                    break;
                default:
                    name = oldname;
                    handled = false;
                    break;

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
                    if (! options.ClrVersionWasSet )
                    {
                        if (filename.ToLower().Contains("\\v2") || filename.ToLower().Contains("\\2."))
                        {
                            options.ClrVersionWasSet = true;
                            options.ClrVersion = 2;
                        }
                        else if (filename.ToLower().Contains("\\v3") || filename.ToLower().Contains("\\3."))
                        {
                            options.ClrVersionWasSet = true;
                            options.ClrVersion = 2;
                        }
                        else if (filename.ToLower().Contains("\\v4") || filename.ToLower().Contains("\\4."))
                        {
                            options.ClrVersionWasSet = true;
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
                if (!options.NamedArgsHasBeenSet)
                    options.AllowNamedArguments = true;
            }
            else
            {
                if (!options.NamedArgsHasBeenSet)
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
                    options.TargetDLL == XSharpTargetDLL.XPP || options.TargetDLL == XSharpTargetDLL.RT || options.TargetDLL == XSharpTargetDLL.VFP) {
                    // Ok
                    withRT = true;
                }
                else {
                    AddDiagnostic(diagnostics, ErrorCode.ERR_DialectRequiresReferenceToRuntime, options.Dialect.ToString(),
                        "VulcanRT.DLL and VulcanRTFuncs.DLL or XSharp.Core.DLL and XSharp.RT.DLL");
                    newDialect = XSharpDialect.Core;
                }
            }
            if (! withRT)
            {
                if (options.Vo5)
                {
                    AddDiagnostic(diagnostics, ErrorCode.ERR_CompilerOptionNotSupportedForDialect, "vo5", "Implicit CLIPPER calling convention", options.Dialect.ToString());
                    options.Vo5 = false;
                }
                if (options.Vo6)
                {
                    AddDiagnostic(diagnostics, ErrorCode.ERR_CompilerOptionNotSupportedForDialect, "vo6", "Implicit pointer conversions", options.Dialect.ToString());
                    options.Vo6 = false;
                }
                if (options.Vo7)
                {
                    AddDiagnostic(diagnostics, ErrorCode.ERR_CompilerOptionNotSupportedForDialect, "vo7", "Implicit casts and Conversions", options.Dialect.ToString());
                    options.Vo7 = false;
                }
                if (options.Vo11)
                {
                    AddDiagnostic(diagnostics, ErrorCode.ERR_CompilerOptionNotSupportedForDialect, "vo11", "Compatible numeric conversions", options.Dialect.ToString());
                    options.Vo11 = false;
                }
                if (options.Vo12)
                {
                    AddDiagnostic(diagnostics, ErrorCode.ERR_CompilerOptionNotSupportedForDialect, "vo12", "Clipper Integer divisions", options.Dialect.ToString());
                    options.Vo12 = false;
                }
                if (options.Vo13 )
                {
                    AddDiagnostic(diagnostics, ErrorCode.ERR_CompilerOptionNotSupportedForDialect, "vo13", "VO Compatible string comparisons", options.Dialect.ToString());
                    options.Vo13 = false;
                }
                if (options.Vo14 )
                {
                    AddDiagnostic(diagnostics, ErrorCode.ERR_CompilerOptionNotSupportedForDialect, "vo14", "Float literal Values", options.Dialect.ToString());
                    options.Vo14 = false;
                }
                if (options.Vo15)
                {
                    AddDiagnostic(diagnostics, ErrorCode.ERR_CompilerOptionNotSupportedForDialect, "vo15", "Allow untyped Locals and return types", options.Dialect.ToString());
                    options.Vo15 = false;
                }
                if (options.Vo16)
                {
                    AddDiagnostic(diagnostics, ErrorCode.ERR_CompilerOptionNotSupportedForDialect, "vo16", "Generate Clipper calling convention constructors for classes without constructor", options.Dialect.ToString());
                    options.Vo16 = false;
                }
            }
            else
            {
                if (!options.ExplicitVO15)
                {
                    options.Vo15 = true;            // Untyped allowed
                }
                if (! options.ExplicitVO9 && options.Dialect == XSharpDialect.FoxPro)
                {
                    options.Vo9 = true;             // generate default return values
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
