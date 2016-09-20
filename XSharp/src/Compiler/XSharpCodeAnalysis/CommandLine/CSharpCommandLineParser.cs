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
using System.Collections.Generic;

namespace Microsoft.CodeAnalysis.CSharp
{

    public partial class CSharpCommandLineParser : CommandLineParser
    {
        private XSharpSpecificCompilationOptions options;

        public XSharpSpecificCompilationOptions XSharpSpecificCompilationOptions
        {
            get 
            {
                return options;
            }
        }
        void ResetXSharpCommandlineOptions()
        {
            options = new XSharpSpecificCompilationOptions();
        }
        bool hasSeenOvf = false;
        string previousArgument = string.Empty;
        bool previousOvfValue = false;
        bool ParseXSharpArgument(ref string name, ref string value, string arg, List<Diagnostic> diagnostics)
        {
            if (options == null)
                options = new XSharpSpecificCompilationOptions();

            bool handled = true;
            bool positive = !name.EndsWith("-");
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
                    options.CompactFramework = positive;
                    OptionNotImplemented(diagnostics, "cf", "Compiling for Compact Framework");
                    break;
                case "creatingruntime":
                    options.CreatingRuntime = true;
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
                    OptionNotImplemented(diagnostics, "clr", "Specify CLR version");
                    break;
                case "cs": 
                    options.CaseSensitive = positive;
                    OptionNotImplemented(diagnostics, "cs", "Case Sensitivity");
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
                case "norun":   
                    options.NoRun = positive;
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
                    if (hasSeenOvf && positive != previousOvfValue)
                    {
                        AddDiagnostic(diagnostics, ErrorCode.ERR_ConflictingCommandLineOptions, arg, previousArgument);
                    }
                    previousArgument = arg;
                    previousOvfValue = positive;
                    hasSeenOvf = true;
                    if (positive)
                        name = "checked+";
                    else
                        name = "checked-";
                    handled = false;
                    break;
                case "ppo":
                    options.PreProcessorOutput = positive;
                    OptionNotImplemented(diagnostics, "ppo", "Write preprocessor output to a file");
                    break;
                case "r":
                case "reference":
                    if (!string.IsNullOrEmpty(value))
                    { 
                        if (value.ToLowerInvariant().Contains("vulcanrtfuncs.dll"))
                            options.VulcanRTFuncsIncluded = true;
                        if (value.ToLowerInvariant().Contains("vulcanrt.dll"))
                            options.VulcanRTIncluded = true;
                    }
                    handled = false;
                    break;
                case "showincludes":
                    options.ShowIncludes = positive;
                    OptionNotImplemented(diagnostics, "showincludes", "Show list of included include files");
                    break;
                case "vo1":     // Init & Axit mapped to .ctor and .dtor
                    options.Vo1 = positive;
                    OptionNotImplemented(diagnostics, "vo1", "VO Init and Axit methods");
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
                    OptionNotImplemented(diagnostics, "vo6", "resolve typed function PTR to PTR");
                    break;
                case "vo7":     // Compatible implicit cast & conversion
                    options.Vo7 = positive;
                    OptionNotImplemented(diagnostics, "vo7", "VO compatible implicit casts and conversions");
                    break;
                case "vo8":     // Compatible preprocessor
                    options.Vo8 = positive;
                    break;
                case "vo9":     // Allow missing RETURN
                    options.Vo9 = positive;
                    break;
                case "vo10":    // Compatible IIF 
                    options.Vo10 = positive;
                    break;
                case "vo11":    // VO arithmetic conversions
                    options.Vo11 = positive;
                    OptionNotImplemented(diagnostics, "vo11", "VO compatible artihmetic conversions");
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
                    break;
                case "wx":       // disable warning
                    name = "warnaserror+";
                    handled = false;
                    break;
                default:
                    handled = false;
                    break;

            }
            return handled;
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
                    dialect = XSharpDialect.FoxPro;
                    return true;

                case "harbour":
                case "xharbour":
                    dialect = XSharpDialect.Harbour;
                    return true;
                case "xbase++":
                case "xbasepp":
                    dialect = XSharpDialect.XBasePP;
                    return true;
                default:
                    dialect = XSharpDialect.Core;
                    return false;
            }
        }

        private void ValidateXSharpSettings(List<Diagnostic> diagnostics) {
            bool isVo = false;
            var newDialect = options.Dialect;
            if (options.Dialect == XSharpDialect.VO || options.Dialect == XSharpDialect.Vulcan) {
                if (options.VulcanRTFuncsIncluded && options.VulcanRTIncluded) {
                    // Ok;
                    isVo = true;
                }
                else if(options.CreatingRuntime) {
                    // Ok
                    isVo = true;
                }
                else {
                    AddDiagnostic(diagnostics, ErrorCode.ERR_DialectRequiresReferenceToRuntime, options.Dialect.ToString(), "VulcanRT.DLL and VulcanRTFuncs.DLL");
                    newDialect = XSharpDialect.Core;
                }
            }
            if (! isVo)
            {
                if (options.Vo5)
                {
                    AddDiagnostic(diagnostics, ErrorCode.ERR_CompilerOptionNotSupportedForDialect, "vo5", "Implicit CLIPPER calling convention", options.Dialect.ToString());
                    options.Vo5 = false;
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
            }
            options.Dialect = newDialect;
        }

        private void OptionNotImplemented(List<Diagnostic> diagnostics, string option, string description )
        {
            AddDiagnostic(diagnostics, ErrorCode.WRN_CompilerOptionNotImplementedYet, option, description);
        }
        
    }
}