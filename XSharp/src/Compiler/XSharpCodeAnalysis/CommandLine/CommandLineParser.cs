// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using Microsoft.CodeAnalysis.Emit;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;

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

        bool ParseXSharpArgument(ref string name, ref string value, List<Diagnostic> diagnostics)
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
                case "a":   // AST ? Map to analyzer ? (analyzer)
                    name = "analyzer";
                    handled = false;
                    break;
                case "az":  
                    options.ArrayZero = positive;
                    break;
                case "cf":  
                    options.CompactFramework = positive;
                    break;
                case "clr": // CLR
                case "cs": 
                    options.CaseSensitive = positive;
                    break;
                case "i":   
                    if (value == null)
                    {
                        AddDiagnostic(diagnostics, ErrorCode.ERR_SwitchNeedsString, MessageID.IDS_Text.Localize(), "/i:");
                    }
                    else
                    {
                        options.IncludePaths = value;
                    }
                    break;
                    
                case "ild": // Ildasm
                    options.IlDasm = positive;
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
                    if (positive)
                        name = "checked+";
                    else
                        name = "checked-";
                    handled = false;
                    break;
                case "pev":     
                    options.PeVerify = positive;
                    break;
                case "ppo":
                    options.PreProcessorOutput = positive;
                    break;
                case "s":    
                    options.SyntaxCheck = positive;
                    break;
                case "showincludes":
                    options.ShowIncludes = positive;
                    break;
                case "time":
                    options.Time = positive;
                    break;
                case "v":     
                    options.Verbose = positive;
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
                    break;
                case "vo10":    // Compatible IIF 
                    options.Vo10 = positive;
                    break;
                case "vo11":    // VO arithmetic conversions
                    options.Vo11 = positive;
                    break;
                case "vo12":    // Clipper integer divisions
                    options.Vo12 = positive;
                    break;
                case "vo13":    // VO String comparisons
                    options.Vo13 = positive;
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
    }
}