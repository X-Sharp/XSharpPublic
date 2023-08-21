//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#nullable disable
using System.IO;
using System.Runtime.InteropServices;
using System;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace Microsoft.CodeAnalysis.CSharp
{
    public sealed partial class CSharpCompilationOptions
    {
#if VSPARSER
        public bool AllowUnsafe { get; private set; }
#endif
        #region private fields (need to be accessed with HasOption())
        private bool ArrayZero;
        private bool AllowDotForInstanceMembers;
        private bool VONullStrings;
        private bool VOImplicitCastsAndConversions;
        private bool MemVars;
        private bool UndeclaredMemVars;

        #endregion

        internal bool SuppressVo4;
        public bool MacroScript { get; private set; }
        public bool VirtualInstanceMethods { get; private set; }
        //public bool VOAllowMissingReturns { get; private set; }  // Handled in the parser
        public bool VOArithmeticConversions { get; private set; }
        //public bool VOClipperCallingConvention { get; private set; }// Handled in the parser
        //public bool VOClipperConstructors { get; private set; }// Handled in the parser
        //public bool VOClipperIntegerDivisions { get; private set; }// Handled in the parser
        public bool VOCompatibleIIF { get; private set; }
        //public bool VOFloatConstants { get; private set; }// Handled in the parser
        //public bool VoInitAxitMethods { get; private set; }// Handled in the parser
        //public bool VOPreprocessorBehaviour { get; private set; }// Handled in the parser
        public bool VOResolveTypedFunctionPointersToPtr { get; private set; }
        public bool VOSignedUnsignedConversion { get; private set; }
        public bool VOStringComparisons { get; private set; }
        //public bool VOUntypedAllowed { get; private set; }// Handled in the parser

        //public bool XPPInheritFromAbstract { get; private set; } // Handled in the parser
        //public bool FoxInheritUnknown { get; private set; } // Handled in the parser
        public XSharpDialect Dialect { get; private set; }
        public bool ImplicitNameSpace { get; private set; }
        //bool InitLocals { get; set; }
        public bool LateBinding { get; private set; }
        public bool EnforceSelf { get; private set; }
        public bool HasDefaultTree { get; set; } = false;
        public bool HasRuntime { get { return this.Dialect.NeedsRuntime(); } }
        public bool FoxArraySupport { get; private set; } = false;
        public XSharpTargetDLL TargetDLL { get; private set; }
        public bool UseNativeVersion { get; private set; } = false;

        public RuntimeAssemblies RuntimeAssemblies;
        public bool XSharpRuntime => RuntimeAssemblies.HasFlag(RuntimeAssemblies.XSharpRT) |
            RuntimeAssemblies.HasFlag(RuntimeAssemblies.XSharpCore);
        // Access to the console output
        public TextWriter ConsoleOutput { get; private set; }
        public bool NoWin32Manifest { get; private set; }
        public void SetXSharpSpecificOptions(XSharpSpecificCompilationOptions opt)
        {
            if (opt != null)
            {
                ArrayZero = opt.ArrayZero;
                AllowDotForInstanceMembers = opt.AllowDotForInstanceMembers;
                //VoInitAxitMethods = opt.Vo1;              // Handled in the parser
                VONullStrings = opt.Vo2;
                VirtualInstanceMethods = opt.Vo3;
                VOSignedUnsignedConversion = opt.Vo4;
                //VOClipperCallingConvention = opt.Vo5;     // Handled in the parser
                VOResolveTypedFunctionPointersToPtr = opt.Vo6;
                VOImplicitCastsAndConversions = opt.Vo7;
                //VOPreprocessorBehaviour = opt.Vo8;        // Handled in the parser
                //VOAllowMissingReturns = opt.Vo9;          // Handled in the parser
                VOCompatibleIIF = opt.Vo10;
                VOArithmeticConversions = opt.Vo11;
                //VOClipperIntegerDivisions = opt.Vo12;     // Handled in the parser
                VOStringComparisons = opt.Vo13;
                //VOFloatConstants = opt.Vo14;              // Handled in the parser
                //VOUntypedAllowed = opt.Vo15;              // Handled in the parser
                //VOClipperConstructors = opt.Vo16;         // Handled in the parser
                //XPPInheritFromAbstract = opt.Xpp1;        // Handled in the parser
                //FoxInheritUnknown= opt.Fox1;              // Handled in the parser
                //EnforceVirtual = opt.EnforceVirtual;      // Handled in the parser
                FoxArraySupport = opt.Fox2;
                Dialect = opt.Dialect;
                ImplicitNameSpace = opt.ImplicitNameSpace;
                LateBinding = opt.LateBinding;
                EnforceSelf = opt.EnforceSelf;
                UndeclaredMemVars = opt.UndeclaredMemVars;
                MemVars = opt.MemVars;
                TargetDLL = opt.TargetDLL;
                RuntimeAssemblies = opt.RuntimeAssemblies;
                //InitLocals = opt.InitLocals;
                AllowUnsafe = opt.AllowUnsafe;
                UseNativeVersion = opt.UseNativeVersion;

            }
        }
#if !VSPARSER
        internal bool SupportsMemvars(SyntaxNode syntax)
        {
            return this.Dialect.SupportsMemvars() && HasOption(CompilerOption.MemVars, syntax);
        }
        internal bool LateBindingOrFox(SyntaxNode syntax)
        {
            if (!HasRuntime)
                return false;
            if (Dialect == XSharpDialect.FoxPro)
                return true;
            return CheckOption(CompilerOption.LateBinding, LateBinding, syntax);
        }

        internal bool HasOption(CompilerOption option, SyntaxNode syntax)
        {
            switch (option)
            {
                case CompilerOption.ArrayZero:
                    return CheckOption(option, ArrayZero, syntax);

                case CompilerOption.AllowDotForInstanceMembers:
                    return CheckOption(option, AllowDotForInstanceMembers, syntax);

                case CompilerOption.LateBinding:
                    return CheckOption(option, LateBinding, syntax);

                case CompilerOption.Overflow:
                    return CheckOption(option, CheckOverflow, syntax);


                case CompilerOption.MemVars:
                    return CheckOption(option, MemVars, syntax);

                case CompilerOption.EnforceSelf:
                    return CheckOption(option, EnforceSelf, syntax);

                case CompilerOption.UndeclaredMemVars:
                    return CheckOption(option, UndeclaredMemVars, syntax);

                case CompilerOption.Vo2: // NullStrings:    // vo2
                    return CheckOption(option, VONullStrings, syntax);

                case CompilerOption.Vo3: // VirtualInstanceMethods:
                    return CheckOption(option, VirtualInstanceMethods, syntax);

                case CompilerOption.Vo4: // vo4
                    return !SuppressVo4 && CheckOption(option, VOSignedUnsignedConversion, syntax);

                case CompilerOption.Vo6: //  ResolveTypedFunctionPointersToPtr: // vo6
                    return CheckOption(option, VOResolveTypedFunctionPointersToPtr, syntax);

                case CompilerOption.Vo7: //  ImplicitCastsAndConversions: // vo7
                    return CheckOption(option, VOImplicitCastsAndConversions, syntax);

                case CompilerOption.Vo10: // CompatibleIIF:  // vo10
                    return CheckOption(option, VOCompatibleIIF, syntax);

                case CompilerOption.Vo11: // ArithmeticConversions: // vo11
                    return CheckOption(option, VOArithmeticConversions, syntax);

                case CompilerOption.Vo13: //  StringComparisons: // vo13
                    return CheckOption(option, VOStringComparisons, syntax);

                case CompilerOption.Fox2: // Fox Array Support
                    return CheckOption(option, FoxArraySupport, syntax);

                // other options are not handled or only handled during parsing
                case CompilerOption.Vo1: // InitAxitConstructorDestructor:
                case CompilerOption.Vo5: //  ClipperCallingConvention:
                case CompilerOption.Vo9: //  AllowMissingReturns:   
                case CompilerOption.Vo12: // ClipperIntegerDivisions:   
                case CompilerOption.Vo14: // FloatConstants: 
                case CompilerOption.Vo15: // UntypedAllowed: 
                case CompilerOption.Vo16: // DefaultClipperContructors: 
                case CompilerOption.EnforceOverride:
                    return false;

            }
            return false;
        }

        private bool CheckOption(CompilerOption option, bool defaultValue, SyntaxNode node)
        {
            bool result = defaultValue;
            if (node is CSharpSyntaxNode csn)
            {
                var unit = csn.SyntaxTree.GetRoot() as CompilationUnitSyntax;
                if (unit != null && unit.PragmaOptions != null)
                {
                    var context = csn.XNode as XSharpParserRuleContext;
                    int line = context.Start.Line;
                    foreach (var pragmaoption in unit.PragmaOptions)
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
            }
            return result;
        }

#endif
        public void SetOptions(CSharpCommandLineArguments opt)
        {
#if !VSPARSER
            NoWin32Manifest = opt.NoWin32Manifest;
#endif
        }

        public void SetXSharpSpecificOptions(CSharpCompilationOptions opt)
        {
            ArrayZero = opt.ArrayZero;
            AllowDotForInstanceMembers = opt.AllowDotForInstanceMembers;
            MacroScript = opt.MacroScript;
            Dialect = opt.Dialect;
            ImplicitNameSpace = opt.ImplicitNameSpace;
            LateBinding = opt.LateBinding;
            TargetDLL = opt.TargetDLL;
            RuntimeAssemblies = opt.RuntimeAssemblies;
            //EnforceVirtual = opt.EnforceVirtual;  // Handled in the parser
            EnforceSelf = opt.EnforceSelf;
            //VoInitAxitMethods = opt.VoInitAxitMethods; // vo1 // Handled in the parser
            VONullStrings = opt.VONullStrings; // vo2
            VirtualInstanceMethods = opt.VirtualInstanceMethods; // vo3   
            VOSignedUnsignedConversion = opt.VOSignedUnsignedConversion; // vo4
            //VOClipperCallingConvention = opt.VOClipperCallingConvention;  // vo5  // Handled in the parser
            VOResolveTypedFunctionPointersToPtr = opt.VOResolveTypedFunctionPointersToPtr; // vo6
            VOImplicitCastsAndConversions = opt.VOImplicitCastsAndConversions; // vo7
            //VOPreprocessorBehaviour = opt.VOPreprocessorBehaviour; // vo8 // Handled in the parser
            //VOAllowMissingReturns = opt.VOAllowMissingReturns; // vo9 // Handled in the parser
            VOCompatibleIIF = opt.VOCompatibleIIF; // vo10    
            VOArithmeticConversions = opt.VOArithmeticConversions; // vo11
            //VOClipperIntegerDivisions = opt.VOClipperIntegerDivisions; // vo12    // Handled in the parser
            VOStringComparisons = opt.VOStringComparisons; // vo13
            //VOFloatConstants = opt.VOFloatConstants; // vo14  // Handled in the parser
            //VOUntypedAllowed = opt.VOUntypedAllowed; // vo15  // Handled in the parser
            //VOClipperConstructors = opt.VOClipperConstructors; // vo16// Handled in the parser
            FoxArraySupport = opt.FoxArraySupport;
            ConsoleOutput = opt.ConsoleOutput;
            UndeclaredMemVars = opt.UndeclaredMemVars;
            MemVars = opt.MemVars;
            //InitLocals = opt.InitLocals;
            AllowUnsafe = opt.AllowUnsafe;
            UseNativeVersion = opt.UseNativeVersion;
            NoWin32Manifest = opt.NoWin32Manifest;
        }

        internal CSharpCompilationOptions WithXSharpSpecificOptions(XSharpSpecificCompilationOptions opt)
        {
            SetXSharpSpecificOptions(opt);
            return this;
        }
#if !VSPARSER
        public CSharpCompilationOptions WithMacroScript(bool macroScript)
        {
            if (macroScript == this.MacroScript)
            {
                return this;
            }
            var result = new CSharpCompilationOptions(this);
            result.SetXSharpSpecificOptions(this);
            result.MacroScript = macroScript;
            return result;
        }
        internal static void FixResources(CommandLineArguments args, Compilation compilation)
        {
            if (!string.IsNullOrEmpty(args.Win32ResourceFile) ||
                !string.IsNullOrEmpty(args.Win32Icon) ||
                !string.IsNullOrEmpty(args.Win32Manifest))
            {
                var file = Path.Combine(args.OutputDirectory, args.OutputFileName);
                if (File.Exists(file))
                {
                    var hRes = BeginUpdateResource(file, false);
                    if (hRes != IntPtr.Zero)
                    {
                        var res = EndUpdateResource(hRes, false);
                    }
                    if (compilation.HasStrongName)
                    {
                        // We have to resign the assembly because updating the resources
                        // may have invalidated the signature.
                        var provider = new DesktopStrongNameProvider();
                        provider.SignFile(compilation.StrongNameKeys, file);
                    }
                }
            }
        }

        [DllImport("kernel32.dll", SetLastError = true)]
        static extern IntPtr BeginUpdateResource(string pFileName, [MarshalAs(UnmanagedType.Bool)] bool bDeleteExistingResources);
        [DllImport("kernel32.dll", SetLastError = true)]
        static extern bool EndUpdateResource(IntPtr hUpdate, bool fDiscard);
#endif
    }
}
