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
using System.IO;
using System.Runtime.InteropServices;
using System;
namespace Microsoft.CodeAnalysis.CSharp
{
    public sealed partial class CSharpCompilationOptions
    {
        public bool ArrayZero { get; private set; }
        public int ClrVersion { get; private set; }
        public bool MacroScript { get; private set; }
        public string DefaultIncludeDir { get; set; }
        public string WindowsDir { get; set; }
        public string SystemDir { get; set; }
        public bool VONullStrings { get; private set; }
        public bool VirtualInstanceMethods { get; private set; }
        //public bool VOAllowMissingReturns { get; private set; }  // Handled in the parser
        public bool VOArithmeticConversions { get; private set; }
        //public bool VOClipperCallingConvention { get; private set; }// Handled in the parser
        //public bool VOClipperConstructors { get; private set; }// Handled in the parser
        //public bool VOClipperIntegerDivisions { get; private set; }// Handled in the parser
        public bool VOCompatibleIIF { get; private set; }
        //public bool VOFloatConstants { get; private set; }// Handled in the parser
        public bool VOImplicitCastsAndConversions { get; private set; }
        public bool VOImplicitSignedUnsignedConversions { get; private set; }
        //public bool VoInitAxitMethods { get; private set; }// Handled in the parser
        //public bool VOPreprocessorBehaviour { get; private set; }// Handled in the parser
        public bool VOResolveTypedFunctionPointersToPtr { get; private set; }
        public bool VOSignedUnsignedConversion { get; private set; }
        public bool VOStringComparisons { get; private set; }
        //public bool VOUntypedAllowed { get; private set; }// Handled in the parser
        public XSharpDialect Dialect { get; private set; }
        public bool ImplicitNameSpace { get; private set; }
        public bool LateBinding { get; private set; }
        public bool NoClipCall { get; private set; }
        public bool HasDefaultTree { get; set; } = false;

        public bool IsDialectVO { get { return this.Dialect.IsDialectVO(); } }
        public bool SupportsMemvars { get { return this.Dialect.SupportsMemvars(); } }

        public XSharpTargetDLL TargetDLL { get; private set; }
        //public bool vo1 => VoInitAxitMethods;
        public bool vo2 => VONullStrings;
        public bool vo3 => VirtualInstanceMethods;
        public bool vo4 => VOSignedUnsignedConversion;
        //public bool vo5 => VOClipperCallingConvention;
        public bool vo6 => VOResolveTypedFunctionPointersToPtr;
        public bool vo7 => VOImplicitCastsAndConversions;
        //public bool vo8 => VOPreprocessorBehaviour;
        //public bool vo9 => VOAllowMissingReturns;
        public bool vo10 => VOCompatibleIIF;
        public bool vo11 => VOArithmeticConversions;
        //public bool vo12 => VOClipperIntegerDivisions;
        public bool vo13 => VOStringComparisons;
        //public bool vo14 => VOFloatConstants;
        //public bool vo15 => VOUntypedAllowed;
        //public bool vo16 => VOClipperConstructors;
        public ParseLevel ParseLevel { get; set; } = ParseLevel.Complete;

        public RuntimeAssemblies RuntimeAssemblies ;
        public bool XSharpRuntime => RuntimeAssemblies.HasFlag(RuntimeAssemblies.XSharpVO) |
            RuntimeAssemblies.HasFlag(RuntimeAssemblies.XSharpCore);
        // Access to the console output
        public TextWriter ConsoleOutput { get; private set; }

        public void SetXSharpSpecificOptions(XSharpSpecificCompilationOptions opt)
        {
            if (opt != null)
            {
                ArrayZero = opt.ArrayZero;
                ClrVersion = opt.ClrVersion;
                //VoInitAxitMethods = opt.Vo1;
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
                Dialect = opt.Dialect;
                ImplicitNameSpace = opt.ImplicitNameSpace;
                LateBinding = opt.LateBinding;
                
                ParseLevel = opt.ParseLevel;
                TargetDLL = opt.TargetDLL;
                RuntimeAssemblies = opt.RuntimeAssemblies;
            }
        }

        public void SetOptions(CSharpCommandLineArguments opt)
        {
        }

        public void SetXSharpSpecificOptions(CSharpCompilationOptions opt)
        {
            ArrayZero = opt.ArrayZero;
            ClrVersion = opt.ClrVersion;
            MacroScript = opt.MacroScript;
            DefaultIncludeDir = opt.DefaultIncludeDir;
            WindowsDir = opt.WindowsDir;
            SystemDir = opt.SystemDir;
            Dialect = opt.Dialect;
            ImplicitNameSpace = opt.ImplicitNameSpace;
            LateBinding = opt.LateBinding;
            TargetDLL = opt.TargetDLL;
            RuntimeAssemblies = opt.RuntimeAssemblies;
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
            ConsoleOutput = opt.ConsoleOutput;
            ParseLevel = opt.ParseLevel; 
        }

        internal CSharpCompilationOptions WithXSharpSpecificOptions(XSharpSpecificCompilationOptions opt)
        {
            SetXSharpSpecificOptions(opt);
            return this;
        }
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
                var file = System.IO.Path.Combine(args.OutputDirectory, args.OutputFileName);
                if (System.IO.File.Exists(file))
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
                        using (var inputstream = provider.CreateInputStream())
                        {
                            using (var fs = new FileStream(file, FileMode.Open, FileAccess.Read, FileShare.Read))
                            {
                                fs.CopyTo(inputstream);
                            }
                            using (var outputstream = new FileStream(file, FileMode.Create, FileAccess.Write, FileShare.Write))
                            {
                                provider.SignAssembly(compilation.StrongNameKeys, inputstream, outputstream);
                                outputstream.Flush();
                            }
                        }
                    }
                }
            }

        }

        [DllImport("kernel32.dll", SetLastError = true)]
        static extern IntPtr BeginUpdateResource(string pFileName, [MarshalAs(UnmanagedType.Bool)]bool bDeleteExistingResources);
        [DllImport("kernel32.dll", SetLastError = true)]
        static extern bool EndUpdateResource(IntPtr hUpdate, bool fDiscard);
    }
}
