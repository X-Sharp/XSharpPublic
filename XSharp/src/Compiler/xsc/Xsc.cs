//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using Roslyn.Utilities;
using Microsoft.Win32;
using System.Runtime.InteropServices;
using System.Diagnostics;
using Microsoft.CodeAnalysis.CommandLine;

namespace Microsoft.CodeAnalysis.CSharp.CommandLine
{
    internal sealed partial class Xsc : CSharpCompiler
    {
        internal Xsc(string responseFile, BuildPaths buildPaths, string[] args, IAnalyzerAssemblyLoader analyzerLoader)
            : base(CSharpCommandLineParser.Default, responseFile, args, buildPaths, Environment.GetEnvironmentVariable("LIB"), analyzerLoader)
        {
        }

        // Original csc Run() function
        /*
        internal static int Run(string[] args, BuildPaths buildPaths, TextWriter textWriter, IAnalyzerAssemblyLoader analyzerLoader)
        {
            FatalError.Handler = FailFast.OnFatalException;

            var responseFile = Path.Combine(buildPaths.ClientDirectory, CSharpCompiler.ResponseFileName);
            var compiler = new Xsc(responseFile, buildPaths, args, analyzerLoader);
            return ConsoleUtil.RunWithUtf8Output(compiler.Arguments.Utf8Output, textWriter, tw => compiler.Run(tw));
        }
        */


        internal static int Run(string[] args, BuildPaths buildPaths, TextWriter textWriter, IAnalyzerAssemblyLoader analyzerLoader)
        {
            FatalError.Handler = FailFast.OnFatalException;
            string[] paths = GetPaths(); 
            XSharpSpecificCompilationOptions.SetDefaultIncludeDir(paths[0]);
            XSharpSpecificCompilationOptions.SetWinDir(paths[1]);
            XSharpSpecificCompilationOptions.SetSysDir(paths[2]);
            var responseFile = Path.Combine(buildPaths.ClientDirectory, CSharpCompiler.ResponseFileName);
            var compiler = new Xsc(responseFile, buildPaths, args, analyzerLoader);
            var result = ConsoleUtil.RunWithUtf8Output(compiler.Arguments.Utf8Output, textWriter, tw => compiler.Run(tw));

            return result;
        }

    }
    
}

