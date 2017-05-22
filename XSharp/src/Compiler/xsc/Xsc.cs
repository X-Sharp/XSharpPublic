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

