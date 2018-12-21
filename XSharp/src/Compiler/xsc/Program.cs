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
using Microsoft.CodeAnalysis.CommandLine;

namespace Microsoft.CodeAnalysis.CSharp.CommandLine
{
    public class Program
    {
        public static int Main(string[] args)
        {
            try
            {
#if NET46
#if DEBUG
                System.Diagnostics.Debug.Listeners.Add(new System.Diagnostics.ConsoleTraceListener());
#endif
#endif
                return MainCore(args);
            }
            catch (FileNotFoundException e)
            {
                // Catch exception from missing compiler assembly.
                // Report the exception message and terminate the process.
                Console.WriteLine(e.Message);
                return CommonCompiler.Failed;
            }
        }

        private static int MainCore(string[] args)
        {
#if NET46
            var loader = new DesktopAnalyzerAssemblyLoader();
#else
            var loader = new CoreClrAnalyzerAssemblyLoader();
#endif
            return DesktopBuildClient.Run(args, RequestLanguage.CSharpCompile, Xsc.Run, loader);
        }

        public static int Run(string[] args, string clientDir, string workingDir, string sdkDir, string tempDir, TextWriter textWriter, IAnalyzerAssemblyLoader analyzerLoader)
            => Xsc.Run(args, new BuildPaths(clientDir: clientDir, workingDir: workingDir, sdkDir: sdkDir, tempDir: tempDir), textWriter, analyzerLoader);
    }
}
