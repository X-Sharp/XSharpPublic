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
using System;
using System.IO;
using System.Runtime.InteropServices;
using Microsoft.CodeAnalysis.BuildTasks;
using static Microsoft.CodeAnalysis.CompilerServer.BuildProtocolConstants;

namespace Microsoft.CodeAnalysis.CSharp.CommandLine
{
    public class Program
    {
        public static int Main(string[] args)
        {
#if (DEBUG)
            System.Diagnostics.Debug.Listeners.Add(new System.Diagnostics.ConsoleTraceListener());
            //System.Diagnostics.Trace.Listeners.Add(new System.Diagnostics.ConsoleTraceListener());
#endif
            return BuildClient.RunWithConsoleOutput(
                    args,
                    clientDir: AppDomain.CurrentDomain.BaseDirectory,
                    workingDir: Directory.GetCurrentDirectory(),
                    sdkDir: RuntimeEnvironment.GetRuntimeDirectory(),
                    analyzerLoader: new SimpleAnalyzerAssemblyLoader(),
                    language: RequestLanguage.CSharpCompile,
                    fallbackCompiler: Xsc.Run);
        }
    }
}
