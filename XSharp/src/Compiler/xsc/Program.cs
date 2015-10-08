// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

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
            System.Diagnostics.Trace.Listeners.Add(new System.Diagnostics.ConsoleTraceListener());
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
