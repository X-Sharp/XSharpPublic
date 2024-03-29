﻿//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

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
#if BOOTSTRAP
            ExitingTraceListener.Install();
#endif

            using var logger = new CompilerServerLogger();
            return BuildClient.Run(args, RequestLanguage.CSharpCompile, Xsc.Run, logger);
        }

        public static int Run(string[] args, string clientDir, string workingDir, string sdkDir, string tempDir, TextWriter textWriter, IAnalyzerAssemblyLoader analyzerLoader)
            => Xsc.Run(args, new BuildPaths(clientDir: clientDir, workingDir: workingDir, sdkDir: sdkDir, tempDir: tempDir), textWriter, analyzerLoader);
    }
}
