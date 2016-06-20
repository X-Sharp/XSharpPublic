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
using Microsoft.VisualStudio.Shell.Interop;
using Roslyn.Utilities;
using Microsoft.Win32;
namespace Microsoft.CodeAnalysis.CSharp.CommandLine
{
    internal sealed class Xsc : CSharpCompiler
    {
        internal Xsc(string responseFile, string clientDirectory, string baseDirectory, string sdkDirectory, string[] args, IAnalyzerAssemblyLoader analyzerLoader)
            : base(CSharpCommandLineParser.Default, responseFile, args, clientDirectory, baseDirectory, sdkDirectory, Environment.GetEnvironmentVariable("LIB"), analyzerLoader)
        {
        }

        internal static int Run(string clientDirectory, string sdkDirectory, string[] args, IAnalyzerAssemblyLoader analyzerLoader)
        {
            FatalError.Handler = FailFast.OnFatalException;
            var includeDir = Environment.GetEnvironmentVariable("INCLUDE");
            string XSharpIncludeDir = String.Empty;
            string VulcanIncludeDir = string.Empty;
            try {
                string key;
                if (Environment.Is64BitProcess)
                    key = @"HKEY_LOCAL_MACHINE\" + global::XSharp.Constants.RegistryKey64;
                else
                    key = @"HKEY_LOCAL_MACHINE\" + global::XSharp.Constants.RegistryKey;
                XSharpIncludeDir = (string)Registry.GetValue(key, global::XSharp.Constants.RegistryValue, "");
            }
            catch (Exception ) { }
            try {
                string key;
                if(Environment.Is64BitProcess)
                    key = @"HKEY_LOCAL_MACHINE\SOFTWARE\Wow6432Node\Grafx\Vulcan.NET";
                else
                    key = @"HKEY_LOCAL_MACHINE\SOFTWARE\Grafx\Vulcan.NET";
                VulcanIncludeDir = (string)Registry.GetValue(key, "InstallPath", "");
            } catch(Exception) { }
            if(!String.IsNullOrEmpty(XSharpIncludeDir))
            {
                if (!XSharpIncludeDir.EndsWith("\\"))
                    XSharpIncludeDir += @"\";
                XSharpIncludeDir += @"Include\";
            }

            if(!String.IsNullOrEmpty(VulcanIncludeDir)) {
                if(!VulcanIncludeDir.EndsWith("\\"))
                    VulcanIncludeDir += @"\";
                VulcanIncludeDir += @"Include\";
            }
            includeDir = includeDir ?? "" + XSharpIncludeDir;
            if(!string.IsNullOrEmpty(VulcanIncludeDir))
                includeDir += ";" + VulcanIncludeDir;
            XSharpSpecificCompilationOptions.SetDefaultIncludeDir(includeDir);
            XSharpSpecificCompilationOptions.SetWinDir(Environment.GetFolderPath(Environment.SpecialFolder.Windows));
            XSharpSpecificCompilationOptions.SetSysDir(Environment.GetFolderPath(Environment.SpecialFolder.System));
            var responseFile = Path.Combine(clientDirectory, CSharpCompiler.ResponseFileName);
            Xsc compiler = new Xsc(responseFile, clientDirectory, Directory.GetCurrentDirectory(), sdkDirectory, args, analyzerLoader);

            return ConsoleUtil.RunWithOutput(compiler.Arguments.Utf8Output, (textWriterOut, _) => compiler.Run(textWriterOut));
        }

        protected override uint GetSqmAppID()
        {
            return SqmServiceProvider.CSHARP_APPID;
        }

        protected override void CompilerSpecificSqm(IVsSqmMulti sqm, uint sqmSession)
        {
            sqm.SetDatapoint(sqmSession, SqmServiceProvider.DATAID_SQM_ROSLYN_COMPILERTYPE, (uint)SqmServiceProvider.CompilerType.Compiler);
            sqm.SetDatapoint(sqmSession, SqmServiceProvider.DATAID_SQM_ROSLYN_LANGUAGEVERSION, (uint)Arguments.ParseOptions.LanguageVersion);
            sqm.SetDatapoint(sqmSession, SqmServiceProvider.DATAID_SQM_ROSLYN_WARNINGLEVEL, (uint)Arguments.CompilationOptions.WarningLevel);

            //Project complexity # of source files, # of references
            sqm.SetDatapoint(sqmSession, SqmServiceProvider.DATAID_SQM_ROSLYN_SOURCES, (uint)Arguments.SourceFiles.Length);
            sqm.SetDatapoint(sqmSession, SqmServiceProvider.DATAID_SQM_ROSLYN_REFERENCES, (uint)Arguments.ReferencePaths.Length);
        }

        public override Compilation CreateCompilation(TextWriter consoleOutput, TouchedFileLogger touchedFilesLogger, ErrorLogger errorLogger)
        {
            var result = base.CreateCompilation(consoleOutput, touchedFilesLogger, errorLogger);
            return result;
        }
    }
}
