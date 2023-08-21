// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Runtime.InteropServices;
using System.Threading;
using Microsoft.CodeAnalysis.CommandLine;
#if XSHARP
using Microsoft.CodeAnalysis.CSharp;
#endif

namespace Microsoft.CodeAnalysis.CompilerServer
{
    internal static class BuildProtocolUtil
    {
        internal static RunRequest GetRunRequest(BuildRequest req)
        {
            string? currentDirectory;
            string? libDirectory;
            string? tempDirectory;
            string[] arguments = GetCommandLineArguments(req, out currentDirectory, out tempDirectory, out libDirectory);
            string language = "";
            switch (req.Language)
            {
                case RequestLanguage.CSharpCompile:
                    language = LanguageNames.CSharp;
                    break;
                case RequestLanguage.VisualBasicCompile:
                    language = LanguageNames.VisualBasic;
                    break;
            }

            return new RunRequest(language, currentDirectory, tempDirectory, libDirectory, arguments);
        }

        internal static string[] GetCommandLineArguments(BuildRequest req, out string? currentDirectory, out string? tempDirectory, out string? libDirectory)
        {
            currentDirectory = null;
            libDirectory = null;
            tempDirectory = null;
            List<string> commandLineArguments = new List<string>();

            foreach (BuildRequest.Argument arg in req.Arguments)
            {
                if (arg.ArgumentId == BuildProtocolConstants.ArgumentId.CurrentDirectory)
                {
                    currentDirectory = arg.Value;
                }
                else if (arg.ArgumentId == BuildProtocolConstants.ArgumentId.TempDirectory)
                {
                    tempDirectory = arg.Value;
                }
                else if (arg.ArgumentId == BuildProtocolConstants.ArgumentId.LibEnvVariable)
                {
#if XSHARP
                    string tmp = arg.Value ?? "";
                    if (tmp.Contains(":::"))
                    {
                        var values = tmp.Split(new string[] { ":::" }, StringSplitOptions.None);
                        libDirectory = values[0];
                        if (values.Length == 4)
                        {
                            XSharpSpecificCompilationOptions.SetDefaultIncludeDir(values[1]);
                            XSharpSpecificCompilationOptions.SetWinDir(values[2]);
                            XSharpSpecificCompilationOptions.SetSysDir(values[3]);
                        }
                    }
#else
                    libDirectory = arg.Value;
#endif
                }
                else if (arg.ArgumentId == BuildProtocolConstants.ArgumentId.CommandLineArgument)
                {
                    if (arg.Value is object)
                    {
                        int argIndex = arg.ArgumentIndex;
                        while (argIndex >= commandLineArguments.Count)
                            commandLineArguments.Add("");
                        commandLineArguments[argIndex] = arg.Value;
                    }
                }
            }

            return commandLineArguments.ToArray();
        }
    }
}
