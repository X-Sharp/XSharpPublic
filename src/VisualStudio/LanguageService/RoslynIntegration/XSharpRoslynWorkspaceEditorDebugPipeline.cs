//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.VisualStudio.Shell;
using XSharp.Settings;
using XSharpModel;

namespace XSharp.LanguageService.RoslynIntegration
{
    internal sealed class XSharpRoslynWorkspaceEditorDebugPipeline : IXSharpEditorDebugPipeline
    {
        public Task InitializeAsync(AsyncPackage package, CancellationToken cancellationToken)
        {
            XSettings.RoslynEditorDebugPipelineInitialized = true;
            XSettings.Information("Roslyn workspace editor/debug pipeline scaffold initialized.");
            return Task.CompletedTask;
        }

        public void OnBeforeDebugLaunch(string projectFileName)
        {
            if (!string.IsNullOrEmpty(projectFileName))
            {
                XSettings.Information("Roslyn pipeline debug launch requested for: " + projectFileName);
            }
        }

        public void OnProjectParseOptionsChanged(string projectFileName, XParseOptions parseOptions)
        {
            if (parseOptions == null)
            {
                return;
            }

            XDebuggerSettings.Dialect = (int)parseOptions.Dialect;
            XDebuggerSettings.ArrayZero = parseOptions.ArrayZero;
            XDebuggerSettings.Vo4 = parseOptions.Vo4;
            XDebuggerSettings.Vo6 = parseOptions.Vo6;
            XDebuggerSettings.Vo7 = parseOptions.Vo7;
            XDebuggerSettings.Vo10 = parseOptions.Vo10;
            XDebuggerSettings.Vo12 = parseOptions.Vo12;
            XDebuggerSettings.Vo13 = parseOptions.Vo13;
            XDebuggerSettings.Vo14 = parseOptions.Vo14;
            XDebuggerSettings.MemVars = parseOptions.SupportsMemvars;
            XDebuggerSettings.UndeclaredMemvars = parseOptions.SupportsUndeclaredMemVars;
            XDebuggerSettings.LateBinding = parseOptions.LateBinding;
            XDebuggerSettings.CaseSensitive = parseOptions.CaseSensitive;

            if (!string.IsNullOrEmpty(projectFileName))
            {
                XSettings.Information("Roslyn pipeline parse options synced for: " + projectFileName);
            }
        }

        public void Terminate()
        {
            XSettings.RoslynEditorDebugPipelineInitialized = false;
        }
    }
}
