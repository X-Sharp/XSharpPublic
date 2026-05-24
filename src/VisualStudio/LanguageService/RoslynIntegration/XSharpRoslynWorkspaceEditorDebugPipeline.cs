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

        public void Terminate()
        {
            XSettings.RoslynEditorDebugPipelineInitialized = false;
        }
    }
}
