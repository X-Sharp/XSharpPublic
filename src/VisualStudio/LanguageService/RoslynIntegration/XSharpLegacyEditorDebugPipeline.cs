//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System.Threading;
using System.Threading.Tasks;
using Microsoft.VisualStudio.Shell;
using XSharp.Settings;

namespace XSharp.LanguageService.RoslynIntegration
{
    internal sealed class XSharpLegacyEditorDebugPipeline : IXSharpEditorDebugPipeline
    {
        public Task InitializeAsync(AsyncPackage package, CancellationToken cancellationToken)
        {
            XSettings.RoslynEditorDebugPipelineInitialized = false;
            return Task.CompletedTask;
        }

        public void OnBeforeDebugLaunch(string projectFileName)
        {
        }

        public void Terminate()
        {
        }
    }
}
