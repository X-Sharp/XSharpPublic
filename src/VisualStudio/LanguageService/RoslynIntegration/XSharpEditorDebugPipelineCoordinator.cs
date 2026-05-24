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
    public static class XSharpEditorDebugPipelineCoordinator
    {
        private const string RoslynPipelineEnvVar = "XSHARP_USE_ROSLYN_PIPELINE";
        private static IXSharpEditorDebugPipeline _pipeline = new XSharpLegacyEditorDebugPipeline();

        private static bool IsRoslynPipelineRequested()
        {
            var env = Environment.GetEnvironmentVariable(RoslynPipelineEnvVar);
            if (!string.IsNullOrEmpty(env))
            {
                if (string.Equals(env, "1", StringComparison.OrdinalIgnoreCase) ||
                    string.Equals(env, "true", StringComparison.OrdinalIgnoreCase) ||
                    string.Equals(env, "yes", StringComparison.OrdinalIgnoreCase))
                {
                    return true;
                }
                if (string.Equals(env, "0", StringComparison.OrdinalIgnoreCase) ||
                    string.Equals(env, "false", StringComparison.OrdinalIgnoreCase) ||
                    string.Equals(env, "no", StringComparison.OrdinalIgnoreCase))
                {
                    return false;
                }
            }

            return XSettings.UseRoslynEditorDebugPipeline;
        }

        private static IXSharpEditorDebugPipeline CreatePipeline()
            => IsRoslynPipelineRequested()
                ? new XSharpRoslynWorkspaceEditorDebugPipeline()
                : new XSharpLegacyEditorDebugPipeline();

        public static async Task InitializeAsync(AsyncPackage package, CancellationToken cancellationToken)
        {
            _pipeline = CreatePipeline();
            await _pipeline.InitializeAsync(package, cancellationToken).ConfigureAwait(false);
        }

        public static void OnBeforeDebugLaunch(string projectFileName)
        {
            _pipeline?.OnBeforeDebugLaunch(projectFileName);
        }

        public static void OnProjectParseOptionsChanged(string projectFileName, XParseOptions parseOptions)
        {
            _pipeline?.OnProjectParseOptionsChanged(projectFileName, parseOptions);
        }

        public static void Terminate()
        {
            _pipeline?.Terminate();
            _pipeline = new XSharpLegacyEditorDebugPipeline();
        }
    }
}
