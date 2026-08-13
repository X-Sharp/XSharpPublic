//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System.ComponentModel.Composition;
using System.Threading;
using System.Threading.Tasks;

using Microsoft.VisualStudio.ProjectSystem;
using Microsoft.VisualStudio.ProjectSystem.Properties;

namespace XSharp.VisualStudio.ProjectSystem
{
    /// <summary>
    /// Provides strongly-typed access to the XSharp-specific MSBuild project
    /// properties that are defined in <see cref="XSharpProjectFileConstants"/>.
    ///
    /// CPS exposes project properties through the <see cref="IProjectPropertiesContext"/>
    /// / <see cref="IProjectProperties"/> abstraction.  This class wraps that
    /// abstraction and adds convenience methods for the XSharp build options.
    /// </summary>
    [Export]
    [AppliesTo(XSharpProjectCapabilities.XSharp)]
    internal sealed class XSharpProjectProperties
    {
        private readonly ConfiguredProject _project;

        [ImportingConstructor]
        public XSharpProjectProperties(ConfiguredProject project)
        {
            _project = project;
        }

        // ─── Helper ─────────────────────────────────────────────────────────────────

        private IProjectProperties GetCommonProperties()
        {
            return _project.Services.ProjectPropertiesProvider
                           .GetCommonProperties();
        }

        private IProjectProperties GetConfigurationProperties()
        {
            return _project.Services.ProjectPropertiesProvider
                           .GetCommonProperties(); // configuration-specific read would use a context
        }

        // ─── Application properties ──────────────────────────────────────────────────

        public Task<string> GetAssemblyNameAsync(CancellationToken ct = default) =>
            GetCommonProperties().GetEvaluatedPropertyValueAsync(
                XSharpProjectFileConstants.AssemblyName);

        public Task<string> GetOutputTypeAsync(CancellationToken ct = default) =>
            GetCommonProperties().GetEvaluatedPropertyValueAsync(
                XSharpProjectFileConstants.OutputType);

        public Task<string> GetRootNamespaceAsync(CancellationToken ct = default) =>
            GetCommonProperties().GetEvaluatedPropertyValueAsync(
                XSharpProjectFileConstants.RootNamespace);

        public Task<string> GetStartupObjectAsync(CancellationToken ct = default) =>
            GetCommonProperties().GetEvaluatedPropertyValueAsync(
                XSharpProjectFileConstants.StartupObject);

        public Task<string> GetApplicationIconAsync(CancellationToken ct = default) =>
            GetCommonProperties().GetEvaluatedPropertyValueAsync(
                XSharpProjectFileConstants.ApplicationIcon);

        // ─── Target framework ────────────────────────────────────────────────────────

        /// <summary>Single-target framework (e.g. <c>net48</c>).</summary>
        public Task<string> GetTargetFrameworkAsync(CancellationToken ct = default) =>
            GetCommonProperties().GetEvaluatedPropertyValueAsync(
                XSharpProjectFileConstants.TargetFramework);

        /// <summary>
        /// Semi-colon-separated list of target frameworks for multi-targeting
        /// (e.g. <c>net48;net6.0-windows</c>).  VS2022+ / CPS only.
        /// </summary>
        public Task<string> GetTargetFrameworksAsync(CancellationToken ct = default) =>
            GetCommonProperties().GetEvaluatedPropertyValueAsync(
                XSharpProjectFileConstants.TargetFrameworks);

        // ─── XSharp dialect / language options ──────────────────────────────────────

        public Task<string> GetDialectAsync(CancellationToken ct = default) =>
            GetCommonProperties().GetEvaluatedPropertyValueAsync(
                XSharpProjectFileConstants.Dialect);

        public Task<string> GetDefineConstantsAsync(CancellationToken ct = default) =>
            GetConfigurationProperties().GetEvaluatedPropertyValueAsync(
                XSharpProjectFileConstants.DefineConstants);

        public Task<string> GetIncludePathsAsync(CancellationToken ct = default) =>
            GetCommonProperties().GetEvaluatedPropertyValueAsync(
                XSharpProjectFileConstants.IncludePaths);

        public Task<string> GetWarningLevelAsync(CancellationToken ct = default) =>
            GetConfigurationProperties().GetEvaluatedPropertyValueAsync(
                XSharpProjectFileConstants.WarningLevel);

        public Task<string> GetNoWarnAsync(CancellationToken ct = default) =>
            GetConfigurationProperties().GetEvaluatedPropertyValueAsync(
                XSharpProjectFileConstants.NoWarn);

        public Task<string> GetTreatWarningsAsErrorsAsync(CancellationToken ct = default) =>
            GetConfigurationProperties().GetEvaluatedPropertyValueAsync(
                XSharpProjectFileConstants.TreatWarningsAsErrors);

        // ─── Build / output ──────────────────────────────────────────────────────────

        public Task<string> GetOutputPathAsync(CancellationToken ct = default) =>
            GetConfigurationProperties().GetEvaluatedPropertyValueAsync(
                XSharpProjectFileConstants.OutputPath);

        public Task<string> GetPlatformTargetAsync(CancellationToken ct = default) =>
            GetConfigurationProperties().GetEvaluatedPropertyValueAsync(
                XSharpProjectFileConstants.PlatformTarget);

        public Task<string> GetOptimizeAsync(CancellationToken ct = default) =>
            GetConfigurationProperties().GetEvaluatedPropertyValueAsync(
                XSharpProjectFileConstants.Optimize);

        public Task<string> GetDocumentationFileAsync(CancellationToken ct = default) =>
            GetConfigurationProperties().GetEvaluatedPropertyValueAsync(
                XSharpProjectFileConstants.DocumentationFile);

        // ─── Debug ───────────────────────────────────────────────────────────────────

        public Task<string> GetDebugTypeAsync(CancellationToken ct = default) =>
            GetConfigurationProperties().GetEvaluatedPropertyValueAsync(
                XSharpProjectFileConstants.DebugType);

        public Task<string> GetDebuggerCommandAsync(CancellationToken ct = default) =>
            GetConfigurationProperties().GetEvaluatedPropertyValueAsync(
                XSharpProjectFileConstants.DebuggerCommand);

        public Task<string> GetDebuggerCommandArgumentsAsync(CancellationToken ct = default) =>
            GetConfigurationProperties().GetEvaluatedPropertyValueAsync(
                XSharpProjectFileConstants.DebuggerCommandArguments);

        public Task<string> GetDebuggerWorkingDirectoryAsync(CancellationToken ct = default) =>
            GetConfigurationProperties().GetEvaluatedPropertyValueAsync(
                XSharpProjectFileConstants.DebuggerWorkingDirectory);

        // ─── Signing ─────────────────────────────────────────────────────────────────

        public Task<string> GetSignAssemblyAsync(CancellationToken ct = default) =>
            GetCommonProperties().GetEvaluatedPropertyValueAsync(
                XSharpProjectFileConstants.SignAssembly);

        public Task<string> GetAssemblyOriginatorKeyFileAsync(CancellationToken ct = default) =>
            GetCommonProperties().GetEvaluatedPropertyValueAsync(
                XSharpProjectFileConstants.AssemblyOriginatorKeyFile);

        // ─── Build events ────────────────────────────────────────────────────────────

        public Task<string> GetPreBuildEventAsync(CancellationToken ct = default) =>
            GetConfigurationProperties().GetEvaluatedPropertyValueAsync(
                XSharpProjectFileConstants.PreBuildEvent);

        public Task<string> GetPostBuildEventAsync(CancellationToken ct = default) =>
            GetConfigurationProperties().GetEvaluatedPropertyValueAsync(
                XSharpProjectFileConstants.PostBuildEvent);

        public Task<string> GetRunPostBuildEventAsync(CancellationToken ct = default) =>
            GetConfigurationProperties().GetEvaluatedPropertyValueAsync(
                XSharpProjectFileConstants.RunPostBuildEvent);
    }
}
