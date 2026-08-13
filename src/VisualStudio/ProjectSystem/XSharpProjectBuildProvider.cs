//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System.Collections.Immutable;
using System.ComponentModel.Composition;
using System.Threading;
using System.Threading.Tasks;

using Microsoft.VisualStudio.ProjectSystem;
using Microsoft.VisualStudio.ProjectSystem.Build;

namespace XSharp.VisualStudio.ProjectSystem
{
    /// <summary>
    /// Provides XSharp-specific MSBuild build properties to the CPS build pipeline.
    ///
    /// The properties exposed here augment what CPS reads automatically from the
    /// project file, adding XSharp dialect and language options that CPS would
    /// otherwise not know about.
    /// </summary>
    [Export(typeof(IBuildPropertiesProvider))]
    [AppliesTo(XSharpProjectCapabilities.XSharp)]
    internal sealed class XSharpProjectBuildProvider : IBuildPropertiesProvider
    {
        // Reserved for future use: inject or transform per-project build properties
        // (e.g. dialect, include paths) at build time.
        // private readonly ConfiguredProject _configuredProject;

        [ImportingConstructor]
        public XSharpProjectBuildProvider(ConfiguredProject configuredProject)
        {
            // configuredProject reserved for future build-property injection.
        }

        /// <inheritdoc />
        public Task<IImmutableDictionary<string, string>> GetBuildPropertiesAsync(
            BuildPropertyContext context,
            CancellationToken cancellationToken)
        {
            // Return an empty dictionary — the actual XSharp build properties
            // (Dialect, VOCompatibility options, etc.) are already expressed in the
            // .xsproj MSBuild file and are picked up by CPS automatically.
            // Override this method to inject or transform properties at build time
            // if needed in the future.
            IImmutableDictionary<string, string> empty =
                ImmutableDictionary<string, string>.Empty;

            return Task.FromResult(empty);
        }
    }

    /// <summary>
    /// Implements a fast "up-to-date" check for XSharp projects, telling VS that the
    /// project needs a full rebuild only when its source or project file has actually
    /// changed.
    ///
    /// This is a lightweight placeholder implementation.  It currently delegates to
    /// CPS's default behaviour (always returns <c>false</c> so that MSBuild performs
    /// the real incremental check).  A full implementation would inspect timestamps of
    /// <c>.prg</c> / <c>.xs</c> files and compare them against output assembly timestamps.
    /// </summary>
    [Export(typeof(IBuildUpToDateCheckProvider))]
    [AppliesTo(XSharpProjectCapabilities.XSharp)]
    [ExportMetadata("Name", "XSharpUpToDateCheck")]
    internal sealed class XSharpUpToDateCheckProvider : IBuildUpToDateCheckProvider
    {
        // Reserved for future use: compare source timestamps against output assembly
        // to implement a fast "up-to-date" check without invoking MSBuild.
        // private readonly ConfiguredProject _configuredProject;

        [ImportingConstructor]
        public XSharpUpToDateCheckProvider(ConfiguredProject configuredProject)
        {
            // configuredProject reserved for future up-to-date timestamp checking.
        }

        /// <inheritdoc />
        /// <remarks>
        /// Returns <c>false</c> to defer to MSBuild's own incremental build logic.
        /// Set <paramref name="failureReason"/> to an explanatory message when
        /// returning <c>false</c> so that the Output window explains why a rebuild
        /// was triggered.
        /// </remarks>
        public Task<bool> IsUpToDateAsync(
            BuildAction buildAction,
            TextWriter logger,
            CancellationToken cancellationToken = default)
        {
            // Defer to MSBuild for incremental build decisions.
            return Task.FromResult(false);
        }

        /// <inheritdoc />
        public Task<bool> IsUpToDateCheckEnabledAsync(CancellationToken cancellationToken = default)
        {
            return Task.FromResult(true);
        }
    }
}
