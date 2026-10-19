//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.ComponentModel.Composition;
using System.IO;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.Build.Framework;
using Microsoft.VisualStudio.ProjectSystem;
using Microsoft.VisualStudio.ProjectSystem.Build;
using XSharp.Settings;
using XSharpModel;
using ILogger = Microsoft.Build.Framework.ILogger;

namespace XSharp.ProjectSystem.LanguageService
{
    /// <summary>
    /// Collects the errors and warnings of real builds of an X# CPS project, for the error squiggles in the editor.
    /// </summary>
    /// <remarks>
    /// In MPFproj the build logger (XSharpIDEBuildLogger) fills the ErrorListManager, and
    /// XSharpProjectNode.GetIntellisenseErrors returns these build errors to XSharpErrorColorizer. Under CPS the build
    /// errors only reach the Error List of VS, so this logger keeps a copy for
    /// <see cref="XSharpProjectAdapter.GetIntellisenseErrors"/>. Like XSharpIDEBuildLogger the errors of a
    /// configuration are replaced when it is built again. Design-time builds are not logged (they do not run the
    /// compiler and use other targets).
    /// </remarks>
    [Export(typeof(IBuildLoggerProviderAsync))]
    [AppliesTo(XSharpCapabilities.XSharpCps)]
    internal sealed class BuildErrorLoggerProvider : IBuildLoggerProviderAsync
    {
        private static readonly Task<IImmutableSet<ILogger>> noLoggers =
            Task.FromResult<IImmutableSet<ILogger>>(ImmutableHashSet<ILogger>.Empty);

        private static readonly HashSet<string> BuildTargets = new HashSet<string>(StringComparer.OrdinalIgnoreCase)
        {
            "Build", "Rebuild", "Clean",
        };

        private readonly ConfiguredProject configuredProject;

        [ImportingConstructor]
        public BuildErrorLoggerProvider(ConfiguredProject configuredProject)
        {
            this.configuredProject = configuredProject;
        }

        public Task<IImmutableSet<ILogger>> GetLoggersAsync(IReadOnlyList<string> targets, IImmutableDictionary<string, string> properties, CancellationToken cancellationToken)
        {
            try
            {
                if (targets == null || !targets.SelectMany(t => t.Split(';')).Any(t => BuildTargets.Contains(t.Trim())))
                    return noLoggers;
                if (properties != null && properties.TryGetValue("DesignTimeBuild", out var designTime) &&
                    string.Equals(designTime, "true", StringComparison.OrdinalIgnoreCase))
                    return noLoggers;
                var projectFile = configuredProject.UnconfiguredProject.FullPath;
                if (!(XSolution.FindProjectByFileName(projectFile)?.ProjectNode is XSharpProjectAdapter adapter))
                    return noLoggers;
                var logger = new BuildErrorLogger(adapter, configuredProject.ProjectConfiguration?.Name, projectFile);
                return Task.FromResult<IImmutableSet<ILogger>>(ImmutableHashSet.Create<ILogger>(logger));
            }
            catch (Exception e)
            {
                XSettings.Exception(e);
                return noLoggers;
            }
        }

        private sealed class BuildErrorLogger : ILogger
        {
            private readonly XSharpProjectAdapter adapter;
            private readonly string configuration;
            private readonly string projectFile;
            private readonly string projectFolder;
            private readonly List<BuildErrorPosition> positions = new List<BuildErrorPosition>();
            private IEventSource eventSource;

            public BuildErrorLogger(XSharpProjectAdapter adapter, string configuration, string projectFile)
            {
                this.adapter = adapter;
                this.configuration = configuration;
                this.projectFile = projectFile;
                projectFolder = Path.GetDirectoryName(projectFile);
            }

            public LoggerVerbosity Verbosity { get; set; } = LoggerVerbosity.Quiet;
            public string Parameters { get; set; }

            public void Initialize(IEventSource eventSource)
            {
                this.eventSource = eventSource;
                eventSource.ErrorRaised += OnErrorRaised;
                eventSource.WarningRaised += OnWarningRaised;
                eventSource.ProjectFinished += OnProjectFinished;
                eventSource.BuildFinished += OnBuildFinished;
            }

            public void Shutdown()
            {
                if (eventSource != null)
                {
                    eventSource.ErrorRaised -= OnErrorRaised;
                    eventSource.WarningRaised -= OnWarningRaised;
                    eventSource.ProjectFinished -= OnProjectFinished;
                    eventSource.BuildFinished -= OnBuildFinished;
                    eventSource = null;
                }
                Commit();
            }

            /// <summary>
            /// Publishes the errors collected so far. Called when this project finishes, when the build finishes and
            /// at shutdown; each call replaces the previous result, so repeated calls are harmless.
            /// Also after a failed or cancelled build: the errors of the previous build are outdated.
            /// </summary>
            private void Commit()
            {
                try
                {
                    List<BuildErrorPosition> result;
                    lock (positions)
                    {
                        result = positions.ToList();
                    }
                    adapter.SetBuildErrors(configuration, result);
                }
                catch (Exception e)
                {
                    XSettings.Exception(e);
                }
            }

            private void OnProjectFinished(object sender, ProjectFinishedEventArgs e)
            {
                if (string.Equals(e.ProjectFile, projectFile, StringComparison.OrdinalIgnoreCase))
                    Commit();
            }

            private void OnBuildFinished(object sender, BuildFinishedEventArgs e) => Commit();

            private void OnErrorRaised(object sender, BuildErrorEventArgs e) => Add(e.File, e.ProjectFile, e.LineNumber, e.ColumnNumber);

            private void OnWarningRaised(object sender, BuildWarningEventArgs e) => Add(e.File, e.ProjectFile, e.LineNumber, e.ColumnNumber);

            private void Add(string file, string projectFile, int line, int column)
            {
                if (string.IsNullOrEmpty(file) || line <= 0)
                    return;
                var folder = string.IsNullOrEmpty(projectFile) ? projectFolder : Path.GetDirectoryName(projectFile);
                var position = new BuildErrorPosition(XSharpCommandLine.MakeFullPath(file, folder), line, Math.Max(column, 1));
                lock (positions)
                {
                    positions.Add(position);
                }
            }
        }
    }
}
