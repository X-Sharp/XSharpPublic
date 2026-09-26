//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Collections.Generic;
using System.Threading;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Threading;
using XSharpModel;
using DiagnosticSeverity = LanguageService.CodeAnalysis.DiagnosticSeverity;

namespace XSharp.ProjectSystem.LanguageService
{
    /// <summary>
    /// Shows the intellisense errors of a CPS project in the Error List.
    /// </summary>
    /// <remarks>
    /// Replaces the ErrorListManager of MPFproj (ProjectPackage), which needs the MPFproj hierarchy.
    /// Updates arrive on background threads (parser); they are coalesced and applied on the UI thread.
    /// </remarks>
    internal sealed class IntellisenseErrorList : IDisposable
    {
        private readonly JoinableTaskFactory joinableTaskFactory;
        private readonly string projectName;
        private ErrorListProvider provider;
        private IReadOnlyList<XError> pending;
        private int refreshScheduled;
        private bool disposed;

        public IntellisenseErrorList(JoinableTaskFactory joinableTaskFactory, string projectName)
        {
            this.joinableTaskFactory = joinableTaskFactory;
            this.projectName = projectName;
        }

        /// <summary>
        /// Replace the errors of the project with <paramref name="errors"/>.
        /// </summary>
        public void Update(IReadOnlyList<XError> errors)
        {
            Volatile.Write(ref pending, errors);
            if (Interlocked.Exchange(ref refreshScheduled, 1) == 0)
            {
                joinableTaskFactory.RunAsync(RefreshAsync).FileAndForget("XSharp/ProjectSystemCPS/ErrorList");
            }
        }

        private async System.Threading.Tasks.Task RefreshAsync()
        {
            await joinableTaskFactory.SwitchToMainThreadAsync();
            Interlocked.Exchange(ref refreshScheduled, 0);
            if (disposed)
                return;
            var errors = Volatile.Read(ref pending) ?? Array.Empty<XError>();
            if (provider == null)
            {
                provider = new ErrorListProvider(ServiceProvider.GlobalProvider)
                {
                    ProviderName = "X# IntelliSense (" + projectName + ")",
                };
            }
            provider.SuspendRefresh();
            try
            {
                provider.Tasks.Clear();
                foreach (var error in errors)
                {
                    var task = new ErrorTask
                    {
                        Document = error.Path,
                        // XError positions are 1-based, the error list is 0-based
                        Line = Math.Max(0, error.Span.Line - 1),
                        Column = Math.Max(0, error.Span.Column - 1),
                        Text = string.IsNullOrEmpty(error.ErrCode) ? error.ToString() : error.ErrCode + ": " + error.ToString(),
                        ErrorCategory = ToCategory(error.Severity),
                        Category = TaskCategory.CodeSense,
                    };
                    task.Navigate += (sender, e) =>
                    {
                        ThreadHelper.ThrowIfNotOnUIThread();
                        provider?.Navigate((ErrorTask)sender, VSConstants.LOGVIEWID.TextView_guid);
                    };
                    provider.Tasks.Add(task);
                }
            }
            finally
            {
                provider.ResumeRefresh();
            }
        }

        private static TaskErrorCategory ToCategory(DiagnosticSeverity severity)
        {
            switch (severity)
            {
                case DiagnosticSeverity.Error:
                    return TaskErrorCategory.Error;
                case DiagnosticSeverity.Warning:
                    return TaskErrorCategory.Warning;
                default:
                    return TaskErrorCategory.Message;
            }
        }

        public void Dispose()
        {
            disposed = true;
            var old = provider;
            provider = null;
            if (old != null)
            {
                joinableTaskFactory.RunAsync(async () =>
                {
                    await joinableTaskFactory.SwitchToMainThreadAsync();
                    old.Tasks.Clear();
                    old.Dispose();
                }).FileAndForget("XSharp/ProjectSystemCPS/ErrorList");
            }
        }
    }
}
