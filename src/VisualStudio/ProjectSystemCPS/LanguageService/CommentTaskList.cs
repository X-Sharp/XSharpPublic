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

namespace XSharp.ProjectSystem.LanguageService
{
    /// <summary>
    /// Shows the comment tasks (TODO, HACK etc.) of a CPS project in the Task List.
    /// </summary>
    /// <remarks>
    /// Replaces the TaskListManager of MPFproj (ProjectPackage), which XSharpProjectNode fills after each
    /// project or file walk. Updates are coalesced and applied on the UI thread.
    /// </remarks>
    internal sealed class CommentTaskList : IDisposable
    {
        private readonly JoinableTaskFactory joinableTaskFactory;
        private readonly string projectName;
        private TaskProvider provider;
        private IList<XCommentTask> pending;
        private int refreshScheduled;
        // Written by Dispose on any thread, read by RefreshAsync on the UI thread
        private volatile bool disposed;

        public CommentTaskList(JoinableTaskFactory joinableTaskFactory, string projectName)
        {
            this.joinableTaskFactory = joinableTaskFactory;
            this.projectName = projectName;
        }

        public void Update(IList<XCommentTask> tasks)
        {
            Volatile.Write(ref pending, tasks);
            if (Interlocked.Exchange(ref refreshScheduled, 1) == 0)
            {
                joinableTaskFactory.RunAsync(RefreshAsync).FileAndForget("XSharp/ProjectSystemCPS/TaskList");
            }
        }

        private async System.Threading.Tasks.Task RefreshAsync()
        {
            await joinableTaskFactory.SwitchToMainThreadAsync();
            Interlocked.Exchange(ref refreshScheduled, 0);
            if (disposed)
                return;
            var tasks = Volatile.Read(ref pending) ?? Array.Empty<XCommentTask>();
            if (provider == null)
            {
                provider = new TaskProvider(ServiceProvider.GlobalProvider)
                {
                    ProviderName = "X# Comments (" + projectName + ")",
                };
            }
            provider.SuspendRefresh();
            try
            {
                provider.Tasks.Clear();
                foreach (var commentTask in tasks)
                {
                    if (commentTask?.File == null)
                        continue;
                    var task = new TaskListItem
                    {
                        Document = commentTask.File.FullPath,
                        // XCommentTask positions are 1-based, the task list is 0-based
                        Line = Math.Max(0, commentTask.Line - 1),
                        Column = Math.Max(0, commentTask.Column - 1),
                        Text = commentTask.Comment,
                        Priority = (TaskPriority)commentTask.Priority,
                        Category = TaskCategory.Comments,
                    };
                    task.Navigate += (sender, e) =>
                    {
                        ThreadHelper.ThrowIfNotOnUIThread();
                        provider?.Navigate((TaskListItem)sender, VSConstants.LOGVIEWID.TextView_guid);
                    };
                    provider.Tasks.Add(task);
                }
            }
            finally
            {
                provider.ResumeRefresh();
            }
        }

        /// <summary>
        /// Removes the entries. The provider is only touched on the UI thread, after <c>disposed</c> is set: a
        /// refresh that runs before this sees the flag or has already created the provider, which is then disposed here.
        /// </summary>
        public void Dispose()
        {
            disposed = true;
            joinableTaskFactory.RunAsync(async () =>
            {
                await joinableTaskFactory.SwitchToMainThreadAsync();
                var old = provider;
                provider = null;
                if (old != null)
                {
                    old.Tasks.Clear();
                    old.Dispose();
                }
            }).FileAndForget("XSharp/ProjectSystemCPS/TaskList");
        }
    }
}
