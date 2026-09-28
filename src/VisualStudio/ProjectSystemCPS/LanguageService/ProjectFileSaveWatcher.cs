//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Threading.Tasks;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Threading;
using XSharp.Settings;
using XSharpModel;

namespace XSharp.ProjectSystem.LanguageService
{
    /// <summary>
    /// Re-walks a source file of an X# CPS project after it was saved, so the code model (and with it the comment
    /// tasks in the Task List) reflects the saved file.
    /// </summary>
    /// <remarks>
    /// MPFproj does this in XSharpProjectNode.OnFileChanged: WalkFile(file, notify: true), whose FileWalkComplete event
    /// refreshes the Task List. Under CPS nothing re-walked a saved file: a new TODO only showed up after the next
    /// project walk (e.g. after a design-time build), verified in VS. The walk runs on a background thread.
    /// </remarks>
    internal sealed class ProjectFileSaveWatcher : IVsRunningDocTableEvents
    {
        private static ProjectFileSaveWatcher _instance;

        private readonly IVsRunningDocumentTable _rdt;
        private readonly JoinableTaskFactory _joinableTaskFactory;

        private ProjectFileSaveWatcher(IVsRunningDocumentTable rdt, JoinableTaskFactory joinableTaskFactory)
        {
            _rdt = rdt;
            _joinableTaskFactory = joinableTaskFactory;
        }

        /// <summary>
        /// Starts watching, once per session. Called when the first X# CPS project loads.
        /// </summary>
        public static void EnsureAdvised(JoinableTaskFactory joinableTaskFactory)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            if (_instance != null ||
                !(ServiceProvider.GlobalProvider.GetService(typeof(SVsRunningDocumentTable)) is IVsRunningDocumentTable rdt))
            {
                return;
            }
            var instance = new ProjectFileSaveWatcher(rdt, joinableTaskFactory);
            if (ErrorHandler.Succeeded(rdt.AdviseRunningDocTableEvents(instance, out uint _)))
            {
                // Process lifetime, like NewFormDesignerRedirect
                _instance = instance;
            }
        }

        /// <summary>
        /// Passes the comment tokens (Tools > Options > Environment > Task List) to the code model when nobody did yet.
        /// </summary>
        /// <remarks>
        /// The X# project package sets them when an MPFproj project opens and after the options dialog closes
        /// (XSharpProjectPackage.SetCommentTokens). In a solution with only CPS projects the parser would otherwise
        /// find no comment tasks at all.
        /// </remarks>
        public static void EnsureCommentTokens()
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            if (XSolution.CommentTokens.Count > 0 ||
                !(ServiceProvider.GlobalProvider.GetService(typeof(SVsTaskList)) is IVsCommentTaskInfo taskInfo) ||
                ErrorHandler.Failed(taskInfo.EnumTokens(out IVsEnumCommentTaskTokens enumTokens)) || enumTokens == null)
            {
                return;
            }
            var tokens = new List<XCommentToken>();
            var buffer = new IVsCommentTaskToken[1];
            while (enumTokens.Next(1, buffer, out uint fetched) == VSConstants.S_OK && fetched == 1)
            {
                var priority = new VSTASKPRIORITY[1];
                if (ErrorHandler.Succeeded(buffer[0].Text(out string text)) && !string.IsNullOrEmpty(text) &&
                    ErrorHandler.Succeeded(buffer[0].Priority(priority)))
                {
                    tokens.Add(new XCommentToken(text, (int)priority[0]));
                }
            }
            if (tokens.Count > 0)
            {
                XSolution.SetCommentTokens(tokens);
                XSettings.Information($"ProjectFileSaveWatcher: {tokens.Count} comment tokens set");
            }
        }

        public int OnAfterSave(uint docCookie)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            try
            {
                if (ErrorHandler.Failed(_rdt.GetDocumentInfo(docCookie, out uint _, out uint _, out uint _, out string moniker,
                        out IVsHierarchy _, out uint _, out IntPtr docData)))
                {
                    return VSConstants.S_OK;
                }
                if (docData != IntPtr.Zero)
                {
                    Marshal.Release(docData);
                }
                var file = string.IsNullOrEmpty(moniker) ? null : XSolution.FindFile(moniker);
                var project = file?.Project;
                if (project == null || !(project.ProjectNode is XSharpProjectAdapter) || !file.IsSource)
                {
                    return VSConstants.S_OK;
                }
                _joinableTaskFactory.RunAsync(async () =>
                {
                    await TaskScheduler.Default;
                    try
                    {
                        project.WalkFile(file, true);
                    }
                    catch (Exception e)
                    {
                        XSettings.Exception(e);
                    }
                }).FileAndForget("XSharp/ProjectSystemCPS/ProjectFileSaveWatcher");
            }
            catch (Exception e)
            {
                XSettings.Exception(e);
            }
            return VSConstants.S_OK;
        }

        #region IVsRunningDocTableEvents members that are not used
        public int OnAfterFirstDocumentLock(uint docCookie, uint dwRDTLockType, uint dwReadLocksRemaining, uint dwEditLocksRemaining) => VSConstants.S_OK;
        public int OnBeforeLastDocumentUnlock(uint docCookie, uint dwRDTLockType, uint dwReadLocksRemaining, uint dwEditLocksRemaining) => VSConstants.S_OK;
        public int OnAfterAttributeChange(uint docCookie, uint grfAttribs) => VSConstants.S_OK;
        public int OnBeforeDocumentWindowShow(uint docCookie, int fFirstShow, IVsWindowFrame pFrame) => VSConstants.S_OK;
        public int OnAfterDocumentWindowHide(uint docCookie, IVsWindowFrame pFrame) => VSConstants.S_OK;
        #endregion
    }
}
