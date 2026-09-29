//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System.Collections.Generic;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using XSharp.Settings;
using XSharpModel;

namespace XSharp.ProjectSystem.LanguageService
{
    /// <summary>
    /// The comment tokens (TODO, HACK etc.) of the code model's parser.
    /// </summary>
    /// <remarks>
    /// Saved files are walked again by <see cref="SourceFileWatcher"/> (it replaced the editor save watcher that lived
    /// in this file), which refreshes the comment tasks in the Task List.
    /// </remarks>
    internal static class CommentTokens
    {
        /// <summary>
        /// Passes the comment tokens (Tools > Options > Environment > Task List) to the code model when nobody did yet.
        /// </summary>
        /// <remarks>
        /// The X# project package sets them when an MPFproj project opens and after the options dialog closes
        /// (XSharpProjectPackage.SetCommentTokens). In a solution with only CPS projects the parser would otherwise
        /// find no comment tasks at all.
        /// </remarks>
        public static void EnsureSet()
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
                XSettings.Information($"CommentTokens: {tokens.Count} comment tokens set");
            }
        }
    }
}
