using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TextManager.Interop;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using XSharp.Settings;
using XSharpModel;

namespace XSharp.Project
{
    internal class XSharpShellLink : IXVsShellLink
    {
        static ILogger Logger => XSettings.Logger;

        static bool hasEnvironmentvariable = false;
        static XSharpShellLink()
        {
            hasEnvironmentvariable = !String.IsNullOrEmpty(System.Environment.GetEnvironmentVariable("XSharpMsBuildDir"));
            if (!hasEnvironmentvariable)
            {
                VS.MessageBox.ShowWarning("The environment variable 'XSharpMsBuildDir' is missing. \rSome projects may have problems loading. \rPlease run the XSharp setup program again.");
            }
        }

        internal XSharpShellLink()
        {
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                VS.Events.SolutionEvents.OnBeforeOpenSolution += SolutionEvents_OnBeforeOpenSolution;
                VS.Events.SolutionEvents.OnAfterOpenSolution += SolutionEvents_OnAfterOpenSolution;
                VS.Events.SolutionEvents.OnAfterCloseSolution += SolutionEvents_OnAfterCloseSolution;
                VS.Events.SolutionEvents.OnBeforeCloseSolution += SolutionEvents_OnBeforeCloseSolution;
                VS.Events.SolutionEvents.OnBeforeOpenProject += SolutionEvents_OnBeforeOpenProject;
                VS.Events.SolutionEvents.OnAfterOpenProject += SolutionEvents_OnAfterOpenProject;
                VS.Events.SolutionEvents.OnBeforeCloseProject += SolutionEvents_OnBeforeCloseProject;
                VS.Events.SolutionEvents.OnAfterRenameProject += SolutionEvents_OnAfterRenameProject;
                VS.Events.SolutionEvents.OnAfterBackgroundSolutionLoadComplete += SolutionEvents_OnAfterBackgroundSolutionLoadComplete;
                VS.Events.BuildEvents.SolutionBuildStarted += BuildEvents_SolutionBuildStarted;
                VS.Events.BuildEvents.SolutionBuildDone += BuildEvents_SolutionBuildDone;
                VS.Events.BuildEvents.SolutionBuildCancelled += BuildEvents_SolutionBuildCancelled;

                _ = await VS.Commands.InterceptAsync(KnownCommands.File_CloseSolution, CloseDesignerWindows);
                _ = await VS.Commands.InterceptAsync(KnownCommands.File_Exit, CloseDesignerWindows);
                VS.Events.ShellEvents.ShutdownStarted += ShellEvents_ShutdownStarted;
                var sol = await VS.Solutions.GetCurrentSolutionAsync();
                if (sol is Solution)
                {
                    SolutionEvents_OnAfterOpenSolution(sol);
                }
            });

        }

        #region DesignerWindows
        private CommandProgression CloseDesignerWindows()
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            SaveDesignerWindows();
            return CommandProgression.Continue;
        }

        private void SaveDesignerWindows()
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            var files = GetAllDesignerWindows(false);
            XDatabase.SaveOpenDesignerFiles(files);
        }
        private void CloseAllDesignerWindows()
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            GetAllDesignerWindows(true);
        }

        private List<string> GetAllDesignerWindows(bool close = false)
        {
            var files = new List<string>();
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                var documents = await VS.Windows.GetAllDocumentWindowsAsync();
                foreach (var doc in documents.ToArray())
                {
                    var caption = doc.Caption;
                    if (caption.EndsWith("]"))
                    {
                        string docName = "";
                        // use reflection to get the _frame field which has the full URL to the file
                        // and please note that we do not check for "[Design]" because that can (will)
                        // be translated in localized versions of VS, for example [Entwurf] in German.
                        var field = doc.GetType().GetField("_frame", BindingFlags.Instance | BindingFlags.NonPublic);
                        if (field != null)
                        {
                            dynamic _frame = field.GetValue(doc);
                            docName = _frame.EffectiveDocumentMoniker;
                            var type = XFileTypeHelpers.GetFileType(docName);
                            if (type != XFileType.SourceCode)
                                continue;
                            string capt = _frame.EditorCaption;
                            if (capt == null || !capt.EndsWith("]"))
                                continue;

                        }
#if !DEV17
                        await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
#endif
                        if (string.IsNullOrEmpty(docName) && doc is IVsWindowFrame frame)
                        {
                            frame.GetProperty((int)__VSFPROPID.VSFPROPID_pszMkDocument, out var objdocName);
                            if (objdocName is string fileName)
                            {
                                docName = fileName;
                            }
                        }
                        if (!string.IsNullOrEmpty(docName))
                        {
                            await doc.CloseFrameAsync(FrameCloseOption.NoSave);
                            files.Add(docName);
                        }
                    }
                }
            });
            return files;
        }

        private void RestoreDesignerWindows()
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                Logger.SingleLine();
                Logger.Information("Start restoring windows in [Design] mode");
                Logger.SingleLine();
                var files = XDatabase.GetOpenDesignerFiles();
                if (files.Count > 0)
                {
                    CloseAllDesignerWindows();
                }
                var selection = await VS.Solutions.GetActiveItemsAsync();
                if (files.Count > 0)
                {
                    foreach (var file in files)
                    {
                        try
                        {
                            Logger.SingleLine();
                            Logger.Information("Restoring " + file);
                            Logger.SingleLine();
                            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                            VsShellUtilities.OpenDocument(ServiceProvider.GlobalProvider, file, VSConstants.LOGVIEWID_Designer, out _, out _, out _);

                            Logger.SingleLine();
                            Logger.Information("Restored " + file);
                            Logger.SingleLine();
                        }
                        catch (Exception e)
                        {
                            Logger.DoubleLine();
                            Logger.Exception(e, "Restoring [Design] mode windows");
                            Logger.DoubleLine();
                        }
                    }
                    var sel = selection.Where(s => s.Type == SolutionItemType.PhysicalFile).FirstOrDefault();
                    if (sel != null)
                    {
                        _ = await VS.Documents.OpenAsync(sel.FullPath);
                    }
                    Logger.SingleLine();
                    Logger.Information("End restoring windows in [Design] mode");
                    Logger.SingleLine();
                }
            });
        }



        #endregion

 
        private void ShellEvents_ShutdownStarted()
        {
            XSolution.IsClosing = true;
            XSolution.IsShuttingDown = true;
            XSolution.Close();

            Logger.SingleLine();
            Logger.Information("Shutdown VS");
            Logger.SingleLine();
        }

        #region Project Events

        private void SolutionEvents_OnAfterRenameProject(Community.VisualStudio.Toolkit.Project project)
        {
            if (IsXSharpProject(project?.FullPath))
            {
                Logger.SingleLine();
                Logger.Information("Renamed project: " + project?.FullPath ?? "");
                Logger.SingleLine();
            }
        }

        private void SolutionEvents_OnBeforeCloseProject(Community.VisualStudio.Toolkit.Project project)
        {
            if (IsXSharpProject(project?.FullPath))
            {
                Logger.SingleLine();
                Logger.Information("Closing project: " + project.FullPath ?? "");
                Logger.SingleLine();
            }

        }

        private void SolutionEvents_OnAfterOpenProject(Community.VisualStudio.Toolkit.Project project)
        {
            if (IsXSharpProject(project?.FullPath))
            {
                Logger.SingleLine();
                Logger.Information("Opened project: " + project.FullPath ?? "");
                Logger.SingleLine();
            }
        }

        private void SolutionEvents_OnBeforeOpenProject(string projectFileName)
        {
            if (IsXSharpProject(projectFileName))
            {
                Logger.SingleLine();
                Logger.Information("Before Opening project: " + projectFileName ?? "");
                Logger.SingleLine();
                checkProjectFile(projectFileName);
            }
        }
        bool IsXSharpProject(string fileName)
        {
            if (string.IsNullOrEmpty(fileName))
                return false;
            return string.Equals(Path.GetExtension(fileName), ".xsproj", StringComparison.OrdinalIgnoreCase);
        }
        const string oldText = @"$(MSBuildExtensionsPath)\XSharp";
        const string newText = @"$(XSharpMsBuildDir)";
        const string MsTestGuid = @"{3AC096D0-A1C2-E12C-1390-A8335801FDAB};";

        private void checkProjectFile(string fileName)
        {
            if (fileName != null && fileName.ToLower().EndsWith("xsproj") && File.Exists(fileName))
            {
                string xml = File.ReadAllText(fileName);
                var original = Path.ChangeExtension(fileName, ".original");
                bool changed = false;
                if (hasEnvironmentvariable)
                {
                    var pos = xml.IndexOf(oldText, StringComparison.OrdinalIgnoreCase);
                    if (pos >= 0)
                    {
                        while (pos > 0)
                        {
                            xml = xml.Substring(0, pos) + newText + xml.Substring(pos + oldText.Length);
                            pos = xml.IndexOf(oldText, StringComparison.OrdinalIgnoreCase);
                        }
                        DeleteFileSafe(original);
                        File.Copy(fileName, original);
                        DeleteFileSafe(fileName);
                        File.WriteAllText(fileName, xml);
                        changed = true;
                    }
                }
                var testpos = xml.IndexOf(MsTestGuid, StringComparison.OrdinalIgnoreCase);
                if (testpos >= 0)
                {
                    var left = xml.Substring(0, testpos);
                    var right = xml.Substring(testpos + MsTestGuid.Length);
                    if (!changed)
                    {
                        DeleteFileSafe(original);
                        File.Copy(fileName, original);
                    }
                    xml = left + right;
                    DeleteFileSafe(fileName);
                    File.WriteAllText(fileName, xml);
                    changed = true;
                }
                if (changed)
                {
                    Logger.SingleLine();
                    Logger.Information("==> Project must be upgraded: " + fileName);
                    Logger.SingleLine();

                    XSharpProjectNode.ChangedProjectFiles.Add(fileName, original);
                }
            }
        }

        public static bool DeleteFileSafe(string fileName)
        {
            try
            {
                if (File.Exists(fileName))
                {
                    File.SetAttributes(fileName, FileAttributes.Normal);
                    File.Delete(fileName);

                }
            }
            catch (Exception e)
            {
                Logger.Exception(e, "DeleteFileSafe");
                return false;
            }
            return true;

        }
        #endregion

        #region Solution Events

        private void SolutionEvents_OnAfterBackgroundSolutionLoadComplete()
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            RestoreDesignerWindows();
        }

        string solutionName = "";
        private void SolutionEvents_OnBeforeCloseSolution()
        {
            bool hasXsProject = XSolution.Projects.Count > 0;
            XSolution.IsClosing = true;
            XSolution.Close();
            // close OUR documents that are opened in design mode.
            if (!hasXsProject)
            {
                return;
            }
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                var frames = await VS.Windows.GetAllDocumentWindowsAsync();
                if (frames != null)
                {
                    foreach (var frame in frames.ToList())
                    {
                        if (frame.Caption.EndsWith("]"))
                        {
                            // no need to save here. VS has shown a dialog with the dirty files already
                            await frame.CloseFrameAsync(FrameCloseOption.NoSave);
                        }
                    }
                }
            });

            Logger.SingleLine();
            Logger.Information("Closing solution: " + solutionName);
            Logger.SingleLine();
        }

        private void SolutionEvents_OnAfterCloseSolution()
        {
            XSolution.IsClosing = false;
            Logger.SingleLine();
            Logger.Information("Closed solution: " + solutionName);
            Logger.SingleLine();
            solutionName = "";
        }

        private void SolutionEvents_OnAfterOpenSolution(Solution solution)
        {
            if (solution is Solution sol)
            {
                var file = sol.FullPath;
                if (!string.IsNullOrEmpty(file))
                {
                    XSolution.Open(file);
                }
            }

            Logger.SingleLine();
            Logger.Information("Opened Solution: " + solution?.FullPath ?? "");
            Logger.SingleLine();
            solutionName = solution?.FullPath;
        }

        private void SolutionEvents_OnBeforeOpenSolution(string solutionFileName)
        {
            Logger.SingleLine();
            Logger.Information("Opening Solution: " + solutionFileName ?? "");
            Logger.SingleLine();
            solutionName = solutionFileName;
        }
        #endregion

        #region StatusBar and Messages
        public void SetStatusBarAnimation(bool onOff, short id)
        {
            if (onOff)
                VS.StatusBar.StartAnimationAsync((StatusAnimation)id).FireAndForget();
            else
                VS.StatusBar.EndAnimationAsync((StatusAnimation)id).FireAndForget();

        }

        public void SetStatusBarProgress(string cMessage, int nItem, int nTotal)
        {
            VS.StatusBar.ShowProgressAsync(cMessage, nItem, nTotal).FireAndForget();
        }

        public void SetStatusBarText(string cText)
        {
            VS.StatusBar.ShowMessageAsync(cText).FireAndForget();
        }

        public int ShowMessageBox(string message)
        {
            string title = string.Empty;
            return (int)VS.MessageBox.Show(title, message);
        }
        #endregion

        #region Documents

        /// <summary>
        /// Open a document with 0 based line numbers
        /// </summary>
        /// <param name="file"></param>
        /// <param name="line"></param>
        /// <param name="column"></param>
        /// <param name="preview"></param>
        /// <returns></returns>
        private async System.Threading.Tasks.Task OpenDocumentAsync(string file, int line, int column, bool preview)
        {
            try
            {
                Logger.Information("OpenDocumentAsync: {file} {line} {column} {preview}");

                DocumentView view;
                if (preview)
                {
                    view = await VS.Documents.OpenInPreviewTabAsync(file);
                }
                else
                {
                    view = await VS.Documents.OpenViaProjectAsync(file);
                    if (view == null)
                    {
                        view = await VS.Documents.OpenAsync(file);
                    }
                }
                IVsTextView textView = null;
                if (view != null)
                {
                    textView = await view.TextView.ToIVsTextViewAsync();
                }
                if (textView != null)
                {
                    //
                    TextSpan span = new TextSpan();
                    span.iStartLine = line;
                    span.iStartIndex = column;
                    span.iEndLine = line;
                    span.iEndIndex = column;
                    //
                    textView.SetCaretPos(span.iStartLine, span.iStartIndex);
                    textView.EnsureSpanVisible(span);
                    if (span.iStartLine > 5)
                        textView.SetTopLine(span.iStartLine - 5);
                    else
                        textView.SetTopLine(0);
                    textView.SetCaretPos(line, column);
                }
                if (preview)
                    await VS.Documents.OpenInPreviewTabAsync(file);
                else
                    await VS.Documents.OpenAsync(file);
            }
            catch (Exception)
            {

                throw;
            }

        }

        public bool IsDocumentOpen(string file)
        {
            return ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                return await VS.Documents.IsOpenAsync(file);
            });
        }
        /// <summary>
        /// Open a document with 0 based line/column numbers
        /// </summary>
        /// <param name="file"></param>
        /// <param name="line"></param>
        /// <param name="column"></param>
        /// <param name="preview"></param>
        public void OpenDocument(string file, int line, int column, bool preview)
        {
            OpenDocumentAsync(file, line, column, preview).FireAndForget();
        }

        #endregion

        #region BuildEvents
        public bool IsVsBuilding => building;
        private void BuildEvents_SolutionBuildCancelled()
        {
            building = false;
        }
        bool building;

        private void BuildEvents_SolutionBuildDone(bool result)
        {
            building = false;
        }

        private void BuildEvents_SolutionBuildStarted(object sender, EventArgs e)
        {
            building = true;
        }
        #endregion
    }
}
