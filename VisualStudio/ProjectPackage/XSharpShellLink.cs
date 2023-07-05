using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.TextManager.Interop;
using System;
using System.IO;
using XSharpModel;
using XSharp.Settings;
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

                VS.Events.SolutionEvents.OnBeforeOpenProject += SolutionEvents_OnBeforeOpenProject;
                VS.Events.BuildEvents.SolutionBuildStarted += BuildEvents_SolutionBuildStarted;
                VS.Events.BuildEvents.SolutionBuildDone += BuildEvents_SolutionBuildDone;
                VS.Events.BuildEvents.SolutionBuildCancelled += BuildEvents_SolutionBuildCancelled;

            });

        }


        private void SolutionEvents_OnBeforeOpenProject(string projectFileName)
        {
            Logger.SingleLine();
            Logger.Information("Before Opening project: " + projectFileName ?? "");
            Logger.SingleLine();
            checkProjectFile(projectFileName);
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
    }
}
