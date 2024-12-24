using Community.VisualStudio.Toolkit;
using LanguageService.CodeAnalysis.Text;
using LanguageService.CodeAnalysis.XSharp;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.SyntaxTree;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.TextManager.Interop;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using VSLangProj;
using XSharp.Settings;
using XSharpModel;
using static XSharp.Parser.VsParser;
using TM = Microsoft.VisualStudio.TextManager.Interop;

namespace XSharp.LanguageService
{
    internal class XSharpShellLink : IXVsShellLink
    {
        static ILogger Logger => XSettings.Logger;

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

#if LIBRARYMANAGER

                VS.Events.SolutionEvents.OnAfterLoadProject += SolutionEvents_OnAfterLoadProject;
                VS.Events.SolutionEvents.OnBeforeUnloadProject += SolutionEvents_OnBeforeUnloadProject;
#endif
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
#if LIBRARYMANAGER
        private void SolutionEvents_OnBeforeUnloadProject(Project project)
        {

            IXSharpLibraryManager libraryManager = VS.GetRequiredService<IXSharpLibraryManager, IXSharpLibraryManager>();
            if (libraryManager != null)
            {
                project.GetItemInfo(out var hier, out var id, out var parent);
                libraryManager.UnregisterHierarchy(hier);
            }

        }

        private void SolutionEvents_OnAfterLoadProject(Project project)
        {

            if (!IsXSharpProject(project.FullPath))
                return;
            var framework = "";
            //ThreadHelper.JoinableTaskFactory.Run(async delegate
            //{
            //    framework = await project.GetAttributeAsync("TargetFramework");
            //});
            IXSharpLibraryManager libraryManager = VS.GetRequiredService<IXSharpLibraryManager, IXSharpLibraryManager>();
            if (libraryManager != null)
            {
                project.GetItemInfo(out var hier, out var id, out var parent);
                var prj = XSolution.FindProject(project.FullPath, framework);
                if (prj != null)
                {
                    libraryManager.RegisterHierarchy(hier, prj, prj.ProjectNode);
                }
                
            }

        }
#endif
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
            if (!XSolution.HasProjects)
                return;
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
            //if (IsXSharpProject(project?.FullPath))
            {
                Logger.SingleLine();
                Logger.Information("Renamed project: " + project?.FullPath ?? "");
                Logger.SingleLine();
            }
        }

        private void SolutionEvents_OnBeforeCloseProject(Project project)
        {
            //if (IsXSharpProject(project?.FullPath))
            {
                Logger.SingleLine();
                Logger.Information("Closing project: " + project.FullPath ?? "");
                Logger.SingleLine();
            }
            if (IsXSharpProject(project?.FullPath))
            {
#if LIBRARYMANAGER
                SolutionEvents_OnBeforeUnloadProject(project);
#endif
            }

        }
        private void SolutionEvents_OnBeforeOpenProject(string project)
        {
            //if (IsXSharpProject(project))
            {
                Logger.SingleLine();
                Logger.Information("Opening project: " + project ?? "");
                Logger.SingleLine();
            }

        }
        private void SolutionEvents_OnAfterOpenProject(Community.VisualStudio.Toolkit.Project project)
        {
            //if (IsXSharpProject(project?.FullPath))
            {
                Logger.SingleLine();
                Logger.Information("Opened project: " + project.FullPath ?? "");
                Logger.SingleLine();
            }
        }


        bool IsXSharpProject(string fileName)
        {
            if (string.IsNullOrEmpty(fileName))
                return false;
            return string.Equals(System.IO.Path.GetExtension(fileName), ".xsproj", StringComparison.OrdinalIgnoreCase);
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
            XSolution.IsClosing = true;
            XSolution.Close();
            // close OUR documents that are opened in design mode.
            if (!XSolution.HasProjects)
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
#if LIBRARYMANAGER
            var projects = GetProjects(solution);


            foreach (var project in projects)
            {
                SolutionEvents_OnAfterLoadProject(project);
            }

#endif
        }

        private List<Project> GetProjects(SolutionItem parent)
        {
            var result = new List<Project>();
            foreach (var item in parent.Children)
            {
                switch (item)
                {
                    case Project project:
                        result.Add(project);
                        break;
                    case SolutionFolder folder:
                        result.AddRange(GetProjects(folder));
                        break;
                    case PhysicalFolder physicalFolder:
                        result.AddRange(GetProjects(physicalFolder));
                        break;
                    case PhysicalFile file:
                        break;
                    case SolutionItem solutionItem:
                        break;
                }
            }
            return result;
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
                TM.IVsTextView textView = null;
                if (view != null)
                {
                    textView = await view.TextView.ToIVsTextViewAsync();
                }
                if (textView != null)
                {
                    TM.TextSpan span = new TM.TextSpan();
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


        public object FindProject(string sUrl)
        {
            var sol = VS.Solutions.GetCurrentSolution();
            return findProject(sol, sUrl);
        }
        private Project findProject(SolutionItem parent, string sUrl)
        {
            if (parent != null)
            {
                foreach (var child in parent.Children)
                {
                    if (child is null)
                        continue;
                    if (child is Project project)
                    {
                        if (string.Compare(project.FullPath, sUrl, StringComparison.OrdinalIgnoreCase) == 0)
                        {
                            return project;
                        }
                    }
                    foreach (var item in child.Children)
                    {
                        var prj = findProject(item, sUrl);
                        if (prj != null)
                            return prj;
                    }
                }
            }
            return null;
        }
        public void RunInForeGroundThread(Action a)
        {

            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                a();
            });
        }

        public string DocumentGetText(string file, ref bool IsOpen)
        {
            bool open = false;
            var result = ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                var doc = await VS.Documents.GetDocumentViewAsync(file);
                open = doc != null;
                if (open)
                {
                    return doc.TextBuffer.CurrentSnapshot.GetText();
                }
                return File.ReadAllText(file);
            });
            IsOpen = open;
            return result;
        }

        private void writeToFile(string file, int line, string text)
        {
            var lines = File.ReadAllLines(file).ToList();
            if (line >= 0 && line < lines.Count)
            {
                lines.Insert(line, text);
            }
            else
            {
                lines.Add(text);
            }
            File.WriteAllLines(file, lines);
        }
        private void writeToDocument(DocumentView doc, int line, string text)
        {
            var textBuffer = doc.TextBuffer.CurrentSnapshot.TextBuffer;
            var lineCount = textBuffer.CurrentSnapshot.LineCount;
            int insertPos = textBuffer.CurrentSnapshot.Length;
            if (line > 0 && line < lineCount)
            {
                var textLine = textBuffer.CurrentSnapshot.GetLineFromLineNumber(line);
                insertPos = textLine.End.Position;

            }
            textBuffer.Insert(insertPos, text + Environment.NewLine);
        }
        public bool DocumentInsertLine(string file, int line, string text)
        {
            if (!File.Exists(file))
            {
                return false;
            }
            var result = ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                var doc = await VS.Documents.GetDocumentViewAsync(file);
                if (doc != null)
                {
                    writeToDocument(doc, line, text);
                }
                else
                {
                    writeToFile(file, line, text);
                }
                return true;
            });
            return result;
        }

        private void writeToDocument(DocumentView doc, string text)
        {
            doc.TextBuffer.CurrentSnapshot.TextBuffer.Replace(new Span(0, doc.TextBuffer.CurrentSnapshot.Length), text);
            return;
        }
        public bool DocumentSetText(string file, string text)
        {
            return ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                var doc = await VS.Documents.GetDocumentViewAsync(file);
                if (doc != null)
                {
                    writeToDocument(doc, text);
                }
                else
                {
                    File.WriteAllText(file, text);
                }
                return true;
            });
        }

        public void ClearIntellisenseErrors(string file)
        {
            return;
        }
        public string SynchronizeKeywordCase(string code, string fileName)
        {
            if (XEditorSettings.KeywordCase == KeywordCase.None)
                return code;
            // we also normalize the line endings
            code = code.Replace("\n", "");
            code = code.Replace("\r", "\r\n");

            var file = XSolution.FindFullPath(fileName);
            var sb = new StringBuilder();
            XParseOptions parseOptions;
            if (file != null)
            {
                parseOptions = file.Project.ParseOptions;
            }
            else
                parseOptions = XParseOptions.Default;
            ITokenStream tokenStream;
            var reporter = new ErrorIgnorer();
            bool ok = Lex(code, fileName, (XSharpParseOptions)parseOptions, reporter, out tokenStream, out _);
            var stream = tokenStream as BufferedTokenStream;
            var tokens = stream.GetTokens();
            foreach (var token in tokens)
            {
                if (XSharpLexer.IsKeyword(token.Type))
                {
                    sb.Append(XLiterals.FormatKeyword(token.Text));
                }
                else
                {
                    sb.Append(token.Text);
                }
            }
            return sb.ToString();
        }

    }
    internal class ErrorIgnorer : IErrorListener
    {
        #region IErrorListener
        public void ReportError(string fileName, LinePositionSpan span, string errorCode, string message, object[] args)
        {
            ; //  _errors.Add(new XError(fileName, span, errorCode, message, args));
        }

        public void ReportWarning(string fileName, LinePositionSpan span, string errorCode, string message, object[] args)
        {
            ; //  _errors.Add(new XError(fileName, span, errorCode, message, args));
        }
        #endregion
    }
}
