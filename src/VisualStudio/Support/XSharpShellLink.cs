using Community.VisualStudio.Toolkit;

//using EnvDTE;

using LanguageService.CodeAnalysis.Text;
using LanguageService.CodeAnalysis.XSharp;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.SyntaxTree;

using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.TextManager.Interop;

using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;

using XSharp.Settings;

using XSharpModel;

using static XSharp.Parser.VsParser;


using TM = Microsoft.VisualStudio.TextManager.Interop;

namespace XSharp.Support
{
    internal class XSharpShellLink : IXVsShellLink
    {
        static ILogger Logger => XSettings.Logger;

        internal XSharpShellLink()
        {
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                // Logger may not be fully initialized yet, so guard against null
                Logger?.Information("Initialize XSharpShellLink");




                VS.Events.BuildEvents.SolutionBuildStarted += BuildEvents_SolutionBuildStarted;
                VS.Events.BuildEvents.SolutionBuildDone += BuildEvents_SolutionBuildDone;
                VS.Events.BuildEvents.SolutionBuildCancelled += BuildEvents_SolutionBuildCancelled;

                VS.Events.ShellEvents.ShutdownStarted += ShellEvents_ShutdownStarted;
                //VS.Events.ShellEvents.EnvironmentColorChanged   += ShellEvents_EnvironmentColorChanged;
                //VS.Events.ShellEvents.ShellAvailable += ShellEvents_ShellAvailable;


                VS.Events.BuildEvents.SolutionConfigurationChanged += BuildEvents_SolutionConfigurationChanged;
                VS.Events.BuildEvents.ProjectConfigurationChanged += BuildEvents_ProjectConfigurationChanged; ;
                VS.Events.BuildEvents.ProjectBuildDone += BuildEvents_ProjectBuildDone;
                VS.Events.BuildEvents.ProjectBuildStarted += BuildEvents_ProjectBuildStarted;
                VS.Events.BuildEvents.ProjectCleanDone += BuildEvents_ProjectCleanDone;
                VS.Events.BuildEvents.ProjectCleanStarted += BuildEvents_ProjectCleanStarted;


                VS.Events.SolutionEvents.OnBeforeOpenSolution += SolutionEvents_OnBeforeOpenSolution;
                VS.Events.SolutionEvents.OnAfterMergeSolution += SolutionEvents_OnAfterMergeSolution;

                VS.Events.SolutionEvents.OnAfterOpenSolution += SolutionEvents_OnAfterOpenSolution;
                VS.Events.SolutionEvents.OnBeforeCloseSolution += SolutionEvents_OnBeforeCloseSolution;
                VS.Events.SolutionEvents.OnAfterCloseSolution += SolutionEvents_OnAfterCloseSolution;
                VS.Events.SolutionEvents.OnAfterBackgroundSolutionLoadComplete += SolutionEvents_OnAfterBackgroundSolutionLoadComplete;


                VS.Events.SolutionEvents.OnBeforeOpenProject += SolutionEvents_OnBeforeOpenProject;
                VS.Events.SolutionEvents.OnAfterOpenProject += SolutionEvents_OnAfterOpenProject;
                VS.Events.SolutionEvents.OnAfterLoadProject += SolutionEvents_OnAfterLoadProject;
                VS.Events.SolutionEvents.OnBeforeCloseProject += SolutionEvents_OnBeforeCloseProject;
                VS.Events.SolutionEvents.OnAfterRenameProject += SolutionEvents_OnAfterRenameProject;
                VS.Events.SolutionEvents.OnBeforeUnloadProject += SolutionEvents_OnBeforeUnloadProject;

                //VS.Events.SolutionEvents.OnAfterOpenFolder += SolutionEvents_OnAfterOpenFolder;
                //VS.Events.SolutionEvents.OnBeforeCloseFolder += SolutionEvents_OnBeforeCloseFolder;

                //VS.Events.DocumentEvents.Closed += DocumentEvents_Closed;
                //VS.Events.DocumentEvents.Opened += DocumentEvents_Opened;
                //VS.Events.DocumentEvents.Saved += DocumentEvents_Saved;
                //VS.Events.DocumentEvents.BeforeDocumentWindowShow += DocumentEvents_BeforeDocumentWindowShow;
                //VS.Events.DocumentEvents.AfterDocumentWindowHide += DocumentEvents_AfterDocumentWindowHide;


                Logger?.Information("Initialized XSharpShellLink");
            });


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
                Logger.Information($"OpenDocumentAsync: {file} {line} {column} {preview}");

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
            if (string.IsNullOrEmpty(file))
                return false;
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

        #region Solution Events
        string solutionName = "";

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
            Logger.SingleLine();
            Logger.Information("Opened Solution: " + solution?.FullPath ?? "");
            Logger.SingleLine();
            solutionName = solution?.FullPath;
            XSolution.AfterOpen();
        }
        private void SolutionEvents_OnAfterBackgroundSolutionLoadComplete()
        {
            Logger.SingleLine();
            Logger.Information("Background Solution Load Complete");
        }
        private void SolutionEvents_OnBeforeCloseSolution()
        {
            _projects.Clear();
            Logger.SingleLine();
            Logger.Information("Closing solution: " + solutionName);
            Logger.SingleLine();
            XSolution.IsClosing = true;
            XSolution.Close();
        }


        private void SolutionEvents_OnAfterMergeSolution()
        {
            Logger.SingleLine();
            Logger.Information("After Merge solution");
        }
        private void SolutionEvents_OnBeforeOpenSolution(string solutionFileName)
        {
            Logger.SingleLine();
            Logger.Information("Opening Solution: " + solutionFileName ?? "");
            Logger.SingleLine();
            solutionName = solutionFileName;
            if (!string.IsNullOrEmpty(solutionName) && File.Exists(solutionName))
            {
                XSolution.Open(solutionName);
            }
        }

        #endregion

        #region BuildEvents
        private void BuildEvents_ProjectCleanDone(ProjectBuildDoneEventArgs args)
        {
            Logger.SingleLine();
            Logger.Information("Project Clean done: " + args.Project.Name + " " + args.IsSuccessful); ;
        }

        private void BuildEvents_ProjectBuildStarted(Project project)
        {
            Logger.SingleLine();
            Logger.Information("Project Build Started: " + project.Name);
        }

        private void BuildEvents_ProjectBuildDone(ProjectBuildDoneEventArgs args)
        {
            Logger.SingleLine();
            Logger.Information("Project Build Done: " + args.Project.Name + " " + args.IsSuccessful);
        }

        private void BuildEvents_ProjectConfigurationChanged(Project project)
        {
            Logger.SingleLine();
            Logger.Information("Project Configuration changed: " + project.Name);
        }

        private void BuildEvents_SolutionConfigurationChanged()
        {
            Logger.SingleLine();
            Logger.Information("Solution Configuration changed");
        }
        private void BuildEvents_ProjectCleanStarted(Project project)
        {
            Logger.SingleLine();
            Logger.Information("Project Clean started: " + project.Name);
        }

        private void BuildEvents_ProjectCleanDone1(ProjectBuildDoneEventArgs args)
        {
            Logger.SingleLine();
            Logger.Information("Project Build done: " + args.Project.Name + " " + args.IsSuccessful);
        }

        #endregion
        #region ProjectEvents
        private void SolutionEvents_OnAfterOpenProject(Project project)
        {
            Logger.SingleLine();
            Logger.Information("Opened project: " + project.FullPath ?? "");
            Logger.SingleLine();
            if (!_projects.ContainsKey(project.FullPath))
            {
                _projects.TryAdd(project.FullPath, project);
            }

        }
        private void SolutionEvents_OnAfterLoadProject(Project project)
        {
            Logger.SingleLine();
            Logger.Information("Loaded project: " + project.FullPath ?? "");
            Logger.SingleLine();
            if (! _projects.ContainsKey(project.FullPath))
            {
                _projects.TryAdd(project.FullPath, project);

            }
        }

        private void SolutionEvents_OnBeforeUnloadProject(Project project)
        {
            Logger.SingleLine();
            Logger.Information("Unloading project: " + project.FullPath ?? "");
            Logger.SingleLine();
            if (_projects.ContainsKey(project.FullPath))
            {
                _projects.TryRemove(project.FullPath, out _);
            }

        }

        private void SolutionEvents_OnBeforeCloseProject(Project project)
        {
            Logger.SingleLine();
            Logger.Information("Closing project: " + project.FullPath ?? "");
            Logger.SingleLine();
            if (_projects.ContainsKey(project.FullPath))
            {
                _projects.TryRemove(project.FullPath, out _);
            }

        }

        private void SolutionEvents_OnAfterRenameProject(Project project)
        {
            Logger.SingleLine();
            Logger.Information("Renamed project: " + project?.FullPath ?? "");
            Logger.SingleLine();


        }
        private void SolutionEvents_OnBeforeOpenProject(string projectFileName)
        {
            Logger.SingleLine();
            var ext = Path.GetExtension(projectFileName);
            if (!string.IsNullOrEmpty(ext))
                Logger.Information("Opening project: " + projectFileName ?? "");
            else
                Logger.Information("Opening folder: " + projectFileName ?? "");
            Logger.SingleLine();
            if (!string.IsNullOrEmpty(projectFileName))
            {
                if (_projects.ContainsKey(projectFileName))
                {
                    _projects.TryRemove(projectFileName, out _);
                }
            }
        }


        #endregion

        #region BuildEvents
        public bool IsVsBuilding => building;

        private void BuildEvents_SolutionBuildCancelled()
        {
            Logger.Information("Solution Build Cancelled");
            building = false;
        }
        bool building;

        private void BuildEvents_SolutionBuildDone(bool result)
        {
            Logger.Information($"Solution Build Done, result {result}");
            building = false;
        }

        private void BuildEvents_SolutionBuildStarted(object sender, EventArgs e)
        {
            Logger.Information("Solution Build Started");
            building = true;
        }
        #endregion

        #region Shell Events
        private void ShellEvents_ShutdownStarted()
        {
            XSolution.IsClosing = true;
            XSolution.IsShuttingDown = true;
            XSolution.Close();

            Logger.SingleLine();
            Logger.Information("Shutdown VS");
            Logger.SingleLine();
        }
        #endregion


        #region Document Events
        private void DocumentEvents_Opened(string doc)
        {
            Logger.Information("Opened document: " + doc ?? "");
        }
        #endregion


        public ConcurrentDictionary<string, Project> _projects = new ConcurrentDictionary<string, Project>(StringComparer.OrdinalIgnoreCase);

        public object FindVsProject(string sUrl)
        {
            if (_projects.ContainsKey(sUrl))
                return _projects[sUrl];
            var sol = VS.Solutions.GetCurrentSolution();
            var prj = findProject(sol, sUrl);
            _projects.TryAdd(sUrl, prj);
            return prj;
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
                if (XSharpLexer.IsKeyword(token.Type) || XSharpLexer.IsWordOperator(token.Type))
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
