using Community.VisualStudio.Toolkit;

using LanguageService.CodeAnalysis.Text;
using LanguageService.CodeAnalysis.XSharp;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.SyntaxTree;

using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.TextManager.Interop;

using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;

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
                Logger.Information("Initialize XSharpShellLink");
#if LIBRARYMANAGER

                VS.Events.SolutionEvents.OnAfterLoadProject += SolutionEvents_OnAfterLoadProject;
                VS.Events.SolutionEvents.OnBeforeUnloadProject += SolutionEvents_OnBeforeUnloadProject;
#endif
                VS.Events.BuildEvents.SolutionBuildStarted += BuildEvents_SolutionBuildStarted;
                VS.Events.BuildEvents.SolutionBuildDone += BuildEvents_SolutionBuildDone;
                VS.Events.BuildEvents.SolutionBuildCancelled += BuildEvents_SolutionBuildCancelled;


                Logger.Information("Initialized XSharpShellLink");
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
