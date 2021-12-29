using System;
using XSharpModel;
using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.TextManager.Interop;

namespace XSharp.Project
{
    internal class XSharpShellLink : IXVsShellLink
    {

        bool building;
        bool success;

        internal XSharpShellLink()
        {
            ThreadHelper.JoinableTaskFactory.Run(async delegate
           {
               await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
               VS.Events.BuildEvents.SolutionBuildStarted += BuildEvents_SolutionBuildStarted;
               VS.Events.BuildEvents.SolutionBuildDone += BuildEvents_SolutionBuildDone;
               VS.Events.BuildEvents.SolutionBuildCancelled += BuildEvents_SolutionBuildCancelled;
           });
            
        }

        private void BuildEvents_SolutionBuildCancelled()
        {
            building = false;
        }

        private void BuildEvents_SolutionBuildDone(bool result)
        {
            building = false;
            success = result;
        }

        private void BuildEvents_SolutionBuildStarted(object sender, EventArgs e)
        {
            building = true;
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

        public void WriteException(Exception ex)
        {
            XSharpOutputPane.DisplayException(ex);
        }

        public void WriteOutputMessage(string message)
        {
            XSharpOutputPane.DisplayOutputMessage(message);
        }


        private async System.Threading.Tasks.Task OpenDocumentAsync(string file, int line, int column, bool preview)
        {
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
            var textView = await view.TextView.ToIVsTextViewAsync();
            if (textView != null)
            {
                //
                TextSpan span = new TextSpan();
                span.iStartLine = line - 1;
                span.iStartIndex = column - 1;
                span.iEndLine = line - 1;
                span.iEndIndex = column - 1;
                //
                textView.SetCaretPos(span.iStartLine, span.iStartIndex);
                textView.EnsureSpanVisible(span);
                if (span.iStartLine > 5)
                    textView.SetTopLine(span.iStartLine - 5);
                else
                    textView.SetTopLine(0);
            }
            if (preview)
                await VS.Documents.OpenInPreviewTabAsync(file);
            else
                await VS.Documents.OpenAsync(file);
            
        }

        public bool IsDocumentOpen(string file)
        {
            return ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                return await VS.Documents.IsOpenAsync(file);
            });
        }
        public void OpenDocument(string file, int line, int column, bool preview)
        {
            OpenDocumentAsync(file, line, column, preview).FireAndForget();
        }

        public bool IsVsBuilding => building;
        public bool LastBuildResult => success;


    }
}
