using System;
using System.Diagnostics;
using System.Globalization;
using System.CodeDom.Compiler;
using System.Runtime.InteropServices;
using System.Text;
using System.Text.RegularExpressions;

using Microsoft.Build.Framework;
using Microsoft.Build.Utilities;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.Win32;

using IOleServiceProvider = Microsoft.VisualStudio.OLE.Interop.IServiceProvider;
using Microsoft.VisualStudio.Project;
namespace XSharp.Project
{
    /// <summary>
    /// This class implements an MSBuild logger that output events to 
    /// VS outputwindow and tasklist.
    /// </summary>
    [ComVisible(true)]
    internal class XSharpIDEBuildLogger : IDEBuildLogger
    {
        /// <summary>
        /// Constructor.  Inititialize member data.
        /// </summary>
        /// 

        IVsHierarchy _hierarchy;
        public XSharpIDEBuildLogger(IVsOutputWindowPane output, TaskProvider taskProvider, IVsHierarchy hierarchy) : 
            base(output, taskProvider, hierarchy)
        {
            _hierarchy = hierarchy;
        }



        /// <summary>
        /// Method was duplicated from IDEBuildLogger because that method does not add a NavigateTo
        /// </summary>
        /// <param name="errorEvent"></param>
        protected override void QueueTaskEvent(BuildEventArgs errorEvent)
        {
            this.taskQueue.Enqueue(() =>
            {
                ErrorTask task = new ErrorTask();

                if (errorEvent is BuildErrorEventArgs)
                {
                    BuildErrorEventArgs errorArgs = (BuildErrorEventArgs)errorEvent;
                    task.Document = errorArgs.File;
                    task.ErrorCategory = TaskErrorCategory.Error;
                    task.Line = errorArgs.LineNumber - 1; // The task list does +1 before showing this number.
                    task.Column = errorArgs.ColumnNumber;
                    task.Priority = TaskPriority.High;
                }
                else if (errorEvent is BuildWarningEventArgs)
                {
                    BuildWarningEventArgs warningArgs = (BuildWarningEventArgs)errorEvent;
                    task.Document = warningArgs.File;
                    task.ErrorCategory = TaskErrorCategory.Warning;
                    task.Line = warningArgs.LineNumber - 1; // The task list does +1 before showing this number.
                    task.Column = warningArgs.ColumnNumber;
                    task.Priority = TaskPriority.Normal;
                }

                task.Text = errorEvent.Message;
                task.Category = TaskCategory.BuildCompile;
                task.HierarchyItem = _hierarchy;
                task.Navigate += new EventHandler(NavigateTo);
                return task;
            });
        }

        private void NavigateTo(object sender, EventArgs arguments)
        {
            Microsoft.VisualStudio.Shell.Task task =
                sender as Microsoft.VisualStudio.Shell.Task;

            if (task == null)
                throw new ArgumentException("sender");

            // Get the doc data for the task's document
            if (String.IsNullOrEmpty(task.Document))
                return;

            IVsUIShellOpenDocument openDoc = ServiceProvider.GetService(
                typeof(IVsUIShellOpenDocument)) as IVsUIShellOpenDocument;

            if (openDoc == null)
                return;

            IVsWindowFrame frame;
            IOleServiceProvider sp;
            IVsUIHierarchy hier;
            uint itemid;
            Guid logicalView = VSConstants.LOGVIEWID_Code;

            if (Microsoft.VisualStudio.ErrorHandler.Failed(openDoc.OpenDocumentViaProject(
                task.Document, ref logicalView, out sp, out hier, out itemid, out frame))
                || frame == null
            )
                return;

            object docData;
            frame.GetProperty((int)__VSFPROPID.VSFPROPID_DocData, out docData);

            // Get the VsTextBuffer
            VsTextBuffer buffer = docData as VsTextBuffer;
            if (buffer == null)
            {
                IVsTextBufferProvider bufferProvider = docData as IVsTextBufferProvider;
                if (bufferProvider != null)
                {
                    IVsTextLines lines;
                    Microsoft.VisualStudio.ErrorHandler.ThrowOnFailure(bufferProvider.GetTextBuffer(out lines));
                    buffer = lines as VsTextBuffer;
                    Debug.Assert(buffer != null, "IVsTextLines does not implement IVsTextBuffer");

                    if (buffer == null)
                        return;
                }
            }

            // Finally, perform the navigation.
            IVsTextManager mgr = ServiceProvider.GetService(
                typeof(VsTextManagerClass)) as IVsTextManager;

            if (mgr == null)
                return;

            mgr.NavigateToLineAndColumn(buffer, ref logicalView,
                task.Line, task.Column, task.Line, task.Column);
        }

    }
}
