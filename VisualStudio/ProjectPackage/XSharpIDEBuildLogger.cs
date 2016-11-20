using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.Build.Framework;
namespace XSharp.Project
{
    internal class XSharpIDEBuildLogger : IDEBuildLogger
    {
        ErrorListProvider errorlistProvider;
        OutputErrorsFactory outputErrors;
        List<IErrorListItem> errorItems;
        XSharpProjectNode node;
        bool mustLog;
        internal XSharpIDEBuildLogger(IVsOutputWindowPane output, TaskProvider taskProvider, IVsHierarchy hierarchy) : base(output, taskProvider, hierarchy)
        {

        }
        internal ErrorListProvider ErrorlistProvider
        {
            get { return errorlistProvider; }
            set {
                errorlistProvider = value;
                outputErrors = new OutputErrorsFactory(value);
                errorlistProvider.AddErrorListFactory(outputErrors);
            }
        }
        internal XSharpProjectNode ProjectNode
        {
            get { return node; }
            set { node = value; }
        }

        public override void Initialize(IEventSource eventSource)
        {
            base.Initialize(eventSource);
        }


        internal void Clear()
        {
            if (outputErrors != null)
                outputErrors.ClearErrors();
        }
        protected override void BuildStartedHandler(object sender, BuildStartedEventArgs buildEvent)
        {
            base.BuildStartedHandler(sender, buildEvent);
            Clear();
        }
        protected override void ProjectStartedHandler(object sender, ProjectStartedEventArgs buildEvent)
        {
            base.ProjectStartedHandler(sender, buildEvent);
            if (String.IsNullOrEmpty(buildEvent.TargetNames))
            {
                errorItems = new List<IErrorListItem>();
                mustLog = true;
            }
            else
            {
                mustLog = false;
            }
        }
        protected override void ProjectFinishedHandler(object sender, ProjectFinishedEventArgs buildEvent)
        {
            base.ProjectFinishedHandler(sender, buildEvent);
            if (mustLog)
            {
                outputErrors.ClearErrors();
                outputErrors.AddErrorItems(errorItems);
            }
        }

        protected override void QueueTaskEvent(BuildEventArgs errorEvent)
        {
            if (mustLog)
            {
                if (errorEvent is BuildErrorEventArgs)
                    ReportError((BuildErrorEventArgs)errorEvent);
                else
                    ReportWarning((BuildWarningEventArgs)errorEvent);
            }

        }

        protected void ReportError(BuildErrorEventArgs args)
        {
            var error = new ErrorListItem()
            {
                Column = args.ColumnNumber,
                ErrorCategory = "Build",
                ErrorCode = args.Code,
                ErrorSource = Constants.Product,
                Filename = args.File,
                Line = args.LineNumber,
                Message = args.Message,
                ProjectName = ProjectNode.Caption,
                Severity = MessageSeverity.Error
            };
            errorItems.Add(error);
        }
        protected void ReportWarning(BuildWarningEventArgs args)
        {
            var error = new ErrorListItem()
            {
                Column = args.ColumnNumber,
                ErrorCategory = "Build",
                ErrorCode = args.Code,
                ErrorSource = Constants.Product,
                Filename = args.File,
                Line = args.LineNumber,
                Message = args.Message,
                ProjectName = ProjectNode.Caption,
                Severity = MessageSeverity.Warning
            };
            errorItems.Add(error);
        }

        internal class ErrorListItem : IErrorListItem
        {
            public int Column { get; set; }

            public object ErrorCode { get; set; }


            public object ErrorSource { get; set; }

            public string Filename { get; set; }
            public int Line { get; set; }

            public string Message { get; set; }

            public string ProjectName { get; set; }

            public MessageSeverity Severity { get; set; }
            public string ErrorCategory { get; set; }
        }
    }
}