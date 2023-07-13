//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.Build.Framework;
using Microsoft.VisualStudio.Shell.TableManager;
using XSharpModel;
using XSharp.Settings;
namespace XSharp.Project
{
    internal class XSharpIDEBuildLogger : IDEBuildLogger
    {
        ErrorListManager errorlistManager;
        XSharpProjectNode node;
        bool mustLog;
        private XSharpModel.ILogger Logger => XSettings.Logger;
        internal XSharpIDEBuildLogger(IVsOutputWindowPane output, TaskProvider taskProvider, IVsHierarchy hierarchy) : base(output, taskProvider, hierarchy)
        {

        }
        internal ErrorListManager ErrorListManager
        {
            get { return errorlistManager; }
            set
            {
                errorlistManager = value;
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
            errorlistManager.ClearBuildErrors();
            errorlistManager.Refresh();
        }
        protected int errors;
        protected int warnings;
        protected bool mustRefresh = false;

        protected override void BuildStartedHandler(object sender, BuildStartedEventArgs buildEvent)
        {
            try
            {
                base.BuildStartedHandler(sender, buildEvent);
                errorlistManager.ClearBuildErrors();
                errors = warnings = 0;
                mustRefresh = false;
                Logger.Debug("Build Started");
            }
            catch (Exception e)
            {
                Logger.Exception(e, "BuildStartedHandler");
            }
        }
        protected override void BuildFinishedHandler(object sender, BuildFinishedEventArgs buildEvent)
        {
            if (mustRefresh)
            {
                try
                {
                    var msg = $"{warnings} Warning(s), {errors} Error(s)";
                    if (errors != 0)
                    {
                        QueueOutputText(MessageImportance.High, msg+"\n");
                    }
                    else
                    {

                        QueueOutputText(MessageImportance.Normal, msg+"\n");
                    }
                    base.BuildFinishedHandler(sender, buildEvent);
                    Logger.Debug("Build Finished : " +msg  );
                }
                catch (Exception e)
                {
                    Logger.Exception(e, "BuildFinishedHandler");
                }
            }
        }
        protected override void ProjectStartedHandler(object sender, ProjectStartedEventArgs buildEvent)
        {
            try
            {
                base.ProjectStartedHandler(sender, buildEvent);
                Logger.Debug("Project Build Started " + buildEvent.ProjectFile );
                if (this.ProjectNode is XSharpProjectNode xprj)
                {
                    xprj.BuildStarted();
                }
                mustLog = true;
            }
            catch (Exception e)
            {
                Logger.Exception(e, "ProjectStartedHandler");
            }
        }
        protected override void ProjectFinishedHandler(object sender, ProjectFinishedEventArgs buildEvent)
        {
            try
            {
                base.ProjectFinishedHandler(sender, buildEvent);
                Logger.Debug("Project Build Finished " + buildEvent.ProjectFile );
                if (mustRefresh)
                {
                    errorlistManager.Refresh();
                }
                if (this.ProjectNode is XSharpProjectNode  xprj)
                {
                    xprj.BuildEnded(mustRefresh);
                }
            }
            catch (Exception e)
            {
                Logger.Exception(e, "ProjectFinishedHandler");
            }

        }

        protected override void QueueTaskEvent(BuildEventArgs errorEvent)
        {
            try
            {
                if (mustLog)
                {
                    if (errorEvent is BuildErrorEventArgs)
                    {
                        ReportError((BuildErrorEventArgs)errorEvent);
                        errors += 1;
                    }
                    else
                    {
                        ReportWarning((BuildWarningEventArgs)errorEvent);
                        warnings += 1;
                    }
                }
            }
            catch (Exception e)
            {
                Logger.Exception(e, "QueueTaskEvent");
            }

        }
        protected override void MessageHandler(object sender, BuildMessageEventArgs messageEvent)
        {
            try
            {
                base.MessageHandler(sender, messageEvent);
                if (messageEvent is TaskCommandLineEventArgs)
                {
                    var taskEvent = messageEvent as TaskCommandLineEventArgs;
                    var cmdLine = taskEvent.CommandLine.ToLower();
                    if (cmdLine.Contains("xsc.exe"))
                    {
                        mustRefresh = true;
                    }
                    else if (cmdLine.Contains("rc.exe"))
                    {
                        mustRefresh = true;
                    }
                }
            }
            catch (Exception e)
            {
                Logger.Exception(e, "MessageHandler");
            }
        }
        protected void ReportError(BuildErrorEventArgs args)
        {
            try
            {
                mustRefresh = true;
                string msg = $"{args.File} {args.LineNumber} {args.ColumnNumber} {args.Code} {args.Message}";
                errorlistManager.AddBuildError(args.File, args.LineNumber, args.ColumnNumber, args.Code, args.Message, MessageSeverity.Error);
                Logger.Debug("Build Error: "+ msg);
            }
            catch (Exception e)
            {
                Logger.Exception(e, "ReportError");
            }
        }
        protected void ReportWarning(BuildWarningEventArgs args)
        {
            try
            {
                mustRefresh = true;
                string msg = $"{args.File} {args.LineNumber} {args.ColumnNumber} {args.Code} {args.Message}";
                errorlistManager.AddBuildError(args.File, args.LineNumber, args.ColumnNumber, args.Code, args.Message, MessageSeverity.Warning);
                Logger.Debug("Build Warning: " + msg);
            }
            catch (Exception e)
            {
                Logger.Exception(e, "ReportWarning");
            }
        }
    }
}
