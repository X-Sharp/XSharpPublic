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

namespace XSharp.Project
{
    internal class XSharpIDEBuildLogger : IDEBuildLogger
    {
        ErrorListManager errorlistManager;
        XSharpProjectNode node;
        bool mustLog;
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
        protected bool didCompile = false;

        protected override void BuildStartedHandler(object sender, BuildStartedEventArgs buildEvent)
        {
            try
            {
                base.BuildStartedHandler(sender, buildEvent);
                errorlistManager.ClearBuildErrors();
                errors = warnings = 0;
                didCompile = false;
            }
            catch (Exception e)
            {
                System.Diagnostics.Debug.WriteLine(e.Message);
            }
        }
        protected override void BuildFinishedHandler(object sender, BuildFinishedEventArgs buildEvent)
        {
            if (didCompile)
            {
                try
                {
                    QueueOutputText(MessageImportance.High, $"{warnings} Warning(s), {errors} Error(s)\n");
                    base.BuildFinishedHandler(sender, buildEvent);
                }
                catch (Exception e)
                {
                    System.Diagnostics.Debug.WriteLine(e.Message);
                }
            }
        }
        protected override void ProjectStartedHandler(object sender, ProjectStartedEventArgs buildEvent)
        {
            try
            {
                base.ProjectStartedHandler(sender, buildEvent);
                mustLog = true;
            }
            catch (Exception e)
            {
                System.Diagnostics.Debug.WriteLine(e.Message);
            }
        }
        protected override void ProjectFinishedHandler(object sender, ProjectFinishedEventArgs buildEvent)
        {
            try
            {
                base.ProjectFinishedHandler(sender, buildEvent);
                if (didCompile)
                {
                    errorlistManager.Refresh();
                }
            }
            catch (Exception e)
            {
                System.Diagnostics.Debug.WriteLine(e.Message);
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
                System.Diagnostics.Debug.WriteLine(e.Message);
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
                    if (taskEvent.CommandLine.ToLower().Contains("xsc.exe"))
                    {
                        didCompile = true;
                    }
                }
                else if (messageEvent is BuildMessageEventArgs)
                {
                    var bme = messageEvent as BuildMessageEventArgs;
                    if (bme.SenderName?.ToLower() == "nativeresourcecompiler")
                    {
                        didCompile = true;
                    }
                }
            }
            catch (Exception e)
            {
                System.Diagnostics.Debug.WriteLine(e.Message);
            }
        }
        protected void ReportError(BuildErrorEventArgs args)
        {
            try
            {
                errorlistManager.AddBuildError(args.File, args.LineNumber, args.ColumnNumber, args.Code, args.Message, MessageSeverity.Error);
            }
            catch (Exception e)
            {
                System.Diagnostics.Debug.WriteLine(e.Message);
            }
        }
        protected void ReportWarning(BuildWarningEventArgs args)
        {
            try
            {
                errorlistManager.AddBuildError(args.File, args.LineNumber, args.ColumnNumber, args.Code, args.Message, MessageSeverity.Warning);
            }
            catch (Exception e)
            {
                System.Diagnostics.Debug.WriteLine(e.Message);
            }
        }
    }
}