//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Shell.TableManager;
using System;
using XSharp.Project;

namespace XSharp.Project
{
    internal interface ITaskListItem 
    {
        /// <summary>
        /// Error message
        /// </summary>
        string Comment { get; set; }

        /// <summary>
        /// File name of the error item
        /// </summary>
        string Filename { get; set; }

        /// <summary>
        /// Project name of the error item
        /// </summary>
        string ProjectName { get; set; }

        VSTASKPRIORITY Priority { get; set; }

        int Line { get; set; }
        int Column { get; set; }

        /// <summary>
        /// ProjectGuid
        /// </summary>
        Guid ProjectGuid { get; set; }

        
    }
    internal class TaskListItem : ITaskListItem
    {
        public int Column { get; set; }

        public string Filename { get; set; }
        public int Line { get; set; }
        public VSTASKPRIORITY Priority { get; set; }

        public string Comment { get; set; }

        public string ProjectName { get; set; }

        public Guid ProjectGuid { get; set; }


    }

}