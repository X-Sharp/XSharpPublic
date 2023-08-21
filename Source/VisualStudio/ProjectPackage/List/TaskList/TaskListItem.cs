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
    internal interface ITaskListItem : IListItem
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
        protected const string ProjectNames = StandardTableKeyNames.ProjectName + "s";
        protected const string ProjectGuids = StandardTableKeyNames.ProjectGuid + "s";


        public int Column { get; set; }

        public string Filename { get; set; }
        public int Line { get; set; }
        public VSTASKPRIORITY Priority { get; set; }

        public string Comment { get; set; }

        public string ProjectName { get; set; }

        public Guid ProjectGuid { get; set; }


        public bool GetValue(string columnName, out object content)
        {
            switch (columnName)
            {
                case StandardTableKeyNames.Priority:
                    content = this.Priority;
                    return true;
                case StandardTableKeyNames.Text:
                    content = this.Comment;
                    return true;
                case StandardTableKeyNames.ProjectName:
                    content = this.ProjectName;
                    return true;
                case StandardTableKeyNames.DocumentName:
                    content = this.Filename;
                    return true;
                case StandardTableKeyNames.Line:
                    content = ListHelpers.GetAdjustedLineOrColumn(this.Line);
                    return true;
                case StandardTableKeyNames.Column:
                    content = ListHelpers.GetAdjustedLineOrColumn(this.Column);
                    return true;
                case StandardTableKeyNames.TaskCategory:
                    content = VSTASKCATEGORY.CAT_COMMENTS;
                    return true;
                case StandardTableKeyNames.ProjectGuid:
                    content = this.ProjectGuid; // _projectGuid;
                    return true;
                case StandardTableKeyNames.HelpKeyword:
                    content = "";
                    return true;
                case ProjectNames:
                    content = new string[] { this.ProjectName };
                    return true;
                case ProjectGuids:
                    content = new Guid[] { this.ProjectGuid };
                    return true;

            }
            content = null;
            return false;
        }
    }

}
