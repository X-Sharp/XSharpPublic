//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Shell.TableControl;
using Microsoft.VisualStudio.Shell.TableManager;
using System;
using System.Collections.Generic;


namespace XSharp.Project
{
    internal class TasksSnapshot : WpfTableEntriesSnapshotBase
    {
        private const int UndefinedLineOrColumn = -1;
        private readonly int _versionNumber;
        public readonly List<ITaskListItem> Tasks;
        private Guid _projectGuid;
        protected const string ProjectNames = StandardTableKeyNames.ProjectName + "s";
        protected const string ProjectGuids = StandardTableKeyNames.ProjectGuid + "s";
        internal TasksSnapshot(int versionNumber, IList<ITaskListItem> tasks, Guid projectGuid)
        {
            _versionNumber = versionNumber;
            _projectGuid = projectGuid;
            Tasks = new List<ITaskListItem>(tasks);
        }

        public override int Count
        {
            get
            {
                return Tasks.Count;
            }
        }

        public override int VersionNumber
        {
            get
            {
                return _versionNumber;
            }
        }

        /// <summary>
        /// Attempts to get a column value for the item at index in the snapshot. Note that this method
        /// gets called for any columns that the control supports. If we do not have a value
        /// to supply for the column or if somehow we got an invalid index the content needs to be
        /// set to null and return false which lets the table control set the value.
        /// </summary>
        public override bool TryGetValue(int index, string columnName, out object content)
        {
            if (index >= 0 && index < Tasks.Count)
            {
                var task = Tasks[index];
                switch (columnName)
                {
                    case StandardTableKeyNames.Priority:
                        content = task.Priority;
                        return true;
                    case StandardTableKeyNames.Text:
                        content = task.Comment;
                        return true;
                    case StandardTableKeyNames.ProjectName:
                        content = task.ProjectName;
                        return true;
                    case StandardTableKeyNames.DocumentName:
                        content = task.Filename;
                        return true;
                    case StandardTableKeyNames.Line:
                        content = GetAdjustedLineOrColumn(task.Line);
                        return true;
                    case StandardTableKeyNames.Column:
                        content = GetAdjustedLineOrColumn(task.Column);
                        return true;
                    case StandardTableKeyNames.TaskCategory:
                        content = VSTASKCATEGORY.CAT_COMMENTS;
                        return true;
                    case StandardTableKeyNames.ProjectGuid:
                        content = task.ProjectGuid; // _projectGuid;
                        return true;
                    case StandardTableKeyNames.HelpKeyword:
                        content = "";
                        return true;
                    case ProjectNames:
                        content = new string[] { task.ProjectName };
                        return true;
                    case ProjectGuids:
                        content = new Guid[] { task.ProjectGuid };
                        return true;

                }
            }

            // This method gets called for anything that the control supports. If we do not have a value
            // to supply for the column or if somehow we got an invalid index the content needs to be
            // set to null and return false which lets the table control set the value.
            content = null;
            return false;
        }


        /// <summary>
        /// Helper to account for the fact the error list is 0 based.
        /// </summary>
        private static int GetAdjustedLineOrColumn(int lineOrColumn)
        {
            if (lineOrColumn != UndefinedLineOrColumn)
            {
                return lineOrColumn > 0 ? lineOrColumn - 1 : 0;
            }

            return lineOrColumn;
        }
    }
}
