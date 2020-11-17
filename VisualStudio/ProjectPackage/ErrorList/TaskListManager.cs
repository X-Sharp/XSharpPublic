//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Shell.TableManager;
using System;
using System.Collections.Generic;

namespace XSharp.Project
{

    /// <summary>
    /// This class wraps the complexity of the error list
    /// Each project gets its own copy of this class
    /// but they all share the same ErrorListProvider and IErrorList control
    /// and ITableManager manager
    /// </summary>
    internal class TaskListManager
    {
        static Dictionary<Guid, TaskListManager> _projects;
        static TaskListProvider _provider = null;
        static ITaskList _taskList;
        static ITableManager _manager;
        static object _gate;
        private XSharpProjectNode Project { get; set; }
        private TasksFactory Factory { get; set; }
        internal IList<ITaskListItem> Items { get; set; }
        bool dirty;

        internal TaskListManager(XSharpProjectNode node)
        {
            Project = node;
            Items = new List<ITaskListItem>();
        }

        static TaskListManager()
        {
            _projects = new Dictionary<Guid, TaskListManager>();
            _gate = new object();
            _taskList = XSharpProjectPackage.XInstance.TaskList;
            _manager = _taskList.TableControl.Manager;
            _provider = new TaskListProvider(_manager);
        }
        internal static TaskListManager RegisterProject(XSharpProjectNode project)
        {

            if (!_projects.ContainsKey(project.ProjectIDGuid))
            {
                var manager = new TaskListManager(project);
                manager.Factory = new TasksFactory(_provider, project.ProjectIDGuid);
                _provider.AddListFactory(manager.Factory);
                _projects.Add(project.ProjectIDGuid, manager);
            }
            return _projects[project.ProjectIDGuid];
        }
      



        internal void Clear()
        {
            lock (this)
            {
                // Replace collection to prevent MT errors
                if (Items.Count != 0)
                {
                    Items = new List<ITaskListItem>();
                    dirty = true;
                }
            }
        }
        internal void AddItem(XSharpModel.XCommentTask task, Guid guid)
        {
            var item = new TaskListItem()
            {
                Filename = task.File.FullPath,
                Line = task.Line,
                Column = task.Column,
                Comment = task.Comment,
                ProjectName = task.File.Project.Name,
                ProjectGuid = guid,
                Priority = (VSTASKPRIORITY) task.Priority
            };
            lock (this)
            {
                Items.Add(item);
                dirty = true;
            }
        }

        internal void Refresh()
        {
            if (!dirty)
                return;

            lock (this)
            {
                Factory.SetItems(Items);
                dirty = false;
            }
        }


        public static bool RemoveProject(XSharpProjectNode project)
        {
            if (!_projects.ContainsKey(project.ProjectIDGuid))
                return false;
            var entry = _projects[project.ProjectIDGuid];
            _provider.RemoveListFactory(entry.Factory);

            return _projects.Remove(project.ProjectIDGuid);
        }

    }

}
