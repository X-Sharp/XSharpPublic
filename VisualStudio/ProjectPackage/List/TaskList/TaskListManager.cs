//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Shell.TableManager;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;

namespace XSharp.Project
{

    /// <summary>
    /// This class wraps the complexity of the task list
    /// Each project gets its own copy of this class
    /// but they all share the same ListProvider and IList control
    /// and ITableManager manager
    /// </summary>
    internal class TaskListManager : ListManager<ITaskListItem>
    {
        static ConcurrentDictionary<Guid, TaskListManager> _projects;
        static ListProvider _provider = null;
        static ITaskList _taskList;
        static ITableManager _manager;

        static TaskListManager()
        {
            _projects = new ConcurrentDictionary<Guid, TaskListManager>();
            _taskList = XSharpProjectPackage.XInstance.TaskList;
            _manager = _taskList.TableControl.Manager;
            _provider = new TaskListProvider(_manager);
        }

        internal TaskListManager(XSharpProjectNode node) : base(node, _provider)
        {
        }

        internal static TaskListManager RegisterProject(XSharpProjectNode project)
        {

            if (!_projects.ContainsKey(project.ProjectIDGuid))
            {
                var manager = new TaskListManager(project);
                _provider.AddListFactory(manager.Factory);
                _projects.TryAdd(project.ProjectIDGuid, manager);
            }
            _projects.TryGetValue(project.ProjectIDGuid, out var tasklistmanager);
            return tasklistmanager;
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
            AddItem(item);
        }

        public static bool RemoveProject(XSharpProjectNode project)
        {
            if (_projects.TryGetValue(project.ProjectIDGuid, out var entry))
            {
                _provider.RemoveListFactory(entry.Factory);
                return _projects.TryRemove(project.ProjectIDGuid, out _);
            }
            return false;
        }

    }

}
