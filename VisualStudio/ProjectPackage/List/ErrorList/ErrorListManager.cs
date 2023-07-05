//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using Microsoft.VisualStudio.Shell.TableManager;
using Microsoft.VisualStudio.Shell;
using System;
using System.Linq;
using System.Collections.Generic;
using XSharpModel;
using System.Collections.Concurrent;
using XSharp.Settings;
namespace XSharp.Project
{

    /// <summary>
    /// This class wraps the complexity of the error list
    /// Each project gets its own copy of this class
    /// but they all share the same ListProvider and IList control
    /// and ITableManager manager
    /// </summary>
    internal class ErrorListManager : ListManager<IErrorListItem>
    {
        static ConcurrentDictionary<Guid, ErrorListManager> _projects;
        static ErrorListProvider _provider = null;
        static IErrorList _errorList;
        static ITableManager _manager;

        internal ErrorListManager(XSharpProjectNode node) : base(node, _provider)
        {
        }

        static ErrorListManager()
        {
            _projects = new ConcurrentDictionary<Guid, ErrorListManager>();
            _errorList = XSharpProjectPackage.XInstance.ErrorList;
            _manager = _errorList.TableControl.Manager;
            _provider = new ErrorListProvider(_manager);
        }
        internal static ErrorListManager RegisterProject(XSharpProjectNode project)
        {
            if (!_projects.ContainsKey(project.ProjectIDGuid))
            {
                var manager = new ErrorListManager(project);
                _provider.AddListFactory(manager.Factory);
                _projects.TryAdd(project.ProjectIDGuid, manager);
            }
            _projects.TryGetValue(project.ProjectIDGuid, out var errorlistmanager);
            return errorlistmanager;
        }
  

        internal void DeleteIntellisenseErrorsFromFile(string fileName)
        {
            var buildErrors = BuildErrors;
            var intellisenseErrors = IntellisenseErrors;
            var newItems = new List<IErrorListItem>();
            var changed = false;

            foreach (var item in intellisenseErrors)
            {
                if (string.Compare(item.Filename, fileName, StringComparison.OrdinalIgnoreCase) != 0)
                {
                    //keep the item
                    newItems.Add(item);
                }
                else
                {
                    changed = true;
                }
            }
            if (changed)
            {
                lock (this)
                {
                    Clear();
                    AddItems(buildErrors);
                    AddItems(newItems);
                }
            }

        }

        private IErrorListItem CreateItem(string file, int line, int column, int length, string errCode,
            string message, MessageSeverity sev, ErrorSource errorSource, string projectName)
        {
            var item = new ErrorListItem()
            {
                Filename = file,
                Line = line,
                Column = column,
                Length = length,
                ErrorCode = errCode,
                Message = message,
                ProjectName = projectName,
                ErrorSource = errorSource,
                Severity = sev,
                BuildTool = errorSource == ErrorSource.Build ? "Build" : "Live",
                ProjectGuid = Project.ProjectIDGuid
            };
            return item;
        }

        internal void AddBuildError(string file, int line, int column, string errCode,
            string message, MessageSeverity sev)
        {

            var item = this.CreateItem(file, line, column, 1, errCode, message, sev, ErrorSource.Build, Project.Caption);
            this.AddItem(item);
        }

        internal void AddIntellisenseError(string file, int line, int column, int length, string errCode,
            string message, MessageSeverity sev)
        {
            var item = this.CreateItem(file, line, column, length, errCode, message, sev, ErrorSource.Other, Project.Caption);
            this.AddItem(item);

        }

        IList<IErrorListItem> BuildErrors => Items.Where(i => i.ErrorSource == ErrorSource.Build).ToArray();
        IList<IErrorListItem> IntellisenseErrors => Items.Where(i => i.ErrorSource != ErrorSource.Build).ToArray();

        internal void ClearBuildErrors()
        {
            var items = IntellisenseErrors;
            this.Clear();
            AddItems(items);
        }


        internal override void Refresh()
        {
            if (!dirty)
                return;
            // dedupe errors based on filename, row, column, 
            var errors = new List<IErrorListItem>();
            HashSet<string> keys = new HashSet<string>();

            var buildErrors = BuildErrors;
            foreach (var item in buildErrors)
            {
                string key = item.Key;
                if (!keys.Contains(key))
                {
                    errors.Add(item);
                    keys.Add(key);
                }
                else
                {
                    ; // duplicate item
                }
            }
            var intellisenseErrors = IntellisenseErrors;
            Dictionary<string, bool> filenames = new Dictionary<string, bool>();

            foreach (var item in intellisenseErrors)
            {
                string key = item.Key;
                string file = item.Filename.ToLower();
                bool isOpen;
                if (filenames.ContainsKey(file))
                {
                    isOpen = filenames[file];
                }
                else
                {
                    isOpen = XSettings.IsDocumentOpen(file);
                    filenames.Add(file, isOpen);
                }
                if (isOpen && !keys.Contains(key))
                {
                    errors.Add(item);
                    keys.Add(key);
                }
                else
                {
                    ; // duplicate item or file is closed
                }
            }
            lock (this)
            {
                Factory.SetItems(errors);
                dirty = false;
            }
        }

        internal List<IXErrorPosition> GetIntellisenseErrorPos(string fileName)
        {
            // dedupe errors based on filename, row, column, 
            List<IXErrorPosition> errorPos = new List<XSharpModel.IXErrorPosition>();
            Dictionary<string, string> keys = new Dictionary<string, string>();

            if (_errorList.AreOtherErrorSourceEntriesShown)
            {
                var intellisenseErrors = IntellisenseErrors;
                Dictionary<string, bool> filenames = new Dictionary<string, bool>();
                fileName = fileName.ToLower();

                foreach (var item in intellisenseErrors)
                {
                    string key = item.Key;
                    string file = item.Filename.ToLower();
                    bool isOpen;
                    if (filenames.ContainsKey(file))
                    {
                        isOpen = filenames[file];
                    }
                    else
                    {
                        isOpen = XSettings.IsDocumentOpen(file);
                        filenames.Add(file, isOpen);
                    }
                    if (isOpen && !keys.ContainsKey(key) && (file == fileName))
                    {
                        errorPos.Add(item);
                        keys.Add(key, key);
                    }
                    else
                    {
                        ; // duplicate item or file is closed
                    }
                }
            }
            return errorPos;
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
