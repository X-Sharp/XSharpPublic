﻿//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using Microsoft.VisualStudio.Shell.TableManager;
using Microsoft.VisualStudio.Shell;
using System;
using System.Collections.Generic;
using XSharpModel;

namespace XSharp.Project
{

    /// <summary>
    /// This class wraps the complexity of the error list
    /// Each project gets its own copy of this class
    /// but they all share the same ErrorListProvider and IErrorList control
    /// and ITableManager manager
    /// </summary>
    internal class ErrorListManager
    {
        static Dictionary<Guid, ErrorListManager> _projects;
        static ErrorListProvider _provider = null;
        static IErrorList _errorList;
        static ITableManager _manager;
        static object _gate;
        private XSharpProjectNode Project { get; set; }
        private ErrorsFactory Factory { get; set; }
        internal IList<IErrorListItem> BuildErrors { get; set; }
        internal IList<IErrorListItem> IntellisenseErrors { get; set; }
        bool dirty;

        internal ErrorListManager(XSharpProjectNode node)
        {
            Project = node;
            BuildErrors = new List<IErrorListItem>();
            IntellisenseErrors = new List<IErrorListItem>();
        }

        static ErrorListManager()
        {
            _projects = new Dictionary<Guid, ErrorListManager>();
            _gate = new object();
            _errorList = XSharpProjectPackage.XInstance.ErrorList;
            _manager = _errorList.TableControl.Manager;
            _provider = new ErrorListProvider(_manager);
        }
        internal static ErrorListManager RegisterProject(XSharpProjectNode project)
        {
            if (!_projects.ContainsKey(project.ProjectIDGuid))
            {
                var manager = new ErrorListManager(project);
                manager.Factory = new ErrorsFactory(_provider, project.ProjectIDGuid);
                _provider.AddListFactory(manager.Factory);
                _projects.Add(project.ProjectIDGuid, manager);
            }
            return _projects[project.ProjectIDGuid];
        }
  

        internal void DeleteIntellisenseErrorsFromFile(string fileName)
        {
            var newItems = new List<IErrorListItem>();
            IList<IErrorListItem> oldItems;
            bool changed = false;
            lock (this)
            {
                // store locally to prevent MT errors
                oldItems = IntellisenseErrors;
            }
            foreach (var item in oldItems)
            {
                if (String.Compare(item.Filename, fileName, StringComparison.OrdinalIgnoreCase) != 0)
                {
                    //keep the item
                    newItems.Add(item);
                }
                else
                    changed = true;
            }
            if (changed)
            {
                lock (this)
                {
                    IntellisenseErrors = newItems;
                    dirty = true;
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
            this.AddError(BuildErrors, item );
        }

        internal void AddIntellisenseError(string file, int line, int column, int length, string errCode,
            string message, MessageSeverity sev)
        {
            var item = this.CreateItem(file, line, column, length, errCode, message, sev, ErrorSource.Other, Project.Caption);
            this.AddError(IntellisenseErrors, item);
        }

        internal void ClearBuildErrors()
        {
            lock (this)
            {
                // Replace collection to prevent MT errors
                if (BuildErrors.Count != 0)
                {
                    BuildErrors = new List<IErrorListItem>();
                    dirty = true;
                }

            }
        }

        private void AddError(IList<IErrorListItem> errors, IErrorListItem item)
        {
            lock (this)
            {
                errors.Add(item);
                dirty = true;
            }
        }

        internal void Refresh()
        {
            if (!dirty)
                return;
            // dedupe errors based on filename, row, column, 
            IList<IErrorListItem> errors = new List<IErrorListItem>();
            Dictionary<string, string> keys = new Dictionary<string, string>();

            if (_errorList.AreBuildErrorSourceEntriesShown)
            {
                var buildErrors = BuildErrors;
                foreach (var item in buildErrors)
                {
                    string key = item.Key;
                    if (!keys.ContainsKey(key))
                    {
                        errors.Add(item);
                        keys.Add(key, key);
                    }
                    else
                    {
                        ; // duplicate item
                    }
                }
            }
            if (_errorList.AreOtherErrorSourceEntriesShown)
            {
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
                    if (isOpen && !keys.ContainsKey(key))
                    {
                        errors.Add(item);
                        keys.Add(key, key);
                    }
                    else
                    {
                        ; // duplicate item or file is closed
                    }
                }
            }
            lock (this)
            {
                Factory.SetErrorItems(errors);
                dirty = false;
            }
        }

        internal List<XSharpModel.IXErrorPosition> GetIntellisenseErrorPos(string fileName)
        {
            // dedupe errors based on filename, row, column, 
            List<XSharpModel.IXErrorPosition> errorPos = new List<XSharpModel.IXErrorPosition>();
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
            if (!_projects.ContainsKey(project.ProjectIDGuid))
                return false;

            var entry = _projects[project.ProjectIDGuid];
            _provider.RemoveListFactory(entry.Factory);
            return _projects.Remove(project.ProjectIDGuid);
        }

    }

}
