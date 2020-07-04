//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using Microsoft.VisualStudio.Shell.TableManager;
using System;
using System.Collections.Generic;
using System.Linq;


namespace XSharp.Project
{
    /// <summary>
    /// Manages the errors collection and updates the CurrentSnapshot by
    ///  generating new Snapshot upon collection changes
    /// </summary>
    internal class TasksFactory : TableEntriesSnapshotFactoryBase
    {
        private readonly IListProvider _listProvider;
        private List<ITaskListItem> _currentTasks = new List<ITaskListItem>();
        private Guid _projectGuid = Guid.Empty;
        internal TasksFactory(IListProvider errorProvider, Guid projectGuid)
        {
            _listProvider = errorProvider;
            _projectGuid = projectGuid;
            CurrentSnapshot = new TasksSnapshot(0, _currentTasks, _projectGuid);
        }

        /// <summary>
        /// Current snapshot of errors
        /// </summary>
        internal TableEntriesSnapshotBase CurrentSnapshot { get; private set; }

        private TasksSnapshot NewSnapShot()
        {
            return new TasksSnapshot(CurrentSnapshot.VersionNumber + 1, _currentTasks, _projectGuid);
        }

         /// <summary>
        /// Adds a new error to the Factory and updates the CurrentSnapshot
        /// </summary>
        /// <param name="newError"></param>
        internal void AddItem(ITaskListItem newError)
        {
            _currentTasks.Add(newError);
            UpdateItems();
        }

        /// <summary>
        /// Adds a new errors to the Factory and updates the CurrentSnapshot
        /// </summary>
        /// <param name="newErrors"></param>
        internal void AddItems(IList<ITaskListItem> newItems)
        {
            _currentTasks.AddRange(newItems);
            UpdateItems();
        }

        /// <summary>
        /// Changes the errors collection for the Factory
        /// </summary>
        /// <param name="newErrors"></param>
        internal void SetItems(IList<ITaskListItem> newItems)
        {
            _currentTasks.Clear();
            _currentTasks.AddRange(newItems);
            UpdateItems();
        }

        /// <summary>
        /// Updates the factory errors snapshot
        /// </summary>
        /// <param name="errorsList"></param>
        internal void UpdateItems()
        {
            CurrentSnapshot = NewSnapShot();
            _listProvider.UpdateAllSinks(this);
        }

        #region ITableEntriesSnapshotFactory members
        public override int CurrentVersionNumber
        {
            get
            {
                return CurrentSnapshot.VersionNumber;
            }
        }

        public override ITableEntriesSnapshot GetCurrentSnapshot()
        {
            return CurrentSnapshot;
        }

        public override ITableEntriesSnapshot GetSnapshot(int versionNumber)
        {
            // In theory the snapshot could change in the middle of the return statement so snap the snapshot just to be safe.
            var snapshot = this.CurrentSnapshot;
            return (versionNumber == snapshot.VersionNumber) ? snapshot : null;
        }

        #endregion
    }

}
