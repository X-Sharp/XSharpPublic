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
    ///  generating new OutputErrorSnapshot upon errors collection changes
    /// </summary>
    internal class ErrorsFactory : TableEntriesSnapshotFactoryBase
    {
        private readonly IListProvider _errorProvider;
        private List<IErrorListItem> _currentErrors = new List<IErrorListItem>();
        private Guid _projectGuid = Guid.Empty;
        internal ErrorsFactory(IListProvider errorProvider, Guid projectGuid)
        {
            _errorProvider = errorProvider;
            _projectGuid = projectGuid;
            CurrentSnapshot = new ErrorSnapshot(0, _currentErrors, _projectGuid);
        }

        /// <summary>
        /// Current snapshot of errors
        /// </summary>
        internal TableEntriesSnapshotBase CurrentSnapshot { get; private set; }

        private ErrorSnapshot NewSnapShot()
        {
            return new ErrorSnapshot(CurrentSnapshot.VersionNumber + 1, _currentErrors, _projectGuid);
        }

         /// <summary>
        /// Adds a new error to the Factory and updates the CurrentSnapshot
        /// </summary>
        /// <param name="newError"></param>
        internal void AddErrorItem(IErrorListItem newError)
        {
            _currentErrors.Add(newError);
            UpdateErrors();
        }

        /// <summary>
        /// Adds a new errors to the Factory and updates the CurrentSnapshot
        /// </summary>
        /// <param name="newErrors"></param>
        internal void AddErrorItems(IList<IErrorListItem> newErrors)
        {
            _currentErrors.AddRange(newErrors);
            UpdateErrors();
        }

        /// <summary>
        /// Changes the errors collection for the Factory
        /// </summary>
        /// <param name="newErrors"></param>
        internal void SetErrorItems(IList<IErrorListItem> newErrors)
        {
            _currentErrors.Clear();
            _currentErrors.AddRange(newErrors);
            UpdateErrors();
        }

        /// <summary>
        /// Updates the factory errors snapshot
        /// </summary>
        /// <param name="errorsList"></param>
        internal void UpdateErrors()
        {
            CurrentSnapshot = NewSnapShot();
            _errorProvider.UpdateAllSinks(this);
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
