using Microsoft.VisualStudio.Shell.TableManager;
using System.Collections.Generic;
using System.Linq;
using Microsoft.VisualStudio.TaskRunnerExplorer;

namespace TaskOutputListener
{
    /// <summary>
    /// Manages the errors collection and updates the CurrentSnapshot by
    ///  generating new OutputErrorSnapshot upon errors collection changes
    /// </summary>
    internal class OutputErrorsFactory : TableEntriesSnapshotFactoryBase
    {
        private readonly IErrorListProvider _errorProvider;
        private List<IErrorListItem> _currentErrors = new List<IErrorListItem>();

        internal OutputErrorsFactory(IErrorListProvider errorProvider)
        {
            _errorProvider = errorProvider;
            CurrentSnapshot = new OutputErrorSnapshot(0, _currentErrors);
        }

        /// <summary>
        /// Current snapshot of errors
        /// </summary>
        internal TableEntriesSnapshotBase CurrentSnapshot { get; private set; }

        /// <summary>
        /// Removes all errors and updates the CurrentSnapshot
        /// </summary>
        internal void ClearErrors()
        {
            _currentErrors.Clear();
            UpdateErrors(new OutputErrorSnapshot(CurrentSnapshot.VersionNumber + 1, _currentErrors));
        }

        /// <summary>
        /// Removes all errors for a given task and updates the CurrentSnapshot
        /// </summary>
        internal void ClearErrors(ITaskRunnerNode task)
        {
            List<IErrorListItem> taskErrors = _currentErrors.Where(e => IsSameCommand(e.Command, task.Command)).ToList();

            if (taskErrors.Any())
            {
                _currentErrors = _currentErrors.Except(taskErrors).ToList();
                UpdateErrors(new OutputErrorSnapshot(CurrentSnapshot.VersionNumber + 1, _currentErrors));
            }
        }

        /// <summary>
        /// Adds a new error to the Factory and updates the CurrentSnapshot
        /// </summary>
        /// <param name="newError"></param>
        internal void AddErrorItem(IErrorListItem newError)
        {
            _currentErrors.Add(newError);
            UpdateErrors(new OutputErrorSnapshot(CurrentSnapshot.VersionNumber + 1, _currentErrors));
        }

        /// <summary>
        /// Adds a new errors to the Factory and updates the CurrentSnapshot
        /// </summary>
        /// <param name="newErrors"></param>
        internal void AddErrorItems(List<IErrorListItem> newErrors)
        {
            _currentErrors.AddRange(newErrors);
            UpdateErrors(new OutputErrorSnapshot(this.CurrentSnapshot.VersionNumber + 1, _currentErrors));
        }

        /// <summary>
        /// Updates the factory errors snapshot
        /// </summary>
        /// <param name="errorsList"></param>
        internal void UpdateErrors(OutputErrorSnapshot errorsList)
        {
            CurrentSnapshot = errorsList;
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

        public override void Dispose()
        {

        }

        private bool IsSameCommand(ITaskRunnerCommand command1, ITaskRunnerCommand command2)
        {
            if (command1.Args == command2.Args &&
                command1.Executable == command2.Executable &&
                command1.Options == command2.Options &&
                command1.WorkingDirectory == command2.WorkingDirectory)
            {
                return true;
            }

            return false;
        }

        #endregion
    }
}
