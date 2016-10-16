using System;
using Microsoft.VisualStudio.Shell.TableManager;

namespace TaskOutputListener
{
    internal class ErrorListSinkManager : IErrorListSinkManager, IDisposable
    {
        private IErrorListProvider ErrorProvider { get; set; }
        private ITableDataSink ErrorTableSink { get; set; }

        internal ErrorListSinkManager(IErrorListProvider errorProvider, ITableDataSink sink)
        {
            ErrorProvider = errorProvider;
            ErrorTableSink = sink;
            ErrorProvider.AddSinkManager(this);
        }

        /// <summary>
        /// Called by the subscriber to the data source to disposes of the cookie (== this object) they were given.
        /// </summary>
        public void Dispose()
        {
            ErrorProvider.RemoveSinkManager(this);
        }

        /// <summary>
        /// This is called if a new project is added after there is already a sink connected. Informs the sink
        /// of the new source of errors
        /// </summary>
        public void AddErrorListFactory(ITableEntriesSnapshotFactory factory)
        {
            ErrorTableSink.AddFactory(factory);
        }

        /// <summary>
        /// If a project is removed from the solution (unloaded, closed, etc) informans the sink
        /// that the source is no longer available.
        /// </summary>
        public void RemoveErrorListFactory(ITableEntriesSnapshotFactory factory)
        {
            ErrorTableSink.RemoveFactory(factory);
        }

        /// <summary>
        /// Called when a snapshot changes to have its errors updated by the subscriber. Pass null to have
        /// all the factories updated.
        /// </summary>
        public void UpdateSink(ITableEntriesSnapshotFactory factory)
        {
            ErrorTableSink.FactorySnapshotChanged(factory);
        }
    }
}