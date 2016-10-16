using Microsoft.VisualStudio.Shell.TableManager;

namespace TaskOutputListener
{
    internal interface IErrorListProvider
    {
        /// <summary>
        /// This is called by the project error manager to add its error list factory
        /// </summary>
        void AddErrorListFactory(ITableEntriesSnapshotFactory factory);

        /// <summary>
        /// This is called by the project error manager to remove the  error list factory from
        /// out list and all sinks
        /// </summary>
        void RemoveErrorListFactory(ITableEntriesSnapshotFactory factory);

        /// <summary>
        /// Called to update all sinks when changes occured. Passing null for the factory will update
        /// all factories no all sinks.
        /// </summary>
        void UpdateAllSinks(ITableEntriesSnapshotFactory factory);

        /// <summary>
        /// Called bvy the error sink manager to add itsself to the list of error sinks
        /// </summary>
        void RemoveSinkManager(IErrorListSinkManager sink);

        /// <summary>
        /// Called bvy the error sink manager to removeitsself to the list of error sinks. Occurs when
        /// the subscribed is done if the sink.
        /// </summary>
        void AddSinkManager(IErrorListSinkManager sink);
    }
}