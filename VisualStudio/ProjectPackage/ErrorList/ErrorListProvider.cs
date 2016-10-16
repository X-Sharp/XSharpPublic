using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Shell.TableControl;
using Microsoft.VisualStudio.Shell.TableManager;

namespace TaskOutputListener
{
    [Export(typeof(IErrorListProvider))]
    internal sealed class ErrorListProvider : IErrorListProvider, ITableDataSource
    {
        internal readonly ITableManager ErrorTableManager;
        private readonly string DataSourceIdentifierString = Vsix.Name;
        private readonly string DataSourceDisplayName = Vsix.Name + " TEST";

        // The list sinks. Probably only one but there could be more. Needs to be threadsafe so is
        // also used as a lock
        private List<IErrorListSinkManager> _managers = new List<IErrorListSinkManager>();
        private List<ITableEntriesSnapshotFactory> _errorListFactories = new List<ITableEntriesSnapshotFactory>();

        [ImportingConstructor]
        internal ErrorListProvider(ITableManagerProvider provider)
        {
            ErrorTableManager = provider.GetTableManager(StandardTables.ErrorsTable);
            ErrorTableManager.AddSource(this, StandardTableColumnDefinitions.DetailsExpander, StandardTableColumnDefinitions.ProjectName,
                                                StandardTableColumnDefinitions.ErrorSeverity, StandardTableColumnDefinitions.ErrorCode,
                                                StandardTableColumnDefinitions.ErrorSource, StandardTableColumnDefinitions.ErrorCategory,
                                                StandardTableColumnDefinitions.Text, StandardTableColumnDefinitions.DocumentName,
                                                StandardTableColumnDefinitions.Line, StandardTableColumnDefinitions.Column);
        }

        public string DisplayName
        {
            get
            {
                // This string should, in general, be localized since it is what would be displayed in any UI that lets the end user pick
                // which ITableDataSources should be subscribed to by an instance of the table control. It really isn't needed for the error
                // list however because it autosubscribes to all the ITableDataSources.
                return DataSourceDisplayName;
            }
        }

        /// <summary>
        /// Identifies this provider. Important as the event provider is associated with this provider name, among other things.
        /// </summary>
        public string Identifier
        {
            get
            {
                return DataSourceIdentifierString;
            }
        }

        public string SourceTypeIdentifier
        {
            get
            {
                return StandardTableDataSources.ErrorTableDataSource;
            }
        }

        public IDisposable Subscribe(ITableDataSink sink)
        {
            // This method is called to each consumer interested in errors. In general, there will be only a single consumer (the error list tool window)
            // but it is always possible for 3rd parties to write code that will want to subscribe.
            return new ErrorListSinkManager(this, sink);
        }

        public void AddSinkManager(IErrorListSinkManager manager)
        {
            // This call can, in theory, happen from any thread so be appropriately thread safe.
            // In practice, it will probably be called only once from the UI thread (by the error list tool window).
            lock (_managers)
            {
                _managers.Add(manager);

                // Add the pre-existing error factories to the manager.
                foreach (var errorFactory in _errorListFactories)
                {
                    manager.AddErrorListFactory(errorFactory);
                }
            }
        }

        public void RemoveSinkManager(IErrorListSinkManager manager)
        {
            // This call can, in theory, happen from any thread so be appropriately thread safe.
            // In practice, it will probably be called only once from the UI thread (by the error list tool window).
            lock (_managers)
            {
                _managers.Remove(manager);
            }
        }

        /// <summary>
        /// This is called by the project error manager to add its error list factory
        /// </summary>
        public void AddErrorListFactory(ITableEntriesSnapshotFactory factory)
        {
            lock (_managers)
            {
                _errorListFactories.Add(factory);

                // Tell the preexisting sinks about the new error source
                foreach (var manager in _managers)
                {
                    manager.AddErrorListFactory(factory);
                }
            }
        }

        /// <summary>
        /// This is called by the project error manager to remove the  error list factory from
        /// out list and all sinks
        /// </summary>
        public void RemoveErrorListFactory(ITableEntriesSnapshotFactory factory)
        {
            lock (_managers)
            {
                _errorListFactories.Remove(factory);

                foreach (var manager in _managers)
                {
                    manager.RemoveErrorListFactory(factory);
                }
            }
        }

        /// <summary>
        /// Called to update all sinks when changes occured. Passing null for factory
        /// will update all factories on all sinks
        /// </summary>
        public void UpdateAllSinks(ITableEntriesSnapshotFactory factory)
        {
            lock (_managers)
            {
                foreach (var manager in _managers)
                {
                    manager.UpdateSink(factory);
                }
            }
        }
    }
}
