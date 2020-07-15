//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System;
using Microsoft.VisualStudio.Shell.TableManager;

namespace XSharp.Project
{
    internal class ListSinkManager : IListSinkManager, IDisposable
    {
        private IListProvider ListProvider { get; set; }
        private ITableDataSink TableSink { get; set; }

        internal ListSinkManager(IListProvider provider, ITableDataSink sink)
        {
            ListProvider = provider;
            TableSink = sink;
            ListProvider.AddSinkManager(this);
        }

        /// <summary>
        /// Called by the subscriber to the data source to disposes of the cookie (== this object) they were given.
        /// </summary>
        public void Dispose()
        {
            ListProvider.RemoveSinkManager(this);
        }

        /// <summary>
        /// This is called if a new project is added after there is already a sink connected. Informs the sink
        /// of the new source of errors
        /// </summary>
        public void AddListFactory(ITableEntriesSnapshotFactory factory)
        {
            TableSink.AddFactory(factory);
        }

        /// <summary>
        /// If a project is removed from the solution (unloaded, closed, etc) informans the sink
        /// that the source is no longer available.
        /// </summary>
        public void RemoveListFactory(ITableEntriesSnapshotFactory factory)
        {
            TableSink.RemoveFactory(factory);
        }

        /// <summary>
        /// Called when a snapshot changes to have its errors updated by the subscriber. Pass null to have
        /// all the factories updated.
        /// </summary>
        public void UpdateSink(ITableEntriesSnapshotFactory factory)
        {
            TableSink.FactorySnapshotChanged(factory);
        }
    }
}