//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using Microsoft.VisualStudio.Shell.TableManager;

namespace XSharp.Project
{
    internal interface IListProvider
    {
        /// <summary>
        /// This is called by the project error manager to add its error list factory
        /// </summary>
        void AddListFactory(ITableEntriesSnapshotFactory factory);

        /// <summary>
        /// This is called by the project error manager to remove the  error list factory from
        /// out list and all sinks
        /// </summary>
        void RemoveListFactory(ITableEntriesSnapshotFactory factory);

        /// <summary>
        /// Called to update all sinks when changes occured. Passing null for the factory will update
        /// all factories no all sinks.
        /// </summary>
        void UpdateAllSinks(ITableEntriesSnapshotFactory factory);

        /// <summary>
        /// Called bvy the error sink manager to add itsself to the list of error sinks
        /// </summary>
        void RemoveSinkManager(IListSinkManager sink);

        /// <summary>
        /// Called bvy the error sink manager to removeitsself to the list of error sinks. Occurs when
        /// the subscribed is done if the sink.
        /// </summary>
        void AddSinkManager(IListSinkManager sink);
    }
}