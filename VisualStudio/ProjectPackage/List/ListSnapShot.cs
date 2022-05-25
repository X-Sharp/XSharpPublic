//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Shell.TableControl;
using Microsoft.VisualStudio.Shell.TableManager;
using System;
using System.Collections.Generic;
using System.Collections;


namespace XSharp.Project
{
    internal class ListSnapshot<T> : WpfTableEntriesSnapshotBase where T: IListItem
    {
        private readonly int _versionNumber;
        public readonly List<T> Items;
        private readonly Guid _projectGuid;
        internal ListSnapshot(int versionNumber, IEnumerable items, Guid projectGuid)
        {
            _versionNumber = versionNumber;
            _projectGuid = projectGuid;
            Items = new List<T>();
            foreach (T item in items)
            {
                Items.Add(item);
            }
        }

        public override int Count
        {
            get
            {
                return Items.Count;
            }
        }

        public override int VersionNumber
        {
            get
            {
                return _versionNumber;
            }
        }

        /// <summary>
        /// Attempts to get a column value for the item at index in the snapshot. Note that this method
        /// gets called for any columns that the control supports. If we do not have a value
        /// to supply for the column or if somehow we got an invalid index the content needs to be
        /// set to null and return false which lets the table control set the value.
        /// </summary>
        public override bool TryGetValue(int index, string columnName, out object content)
        {
            if (index >= 0 && index < Items.Count)
            {
                var item = Items[index];
                return item.GetValue(columnName, out content);
            }

            // This method gets called for anything that the control supports. If we do not have a value
            // to supply for the column or if somehow we got an invalid index the content needs to be
            // set to null and return false which lets the table control set the value.
            content = null;
            return false;
        }
       
    }
    internal static class ListHelpers
    {
        private const int UndefinedLineOrColumn = -1;
        /// <summary>
        /// Helper to account for the fact the error list is 0 based.
        /// </summary>
        internal static int GetAdjustedLineOrColumn(int lineOrColumn)
        {
            if (lineOrColumn != UndefinedLineOrColumn)
            {
                return lineOrColumn > 0 ? lineOrColumn - 1 : 0;
            }

            return lineOrColumn;
        }
    }
}
