//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using Debugger.Support;
using System.Collections.Generic;
using System.Linq;
using XSharp.Debugger.Support;

namespace XSharp.Debugger.UI
{
    internal class WorkareasView : BaseView
    {
        internal const string AllRDDs = "All RDDs";

        const int STATUS = 1;
        const int FIELDS = 2;
        public bool ShowStatus
        {
            get => Contents == STATUS;
            set => Contents = value ? STATUS : FIELDS;
        }
        public bool ShowFields
        {
            get => Contents != STATUS;
            set => Contents = value ? FIELDS : STATUS;
        }
        private string _alias;
        public string Alias
        {   get => _alias;
            set => SetProperty(ref _alias, value);
        }
        private int _contents;
        public int Contents
        {
            get { return _contents; }
            set
            {
                SetProperty(ref _contents, value);
                UpdateAreaInfo();
            }
        }

        private void FilterItems()
        {
            if (_selectedRDD == AllRDDs)
            {
                SelectedItems = Items;
            }
            else
            {
                SelectedItems = Items.Where(i => i.RDD == _selectedRDD).ToList();
            }
        }
        private string _selectedRDD;
        public string SelectedRDD
        {
            get => _selectedRDD;
            set
            {
                SetProperty(ref _selectedRDD, value);
                FilterItems();
            }
        }
        private IList<string> _Rdds;
        public IList<string> RDDs
        {
            get => _Rdds;
            set => SetProperty(ref _Rdds, value);
        }
        private IList<WorkareaItem> _items;

        public IList<WorkareaItem> Items
        {
            get => _items;
            set
            {
                SetProperty(ref _items, value);
                FilterItems();
            }

        }
        private IList<WorkareaItem> _selectedItems;

        public IList<WorkareaItem> SelectedItems
        {
            get => _selectedItems;
            set
            {
                SetProperty(ref _selectedItems, value);
            }
        }

        private IList<NameValueItem> _status;
        private IList<NameValueItem> _fields;

        private IList<NameValueItem> _info;
        public IList<NameValueItem> AreaInfo
        {
            get => _info;
            set => SetProperty(ref _info, value);
        }
        public IList<NameValueItem> Status { get => _status; set { _status = value; UpdateAreaInfo(); } }
        public IList<NameValueItem> Fields { get => _fields; set { _fields = value; UpdateAreaInfo(); } }

        void UpdateAreaInfo()
        {
            if (Contents == STATUS)
            {
                AreaInfo = Status;
            }
            else
            {
                AreaInfo = Fields;
            }
        }

        public WorkareasView()
        {
            _contents = STATUS;
        }

    }
}
