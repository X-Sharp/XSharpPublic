//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using Debugger.Support;
using Microsoft.VisualStudio.PlatformUI;
using System.Collections.Generic;
using System.Linq;

namespace XSharp.Debugger.UI
{
    internal class WorkareasView : BaseView
    {
        internal const string AllRDDs = "All RDDs";
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
        private IList<WorkareaItem> _selecteditems;

        public IList<WorkareaItem> SelectedItems
        {
            get => _selecteditems;
            set
            {
                SetProperty(ref _selecteditems, value);
            }

        }

        public WorkareasView()
        {


        }

    }
}
