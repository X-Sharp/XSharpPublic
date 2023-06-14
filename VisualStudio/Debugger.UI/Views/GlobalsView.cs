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
    internal class GlobalsView : BaseView
    {
        internal const string AllAssemblies = "All assemblies";

        private void FilterItems()
        {
            if (_selectedAssembly == AllAssemblies)
            {
                SelectedItems = Items;
            }
            else
            {
                SelectedItems = Items.Where(i => i.Assembly == _selectedAssembly).ToList();
            }
        }

        private string _selectedAssembly;
        public string SelectedAssembly
        {
            get => _selectedAssembly;
            set
            {
                SetProperty(ref _selectedAssembly, value);
                FilterItems();    
            }
        }
        private IList<string> _assemblies;
        public IList<string> Assemblies
        {
            get => _assemblies;
            set => SetProperty(ref _assemblies, value);
        }
        private IList<GlobalItem> _items;

        public IList<GlobalItem> Items
        {
            get => _items;
            set
            {
                SetProperty(ref _items, value);
                FilterItems();
            }

            }
        private IList<GlobalItem> _selectedItems;
        public IList<GlobalItem> SelectedItems
        {
            get => _selectedItems;
            set => SetProperty(ref _selectedItems, value);
        }


        public GlobalsView()
        {


        }

    }
}
