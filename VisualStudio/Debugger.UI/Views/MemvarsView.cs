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
    internal class MemvarsView : BaseView
    {

        bool _publics;
        bool _privates;
        public bool Publics
        {
            get => _publics ;
            set
            {
                SetProperty(ref _publics, value);
                FilterItems();
            }

        }
        public bool Privates
        {
            get => _privates;
            set
            {
                SetProperty(ref _privates, value);
                FilterItems();
            }
        }
        private void FilterItems()
        {
            if (Publics && Privates)
            {
                SelectedItems = Items;
            }
            else if (Publics)
            {
                SelectedItems = Items.Where(i => i.Type == MemvarType.Public).ToArray();
            }
            else if (Privates)
            {
                SelectedItems = Items.Where(i => i.Type == MemvarType.Private).ToArray();
            }
            else
            {
                SelectedItems = new List<MemvarItem>();
            }
        }

        private IList<MemvarItem> _items;

        public IList<MemvarItem> Items
        {
            get => _items;
            set
            {
                SetProperty(ref _items, value);
                FilterItems();
            }

            }
        private IList<MemvarItem> _selectedItems;
        public IList<MemvarItem> SelectedItems
        {
            get => _selectedItems;
            set => SetProperty(ref _selectedItems, value);
        }


        public MemvarsView()
        {
            _publics = true;
            _privates = true;

        }

    }
}
