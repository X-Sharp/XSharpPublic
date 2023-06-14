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
    internal class SettingsView : BaseView
    {
        const int BYNAME = 1;
        const int BYNUMBER = 2;
        public bool ByName
        {
            get => Sortorder == BYNAME;
            set => Sortorder = value ? BYNAME : BYNUMBER;
        }
        public bool ByNumber
        {
            get => Sortorder != BYNAME;
            set => Sortorder = value ? BYNUMBER : BYNAME;
        }

        private void Sort()
        {
            if (Sortorder == BYNAME)
            {
                var temp = from setting in Items orderby setting.Name select setting;
                SortedItems = temp.ToArray();
            }
            else
            {
                var temp = from setting in Items orderby setting.Key select setting;
                SortedItems = temp.ToArray();

            }
        }
        private int _sortorder;
        public int Sortorder
        {
            get { return _sortorder; }
            set
            {
                SetProperty(ref _sortorder, value);
                Sort();
            }
        }

        private IList<SettingsItem> _items;

        public IList<SettingsItem> Items
        {
            get => _items;
            set
            {
                SetProperty(ref _items, value);
                Sort();
            }

            }
        private IList<SettingsItem> _sortedItems;
        public IList<SettingsItem> SortedItems
        {
            get => _sortedItems;
            set => SetProperty(ref _sortedItems, value);
        }


        public SettingsView()
        {
            _sortorder = BYNAME;
        }

    }
}
