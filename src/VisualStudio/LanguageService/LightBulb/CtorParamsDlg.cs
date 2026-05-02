// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
using Microsoft.VisualStudio.Imaging.Interop;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Windows;
using XSharpModel;

namespace XSharp.LanguageService.Editors.LightBulb
{
    public partial class CtorParamsDlg : Window
    {
        private readonly ObservableCollection<MemberItem> _items = new ObservableCollection<MemberItem>();

        public CtorParamsDlg()
        {
            InitializeComponent();
            _listView.ItemsSource = _items;
        }

        public List<IXMemberSymbol> FieldsNProps
        {
            get
            {
                var result = new List<IXMemberSymbol>();
                foreach (var item in _items)
                {
                    if (item.IsChecked)
                        result.Add(item.Member);
                }
                return result;
            }
        }

        public void FillMembers(List<IXMemberSymbol> fieldsNProps)
        {
            _items.Clear();
            int i = 0;
            foreach (var mbr in fieldsNProps)
            {
                _items.Add(new MemberItem
                {
                    Name = mbr.Name,
                    Member = mbr,
                    IsChecked = true,
                    Order = i++,
                    Moniker = mbr.Kind.GetImageMoniker(mbr.Visibility)
                });
            }
        }

        private void OnUp(object sender, RoutedEventArgs e)
        {
            int idx = _listView.SelectedIndex;
            if (idx > 0)
            {
                _items.Move(idx, idx - 1);
                _listView.SelectedIndex = idx - 1;
            }
        }

        private void OnDown(object sender, RoutedEventArgs e)
        {
            int idx = _listView.SelectedIndex;
            if (idx >= 0 && idx < _items.Count - 1)
            {
                _items.Move(idx, idx + 1);
                _listView.SelectedIndex = idx + 1;
            }
        }

        private void OnSelectAll(object sender, RoutedEventArgs e)
        {
            foreach (var item in _items)
                item.IsChecked = true;
        }

        private void OnDeselectAll(object sender, RoutedEventArgs e)
        {
            foreach (var item in _items)
                item.IsChecked = false;
        }

        private void OnOk(object sender, RoutedEventArgs e)
        {
            bool any = false;
            foreach (var item in _items)
            {
                if (item.IsChecked) { any = true; break; }
            }
            DialogResult = any;
        }

        private void OnCancel(object sender, RoutedEventArgs e)
        {
            DialogResult = false;
        }
    }

    internal sealed class MemberItem : INotifyPropertyChanged
    {
        private bool _isChecked;

        public bool IsChecked
        {
            get => _isChecked;
            set
            {
                if (_isChecked != value)
                {
                    _isChecked = value;
                    OnPropertyChanged();
                }
            }
        }

        public string Name { get; set; }
        public int Order { get; set; }
        public IXMemberSymbol Member { get; set; }
        public ImageMoniker Moniker { get; set; }

        public event PropertyChangedEventHandler PropertyChanged;
        private void OnPropertyChanged([CallerMemberName] string name = null)
            => PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(name));
    }
}
