using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Linq;
using System.Windows;
using System.Windows.Media;
using Microsoft.VisualStudio.PlatformUI;
using XSharpModel;

namespace XSharp.LanguageService.Editors.LightBulb
{
    internal partial class CtorParamsDlg : DialogWindow
    {
        private readonly CtorParamsViewModel _viewModel;

        public List<XSharpModel.IXMemberSymbol> FieldsNProps
        {
            get
            {
                return _viewModel.MemberItems
                    .Where(m => m.IsChecked)
                    .Select(m => m.Symbol)
                    .ToList();
            }
        }

        public CtorParamsDlg()
        {
            _viewModel = new CtorParamsViewModel();
            DataContext = _viewModel;
            InitializeComponent();
        }

        public void FillMembers(List<XSharpModel.IXMemberSymbol> fieldsNProps)
        {
            _viewModel.LoadMembers(fieldsNProps);
        }

        private void btnSelect_Click(object sender, RoutedEventArgs e)
        {
            _viewModel.SelectAll();
        }

        private void btnDeselect_Click(object sender, RoutedEventArgs e)
        {
            _viewModel.DeselectAll();
        }

        private void MoveUp_Click(object sender, RoutedEventArgs e)
        {
            _viewModel.MoveUp();
        }

        private void MoveDown_Click(object sender, RoutedEventArgs e)
        {
            _viewModel.MoveDown();
        }

        private void btnCancel_Click(object sender, RoutedEventArgs e)
        {
            DialogResult = false;
        }

        private void btnOk_Click(object sender, RoutedEventArgs e)
        {
            if (FieldsNProps.Count > 0)
            {
                DialogResult = true;
            }
            else
            {
                DialogResult = false;
            }
        }
    }

    internal class CtorParamsViewModel : INotifyPropertyChanged
    {
        private int _selectedIndex;

        public ObservableCollection<MemberItemViewModel> MemberItems { get; }

        public int SelectedIndex
        {
            get => _selectedIndex;
            set
            {
                if (_selectedIndex != value)
                {
                    _selectedIndex = value;
                    OnPropertyChanged(nameof(SelectedIndex));
                    OnPropertyChanged(nameof(CanMoveUp));
                    OnPropertyChanged(nameof(CanMoveDown));
                }
            }
        }

        public bool CanMoveUp => SelectedIndex > 0;
        public bool CanMoveDown => SelectedIndex >= 0 && SelectedIndex < MemberItems.Count - 1;

        public CtorParamsViewModel()
        {
            MemberItems = new ObservableCollection<MemberItemViewModel>();
        }

        public void LoadMembers(List<XSharpModel.IXMemberSymbol> fieldsNProps)
        {
            MemberItems.Clear();
            int index = 0;
            foreach (var mbr in fieldsNProps)
            {
                ImageSource icon = null;
                if (mbr is XSymbol xmbr)
                {
                    // Use the KindExtensions.GetImageMoniker extension method
                    var moniker = xmbr.Kind.GetImageMoniker(xmbr.Visibility);
                    icon = moniker.GetImageSource();
                }

                MemberItems.Add(new MemberItemViewModel
                {
                    Name = mbr.Name,
                    Symbol = mbr,
                    IsChecked = true,
                    Icon = icon,
                    Index = index++
                });
            }
        }

        public void SelectAll()
        {
            foreach (var item in MemberItems)
            {
                item.IsChecked = true;
            }
        }

        public void DeselectAll()
        {
            foreach (var item in MemberItems)
            {
                item.IsChecked = false;
            }
        }

        public void MoveUp()
        {
            if (CanMoveUp)
            {
                var index = SelectedIndex;
                var item = MemberItems[index];
                MemberItems.RemoveAt(index);
                MemberItems.Insert(index - 1, item);
                SelectedIndex = index - 1;
            }
        }

        public void MoveDown()
        {
            if (CanMoveDown)
            {
                var index = SelectedIndex;
                var item = MemberItems[index];
                MemberItems.RemoveAt(index);
                MemberItems.Insert(index + 1, item);
                SelectedIndex = index + 1;
            }
        }

        public event PropertyChangedEventHandler PropertyChanged;

        protected virtual void OnPropertyChanged(string propertyName)
        {
            PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
        }
    }

    internal class MemberItemViewModel : INotifyPropertyChanged
    {
        private bool _isChecked;

        public string Name { get; set; }
        public IXMemberSymbol Symbol { get; set; }
        public ImageSource Icon { get; set; }
        public int Index { get; set; }

        public bool IsChecked
        {
            get => _isChecked;
            set
            {
                if (_isChecked != value)
                {
                    _isChecked = value;
                    OnPropertyChanged(nameof(IsChecked));
                }
            }
        }

        public event PropertyChangedEventHandler PropertyChanged;

        protected virtual void OnPropertyChanged(string propertyName)
        {
            PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
        }
    }
}
