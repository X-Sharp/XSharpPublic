//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Linq;
using System.Runtime.CompilerServices;

namespace XSharp.ProjectSystem.Properties
{
    /// <summary>
    /// Backs <see cref="GlobalUsingsDialog"/>. Edits an in-memory copy of the
    /// <c>&lt;Using&gt;</c> items; the caller persists <see cref="Usings"/> when the
    /// dialog is accepted.
    /// </summary>
    internal sealed class GlobalUsingsDialogViewModel : INotifyPropertyChanged
    {
        private readonly List<GlobalUsingItem> _allUsings;
        private readonly ObservableCollection<GlobalUsingItem> _visibleUsings
            = new ObservableCollection<GlobalUsingItem>();

        private string _newNamespace = string.Empty;
        private string _newAlias = string.Empty;
        private bool _newStatic;
        private bool _showImported = true;

        public GlobalUsingsDialogViewModel(IEnumerable<GlobalUsingItem> usings)
        {
            _allUsings = usings?.ToList() ?? new List<GlobalUsingItem>();
            RefreshVisibleUsings();
        }

        /// <summary>Gets the items shown in the grid, filtered by <see cref="ShowImported"/>.</summary>
        public ObservableCollection<GlobalUsingItem> VisibleUsings => _visibleUsings;

        /// <summary>Gets the full set of items, including those hidden by the filter.</summary>
        public IReadOnlyList<GlobalUsingItem> Usings => _allUsings;

        /// <summary>Gets or sets whether SDK-provided (imported) usings are listed.</summary>
        public bool ShowImported
        {
            get => _showImported;
            set
            {
                if (SetProperty(ref _showImported, value))
                    RefreshVisibleUsings();
            }
        }

        /// <summary>Gets or sets the namespace text of the add-using row.</summary>
        public string NewNamespace
        {
            get => _newNamespace;
            set
            {
                if (SetProperty(ref _newNamespace, value))
                    OnPropertyChanged(nameof(CanAddUsing));
            }
        }

        /// <summary>Gets or sets the alias text of the add-using row.</summary>
        public string NewAlias
        {
            get => _newAlias;
            set => SetProperty(ref _newAlias, value);
        }

        /// <summary>Gets or sets whether the add-using row describes a static import.</summary>
        public bool NewStatic
        {
            get => _newStatic;
            set => SetProperty(ref _newStatic, value);
        }

        /// <summary>Gets a value indicating whether the add-using row is valid.</summary>
        public bool CanAddUsing => !string.IsNullOrWhiteSpace(_newNamespace);

        /// <summary>Adds the namespace described by the add-using row.</summary>
        public void AddUsing()
        {
            if (!CanAddUsing)
                return;

            var name = _newNamespace.Trim();
            if (_allUsings.Any(u => string.Equals(u.Include, name, StringComparison.OrdinalIgnoreCase)
                                    && string.Equals(u.Alias ?? string.Empty, _newAlias?.Trim() ?? string.Empty,
                                                     StringComparison.OrdinalIgnoreCase)))
            {
                return;
            }

            _allUsings.Add(new GlobalUsingItem
            {
                Include = name,
                Alias = string.IsNullOrWhiteSpace(_newAlias) ? null : _newAlias.Trim(),
                IsStatic = _newStatic,
                IsReadOnly = false
            });

            NewNamespace = string.Empty;
            NewAlias = string.Empty;
            NewStatic = false;
            RefreshVisibleUsings();
        }

        /// <summary>Removes the supplied item, unless it is imported.</summary>
        public void DeleteUsing(GlobalUsingItem item)
        {
            if (item == null || item.IsReadOnly)
                return;
            _allUsings.Remove(item);
            RefreshVisibleUsings();
        }

        private void RefreshVisibleUsings()
        {
            _visibleUsings.Clear();
            foreach (var item in _allUsings.Where(u => _showImported || !u.IsReadOnly)
                                           .OrderBy(u => u.Include, StringComparer.OrdinalIgnoreCase))
            {
                _visibleUsings.Add(item);
            }
        }

        public event PropertyChangedEventHandler PropertyChanged;

        private bool SetProperty<T>(ref T field, T value, [CallerMemberName] string propertyName = null)
        {
            if (Equals(field, value))
                return false;
            field = value;
            OnPropertyChanged(propertyName);
            return true;
        }

        private void OnPropertyChanged([CallerMemberName] string propertyName = null)
        {
            PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
        }
    }
}