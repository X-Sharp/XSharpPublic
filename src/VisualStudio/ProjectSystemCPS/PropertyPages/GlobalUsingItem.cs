//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System.ComponentModel;
using System.Runtime.CompilerServices;

namespace XSharp.ProjectSystem.Properties
{
    /// <summary>
    /// Represents a single <c>&lt;Using&gt;</c> MSBuild item.
    /// </summary>
    internal sealed class GlobalUsingItem : INotifyPropertyChanged
    {
        private string _include;
        private string _alias;
        private bool _isStatic;
        private bool _isReadOnly;

        /// <summary>Gets or sets the namespace being imported.</summary>
        public string Include
        {
            get => _include;
            set => SetProperty(ref _include, value);
        }

        /// <summary>Gets or sets the optional alias for the import.</summary>
        public string Alias
        {
            get => _alias;
            set => SetProperty(ref _alias, value);
        }

        /// <summary>Gets or sets a value indicating whether this is a static import.</summary>
        public bool IsStatic
        {
            get => _isStatic;
            set => SetProperty(ref _isStatic, value);
        }

        /// <summary>
        /// Gets or sets a value indicating whether the item comes from an imported
        /// targets file and therefore cannot be edited or deleted.
        /// </summary>
        public bool IsReadOnly
        {
            get => _isReadOnly;
            set
            {
                if (SetProperty(ref _isReadOnly, value))
                    OnPropertyChanged(nameof(IsEditable));
            }
        }

        /// <summary>Gets a value indicating whether the user may modify this item.</summary>
        public bool IsEditable => !_isReadOnly;

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