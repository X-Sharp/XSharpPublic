//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

namespace XSharp.Project
{
    using System;
    using System.Collections.Generic;
    using System.Collections.ObjectModel;
    using System.ComponentModel;
    using System.Linq;
    using Microsoft.VisualStudio.Project;

    /// <summary>
    /// ViewModel for the Configuration selector combobox shown at the top of per-config
    /// property pages (Build, Debug).  Supports "All Configurations" as well as
    /// individual named configurations (e.g. Debug, Release).
    /// </summary>
    public class XConfigSelectorViewModel : INotifyPropertyChanged
    {
        // =====================================================================================
        // Constants
        // May be we should move the string to the Constants ?
        // =====================================================================================

        public const string AllConfigurations = "All Configurations";

        // =====================================================================================
        // Fields
        // =====================================================================================

        private IReadOnlyList<XProjectConfig> _allConfigs = Array.Empty<XProjectConfig>();
        private string _selectedConfig;
        private bool _initialized;

        // =====================================================================================
        // INotifyPropertyChanged
        // =====================================================================================

        public event PropertyChangedEventHandler PropertyChanged;

        private void OnPropertyChanged(string propertyName)
        {
            PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
        }

        // =====================================================================================
        // Properties
        // =====================================================================================

        /// <summary>
        /// The list of entries shown in the combobox.
        /// First entry is always "All Configurations", followed by each config name.
        /// </summary>
        public ObservableCollection<string> ConfigNames { get; } = new ObservableCollection<string>();

        /// <summary>
        /// The currently selected entry ("All Configurations" or a config name like "Debug").
        /// Setting this raises PropertyChanged so the page ViewModel can re-bind.
        /// </summary>
        public string SelectedConfig
        {
            get => _selectedConfig;
            set
            {
                if (_selectedConfig != value)
                {
                    _selectedConfig = value;
                    OnPropertyChanged(nameof(SelectedConfig));
                    OnPropertyChanged(nameof(ResolvedConfigs));
                }
            }
        }

        /// <summary>
        /// The resolved <see cref="XProjectConfig"/> list for the current selection.
        /// Returns all configs when "All Configurations" is selected,
        /// or a single-element list for a named config.
        /// </summary>
        public IList<XProjectConfig> ResolvedConfigs
        {
            get
            {
                if (_allConfigs.Count == 0)
                    return Array.Empty<XProjectConfig>();

                if (string.IsNullOrEmpty(_selectedConfig) || _selectedConfig == AllConfigurations)
                    return _allConfigs.ToList();

                var match = _allConfigs.FirstOrDefault(c =>
                    string.Equals(c.ConfigName, _selectedConfig, StringComparison.OrdinalIgnoreCase));

                return match != null
                    ? new List<XProjectConfig> { match }
                    : _allConfigs.ToList();
            }
        }

        /// <summary>
        /// True once <see cref="Initialize"/> has been called at least once.
        /// </summary>
        public bool IsInitialized => _initialized;

        // =====================================================================================
        // Methods
        // =====================================================================================

        /// <summary>
        /// Populates the selector from the project's full config list.
        /// On first call the active config is pre-selected; on subsequent calls
        /// the current selection is preserved if it still exists.
        /// </summary>
        /// <param name="allConfigs">All project configurations, from
        /// <see cref="XPropertyPage.GetAllProjectConfigs"/>.</param>
        /// <param name="activeConfigName">The config name VS currently has active
        /// (e.g. "Debug").  Used only on first call.</param>
        public void Initialize(IReadOnlyList<XProjectConfig> allConfigs, string activeConfigName)
        {
            _allConfigs = allConfigs ?? Array.Empty<XProjectConfig>();

            // Build the desired name list without touching the ObservableCollection yet.
            var newNames = new System.Collections.Generic.List<string> { AllConfigurations };
            foreach (var cfg in _allConfigs)
                if (!newNames.Contains(cfg.ConfigName))
                    newNames.Add(cfg.ConfigName);

            // Only rebuild ConfigNames when the list actually changed.
            // Clearing the ObservableCollection causes WPF to reset SelectedItem → null,
            // which propagates back through the TwoWay binding and loses the user's selection.
            bool listChanged = ConfigNames.Count != newNames.Count;
            if (!listChanged)
                for (int i = 0; i < newNames.Count; i++)
                    if (ConfigNames[i] != newNames[i]) { listChanged = true; break; }

            if (listChanged)
            {
                ConfigNames.Clear();
                foreach (var n in newNames)
                    ConfigNames.Add(n);
            }

            if (!_initialized)
            {
                // First call: select the active config if it exists, else "All Configurations"
                _selectedConfig = newNames.Contains(activeConfigName)
                    ? activeConfigName
                    : AllConfigurations;
                _initialized = true;
                // Notify bindings on first init
                OnPropertyChanged(nameof(SelectedConfig));
                OnPropertyChanged(nameof(ResolvedConfigs));
            }
            else
            {
                // Subsequent calls: keep current selection if still valid; do NOT
                // fire OnPropertyChanged(SelectedConfig) — that would re-trigger BindProperties.
                if (!newNames.Contains(_selectedConfig))
                {
                    _selectedConfig = AllConfigurations;
                    OnPropertyChanged(nameof(SelectedConfig));
                    OnPropertyChanged(nameof(ResolvedConfigs));
                }
            }
        }
    }
}
