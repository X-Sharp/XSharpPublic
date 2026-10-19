//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System.Collections.Generic;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell;
using XSharp.Settings;

namespace XSharp.Project
{
    /// <summary>
    /// MVVM ViewModel that backs the XAML/WPF Debug property page for
    /// SDK-style XSharp projects.
    /// </summary>
    /// <remarks>
    /// <para>
    /// Each MSBuild property shown on the Debug page is exposed as an observable
    /// property backed by <see cref="XPropertyPageViewModel.SetProperty{T}"/> so that
    /// WPF data binding automatically triggers
    /// <see cref="System.ComponentModel.INotifyPropertyChanged"/> on every change.
    /// </para>
    /// <para>
    /// <see cref="DebuggerCommand"/> and <see cref="DebuggerWorkingDirectory"/> are
    /// editable TextBoxes whose <em>Browse (…)</em> buttons open
    /// <see cref="XSharpSLEPropertyForm"/> (macro/string editor) — matching the WinForms
    /// <c>btnCommand_Click</c> and <c>btnDebuggerWorkingDirectory_Click</c> handlers.
    /// </para>
    /// </remarks>
    internal sealed class XDebugPropertyPageViewModel : XPropertyPageViewModel
    {
        // =========================================================================================
        // Backing fields — string properties
        // =========================================================================================

        private string _debuggerCommand            = string.Empty;
        private string _debuggerCommandArguments   = string.Empty;
        private string _debugType                  = string.Empty;
        private string _debuggerWorkingDirectory   = string.Empty;

        // =========================================================================================
        // Backing fields — bool checkboxes
        // =========================================================================================

        private bool _enableUnmanagedDebugging;

        // =========================================================================================
        // Backing fields — combo items (debug type)
        // =========================================================================================

        private List<string> _debugTypeValues = new List<string>();

        private bool _isBinding   = false;
        private bool _isNotifying = false;

        // =========================================================================================
        // Config selector
        // =========================================================================================
        private readonly XConfigSelectorViewModel _configSelector = new XConfigSelectorViewModel();

        /// <summary>
        /// The configuration selector combobox ViewModel (bound to the combobox at the top of the page).
        /// </summary>
        public XConfigSelectorViewModel ConfigSelector => _configSelector;

        // =========================================================================================
        // Constructor
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="XDebugPropertyPageViewModel"/> class.
        /// </summary>
        /// <param name="parentPropertyPage">The <see cref="XPropertyPage"/> that owns this panel.</param>
        public XDebugPropertyPageViewModel(XPropertyPage parentPropertyPage)
            : base(parentPropertyPage)
        {
            // Populate debug type combo values from the same converter the WinForms panel uses.
            var conv = new DebugTypeConverter();
            foreach (var v in conv.GetStandardValues())
                _debugTypeValues.Add(v?.ToString() ?? string.Empty);
        }

        // =========================================================================================
        // Internal access — for code-behind browse handlers
        // =========================================================================================

        /// <summary>
        /// Exposes the owning property page so that the code-behind browse button handlers
        /// can access <c>ProjectMgr</c> and call <c>SetProperty</c>.
        /// </summary>
        internal XPropertyPage ParentPage => parentPropertyPage;

        // =========================================================================================
        // Per-config bool helpers — route through ConfigSelector instead of ProjectMgr directly
        // =========================================================================================

        private bool GetBoolForConfigs(string propertyName)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            var value = ((XPropertyPage)parentPropertyPage).GetPropertyForConfigs(
                propertyName, _configSelector.ResolvedConfigs);
            return bool.TryParse(value, out var result) && result;
        }

        private void SetBoolForConfigs(string propertyName, bool value)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            ((XPropertyPage)parentPropertyPage).SetPropertyForConfigs(
                propertyName, value.ToString().ToLowerInvariant(), _configSelector.ResolvedConfigs);
        }

        // =========================================================================================
        // Observable Properties — strings
        // =========================================================================================

        /// <summary>Gets or sets the debugger executable path.</summary>
        public string DebuggerCommand
        {
            get => _debuggerCommand;
            set => SetProperty(ref _debuggerCommand, value);
        }

        /// <summary>Gets or sets the command-line arguments passed to the debugger.</summary>
        public string DebuggerCommandArguments
        {
            get => _debuggerCommandArguments;
            set => SetProperty(ref _debuggerCommandArguments, value);
        }

        /// <summary>Gets or sets the debug information type (e.g. <c>"full"</c>, <c>"pdbonly"</c>, <c>"none"</c>).</summary>
        public string DebugType
        {
            get => _debugType;
            set => SetProperty(ref _debugType, value);
        }

        /// <summary>Gets or sets the debugger working directory.</summary>
        public string DebuggerWorkingDirectory
        {
            get => _debuggerWorkingDirectory;
            set => SetProperty(ref _debuggerWorkingDirectory, value);
        }

        // =========================================================================================
        // Observable Properties — checkboxes
        // =========================================================================================

        /// <summary>Gets or sets whether native/unmanaged code debugging is enabled.</summary>
        public bool EnableUnmanagedDebugging
        {
            get => _enableUnmanagedDebugging;
            set => SetProperty(ref _enableUnmanagedDebugging, value);
        }

        // =========================================================================================
        // Observable Properties — combo items
        // =========================================================================================

        /// <summary>Gets the list of debug type values for the combo box.</summary>
        public List<string> DebugTypeValues => _debugTypeValues;

        // =========================================================================================
        // String properties — labels and tooltips from DebugPropertyPagePanel
        // =========================================================================================

        /// <summary>Gets the localized label for the Debugger Command field.</summary>
        public string CaptDebuggerCommand            => DebugPropertyPagePanel.captDebuggerCommand;
        /// <summary>Gets the localized label for the Debugger Arguments field.</summary>
        public string CaptDebuggerCommandArguments   => DebugPropertyPagePanel.captDebuggerCommandArguments;
        /// <summary>Gets the localized label for the Debug Type combo.</summary>
        public string CaptDebugType                  => DebugPropertyPagePanel.captDebugType;
        /// <summary>Gets the localized label for the Working Directory field.</summary>
        public string CaptDebuggerWorkingDirectory   => DebugPropertyPagePanel.captDebuggerWorkingDirectory;
        /// <summary>Gets the localized label for the Unmanaged Debugging checkbox.</summary>
        public string CaptEnableUnmanagedDebugging   => DebugPropertyPagePanel.captEnableUnmanagedDebugging;

        /// <summary>Gets the localized tooltip for the Debugger Command field.</summary>
        public string DescDebuggerCommand            => DebugPropertyPagePanel.descDebuggerCommand;
        /// <summary>Gets the localized tooltip for the Debugger Arguments field.</summary>
        public string DescDebuggerCommandArguments   => DebugPropertyPagePanel.descDebuggerCommandArguments;
        /// <summary>Gets the localized tooltip for the Debug Type combo.</summary>
        public string DescDebugType                  => DebugPropertyPagePanel.descDebugType;
        /// <summary>Gets the localized tooltip for the Working Directory field.</summary>
        public string DescDebuggerWorkingDirectory   => DebugPropertyPagePanel.descDebuggerWorkingDirectory;
        /// <summary>Gets the localized tooltip for the Unmanaged Debugging checkbox.</summary>
        public string DescEnableUnmanagedDebugging   => DebugPropertyPagePanel.descEnableUnmanagedDebugging;

        // =========================================================================================
        // IPropertyPagePanel — ViewModel lifecycle
        // =========================================================================================

        /// <inheritdoc/>
        public override void HookupEvents()
        {
            // Re-bind when the user picks a different configuration.
            _configSelector.PropertyChanged += (s, e) =>
            {
                ThreadHelper.ThrowIfNotOnUIThread();
                if (_isBinding)
                    return;
                if (e.PropertyName == nameof(XConfigSelectorViewModel.SelectedConfig))
                {
                    BindProperties();
                }
            };

            PropertyChanged += (sender, e) =>
            {
                if (_isBinding || _isNotifying)
                    return;

                NotifyDirty();

                _isNotifying = true;
                try   { OnPropertyChanged("Item[]"); }
                finally { _isNotifying = false; }
            };
        }

        /// <inheritdoc/>
        public override void BindProperties()
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            _isBinding = true;
            try
            {
                // ---- Config selector ----
                var page = (XPropertyPage)parentPropertyPage;
                var allConfigs = page.GetAllProjectConfigs();
                var activeConfigName = page.ProjectConfigs?.Count > 0
                    ? ((XProjectConfig)page.ProjectConfigs[0]).ConfigName
                    : XConfigSelectorViewModel.AllConfigurations;
                _configSelector.Initialize(allConfigs, activeConfigName);
                var configs = _configSelector.ResolvedConfigs;

                // ---- Properties ----
                DebuggerCommand          = page.GetPropertyForConfigs(XSharpProjectFileConstants.DebuggerCommand,          configs) ?? string.Empty;
                DebuggerCommandArguments = page.GetPropertyForConfigs(XSharpProjectFileConstants.DebuggerCommandArguments, configs) ?? string.Empty;
                DebugType                = page.GetPropertyForConfigs(XSharpProjectFileConstants.DebugType,                configs) ?? string.Empty;
                DebuggerWorkingDirectory = page.GetPropertyForConfigs(XSharpProjectFileConstants.DebuggerWorkingDirectory, configs) ?? string.Empty;
                EnableUnmanagedDebugging = GetBoolForConfigs(XSharpProjectFileConstants.EnableUnmanagedDebugging);

                isDirty = false;
            }
            finally
            {
                // Raise PropertyChanged(null) while _isBinding is still true so WPF
                // re-reads ALL value bindings without triggering HookupEvents dirty logic.
                OnPropertyChanged(null);
                try   { OnPropertyChanged("Item[]"); }
                finally { _isBinding = false; }
            }
        }

        /// <inheritdoc/>
        protected override void ApplyChangesCore()
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            var page = (XPropertyPage)parentPropertyPage;
            var configs = _configSelector.ResolvedConfigs;

            page.SetPropertyForConfigs(XSharpProjectFileConstants.DebuggerCommand,          DebuggerCommand          ?? string.Empty, configs);
            page.SetPropertyForConfigs(XSharpProjectFileConstants.DebuggerCommandArguments, DebuggerCommandArguments ?? string.Empty, configs);
            page.SetPropertyForConfigs(XSharpProjectFileConstants.DebugType,                DebugType                ?? string.Empty, configs);
            page.SetPropertyForConfigs(XSharpProjectFileConstants.DebuggerWorkingDirectory, DebuggerWorkingDirectory ?? string.Empty, configs);
            SetBoolForConfigs(XSharpProjectFileConstants.EnableUnmanagedDebugging,          EnableUnmanagedDebugging);

            isDirty = false;
        }
    }
}
