//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using Microsoft.VisualStudio.Shell;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using Microsoft.VisualStudio.Project;
using XSharp.Settings;

namespace XSharp.Project
{
    /// <summary>
    /// MVVM ViewModel that backs the XAML/WPF General (Application) property page for
    /// SDK-style XSharp projects.
    /// </summary>
    /// <remarks>
    /// <para>
    /// This ViewModel exposes one observable property per MSBuild project property shown on
    /// the Application page: assembly name, root namespace, dialect, output type, target
    /// framework, startup object, application icon, and the four checkbox options.
    /// </para>
    /// <para>
    /// All properties use <see cref="XPropertyPageViewModel.SetProperty{T}"/> so that WPF
    /// data binding (<c>{Binding ..., Mode=TwoWay, UpdateSourceTrigger=PropertyChanged}</c>)
    /// automatically triggers <see cref="System.ComponentModel.INotifyPropertyChanged"/> on
    /// every change.
    /// </para>
    /// <para>
    /// <see cref="BindProperties"/> reads from MSBuild and populates the observable
    /// properties.  <see cref="ApplyChanges"/> reads the observable properties and writes
    /// them back to MSBuild.  <see cref="HookupEvents"/> subscribes to
    /// <see cref="System.ComponentModel.INotifyPropertyChanged.PropertyChanged"/> to mark the
    /// page dirty whenever any observable property changes.
    /// </para>
    /// </remarks>
    internal sealed class XGeneralPropertyPageViewModel : XPropertyPageViewModel
    {
        // =========================================================================================
        // Backing fields
        // =========================================================================================

        private string _assemblyName;
        private string _rootNamespace;
        private string _outputType;
        private string _dialect;
        private string _targetFramework;
        private string _targetFrameworks;         // multi-targeting
        private string _startupObject;
        private string _applicationIcon;
        private bool   _autoGenerateBindingRedirects;
        private bool   _suppressDefaultManifest;
        private bool   _preferNativeVersion;
        private bool   _vulcanCompatibleResources;
        private bool   _iconEnabled = true;       // disabled when RC files are present
        private bool   _startupObjectEnabled = true;
        private bool   _isMultiTargeting;
        private string _targetFrameworkLabel;

        private ObservableCollection<string> _outputTypeItems  = new ObservableCollection<string>();
        private ObservableCollection<string> _dialectItems     = new ObservableCollection<string>();
        private ObservableCollection<string> _frameworkItems   = new ObservableCollection<string>();
        private ObservableCollection<string> _startupItems     = new ObservableCollection<string>();

        private bool _isBinding   = false;   // true while BindProperties is loading values

        private bool _isNotifying = false;   // true while firing Item[] refresh pulse

        // =========================================================================================
        // Constructor
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="XGeneralPropertyPageViewModel"/> class.
        /// </summary>
        /// <param name="parentPropertyPage">
        /// The <see cref="XPropertyPage"/> that owns this panel.
        /// </param>
        public XGeneralPropertyPageViewModel(XPropertyPage parentPropertyPage)
            : base(parentPropertyPage)
        {
        }

        // =========================================================================================
        // Observable Properties — text fields
        // =========================================================================================

        /// <summary>Gets or sets the assembly name (MSBuild <c>AssemblyName</c>).</summary>
        public string AssemblyName
        {
            get => _assemblyName;
            set => SetProperty(ref _assemblyName, value);
        }

        /// <summary>Gets or sets the root (default) namespace (MSBuild <c>RootNamespace</c>).</summary>
        public string RootNamespace
        {
            get => _rootNamespace;
            set => SetProperty(ref _rootNamespace, value);
        }

        /// <summary>
        /// Gets or sets the currently selected output type display name
        /// (e.g., "Windows Application").  Stored in MSBuild as <c>OutputType</c>.
        /// </summary>
        public string OutputType
        {
            get => _outputType;
            set => SetProperty(ref _outputType, value);
        }

        /// <summary>
        /// Gets or sets the currently selected dialect display name (e.g., "Visual Objects").
        /// Stored in MSBuild as <c>Dialect</c>.
        /// </summary>
        public string Dialect
        {
            get => _dialect;
            set => SetProperty(ref _dialect, value);
        }

        /// <summary>
        /// Gets or sets the selected target framework display name for single-targeting
        /// projects (e.g., "net8.0").  Stored in MSBuild as <c>TargetFramework</c> (SDK) or
        /// <c>TargetFrameworkVersion</c> (legacy).
        /// </summary>
        public string TargetFramework
        {
            get => _targetFramework;
            set => SetProperty(ref _targetFramework, value);
        }

        /// <summary>
        /// Gets or sets the raw multi-targeting framework list
        /// (e.g., "net8.0;net48").  Stored in MSBuild as <c>XTargetFrameworks</c>.
        /// Only visible when <see cref="IsMultiTargeting"/> is <see langword="true"/>.
        /// </summary>
        public string TargetFrameworks
        {
            get => _targetFrameworks;
            set => SetProperty(ref _targetFrameworks, value);
        }

        /// <summary>
        /// Gets or sets the selected startup object display name
        /// (or <see cref="GeneralPropertyPagePanel.DefaultValue"/> meaning "none").
        /// Stored in MSBuild as <c>StartupObject</c>.
        /// </summary>
        public string StartupObject
        {
            get => _startupObject;
            set => SetProperty(ref _startupObject, value);
        }

        /// <summary>
        /// Gets or sets the path to the application icon file (.ico).
        /// Stored in MSBuild as <c>ApplicationIcon</c>.
        /// </summary>
        public string ApplicationIcon
        {
            get => _applicationIcon;
            set => SetProperty(ref _applicationIcon, value);
        }

        // =========================================================================================
        // Observable Properties — checkboxes
        // =========================================================================================

        /// <summary>
        /// Gets or sets whether binding redirects are auto-generated.
        /// Stored in MSBuild as <c>AutoGenerateBindingRedirects</c>.
        /// </summary>
        public bool AutoGenerateBindingRedirects
        {
            get => _autoGenerateBindingRedirects;
            set => SetProperty(ref _autoGenerateBindingRedirects, value);
        }

        /// <summary>
        /// Gets or sets whether the default Win32 manifest is suppressed.
        /// Stored in MSBuild as <c>NoWin32Manifest</c>.
        /// </summary>
        public bool SuppressDefaultManifest
        {
            get => _suppressDefaultManifest;
            set => SetProperty(ref _suppressDefaultManifest, value);
        }

        /// <summary>
        /// Gets or sets whether native version resource info is preferred over managed info.
        /// Stored in MSBuild as <c>UseNativeVersion</c>.
        /// </summary>
        public bool PreferNativeVersion
        {
            get => _preferNativeVersion;
            set => SetProperty(ref _preferNativeVersion, value);
        }

        /// <summary>
        /// Gets or sets whether Vulcan-compatible managed resources are used.
        /// Stored in MSBuild as <c>VulcanCompatibleResources</c>.
        /// </summary>
        public bool VulcanCompatibleResources
        {
            get => _vulcanCompatibleResources;
            set => SetProperty(ref _vulcanCompatibleResources, value);
        }

        // =========================================================================================
        // Observable Properties — UI state
        // =========================================================================================

        /// <summary>
        /// Gets or sets a value indicating whether the application icon controls are enabled.
        /// They are disabled when the project has RC (Win32 resource) files, because the icon
        /// is then controlled by the resource script rather than the MSBuild property.
        /// </summary>
        public bool IconEnabled
        {
            get => _iconEnabled;
            set => SetProperty(ref _iconEnabled, value);
        }

        /// <summary>
        /// Gets or sets a value indicating whether the startup object combo is enabled.
        /// It is disabled when no startup-eligible classes are found in the project.
        /// </summary>
        public bool StartupObjectEnabled
        {
            get => _startupObjectEnabled;
            set => SetProperty(ref _startupObjectEnabled, value);
        }

        /// <summary>
        /// Gets or sets a value indicating whether the project uses multi-targeting
        /// (<c>XTargetFrameworks</c>).  Controls which target-framework UI element is visible.
        /// </summary>
        public bool IsMultiTargeting
        {
            get => _isMultiTargeting;
            set => SetProperty(ref _isMultiTargeting, value);
        }

        /// <summary>
        /// Gets or sets the label text for the target framework field ("Target Framework:"
        /// for single or "Target Frameworks:" for multi-targeting).
        /// </summary>
        public string TargetFrameworkLabel
        {
            get => _targetFrameworkLabel;
            set => SetProperty(ref _targetFrameworkLabel, value);
        }

        // =========================================================================================
        // String properties — labels and tooltips from GeneralPropertyPagePanel
        // =========================================================================================

        /// <summary>Gets the localized label text for the Application Name field.</summary>
        public string CaptAppName                   => GeneralPropertyPagePanel.captAppName;

        /// <summary>Gets the localized label text for the Default Namespace field.</summary>
        public string CaptNamespace                 => GeneralPropertyPagePanel.captNamespace;

        /// <summary>Gets the localized label text for the Icon field.</summary>
        public string CaptIcon                      => GeneralPropertyPagePanel.captIcon;

        /// <summary>Gets the localized label text for the Startup Object field.</summary>
        public string CaptStartup                   => GeneralPropertyPagePanel.captStartup;

        /// <summary>Gets the localized label text for the Suppress Win32 Manifest checkbox.</summary>
        public string CaptWin32Manifest             => GeneralPropertyPagePanel.captWin32Manifest;

        /// <summary>Gets the localized label text for the Prefer Native Version checkbox.</summary>
        public string CaptPreferNative              => GeneralPropertyPagePanel.captPreferNative;

        /// <summary>Gets the localized label text for the Vulcan Compatible Resources checkbox.</summary>
        public string CaptVulcanCompatibleResources => GeneralPropertyPagePanel.captVulcanCompatibleResouces;

        /// <summary>Gets the localized label text for the Auto-generate Binding Redirects checkbox.</summary>
        public string CaptBindingRedirects          => GeneralPropertyPagePanel.captBindingRedirects;

        /// <summary>Gets the localized tooltip for the Assembly Name / Application Name field.</summary>
        public string DescAssembly                  => GeneralPropertyPagePanel.descAssembly;

        /// <summary>Gets the localized tooltip for the Default Namespace field.</summary>
        public string DescNamespace                 => GeneralPropertyPagePanel.descNamespace;

        /// <summary>Gets the localized tooltip for the Icon field.</summary>
        public string DescIcon                      => GeneralPropertyPagePanel.descIcon;

        /// <summary>Gets the localized tooltip for the Startup Object field.</summary>
        public string DescStartup                   => GeneralPropertyPagePanel.descStartup;

        /// <summary>Gets the localized tooltip for the Suppress Win32 Manifest checkbox.</summary>
        public string DescWin32Manifest             => GeneralPropertyPagePanel.descWin32Manifest;

        /// <summary>Gets the localized tooltip for the Prefer Native Version checkbox.</summary>
        public string DescPreferNative              => GeneralPropertyPagePanel.descPreferNative;

        /// <summary>Gets the localized tooltip for the Vulcan Compatible Resources checkbox.</summary>
        public string DescVulcanCompatibleResources => GeneralPropertyPagePanel.descVulcanCompatibleResouces;

        /// <summary>Gets the localized tooltip for the Auto-generate Binding Redirects checkbox.</summary>
        public string DescBindingRedirects          => GeneralPropertyPagePanel.descBindingRedirects;

        /// <summary>Gets the localized tooltip for the Dialect combo.</summary>
        public string DescDialect                   => GeneralPropertyPagePanel.descDialect;

        /// <summary>Gets the localized tooltip for the Output Type combo.</summary>
        public string DescOutputType                => GeneralPropertyPagePanel.descOutputType;

        // =========================================================================================
        // Observable Collections (combo-box item sources)
        // =========================================================================================

        /// <summary>Gets the list of output-type display names shown in the Output Type combo.</summary>
        public ObservableCollection<string> OutputTypeItems => _outputTypeItems;

        /// <summary>Gets the list of dialect display names shown in the Dialect combo.</summary>
        public ObservableCollection<string> DialectItems => _dialectItems;

        /// <summary>Gets the list of target-framework names shown in the Target Framework combo.</summary>
        public ObservableCollection<string> FrameworkItems => _frameworkItems;

        /// <summary>Gets the list of startup-object names shown in the Startup Object combo.</summary>
        public ObservableCollection<string> StartupItems => _startupItems;

        // =========================================================================================
        // IPropertyPagePanel — ViewModel lifecycle
        // =========================================================================================

        /// <inheritdoc/>
        public override void HookupEvents()
        {
            // When any observable property changes mark the page dirty.
            // We subscribe to our own PropertyChanged event raised by SetProperty<T>.
            PropertyChanged += (sender, e) =>
            {
                ThreadHelper.ThrowIfNotOnUIThread();
                // Ignore pure UI-state changes — they don't represent project property edits.
                if (e.PropertyName == nameof(IconEnabled)
                    || e.PropertyName == nameof(StartupObjectEnabled)
                    || e.PropertyName == nameof(IsMultiTargeting)
                    || e.PropertyName == nameof(TargetFrameworkLabel))
                    return;

                // Ignore re-entrant notifications from BindProperties load or Item[] pulse.
                if (_isBinding || _isNotifying)
                    return;

                NotifyDirty();

                // Pulse Item[] so all Reset-button IsEnabled bindings re-evaluate.
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
                // ---- Determine project style ----
                bool isSdk          = IsSdkProject;
                bool isMultiTarget  = IsMultiTargetingProject;
                IsMultiTargeting    = isMultiTarget;

                // ---- Populate combo item sources ----
                PopulateOutputTypeItems();
                PopulateDialectItems();
                PopulateFrameworkItems(isSdk, isMultiTarget);
                PopulateStartupItems();

                // ---- Target framework label ----
                TargetFrameworkLabel = isMultiTarget
                    ? GeneralPropertyPagePanel.captTargetFrameworks
                    : GeneralPropertyPagePanel.captTargetFramework;

                // ---- Text fields ----
                AssemblyName    = parentPropertyPage.GetProperty(XSharpProjectFileConstants.AssemblyName) ?? string.Empty;
                RootNamespace   = parentPropertyPage.GetProperty(XSharpProjectFileConstants.RootNamespace) ?? string.Empty;
                ApplicationIcon = parentPropertyPage.GetProperty(XSharpProjectFileConstants.ApplicationIcon) ?? string.Empty;

                // ---- Output type (display name) ----
                OutputType = parentPropertyPage.GetProperty(XSharpProjectFileConstants.OutputType) ?? string.Empty;

                // ---- Dialect (display name) ----
                Dialect = parentPropertyPage.GetProperty(XSharpProjectFileConstants.Dialect) ?? string.Empty;

                // ---- Target framework ----
                if (isMultiTarget)
                {
                    TargetFrameworks = parentPropertyPage.GetProperty(XSharpProjectFileConstants.XTargetFrameworks) ?? string.Empty;
                }
                else
                {
                    string fwProp = isSdk
                        ? XSharpProjectFileConstants.TargetFramework
                        : XSharpProjectFileConstants.TargetFrameworkVersion;
                    TargetFramework = parentPropertyPage.GetProperty(fwProp) ?? string.Empty;
                }

                // ---- Startup object ----
                string startupRaw = parentPropertyPage.GetProperty(XSharpProjectFileConstants.StartupObject) ?? string.Empty;
                StartupObject = string.IsNullOrEmpty(startupRaw) ? GeneralPropertyPagePanel.DefaultValue : startupRaw;

                // ---- Checkboxes ----
                AutoGenerateBindingRedirects = GetBoolPropertyValue(XSharpProjectFileConstants.AutoGenerateBindingRedirects);
                SuppressDefaultManifest      = GetBoolPropertyValue(XSharpProjectFileConstants.NoWin32Manifest);
                PreferNativeVersion          = GetBoolPropertyValue(XSharpProjectFileConstants.UseNativeVersion);
                VulcanCompatibleResources    = GetBoolPropertyValue(XSharpProjectFileConstants.VulcanCompatibleResources);

                // ---- UI state ----
                IconEnabled          = !HasRCFiles();
                StartupObjectEnabled = _startupItems.Count > 1;   // more than just DefaultValue entry

                // Clear dirty after loading
                isDirty = false;
            }
            finally
            {
                // Raise PropertyChanged(null) while _isBinding is still true so WPF
                // re-reads ALL value bindings (including ones where the value didn't
                // change), but the HookupEvents lambda sees _isBinding=true and skips
                // NotifyDirty/ApplyChanges.
                OnPropertyChanged(null);
                try   { OnPropertyChanged("Item[]"); }
                finally { _isBinding = false; }
            }
        }

        /// <inheritdoc/>
        protected override void ApplyChangesCore()
        {
            ThreadHelper.ThrowIfNotOnUIThread();

            SetPropertyIfOverriddenOrNonEmpty(XSharpProjectFileConstants.AssemblyName,    AssemblyName    ?? string.Empty);
            SetPropertyIfOverriddenOrNonEmpty(XSharpProjectFileConstants.RootNamespace,   RootNamespace   ?? string.Empty);
            SetPropertyIfOverriddenOrNonEmpty(XSharpProjectFileConstants.ApplicationIcon, ApplicationIcon ?? string.Empty);

            // Output type — page's SetProperty handles display→raw conversion
            parentPropertyPage.SetProperty(XSharpProjectFileConstants.OutputType, OutputType ?? string.Empty);

            // Dialect — page's SetProperty handles display→raw conversion + Allowdot side-effect
            parentPropertyPage.SetProperty(XSharpProjectFileConstants.Dialect, Dialect ?? string.Empty);

            // Target framework
            if (IsMultiTargeting)
            {
                SetPropertyIfOverriddenOrNonEmpty(XSharpProjectFileConstants.XTargetFrameworks, TargetFrameworks ?? string.Empty);
            }
            else
            {
                string fwProp = IsSdkProject
                    ? XSharpProjectFileConstants.TargetFramework
                    : XSharpProjectFileConstants.TargetFrameworkVersion;
                SetPropertyIfOverriddenOrNonEmpty(fwProp, TargetFramework ?? string.Empty);
            }

            // Startup object — convert DefaultValue sentinel back to ""
            string startupRaw = (StartupObject == GeneralPropertyPagePanel.DefaultValue)
                ? string.Empty
                : (StartupObject ?? string.Empty);
            parentPropertyPage.SetProperty(XSharpProjectFileConstants.StartupObject, startupRaw);

            // Checkboxes — only write if already overridden; avoids re-writing a reset value
            SetBoolPropertyValue(XSharpProjectFileConstants.AutoGenerateBindingRedirects, AutoGenerateBindingRedirects);
            SetBoolPropertyValue(XSharpProjectFileConstants.NoWin32Manifest,              SuppressDefaultManifest);
            SetBoolPropertyValue(XSharpProjectFileConstants.UseNativeVersion,             PreferNativeVersion);
            SetBoolPropertyValue(XSharpProjectFileConstants.VulcanCompatibleResources,    VulcanCompatibleResources);

            isDirty = false;
        }

        // =========================================================================================
        // Private helpers — combo population
        // =========================================================================================

        /// <summary>
        /// Populates <see cref="OutputTypeItems"/> from <see cref="OutputTypeConverter"/>.
        /// </summary>
        private void PopulateOutputTypeItems()
        {
            _outputTypeItems.Clear();
            var converter = outputTypeConverter;
            foreach (OutputType val in System.Enum.GetValues(typeof(OutputType)))
            {
                string display = (string)converter.ConvertTo(val, typeof(string));
                if (!string.IsNullOrEmpty(display))
                    _outputTypeItems.Add(display);
            }
        }

        /// <summary>
        /// Populates <see cref="DialectItems"/> from <see cref="DialectConverter"/>.
        /// </summary>
        private void PopulateDialectItems()
        {
            _dialectItems.Clear();
            var converter = dialectConverter;
            foreach (Dialect val in System.Enum.GetValues(typeof(Dialect)))
            {
                string display = (string)converter.ConvertTo(val, typeof(string));
                if (!string.IsNullOrEmpty(display))
                    _dialectItems.Add(display);
            }
        }

        /// <summary>
        /// Populates <see cref="FrameworkItems"/> with the available target frameworks.
        /// For multi-targeting projects the combo is hidden, so the list is left empty.
        /// </summary>
        private void PopulateFrameworkItems(bool isSdk, bool isMultiTarget)
        {
            _frameworkItems.Clear();
            if (isMultiTarget)
                return;

            var project = parentPropertyPage?.ProjectMgr;
            if (project == null)
                return;

            string moniker = GetTargetFrameworkMoniker();
            if (string.IsNullOrEmpty(moniker))
                return;

            if (isSdk)
            {
                if (moniker.StartsWith(".NETFramework") || moniker.StartsWith(".NETCoreApp"))
                {
                    var converter = new SdkFrameworkNameConverter(project.BuildProject);
                    foreach (SdkFrameworkName fn in converter.GetStandardValues(null))
                    {
                        _frameworkItems.Add(fn.DisplayName);
                    }
                }
            }
            else
            {
                var converter = new FrameworkNameConverter();
                foreach (var fn in converter.GetStandardValues(null))
                {
                    _frameworkItems.Add(fn.ToString());
                }
            }
        }

        /// <summary>
        /// Populates <see cref="StartupItems"/> with the startup-eligible class names
        /// discovered in the XSharp code model database, plus the "Not set" sentinel entry.
        /// </summary>
        private void PopulateStartupItems()
        {
            _startupItems.Clear();
            _startupItems.Add(GeneralPropertyPagePanel.DefaultValue);

            List<string> classes = GetStartupClasses();
            foreach (var cls in classes)
                _startupItems.Add(cls);
        }
    }
}
