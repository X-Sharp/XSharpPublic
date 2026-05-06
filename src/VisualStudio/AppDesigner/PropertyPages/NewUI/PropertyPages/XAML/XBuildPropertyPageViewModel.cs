//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Collections.Generic;
using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell;
using XSharp.Settings;

namespace XSharp.Project
{
    /// <summary>
    /// MVVM ViewModel that backs the XAML/WPF Build property page for
    /// SDK-style XSharp projects.
    /// </summary>
    /// <remarks>
    /// <para>
    /// Each MSBuild property shown on the Build page is exposed as an observable
    /// property backed by <see cref="XPropertyPageViewModel.SetProperty{T}"/> so that
    /// WPF data binding automatically triggers
    /// <see cref="System.ComponentModel.INotifyPropertyChanged"/> on every change.
    /// </para>
    /// <para>
    /// The three <em>Treat Warnings as Errors</em> radio buttons are modelled as three
    /// mutually-exclusive bool properties (<see cref="WarningsModeNone"/>,
    /// <see cref="WarningsModeAll"/>, <see cref="WarningsModeSpecific"/>) kept in sync
    /// with each other inside their setters.
    /// </para>
    /// <para>
    /// The <see cref="OutputPath"/>, <see cref="IntermediateOutputPath"/>, and
    /// <see cref="AssemblyOriginatorKeyFile"/> TextBoxes are read-only in the UI;
    /// the user edits them via <em>Browse (…)</em> buttons in the code-behind that
    /// open <see cref="XSharpSLEPropertyForm"/> — matching WinForms behaviour exactly.
    /// </para>
    /// </remarks>
    internal sealed class XBuildPropertyPageViewModel : XPropertyPageViewModel
    {
        // =========================================================================================
        // Backing fields — string properties
        // =========================================================================================

        private string _outputPath             = string.Empty;
        private string _intermediateOutputPath = string.Empty;
        private string _assemblyOriginatorKeyFile = string.Empty;
        private string _defineConstants        = string.Empty;
        private string _commandLineOption      = string.Empty;
        private string _noWarn                 = string.Empty;
        private string _documentationFile      = string.Empty;
        private string _platformTarget         = string.Empty;
        private string _specificWarnings       = string.Empty;

        // =========================================================================================
        // Backing fields — bool checkboxes
        // =========================================================================================

        private bool _ppo;
        private bool _useSharedCompilation;
        private bool _prefer32Bit;
        private bool _registerForComInterop;
        private bool _optimize;
        private bool _signAssembly;
        private bool _suppressRCWarnings;
        private bool _delaySign;
        private bool _xmlDocEnabled;

        // =========================================================================================
        // Backing fields — combos / radio
        // =========================================================================================

        private int  _warningLevel;
        private bool _warningsModeNone     = true;
        private bool _warningsModeAll;
        private bool _warningsModeSpecific;

        // =========================================================================================
        // Backing fields — UI state (not dirty-tracked)
        // =========================================================================================

        private bool _prefer32BitEnabled   = true;
        private bool _specificWarningsEnabled;
        private List<string> _platformTargetValues = new List<string>();

        private bool _isBinding   = false;   // true while BindProperties is loading values
        private bool _isNotifying = false;   // true while firing Item[] refresh pulse

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
        /// Initializes a new instance of the <see cref="XBuildPropertyPageViewModel"/> class.
        /// </summary>
        /// <param name="parentPropertyPage">The <see cref="XPropertyPage"/> that owns this panel.</param>
        public XBuildPropertyPageViewModel(XPropertyPage parentPropertyPage)
            : base(parentPropertyPage)
        {
            // Populate platform combo values from the same converter the WinForms panel uses.
            var conv = new PlatformConverter();
            foreach (var v in conv.GetStandardValues())
                _platformTargetValues.Add(v?.ToString() ?? string.Empty);
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

        /// <summary>Gets or sets the output path.  Edited via the Browse (…) button.</summary>
        public string OutputPath
        {
            get => _outputPath;
            set => SetProperty(ref _outputPath, value);
        }

        /// <summary>Gets or sets the intermediate output path.  Edited via the Browse (…) button.</summary>
        public string IntermediateOutputPath
        {
            get => _intermediateOutputPath;
            set => SetProperty(ref _intermediateOutputPath, value);
        }

        /// <summary>Gets or sets the strong-name key file path.  Edited via the Browse (…) button.</summary>
        public string AssemblyOriginatorKeyFile
        {
            get => _assemblyOriginatorKeyFile;
            set => SetProperty(ref _assemblyOriginatorKeyFile, value);
        }

        /// <summary>Gets or sets the preprocessor define constants.</summary>
        public string DefineConstants
        {
            get => _defineConstants;
            set => SetProperty(ref _defineConstants, value);
        }

        /// <summary>Gets or sets extra command-line options passed to the compiler.</summary>
        public string CommandLineOption
        {
            get => _commandLineOption;
            set => SetProperty(ref _commandLineOption, value);
        }

        /// <summary>Gets or sets the semicolon-separated list of warning numbers to suppress.</summary>
        public string NoWarn
        {
            get => _noWarn;
            set => SetProperty(ref _noWarn, value);
        }

        /// <summary>Gets or sets the XML documentation file name.  Empty means disabled.</summary>
        public string DocumentationFile
        {
            get => _documentationFile;
            set => SetProperty(ref _documentationFile, value);
        }

        /// <summary>Gets or sets the target platform (e.g. <c>AnyCPU</c>, <c>x64</c>).</summary>
        public string PlatformTarget
        {
            get => _platformTarget;
            set => SetProperty(ref _platformTarget, value);
        }

        /// <summary>Gets or sets the specific warning numbers to treat as errors.</summary>
        public string SpecificWarnings
        {
            get => _specificWarnings;
            set => SetProperty(ref _specificWarnings, value);
        }

        // =========================================================================================
        // Observable Properties — checkboxes
        // =========================================================================================

        /// <summary>Gets or sets whether preprocessor output files (<c>.ppo</c>) are generated.</summary>
        public bool PPO
        {
            get => _ppo;
            set => SetProperty(ref _ppo, value);
        }

        /// <summary>Gets or sets whether the shared (Roslyn) compilation service is used.</summary>
        public bool UseSharedCompilation
        {
            get => _useSharedCompilation;
            set => SetProperty(ref _useSharedCompilation, value);
        }

        /// <summary>Gets or sets whether the 32-bit preferred flag is set (<c>Prefer32Bit</c>).
        /// Only meaningful when <see cref="PlatformTarget"/> is <c>AnyCPU</c>.</summary>
        public bool Prefer32Bit
        {
            get => _prefer32Bit;
            set => SetProperty(ref _prefer32Bit, value);
        }

        /// <summary>Gets or sets whether the assembly is registered for COM interop.</summary>
        public bool RegisterForComInterop
        {
            get => _registerForComInterop;
            set => SetProperty(ref _registerForComInterop, value);
        }

        /// <summary>Gets or sets whether compiler optimisations are enabled.</summary>
        public bool Optimize
        {
            get => _optimize;
            set => SetProperty(ref _optimize, value);
        }

        /// <summary>Gets or sets whether the assembly is signed with a strong-name key.</summary>
        public bool SignAssembly
        {
            get => _signAssembly;
            set => SetProperty(ref _signAssembly, value);
        }

        /// <summary>Gets or sets whether RC (Win32 resource compiler) warnings are suppressed.</summary>
        public bool SuppressRCWarnings
        {
            get => _suppressRCWarnings;
            set => SetProperty(ref _suppressRCWarnings, value);
        }

        /// <summary>Gets or sets whether delay-signing is enabled.</summary>
        public bool DelaySign
        {
            get => _delaySign;
            set => SetProperty(ref _delaySign, value);
        }

        /// <summary>
        /// Gets or sets whether XML documentation generation is enabled.
        /// When set to <see langword="true"/> and <see cref="DocumentationFile"/> is empty,
        /// the file name is auto-derived from the assembly name.
        /// </summary>
        public bool XmlDocEnabled
        {
            get => _xmlDocEnabled;
            set
            {
                ThreadHelper.ThrowIfNotOnUIThread();
                if (SetProperty(ref _xmlDocEnabled, value))
                {
                    if (value && string.IsNullOrEmpty(DocumentationFile))
                    {
                        // Auto-derive filename from the assembly name — matches WinForms behaviour.
                        var asmName = parentPropertyPage.GetProperty(XSharpProjectFileConstants.AssemblyName) ?? "NoName";
                        DocumentationFile = asmName + ".Xml";
                        parentPropertyPage.SetProperty(XSharpProjectFileConstants.DocumentationFile, DocumentationFile);
                    }
                    else if (!value)
                    {
                        DocumentationFile = string.Empty;
                        parentPropertyPage.SetProperty(XSharpProjectFileConstants.DocumentationFile, string.Empty);
                    }
                }
            }
        }

        // =========================================================================================
        // Observable Properties — combo / radio buttons
        // =========================================================================================

        /// <summary>Gets or sets the compiler warning level (0–4).</summary>
        public int WarningLevel
        {
            get => _warningLevel;
            set => SetProperty(ref _warningLevel, value);
        }

        /// <summary>
        /// Gets or sets whether the "No warnings" radio button is selected
        /// (<c>TreatWarningsAsErrors = False</c> and <c>WarningsAsErrors</c> is empty).
        /// </summary>
        public bool WarningsModeNone
        {
            get => _warningsModeNone;
            set
            {
                if (SetProperty(ref _warningsModeNone, value) && value)
                {
                    _warningsModeAll      = false;
                    _warningsModeSpecific = false;
                    OnPropertyChanged(nameof(WarningsModeAll));
                    OnPropertyChanged(nameof(WarningsModeSpecific));
                    SpecificWarningsEnabled = false;
                }
            }
        }

        /// <summary>
        /// Gets or sets whether the "Treat all warnings as errors" radio button is selected
        /// (<c>TreatWarningsAsErrors = True</c>).
        /// </summary>
        public bool WarningsModeAll
        {
            get => _warningsModeAll;
            set
            {
                if (SetProperty(ref _warningsModeAll, value) && value)
                {
                    _warningsModeNone     = false;
                    _warningsModeSpecific = false;
                    OnPropertyChanged(nameof(WarningsModeNone));
                    OnPropertyChanged(nameof(WarningsModeSpecific));
                    SpecificWarningsEnabled = false;
                }
            }
        }

        /// <summary>
        /// Gets or sets whether the "Treat specific warnings as errors" radio button is selected.
        /// </summary>
        public bool WarningsModeSpecific
        {
            get => _warningsModeSpecific;
            set
            {
                if (SetProperty(ref _warningsModeSpecific, value) && value)
                {
                    _warningsModeNone = false;
                    _warningsModeAll  = false;
                    OnPropertyChanged(nameof(WarningsModeNone));
                    OnPropertyChanged(nameof(WarningsModeAll));
                    SpecificWarningsEnabled = true;
                }
            }
        }

        // =========================================================================================
        // Observable Properties — UI state (IsEnabled / IsReadOnly; not dirty-tracked)
        // =========================================================================================

        /// <summary>
        /// Gets or sets whether the <see cref="Prefer32Bit"/> checkbox is enabled.
        /// <see langword="true"/> only when <see cref="PlatformTarget"/> is <c>AnyCPU</c>.
        /// </summary>
        public bool Prefer32BitEnabled
        {
            get => _prefer32BitEnabled;
            set => SetProperty(ref _prefer32BitEnabled, value);
        }

        /// <summary>
        /// Gets or sets whether the Specific Warnings TextBox is enabled.
        /// <see langword="true"/> only when <see cref="WarningsModeSpecific"/> is selected.
        /// </summary>
        public bool SpecificWarningsEnabled
        {
            get => _specificWarningsEnabled;
            set => SetProperty(ref _specificWarningsEnabled, value);
        }

        /// <summary>Gets the platform target values for the combo box.</summary>
        public List<string> PlatformTargetValues => _platformTargetValues;

        // =========================================================================================
        // String properties — labels and tooltips from BuildPropertyPagePanel
        // =========================================================================================

        /// <summary>Gets the localized label for the PPO checkbox.</summary>
        public string CaptPPO                       => BuildPropertyPagePanel.PPOCaption;
        /// <summary>Gets the localized label for the Use Shared Compilation checkbox.</summary>
        public string CaptUseSharedCompilation      => BuildPropertyPagePanel.captUseSharedCompilation;
        /// <summary>Gets the localized label for the Prefer 32-bit checkbox.</summary>
        public string CaptPrefer32Bit               => BuildPropertyPagePanel.captPrefer32Bit;
        /// <summary>Gets the localized label for the Register for COM Interop checkbox.</summary>
        public string CaptRegisterForComInterop     => BuildPropertyPagePanel.captRegisterForComInterop;
        /// <summary>Gets the localized label for the XML Documentation File checkbox.</summary>
        public string CaptDocumentationFile         => BuildPropertyPagePanel.captDocumentationFile;
        /// <summary>Gets the localized label for the Optimize checkbox.</summary>
        public string CaptOptimize                  => BuildPropertyPagePanel.captOptimize;
        /// <summary>Gets the localized label for the Sign Assembly checkbox.</summary>
        public string CaptSignAssembly              => BuildPropertyPagePanel.captSignAssembly;
        /// <summary>Gets the localized label for the Suppress RC Warnings checkbox.</summary>
        public string CaptSuppressRCWarnings        => BuildPropertyPagePanel.SuppressRCWarningsCaption;
        /// <summary>Gets the localized label for the Delay Sign checkbox.</summary>
        public string CaptDelaySign                 => BuildPropertyPagePanel.captDelaySign;
        /// <summary>Gets the localized label for the Define Constants field.</summary>
        public string CaptDefineConstants           => BuildPropertyPagePanel.DefCaption;
        /// <summary>Gets the localized label for the Command Line Option field.</summary>
        public string CaptCommandLineOption         => BuildPropertyPagePanel.CmdLineCaption;
        /// <summary>Gets the localized label for the No Warn field.</summary>
        public string CaptNoWarn                    => BuildPropertyPagePanel.captNoWarn;
        /// <summary>Gets the localized label for the Output Path field.</summary>
        public string CaptOutputPath                => BuildPropertyPagePanel.captOutputPath;
        /// <summary>Gets the localized label for the Intermediate Output Path field.</summary>
        public string CaptIntermediateOutputPath    => BuildPropertyPagePanel.captIntermediateOutputPath;
        /// <summary>Gets the localized label for the Assembly Key File field.</summary>
        public string CaptAssemblyOriginatorKeyFile => BuildPropertyPagePanel.captAssemblyOriginatorKeyFile;
        /// <summary>Gets the localized label for the Platform Target combo.</summary>
        public string CaptPlatformTarget            => BuildPropertyPagePanel.captPlatFormTarget;
        /// <summary>Gets the localized label for the Warning Level combo.</summary>
        public string CaptWarningLevel              => BuildPropertyPagePanel.captWarningLevel;

        /// <summary>Gets the localized tooltip for the PPO checkbox.</summary>
        public string DescPPO                       => BuildPropertyPagePanel.PPODescription;
        /// <summary>Gets the localized tooltip for the Use Shared Compilation checkbox.</summary>
        public string DescUseSharedCompilation      => BuildPropertyPagePanel.descUseSharedCompilation;
        /// <summary>Gets the localized tooltip for the Prefer 32-bit checkbox.</summary>
        public string DescPrefer32Bit               => BuildPropertyPagePanel.descPrefer32Bit;
        /// <summary>Gets the localized tooltip for the Register for COM Interop checkbox.</summary>
        public string DescRegisterForComInterop     => BuildPropertyPagePanel.descRegisterForComInterop;
        /// <summary>Gets the localized tooltip for the XML Documentation File checkbox.</summary>
        public string DescDocumentationFile         => BuildPropertyPagePanel.descDocumentationFile;
        /// <summary>Gets the localized tooltip for the XML Documentation File name field.</summary>
        public string DescDocumentationFileName     => BuildPropertyPagePanel.descDocumentationFileName;
        /// <summary>Gets the localized tooltip for the Optimize checkbox.</summary>
        public string DescOptimize                  => BuildPropertyPagePanel.descOptimize;
        /// <summary>Gets the localized tooltip for the Sign Assembly checkbox.</summary>
        public string DescSignAssembly              => BuildPropertyPagePanel.descSignAssembly;
        /// <summary>Gets the localized tooltip for the Suppress RC Warnings checkbox.</summary>
        public string DescSuppressRCWarnings        => BuildPropertyPagePanel.SuppressRCWarningsDescription;
        /// <summary>Gets the localized tooltip for the Delay Sign checkbox.</summary>
        public string DescDelaySign                 => BuildPropertyPagePanel.descDelaySign;
        /// <summary>Gets the localized tooltip for the Define Constants field.</summary>
        public string DescDefineConstants           => BuildPropertyPagePanel.DefDescription;
        /// <summary>Gets the localized tooltip for the Command Line Option field.</summary>
        public string DescCommandLineOption         => BuildPropertyPagePanel.CmdLineDescription;
        /// <summary>Gets the localized tooltip for the No Warn field.</summary>
        public string DescNoWarn                    => BuildPropertyPagePanel.descNoWarn;
        /// <summary>Gets the localized tooltip for the Output Path field.</summary>
        public string DescOutputPath                => BuildPropertyPagePanel.descOutputPath;
        /// <summary>Gets the localized tooltip for the Intermediate Output Path field.</summary>
        public string DescIntermediateOutputPath    => BuildPropertyPagePanel.descIntermediateOutputPath;
        /// <summary>Gets the localized tooltip for the Assembly Key File field.</summary>
        public string DescAssemblyOriginatorKeyFile => BuildPropertyPagePanel.descAssemblyOriginatorKeyFile;
        /// <summary>Gets the localized tooltip for the Platform Target combo.</summary>
        public string DescPlatformTarget            => BuildPropertyPagePanel.descPlatFormTarget;
        /// <summary>Gets the localized tooltip for the Warning Level combo.</summary>
        public string DescWarningLevel              => BuildPropertyPagePanel.descWarningLevel;
        /// <summary>Gets the localized tooltip for the Specific Warnings field.</summary>
        public string DescSpecificWarnings          => BuildPropertyPagePanel.descSpecificWarnings;

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
                // UI-state properties do not represent project edits — skip dirty.
                switch (e.PropertyName)
                {
                    case nameof(Prefer32BitEnabled):
                    case nameof(SpecificWarningsEnabled):
                        return;
                }

                // Ignore re-entrant notifications from BindProperties load or Item[] pulse.
                if (_isBinding || _isNotifying)
                    return;

                // When PlatformTarget changes, re-evaluate Prefer32BitEnabled.
                if (e.PropertyName == nameof(PlatformTarget))
                {
                    Prefer32BitEnabled = string.Equals(
                        PlatformTarget,
                        XSharpProjectFileConstants.AnyCPU,
                        StringComparison.OrdinalIgnoreCase);

                    if (!Prefer32BitEnabled)
                        Prefer32Bit = false;
                }

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
                // --- Config selector ---
                var page = (XPropertyPage)parentPropertyPage;
                var allConfigs = page.GetAllProjectConfigs();
                // Determine the active config name from the first config VS passed via SetObjects.
                var activeConfigName = page.ProjectConfigs?.Count > 0
                    ? ((XProjectConfig)page.ProjectConfigs[0]).ConfigName
                    : XConfigSelectorViewModel.AllConfigurations;
                _configSelector.Initialize(allConfigs, activeConfigName);
                var configs = _configSelector.ResolvedConfigs;

                // --- String fields ---
                OutputPath                = page.GetPropertyForConfigs(XSharpProjectFileConstants.OutputPath,                configs) ?? string.Empty;
                IntermediateOutputPath    = page.GetPropertyForConfigs(XSharpProjectFileConstants.IntermediateOutputPath,    configs) ?? string.Empty;
                AssemblyOriginatorKeyFile = page.GetPropertyForConfigs(XSharpProjectFileConstants.AssemblyOriginatorKeyFile, configs) ?? string.Empty;
                DefineConstants           = page.GetPropertyForConfigs(XSharpProjectFileConstants.DefineConstants,           configs) ?? string.Empty;
                CommandLineOption         = page.GetPropertyForConfigs(XSharpProjectFileConstants.CommandLineOption,         configs) ?? string.Empty;
                NoWarn                    = page.GetPropertyForConfigs(XSharpProjectFileConstants.NoWarn,                    configs) ?? string.Empty;
                DocumentationFile         = page.GetPropertyForConfigs(XSharpProjectFileConstants.DocumentationFile,         configs) ?? string.Empty;

                // ---- XmlDoc enabled flag (derived) ----
                _xmlDocEnabled = !string.IsNullOrEmpty(DocumentationFile);
                OnPropertyChanged(nameof(XmlDocEnabled));

                // ---- Platform ----
                PlatformTarget = page.GetPropertyForConfigs(XSharpProjectFileConstants.PlatformTarget, configs)
                                 ?? XSharpProjectFileConstants.AnyCPU;
                Prefer32BitEnabled = string.Equals(PlatformTarget, XSharpProjectFileConstants.AnyCPU,
                                                   StringComparison.OrdinalIgnoreCase);

                // ---- Bool checkboxes ----
                PPO                   = GetBoolForConfigs("PPO");
                UseSharedCompilation  = GetBoolForConfigs(XSharpProjectFileConstants.UseSharedCompilation);
                Prefer32Bit           = Prefer32BitEnabled && GetBoolForConfigs(XSharpProjectFileConstants.Prefer32Bit);
                RegisterForComInterop = GetBoolForConfigs(XSharpProjectFileConstants.RegisterForComInterop);
                Optimize              = GetBoolForConfigs(XSharpProjectFileConstants.Optimize);
                SignAssembly          = GetBoolForConfigs(XSharpProjectFileConstants.SignAssembly);
                SuppressRCWarnings    = GetBoolForConfigs(XSharpProjectFileConstants.SuppressRCWarnings);
                DelaySign             = GetBoolForConfigs(XSharpProjectFileConstants.DelaySign);

                // ---- Warning level ----
                var warnLevelStr = page.GetPropertyForConfigs(XSharpProjectFileConstants.WarningLevel, configs) ?? "4";
                WarningLevel = int.TryParse(warnLevelStr, out var wl) ? Math.Max(0, Math.Min(4, wl)) : 4;

                // ---- Warnings-as-errors radio group (mirrors WinForms BindProperties) ----
                var specificWarns = page.GetPropertyForConfigs(XSharpProjectFileConstants.WarningsAsErrors, configs) ?? string.Empty;
                specificWarns = specificWarns.Trim();
                SpecificWarnings = specificWarns;

                if (!string.IsNullOrEmpty(specificWarns))
                {
                    _warningsModeNone     = false;
                    _warningsModeAll      = false;
                    _warningsModeSpecific = true;
                    SpecificWarningsEnabled = true;
                }
                else
                {
                    var treatAll = page.GetPropertyForConfigs(XSharpProjectFileConstants.TreatWarningsAsErrors, configs) ?? "false";
                    bool isAll = string.Equals(treatAll.Trim(), "true", StringComparison.OrdinalIgnoreCase);
                    _warningsModeAll      = isAll;
                    _warningsModeNone     = !isAll;
                    _warningsModeSpecific = false;
                    SpecificWarningsEnabled = false;
                }
                OnPropertyChanged(nameof(WarningsModeNone));
                OnPropertyChanged(nameof(WarningsModeAll));
                OnPropertyChanged(nameof(WarningsModeSpecific));

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

            // ---- String fields ----
            page.SetPropertyForConfigs(XSharpProjectFileConstants.OutputPath,                OutputPath ?? string.Empty,                configs);
            page.SetPropertyForConfigs(XSharpProjectFileConstants.IntermediateOutputPath,    IntermediateOutputPath ?? string.Empty,    configs);
            page.SetPropertyForConfigs(XSharpProjectFileConstants.AssemblyOriginatorKeyFile, AssemblyOriginatorKeyFile ?? string.Empty, configs);
            page.SetPropertyForConfigs(XSharpProjectFileConstants.DefineConstants,           DefineConstants ?? string.Empty,           configs);
            page.SetPropertyForConfigs(XSharpProjectFileConstants.CommandLineOption,         CommandLineOption ?? string.Empty,         configs);
            page.SetPropertyForConfigs(XSharpProjectFileConstants.NoWarn,                   NoWarn ?? string.Empty,                    configs);
            page.SetPropertyForConfigs(XSharpProjectFileConstants.DocumentationFile,         DocumentationFile ?? string.Empty,         configs);

            // ---- Platform ----
            page.SetPropertyForConfigs(XSharpProjectFileConstants.PlatformTarget,            PlatformTarget ?? string.Empty,            configs);

            // ---- Bool checkboxes ----
            SetBoolForConfigs("PPO",                                               PPO);
            SetBoolForConfigs(XSharpProjectFileConstants.UseSharedCompilation,     UseSharedCompilation);
            SetBoolForConfigs(XSharpProjectFileConstants.Prefer32Bit,              Prefer32Bit);
            SetBoolForConfigs(XSharpProjectFileConstants.RegisterForComInterop,    RegisterForComInterop);
            SetBoolForConfigs(XSharpProjectFileConstants.Optimize,                 Optimize);
            SetBoolForConfigs(XSharpProjectFileConstants.SignAssembly,             SignAssembly);
            SetBoolForConfigs(XSharpProjectFileConstants.SuppressRCWarnings,       SuppressRCWarnings);
            SetBoolForConfigs(XSharpProjectFileConstants.DelaySign,                DelaySign);

            // ---- Warning level ----
            page.SetPropertyForConfigs(XSharpProjectFileConstants.WarningLevel,    WarningLevel.ToString(), configs);

            // ---- Warnings-as-errors radio group (mirrors WinForms HandleControlValidated) ----
            if (WarningsModeAll)
            {
                page.SetPropertyForConfigs(XSharpProjectFileConstants.TreatWarningsAsErrors, "True",  configs);
                page.SetPropertyForConfigs(XSharpProjectFileConstants.WarningsAsErrors,       " ",     configs);
            }
            else if (WarningsModeNone)
            {
                page.SetPropertyForConfigs(XSharpProjectFileConstants.TreatWarningsAsErrors, "False", configs);
                page.SetPropertyForConfigs(XSharpProjectFileConstants.WarningsAsErrors,       " ",     configs);
            }
            else // Specific
            {
                page.SetPropertyForConfigs(XSharpProjectFileConstants.TreatWarningsAsErrors, "False",                    configs);
                page.SetPropertyForConfigs(XSharpProjectFileConstants.WarningsAsErrors,       SpecificWarnings ?? string.Empty, configs);
            }

            isDirty = false;
        }

        // =========================================================================================
        // Platform change notification — called by XBuildPropertyPageXamlHost
        // =========================================================================================

        /// <summary>
        /// Updates <see cref="Prefer32BitEnabled"/> when the platform target changes externally
        /// (e.g. via the project property changed event).  Mirrors the WinForms
        /// <c>Project_OnProjectPropertyChanged</c> behaviour.
        /// </summary>
        /// <param name="newPlatform">The new platform target value.</param>
        public void NotifyPlatformTargetChanged(string newPlatform)
        {
            bool isAnyCpu = string.Equals(newPlatform, XSharpProjectFileConstants.AnyCPU,
                                          StringComparison.OrdinalIgnoreCase);
            Prefer32BitEnabled = isAnyCpu;
            if (!isAnyCpu)
                Prefer32Bit = false;
            PlatformTarget = newPlatform;
        }
    }
}
