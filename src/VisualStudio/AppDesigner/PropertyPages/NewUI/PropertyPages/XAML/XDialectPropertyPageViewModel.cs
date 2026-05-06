//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell;
using XSharp.Settings;

namespace XSharp.Project
{
    /// <summary>
    /// MVVM ViewModel that backs the XAML/WPF Dialect property page for
    /// SDK-style XSharp projects.
    /// </summary>
    /// <remarks>
    /// <para>
    /// Each MSBuild property shown on the Dialect page is exposed as an observable
    /// property backed by <see cref="XPropertyPageViewModel.SetProperty{T}"/> so that
    /// WPF data binding automatically triggers
    /// <see cref="System.ComponentModel.INotifyPropertyChanged"/> on every change.
    /// </para>
    /// <para>
    /// Dialect-dependent enabling logic is centralised in
    /// <see cref="SetDialectOptions(string)"/>, which mirrors the WinForms
    /// <c>XDialectPropertyPagePanel.EnableDialectOptions</c> implementation exactly,
    /// including calling <see cref="XPropertyPagePanelBase.NotifyDirty"/> when it
    /// unchecks checkboxes in response to a dialect change.
    /// </para>
    /// </remarks>
    internal sealed class XDialectPropertyPageViewModel : XPropertyPageViewModel
    {
        // =========================================================================================
        // Backing fields — VO options (all dialects)
        // =========================================================================================

        private bool _vo1;
        private bool _vo2;
        private bool _vo3;
        private bool _vo4;
        private bool _vo8;
        private bool _vo9;
        private bool _vo10;

        // =========================================================================================
        // Backing fields — VO options (not in Core dialect)
        // =========================================================================================

        private bool _vo5;
        private bool _vo6;
        private bool _vo7;
        private bool _vo11;
        private bool _vo12;
        private bool _vo13;
        private bool _vo14;
        private bool _vo15;
        private bool _vo16;
        private bool _vo17;

        // =========================================================================================
        // Backing fields — dialect-specific options
        // =========================================================================================

        private bool _fox2;
        private bool _xpp1;

        // =========================================================================================
        // Backing fields — UI state (not persisted, not dirty-tracked)
        // =========================================================================================

        private bool _isNotCoreDialect = true;
        private bool _fox2Enabled      = false;
        private bool _xpp1Enabled      = false;

        private bool _isBinding   = false;
        private bool _isNotifying = false;

        // =========================================================================================
        // Constructor
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="XDialectPropertyPageViewModel"/> class.
        /// </summary>
        /// <param name="parentPropertyPage">The <see cref="XPropertyPage"/> that owns this panel.</param>
        public XDialectPropertyPageViewModel(XPropertyPage parentPropertyPage)
            : base(parentPropertyPage)
        {
        }

        // =========================================================================================
        // Observable Properties — VO options (all dialects)
        // =========================================================================================

        /// <summary>Gets or sets the VO1 (Allow Init/Axit) option.</summary>
        public bool Vo1  { get => _vo1;  set => SetProperty(ref _vo1,  value); }

        /// <summary>Gets or sets the VO2 (Initialize Strings) option.</summary>
        public bool Vo2  { get => _vo2;  set => SetProperty(ref _vo2,  value); }

        /// <summary>Gets or sets the VO3 (All Instance methods Virtual) option.</summary>
        public bool Vo3  { get => _vo3;  set => SetProperty(ref _vo3,  value); }

        /// <summary>Gets or sets the VO4 (Implicit numeric conversions) option.</summary>
        public bool Vo4  { get => _vo4;  set => SetProperty(ref _vo4,  value); }

        /// <summary>Gets or sets the VO8 (Compatible Preprocessor) option.</summary>
        public bool Vo8  { get => _vo8;  set => SetProperty(ref _vo8,  value); }

        /// <summary>Gets or sets the VO9 (Handle Problems with Return statements) option.</summary>
        public bool Vo9  { get => _vo9;  set => SetProperty(ref _vo9,  value); }

        /// <summary>Gets or sets the VO10 (Compatible IIF) option.</summary>
        public bool Vo10 { get => _vo10; set => SetProperty(ref _vo10, value); }

        // =========================================================================================
        // Observable Properties — VO options (not in Core dialect)
        // =========================================================================================

        /// <summary>Gets or sets the VO5 (Implicit Clipper calling convention) option. Disabled for Core dialect.</summary>
        public bool Vo5  { get => _vo5;  set => SetProperty(ref _vo5,  value); }

        /// <summary>Gets or sets the VO6 (Implicit Pointer conversions) option. Disabled for Core dialect.</summary>
        public bool Vo6  { get => _vo6;  set => SetProperty(ref _vo6,  value); }

        /// <summary>Gets or sets the VO7 (Implicit casts and conversions) option. Disabled for Core dialect.</summary>
        public bool Vo7  { get => _vo7;  set => SetProperty(ref _vo7,  value); }

        /// <summary>Gets or sets the VO11 (Compatible Numeric conversions) option. Disabled for Core dialect.</summary>
        public bool Vo11 { get => _vo11; set => SetProperty(ref _vo11, value); }

        /// <summary>Gets or sets the VO12 (Compatible integer divisions) option. Disabled for Core dialect.</summary>
        public bool Vo12 { get => _vo12; set => SetProperty(ref _vo12, value); }

        /// <summary>Gets or sets the VO13 (Compatible String Conversions) option. Disabled for Core dialect.</summary>
        public bool Vo13 { get => _vo13; set => SetProperty(ref _vo13, value); }

        /// <summary>Gets or sets the VO14 (Use FLOAT literals) option. Disabled for Core dialect.</summary>
        public bool Vo14 { get => _vo14; set => SetProperty(ref _vo14, value); }

        /// <summary>Gets or sets the VO15 (Treat missing types as usual) option. Disabled for Core dialect.</summary>
        public bool Vo15 { get => _vo15; set => SetProperty(ref _vo15, value); }

        /// <summary>Gets or sets the VO16 (Generate Clipper constructors) option. Disabled for Core dialect.</summary>
        public bool Vo16 { get => _vo16; set => SetProperty(ref _vo16, value); }

        /// <summary>Gets or sets the VO17 (Compatible Begin Sequence..End) option. Disabled for Core dialect.</summary>
        public bool Vo17 { get => _vo17; set => SetProperty(ref _vo17, value); }

        // =========================================================================================
        // Observable Properties — dialect-specific options
        // =========================================================================================

        /// <summary>Gets or sets the Fox2 (Compatible Array Handling) option. Only enabled for the FoxPro dialect.</summary>
        public bool Fox2 { get => _fox2; set => SetProperty(ref _fox2, value); }

        /// <summary>Gets or sets the Xpp1 (Inherit From Abstract class) option. Only enabled for the XPP dialect.</summary>
        public bool Xpp1 { get => _xpp1; set => SetProperty(ref _xpp1, value); }

        // =========================================================================================
        // Observable Properties — UI state (IsEnabled flags; not dirty-tracked)
        // =========================================================================================

        /// <summary>
        /// <see langword="true"/> when the current dialect is not Core;
        /// drives <c>IsEnabled</c> on the VO5–VO7 and VO11–VO17 checkboxes.
        /// </summary>
        public bool IsNotCoreDialect
        {
            get => _isNotCoreDialect;
            set => SetProperty(ref _isNotCoreDialect, value);
        }

        /// <summary>
        /// <see langword="true"/> only when the current dialect is FoxPro;
        /// drives <c>IsEnabled</c> on the Fox2 checkbox.
        /// </summary>
        public bool Fox2Enabled
        {
            get => _fox2Enabled;
            set => SetProperty(ref _fox2Enabled, value);
        }

        /// <summary>
        /// <see langword="true"/> only when the current dialect is XPP;
        /// drives <c>IsEnabled</c> on the Xpp1 checkbox.
        /// </summary>
        public bool Xpp1Enabled
        {
            get => _xpp1Enabled;
            set => SetProperty(ref _xpp1Enabled, value);
        }

        // =========================================================================================
        // String properties — captions and tooltips from DialectPropertyPagePanel
        // =========================================================================================

        public string CaptVO1  => DialectPropertyPagePanel.VO1Caption;
        public string CaptVO2  => DialectPropertyPagePanel.VO2Caption;
        public string CaptVO3  => DialectPropertyPagePanel.VO3Caption;
        public string CaptVO4  => DialectPropertyPagePanel.VO4Caption;
        public string CaptVO5  => DialectPropertyPagePanel.VO5Caption;
        public string CaptVO6  => DialectPropertyPagePanel.VO6Caption;
        public string CaptVO7  => DialectPropertyPagePanel.VO7Caption;
        public string CaptVO8  => DialectPropertyPagePanel.VO8Caption;
        public string CaptVO9  => DialectPropertyPagePanel.VO9Caption;
        public string CaptVO10 => DialectPropertyPagePanel.VO10Caption;
        public string CaptVO11 => DialectPropertyPagePanel.VO11Caption;
        public string CaptVO12 => DialectPropertyPagePanel.VO12Caption;
        public string CaptVO13 => DialectPropertyPagePanel.VO13Caption;
        public string CaptVO14 => DialectPropertyPagePanel.VO14Caption;
        public string CaptVO15 => DialectPropertyPagePanel.VO15Caption;
        public string CaptVO16 => DialectPropertyPagePanel.VO16Caption;
        public string CaptVO17 => DialectPropertyPagePanel.VO17Caption;
        public string CaptFox2 => DialectPropertyPagePanel.FOX2Caption;
        public string CaptXpp1 => DialectPropertyPagePanel.XPP1Caption;

        public string DescVO1  => DialectPropertyPagePanel.VO1Description;
        public string DescVO2  => DialectPropertyPagePanel.VO2Description;
        public string DescVO3  => DialectPropertyPagePanel.VO3Description;
        public string DescVO4  => DialectPropertyPagePanel.VO4Description;
        public string DescVO5  => DialectPropertyPagePanel.VO5Description;
        public string DescVO6  => DialectPropertyPagePanel.VO6Description;
        public string DescVO7  => DialectPropertyPagePanel.VO7Description;
        public string DescVO8  => DialectPropertyPagePanel.VO8Description;
        public string DescVO9  => DialectPropertyPagePanel.VO9Description;
        public string DescVO10 => DialectPropertyPagePanel.VO10Description;
        public string DescVO11 => DialectPropertyPagePanel.VO11Description;
        public string DescVO12 => DialectPropertyPagePanel.VO12Description;
        public string DescVO13 => DialectPropertyPagePanel.VO13Description;
        public string DescVO14 => DialectPropertyPagePanel.VO14Description;
        public string DescVO15 => DialectPropertyPagePanel.VO15Description;
        public string DescVO16 => DialectPropertyPagePanel.VO16Description;
        public string DescVO17 => DialectPropertyPagePanel.VO17Description;
        public string DescFox2 => DialectPropertyPagePanel.FOX2Description;
        public string DescXpp1 => DialectPropertyPagePanel.XPP1Description;

        public string CaptAllDialects => DialectPropertyPagePanel.CatCompatibility;
        public string CaptNotInCore   => DialectPropertyPagePanel.CatNotCore;
        public string CaptFoxCompat   => DialectPropertyPagePanel.FOXCompatibility;
        public string CaptXppCompat   => DialectPropertyPagePanel.XPPCompatibility;

        // =========================================================================================
        // IPropertyPagePanel — ViewModel lifecycle
        // =========================================================================================

        /// <inheritdoc/>
        public override void HookupEvents()
        {
            PropertyChanged += (sender, e) =>
            {
                ThreadHelper.ThrowIfNotOnUIThread();
                // UI-state properties do not represent project edits — skip dirty.
                if (e.PropertyName == nameof(IsNotCoreDialect)
                    || e.PropertyName == nameof(Fox2Enabled)
                    || e.PropertyName == nameof(Xpp1Enabled))
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
                // ---- All-dialect checkboxes ----
                Vo1  = GetBoolPropertyValue(XSharpProjectFileConstants.Vo1);
                Vo2  = GetBoolPropertyValue(XSharpProjectFileConstants.Vo2);
                Vo3  = GetBoolPropertyValue(XSharpProjectFileConstants.Vo3);
                Vo4  = GetBoolPropertyValue(XSharpProjectFileConstants.Vo4);
                Vo8  = GetBoolPropertyValue(XSharpProjectFileConstants.Vo8);
                Vo9  = GetBoolPropertyValue(XSharpProjectFileConstants.Vo9);
                Vo10 = GetBoolPropertyValue(XSharpProjectFileConstants.Vo10);

                // ---- Non-Core checkboxes ----
                Vo5  = GetBoolPropertyValue(XSharpProjectFileConstants.Vo5);
                Vo6  = GetBoolPropertyValue(XSharpProjectFileConstants.Vo6);
                Vo7  = GetBoolPropertyValue(XSharpProjectFileConstants.Vo7);
                Vo11 = GetBoolPropertyValue(XSharpProjectFileConstants.Vo11);
                Vo12 = GetBoolPropertyValue(XSharpProjectFileConstants.Vo12);
                Vo13 = GetBoolPropertyValue(XSharpProjectFileConstants.Vo13);
                Vo14 = GetBoolPropertyValue(XSharpProjectFileConstants.Vo14);
                Vo15 = GetBoolPropertyValue(XSharpProjectFileConstants.Vo15);
                Vo16 = GetBoolPropertyValue(XSharpProjectFileConstants.Vo16);
                Vo17 = GetBoolPropertyValue(XSharpProjectFileConstants.Vo17);

                // ---- Dialect-specific checkboxes ----
                Fox2 = GetBoolPropertyValue(XSharpProjectFileConstants.Fox2);
                Xpp1 = GetBoolPropertyValue(XSharpProjectFileConstants.Xpp1);

                // ---- Dialect-dependent enabling ----
                SetDialectOptions(parentPropertyPage.GetProperty(XSharpProjectFileConstants.Dialect) ?? "Core");

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

            SetBoolPropertyValue(XSharpProjectFileConstants.Vo1,  Vo1);
            SetBoolPropertyValue(XSharpProjectFileConstants.Vo2,  Vo2);
            SetBoolPropertyValue(XSharpProjectFileConstants.Vo3,  Vo3);
            SetBoolPropertyValue(XSharpProjectFileConstants.Vo4,  Vo4);
            SetBoolPropertyValue(XSharpProjectFileConstants.Vo5,  Vo5);
            SetBoolPropertyValue(XSharpProjectFileConstants.Vo6,  Vo6);
            SetBoolPropertyValue(XSharpProjectFileConstants.Vo7,  Vo7);
            SetBoolPropertyValue(XSharpProjectFileConstants.Vo8,  Vo8);
            SetBoolPropertyValue(XSharpProjectFileConstants.Vo9,  Vo9);
            SetBoolPropertyValue(XSharpProjectFileConstants.Vo10, Vo10);
            SetBoolPropertyValue(XSharpProjectFileConstants.Vo11, Vo11);
            SetBoolPropertyValue(XSharpProjectFileConstants.Vo12, Vo12);
            SetBoolPropertyValue(XSharpProjectFileConstants.Vo13, Vo13);
            SetBoolPropertyValue(XSharpProjectFileConstants.Vo14, Vo14);
            SetBoolPropertyValue(XSharpProjectFileConstants.Vo15, Vo15);
            SetBoolPropertyValue(XSharpProjectFileConstants.Vo16, Vo16);
            SetBoolPropertyValue(XSharpProjectFileConstants.Vo17, Vo17);
            SetBoolPropertyValue(XSharpProjectFileConstants.Fox2, Fox2);
            SetBoolPropertyValue(XSharpProjectFileConstants.Xpp1, Xpp1);

            isDirty = false;
        }

        // =========================================================================================
        // Dialect-dependent enabling — mirrors XDialectPropertyPagePanel.EnableDialectOptions
        // =========================================================================================

        /// <summary>
        /// Adjusts which checkboxes are enabled and sets dialect-specific defaults,
        /// mirroring the WinForms <c>XDialectPropertyPagePanel.EnableDialectOptions</c> logic.
        /// </summary>
        /// <param name="dialect">The currently selected dialect name (e.g. <c>"Core"</c>, <c>"FoxPro"</c>).</param>
        public void SetDialectOptions(string dialect)
        {
            ThreadHelper.ThrowIfNotOnUIThread();

            bool isCore   = string.Equals(dialect, "core",   System.StringComparison.OrdinalIgnoreCase);
            bool isFoxPro = string.Equals(dialect, "foxpro", System.StringComparison.OrdinalIgnoreCase);
            bool isXpp    = string.Equals(dialect, "xpp",    System.StringComparison.OrdinalIgnoreCase);

            IsNotCoreDialect = !isCore;
            Fox2Enabled      = isFoxPro;
            Xpp1Enabled      = isXpp;

            if (isCore)
            {
                // Uncheck options not valid for Core and mark dirty (mirrors WinForms behaviour).
                Vo5  = false;
                Vo6  = false;
                Vo7  = false;
                Vo11 = false;
                Vo12 = false;
                Vo13 = false;
                Vo14 = false;
                Vo15 = false;
                Vo16 = false;
                Vo17 = false;
                Xpp1 = false;
                NotifyDirty();
            }

            if (!isFoxPro)
                Fox2 = false;

            if (!isXpp)
                Xpp1 = false;

            // Auto-check Fox2 when switching to FoxPro (mirrors WinForms behaviour).
            if (isFoxPro)
                Fox2 = true;
        }
    }
}
