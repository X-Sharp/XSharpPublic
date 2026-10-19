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
    /// MVVM ViewModel that backs the XAML/WPF Language property page for
    /// SDK-style XSharp projects.
    /// </summary>
    /// <remarks>
    /// <para>
    /// Each MSBuild property shown on the Language page is exposed as an observable
    /// property backed by <see cref="XPropertyPageViewModel.SetProperty{T}"/> so that
    /// WPF data binding automatically triggers
    /// <see cref="System.ComponentModel.INotifyPropertyChanged"/> on every change.
    /// </para>
    /// <para>
    /// Dialect-dependent enabling logic is centralised in
    /// <see cref="SetDialectOptions(string)"/>, which mirrors the WinForms
    /// <c>XLanguagePropertyPagePanel.SetDialectOptions</c> implementation exactly,
    /// including calling <see cref="XPropertyPagePanelBase.NotifyDirty"/> when it
    /// modifies checkbox values in response to a dialect change.
    /// </para>
    /// </remarks>
    internal sealed class XLanguagePropertyPageViewModel : XPropertyPageViewModel
    {
        // =========================================================================================
        // Backing fields — checkboxes
        // =========================================================================================

        private bool _az;
        private bool _cs;
        private bool _ins;
        private bool _initLocals;
        private bool _lb;
        private bool _memVar;
        private bool _namedArgs;
        private bool _noStandardDefs;
        private bool _ns;
        private bool _ovf;
        private bool _undeclared;
        private bool _unsafe;
        private bool _enforceSelf;
        private bool _enforceOverride;
        private bool _allowDot;
        private bool _allowOldStyleAssignments;
        private bool _modernSyntax;

        // =========================================================================================
        // Backing fields — text fields
        // =========================================================================================

        private string _includePaths;
        private string _standardDefs;

        // =========================================================================================
        // Backing fields — UI state (not persisted, not dirty-tracked)
        // =========================================================================================

        private bool _lbEnabled        = true;
        private bool _memVarEnabled     = true;
        private bool _undeclaredEnabled = false;

        // Guards against re-entrancy during load and notification pulse.
        private bool _isBinding   = false;
        private bool _isNotifying = false;

        // =========================================================================================
        // Constructor
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="XLanguagePropertyPageViewModel"/> class.
        /// </summary>
        /// <param name="parentPropertyPage">The <see cref="XPropertyPage"/> that owns this panel.</param>
        public XLanguagePropertyPageViewModel(XPropertyPage parentPropertyPage)
            : base(parentPropertyPage)
        {
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
        // Observable Properties — checkboxes
        // =========================================================================================

        /// <summary>Gets or sets whether zero-based arrays are used (<c>/az</c>).</summary>
        public bool AZ
        {
            get => _az;
            set => SetProperty(ref _az, value);
        }

        /// <summary>Gets or sets whether the compiler is case-sensitive (<c>/cs</c>).</summary>
        public bool CS
        {
            get => _cs;
            set => SetProperty(ref _cs, value);
        }

        /// <summary>Gets or sets whether implicit namespace lookup is enabled (<c>/ins</c>).</summary>
        public bool INS
        {
            get => _ins;
            set => SetProperty(ref _ins, value);
        }

        /// <summary>Gets or sets whether local variables are initialised to their default value (<c>/initlocals</c>).</summary>
        public bool InitLocals
        {
            get => _initLocals;
            set => SetProperty(ref _initLocals, value);
        }

        /// <summary>Gets or sets whether late binding is allowed (<c>/lb</c>).  Disabled for the Core dialect.</summary>
        public bool LB
        {
            get => _lb;
            set => SetProperty(ref _lb, value);
        }

        /// <summary>Gets or sets whether memory variables (MEMVARs) are enabled (<c>/memvar</c>).  Disabled for the Core dialect.</summary>
        public bool MemVar
        {
            get => _memVar;
            set => SetProperty(ref _memVar, value);
        }

        /// <summary>Gets or sets whether named arguments are allowed (<c>/namedargs</c>).</summary>
        public bool NamedArgs
        {
            get => _namedArgs;
            set => SetProperty(ref _namedArgs, value);
        }

        /// <summary>Gets or sets whether the standard header file is suppressed (<c>/nostddefs</c>).</summary>
        public bool NoStandardDefs
        {
            get => _noStandardDefs;
            set => SetProperty(ref _noStandardDefs, value);
        }

        /// <summary>Gets or sets whether classes are prefixed with their namespace (<c>/ns</c>).</summary>
        public bool NS
        {
            get => _ns;
            set => SetProperty(ref _ns, value);
        }

        /// <summary>Gets or sets whether integer overflow exceptions are enabled (<c>/ovf</c>).</summary>
        public bool OVF
        {
            get => _ovf;
            set => SetProperty(ref _ovf, value);
        }

        /// <summary>Gets or sets whether undeclared variables are allowed (<c>/undeclared</c>).  Only enabled when <see cref="MemVar"/> is <see langword="true"/>.</summary>
        public bool Undeclared
        {
            get => _undeclared;
            set => SetProperty(ref _undeclared, value);
        }

        /// <summary>Gets or sets whether unsafe code is allowed (<c>/unsafe</c>).</summary>
        public bool Unsafe
        {
            get => _unsafe;
            set => SetProperty(ref _unsafe, value);
        }

        /// <summary>Gets or sets whether SELF is enforced on instance member access (<c>/enforceself</c>).</summary>
        public bool EnforceSelf
        {
            get => _enforceSelf;
            set => SetProperty(ref _enforceSelf, value);
        }

        /// <summary>Gets or sets whether VIRTUAL / OVERRIDE keywords are enforced (<c>/enforceoverride</c>).</summary>
        public bool EnforceOverride
        {
            get => _enforceOverride;
            set => SetProperty(ref _enforceOverride, value);
        }

        /// <summary>Gets or sets whether the dot operator is allowed for instance member access (<c>/allowdot</c>).</summary>
        public bool AllowDot
        {
            get => _allowDot;
            set => SetProperty(ref _allowDot, value);
        }

        /// <summary>Gets or sets whether old-style assignments (<c>:=</c>) are allowed (<c>/allowoldstyleassignments</c>).</summary>
        public bool AllowOldStyleAssignments
        {
            get => _allowOldStyleAssignments;
            set => SetProperty(ref _allowOldStyleAssignments, value);
        }

        /// <summary>Gets or sets whether modern syntax is used (<c>/modernsyntax</c>).</summary>
        public bool ModernSyntax
        {
            get => _modernSyntax;
            set => SetProperty(ref _modernSyntax, value);
        }

        // =========================================================================================
        // Observable Properties — text fields
        // =========================================================================================

        /// <summary>Gets or sets the additional include paths (<c>/i</c>).</summary>
        public string IncludePaths
        {
            get => _includePaths;
            set => SetProperty(ref _includePaths, value);
        }

        /// <summary>Gets or sets the alternate standard header file (<c>/stddefs</c>).</summary>
        public string StandardDefs
        {
            get => _standardDefs;
            set => SetProperty(ref _standardDefs, value);
        }

        // =========================================================================================
        // Observable Properties — UI state (IsEnabled flags; not dirty-tracked)
        // =========================================================================================

        /// <summary>
        /// Gets or sets whether the Late Binding checkbox is enabled.
        /// <see langword="false"/> for the Core dialect.
        /// </summary>
        public bool LBEnabled
        {
            get => _lbEnabled;
            set => SetProperty(ref _lbEnabled, value);
        }

        /// <summary>
        /// Gets or sets whether the MemVar checkbox is enabled.
        /// <see langword="false"/> for the Core dialect.
        /// </summary>
        public bool MemVarEnabled
        {
            get => _memVarEnabled;
            set => SetProperty(ref _memVarEnabled, value);
        }

        /// <summary>
        /// Gets or sets whether the Undeclared Variables checkbox is enabled.
        /// Only <see langword="true"/> when <see cref="MemVar"/> is <see langword="true"/>.
        /// </summary>
        public bool UndeclaredEnabled
        {
            get => _undeclaredEnabled;
            set => SetProperty(ref _undeclaredEnabled, value);
        }

        // =========================================================================================
        // String properties — labels and tooltips from LanguagePropertyPagePanel
        // =========================================================================================

        /// <summary>Gets the localized label text for the Late Binding checkbox.</summary>
        public string CaptLB                    => LanguagePropertyPagePanel.LBCaption;

        /// <summary>Gets the localized label text for the Named Arguments checkbox.</summary>
        public string CaptNamedArgs             => LanguagePropertyPagePanel.NamedArgCaption;

        /// <summary>Gets the localized label text for the Unsafe Code checkbox.</summary>
        public string CaptUnsafe                => LanguagePropertyPagePanel.UnsafeCaption;

        /// <summary>Gets the localized label text for the Case Sensitive checkbox.</summary>
        public string CaptCS                    => LanguagePropertyPagePanel.CSCaption;

        /// <summary>Gets the localized label text for the Initialize Locals checkbox.</summary>
        public string CaptInitLocals            => LanguagePropertyPagePanel.InitLocalsCaption;

        /// <summary>Gets the localized label text for the Overflow Exceptions checkbox.</summary>
        public string CaptOVF                   => LanguagePropertyPagePanel.OVFCaption;

        /// <summary>Gets the localized label text for the Zero Based Arrays checkbox.</summary>
        public string CaptAZ                    => LanguagePropertyPagePanel.AZCaption;

        /// <summary>Gets the localized label text for the Enforce SELF checkbox.</summary>
        public string CaptEnforceSelf           => LanguagePropertyPagePanel.enforceSelfCaption;

        /// <summary>Gets the localized label text for the Allow Dot checkbox.</summary>
        public string CaptAllowDot              => LanguagePropertyPagePanel.allowDotCaption;

        /// <summary>Gets the localized label text for the Enforce Override checkbox.</summary>
        public string CaptEnforceOverride       => LanguagePropertyPagePanel.EnforceOverrideCaption;

        /// <summary>Gets the localized label text for the Old Style Assignments checkbox.</summary>
        public string CaptOldStyleAssignments   => LanguagePropertyPagePanel.allowOldStyleCaption;

        /// <summary>Gets the localized label text for the Modern Syntax checkbox.</summary>
        public string CaptModernSyntax          => LanguagePropertyPagePanel.ModernSyntaxCaption;

        /// <summary>Gets the localized label text for the Enable Memvars checkbox.</summary>
        public string CaptMemVar                => LanguagePropertyPagePanel.MemVarCaption;

        /// <summary>Gets the localized label text for the Undeclared Variables checkbox.</summary>
        public string CaptUndeclared            => LanguagePropertyPagePanel.UndeclaredCaption;

        /// <summary>Gets the localized label text for the Implicit Namespace Lookup checkbox.</summary>
        public string CaptINS                   => LanguagePropertyPagePanel.INSCaption;

        /// <summary>Gets the localized label text for the Prefix Classes checkbox.</summary>
        public string CaptNS                    => LanguagePropertyPagePanel.NSCaption;

        /// <summary>Gets the localized label text for the Suppress Standard Header checkbox.</summary>
        public string CaptNoStandardDefs        => LanguagePropertyPagePanel.NoStdDefCaption;

        /// <summary>Gets the localized label text for the Include Paths field.</summary>
        public string CaptIncludePaths          => LanguagePropertyPagePanel.INCCaption;

        /// <summary>Gets the localized label text for the Standard Defs field.</summary>
        public string CaptStandardDefs          => LanguagePropertyPagePanel.StdDefCaption;

        /// <summary>Gets the localized tooltip for the Late Binding checkbox.</summary>
        public string DescLB                    => LanguagePropertyPagePanel.LBDescription;

        /// <summary>Gets the localized tooltip for the Named Arguments checkbox.</summary>
        public string DescNamedArgs             => LanguagePropertyPagePanel.NamedArgDescription;

        /// <summary>Gets the localized tooltip for the Unsafe Code checkbox.</summary>
        public string DescUnsafe                => LanguagePropertyPagePanel.UnsafeDescription;

        /// <summary>Gets the localized tooltip for the Case Sensitive checkbox.</summary>
        public string DescCS                    => LanguagePropertyPagePanel.CSDescription;

        /// <summary>Gets the localized tooltip for the Initialize Locals checkbox.</summary>
        public string DescInitLocals            => LanguagePropertyPagePanel.InitLocalsDescription;

        /// <summary>Gets the localized tooltip for the Overflow Exceptions checkbox.</summary>
        public string DescOVF                   => LanguagePropertyPagePanel.OVFDescription;

        /// <summary>Gets the localized tooltip for the Zero Based Arrays checkbox.</summary>
        public string DescAZ                    => LanguagePropertyPagePanel.AZDescription;

        /// <summary>Gets the localized tooltip for the Enforce SELF checkbox.</summary>
        public string DescEnforceSelf           => LanguagePropertyPagePanel.EnforceSelfDescription;

        /// <summary>Gets the localized tooltip for the Allow Dot checkbox.</summary>
        public string DescAllowDot              => LanguagePropertyPagePanel.allowDotDescription;

        /// <summary>Gets the localized tooltip for the Enforce Override checkbox.</summary>
        public string DescEnforceOverride       => LanguagePropertyPagePanel.EnforceOverrideDescription;

        /// <summary>Gets the localized tooltip for the Old Style Assignments checkbox.</summary>
        public string DescOldStyleAssignments   => LanguagePropertyPagePanel.allowOldStyleDescription;

        /// <summary>Gets the localized tooltip for the Modern Syntax checkbox.</summary>
        public string DescModernSyntax          => LanguagePropertyPagePanel.ModernSyntaxDescription;

        /// <summary>Gets the localized tooltip for the Enable Memvars checkbox.</summary>
        public string DescMemVar                => LanguagePropertyPagePanel.MemVarDescription;

        /// <summary>Gets the localized tooltip for the Undeclared Variables checkbox.</summary>
        public string DescUndeclared            => LanguagePropertyPagePanel.UndeclaredDescription;

        /// <summary>Gets the localized tooltip for the Implicit Namespace Lookup checkbox.</summary>
        public string DescINS                   => LanguagePropertyPagePanel.INSDescription;

        /// <summary>Gets the localized tooltip for the Prefix Classes checkbox.</summary>
        public string DescNS                    => LanguagePropertyPagePanel.NSDescription;

        /// <summary>Gets the localized tooltip for the Suppress Standard Header checkbox.</summary>
        public string DescNoStandardDefs        => LanguagePropertyPagePanel.NoStdDefDescription;

        /// <summary>Gets the localized tooltip for the Include Paths field.</summary>
        public string DescIncludePaths          => LanguagePropertyPagePanel.INCDescription;

        /// <summary>Gets the localized tooltip for the Standard Defs field.</summary>
        public string DescStandardDefs          => LanguagePropertyPagePanel.StdDefDescription;

        // =========================================================================================
        // IPropertyPagePanel — ViewModel lifecycle
        // =========================================================================================

        /// <inheritdoc/>
        public override void HookupEvents()
        {
            PropertyChanged += (sender, e) =>
            {
                ThreadHelper.ThrowIfNotOnUIThread();
                // Skip re-entrant calls during load or notification pulse.
                if (_isBinding || _isNotifying)
                    return;

                // UI-state properties do not represent project edits — skip dirty.
                if (e.PropertyName == nameof(LBEnabled)
                    || e.PropertyName == nameof(MemVarEnabled)
                    || e.PropertyName == nameof(UndeclaredEnabled))
                    return;

                // When MemVar changes, re-evaluate UndeclaredEnabled.
                if (e.PropertyName == nameof(MemVar))
                    UndeclaredEnabled = MemVar;

                NotifyDirty();

                // Pulse Item[] so all indexer-bound Reset buttons re-evaluate.
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
            // ---- Checkboxes ----
            AZ                     = GetBoolPropertyValue(XSharpProjectFileConstants.AZ);
            CS                     = GetBoolPropertyValue(XSharpProjectFileConstants.CS);
            INS                    = GetBoolPropertyValue(XSharpProjectFileConstants.INS);
            InitLocals             = GetBoolPropertyValue(XSharpProjectFileConstants.InitLocals);
            LB                     = GetBoolPropertyValue(XSharpProjectFileConstants.LB);
            MemVar                 = GetBoolPropertyValue(XSharpProjectFileConstants.MemVar);
            NamedArgs              = GetBoolPropertyValue(XSharpProjectFileConstants.NamedArgs);
            NoStandardDefs         = GetBoolPropertyValue(XSharpProjectFileConstants.NoStandardDefs);
            NS                     = GetBoolPropertyValue(XSharpProjectFileConstants.NS);
            OVF                    = GetBoolPropertyValue(XSharpProjectFileConstants.OVF);
            Undeclared             = GetBoolPropertyValue(XSharpProjectFileConstants.Undeclared);
            Unsafe                 = GetBoolPropertyValue(XSharpProjectFileConstants.Unsafe);
            EnforceSelf            = GetBoolPropertyValue(XSharpProjectFileConstants.EnforceSelf);
            EnforceOverride        = GetBoolPropertyValue(XSharpProjectFileConstants.EnforceOverride);
            AllowDot               = GetBoolPropertyValue(XSharpProjectFileConstants.Allowdot);
            AllowOldStyleAssignments = GetBoolPropertyValue(XSharpProjectFileConstants.AllowOldStyleAssignments);
            ModernSyntax           = GetBoolPropertyValue(XSharpProjectFileConstants.ModernSyntax);

            // ---- Text fields ----
            IncludePaths  = parentPropertyPage.GetProperty(XSharpProjectFileConstants.IncludePaths)  ?? string.Empty;
            StandardDefs  = parentPropertyPage.GetProperty(XSharpProjectFileConstants.StandardDefs)  ?? string.Empty;

            // ---- Dialect-dependent enabling + defaults ----
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

            SetBoolPropertyValue(XSharpProjectFileConstants.AZ,                       AZ);
            SetBoolPropertyValue(XSharpProjectFileConstants.CS,                       CS);
            SetBoolPropertyValue(XSharpProjectFileConstants.INS,                      INS);
            SetBoolPropertyValue(XSharpProjectFileConstants.InitLocals,               InitLocals);
            SetBoolPropertyValue(XSharpProjectFileConstants.LB,                       LB);
            SetBoolPropertyValue(XSharpProjectFileConstants.MemVar,                   MemVar);
            SetBoolPropertyValue(XSharpProjectFileConstants.NamedArgs,                NamedArgs);
            SetBoolPropertyValue(XSharpProjectFileConstants.NoStandardDefs,           NoStandardDefs);
            SetBoolPropertyValue(XSharpProjectFileConstants.NS,                       NS);
            SetBoolPropertyValue(XSharpProjectFileConstants.OVF,                      OVF);
            SetBoolPropertyValue(XSharpProjectFileConstants.Undeclared,               Undeclared);
            SetBoolPropertyValue(XSharpProjectFileConstants.Unsafe,                   Unsafe);
            SetBoolPropertyValue(XSharpProjectFileConstants.EnforceSelf,              EnforceSelf);
            SetBoolPropertyValue(XSharpProjectFileConstants.EnforceOverride,          EnforceOverride);
            SetBoolPropertyValue(XSharpProjectFileConstants.Allowdot,                AllowDot);
            SetBoolPropertyValue(XSharpProjectFileConstants.AllowOldStyleAssignments, AllowOldStyleAssignments);
            SetBoolPropertyValue(XSharpProjectFileConstants.ModernSyntax,             ModernSyntax);

            parentPropertyPage.SetProperty(XSharpProjectFileConstants.IncludePaths, IncludePaths ?? string.Empty);
            parentPropertyPage.SetProperty(XSharpProjectFileConstants.StandardDefs, StandardDefs ?? string.Empty);

            isDirty = false;
        }

        // =========================================================================================
        // Dialect-dependent enabling — mirrors XLanguagePropertyPagePanel.SetDialectOptions
        // =========================================================================================

        /// <summary>
        /// Adjusts which checkboxes are enabled and sets dialect-specific defaults,
        /// mirroring the WinForms <c>XLanguagePropertyPagePanel.SetDialectOptions</c> logic.
        /// </summary>
        /// <param name="dialect">The currently selected dialect name (e.g. <c>"Core"</c>, <c>"FoxPro"</c>).</param>
        /// <remarks>
        /// When the dialect changes to Core, checkboxes that are not valid for Core
        /// are disabled and unchecked, and <see cref="NotifyDirty"/> is called to
        /// inform the property page that the project has changed — matching the
        /// WinForms behaviour of setting <c>IsDirty = true</c> explicitly.
        /// </remarks>
        public void SetDialectOptions(string dialect)
        {
            ThreadHelper.ThrowIfNotOnUIThread();

            bool isCore   = string.Equals(dialect, Dialect.Core.ToString(),   System.StringComparison.OrdinalIgnoreCase);
            bool isFoxPro = string.Equals(dialect, Dialect.FoxPro.ToString(), System.StringComparison.OrdinalIgnoreCase);

            if (isCore)
            {
                // Disable and uncheck options not valid for Core.
                LBEnabled        = false;
                MemVarEnabled    = false;
                UndeclaredEnabled = false;
                LB               = false;
                MemVar           = false;
                Undeclared       = false;
                NotifyDirty();
            }
            else
            {
                LBEnabled     = true;
                MemVarEnabled = true;
                // Undeclared follows MemVar regardless of dialect.
                UndeclaredEnabled = MemVar;
            }

            // AllowDot default: TRUE for Core or FoxPro if not already set in the project.
            var strAllowDot = parentPropertyPage.GetProperty(XSharpProjectFileConstants.Allowdot);
            if (string.IsNullOrEmpty(strAllowDot) && (isCore || isFoxPro))
            {
                parentPropertyPage.SetProperty(XSharpProjectFileConstants.Allowdot, "True");
                AllowDot = true;
            }

            // AllowOldStyleAssignments default: TRUE for FoxPro if not already set.
            var strAllowOldStyle = parentPropertyPage.GetProperty(XSharpProjectFileConstants.AllowOldStyleAssignments);
            if (string.IsNullOrEmpty(strAllowOldStyle) && isFoxPro)
            {
                parentPropertyPage.SetProperty(XSharpProjectFileConstants.AllowOldStyleAssignments, "True");
                AllowOldStyleAssignments = true;
            }

            // NamedArgs default: TRUE for Core, FALSE for others, if not already set.
            var strNamedArgs = parentPropertyPage.GetProperty(XSharpProjectFileConstants.NamedArgs);
            if (string.IsNullOrEmpty(strNamedArgs))
            {
                parentPropertyPage.SetProperty(XSharpProjectFileConstants.NamedArgs, isCore.ToString());
                NamedArgs = isCore;
            }
        }
    }
}
