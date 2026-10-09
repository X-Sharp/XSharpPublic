//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using Microsoft.VisualStudio.Project;
using System.Windows;

namespace XSharp.Project
{
    /// <summary>
    /// WinForms/WPF bridge host for the Language property page.
    /// Used for SDK-style XSharp projects.
    /// </summary>
    /// <remarks>
    /// <para>
    /// This class extends <see cref="XPropertyPageXamlHost"/>, which wraps an
    /// <c>ElementHost</c> inside a WinForms <c>UserControl</c> so that Visual Studio's
    /// COM property-sheet frame can host WPF content.
    /// </para>
    /// <para>
    /// The WPF content is a <see cref="XLanguagePropertyPageView"/> (<c>UserControl</c>)
    /// whose <c>DataContext</c> is an <see cref="XLanguagePropertyPageViewModel"/> that
    /// reads from and writes back to the MSBuild project via the owning
    /// <see cref="XSharpLanguagePropertyPage"/>.
    /// </para>
    /// <para>
    /// When the General page changes the <c>Dialect</c> property,
    /// <see cref="XSharpLanguagePropertyPage.Project_OnProjectPropertyChanged"/> calls
    /// <see cref="NotifyDialectChanged"/> so the ViewModel can re-apply dialect-specific
    /// enabling logic — mirroring the WinForms path that called
    /// <c>XLanguagePropertyPagePanel.Project_OnProjectPropertyChanged</c> directly.
    /// </para>
    /// </remarks>
    internal sealed class XLanguagePropertyPageXamlHost : XPropertyPageXamlHost
    {
        // =========================================================================================
        // Member Variables
        // =========================================================================================

        /// <summary>The ViewModel that backs the WPF UI.</summary>
        private readonly XLanguagePropertyPageViewModel _viewModel;

        // =========================================================================================
        // Constructor
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="XLanguagePropertyPageXamlHost"/> class.
        /// </summary>
        /// <param name="parentPropertyPage">
        /// The <see cref="XPropertyPage"/> that owns this panel.
        /// </param>
        public XLanguagePropertyPageXamlHost(XPropertyPage parentPropertyPage)
            : base(parentPropertyPage)
        {
            _viewModel = new XLanguagePropertyPageViewModel(parentPropertyPage);
            SetViewModel(_viewModel);
        }

        // =========================================================================================
        // XPropertyPageXamlHost — Abstract overrides
        // =========================================================================================

        /// <summary>
        /// Creates the WPF <see cref="FrameworkElement"/> (the
        /// <see cref="XLanguagePropertyPageView"/> <c>UserControl</c>) that will be
        /// hosted inside the <c>ElementHost</c>.
        /// </summary>
        protected override FrameworkElement CreateXamlContent()
            => new XLanguagePropertyPageView();

        // =========================================================================================
        // IPropertyPagePanel — Lifecycle  (delegates to ViewModel)
        // =========================================================================================

        /// <inheritdoc/>
        public override void HookupEvents()
        {
            Microsoft.VisualStudio.Shell.ThreadHelper.ThrowIfNotOnUIThread();
            _viewModel.HookupEvents();
        }

        /// <inheritdoc/>
        public override void BindProperties()
        {
            Microsoft.VisualStudio.Shell.ThreadHelper.ThrowIfNotOnUIThread();
            _viewModel.BindProperties();
        }

        /// <inheritdoc/>
        protected override void ApplyChangesCore()
        {
            Microsoft.VisualStudio.Shell.ThreadHelper.ThrowIfNotOnUIThread();
            _viewModel.ApplyChanges();
        }

        // =========================================================================================
        // Dialect change notification
        // =========================================================================================

        /// <summary>
        /// Called by <see cref="XSharpLanguagePropertyPage.Project_OnProjectPropertyChanged"/>
        /// when the <c>Dialect</c> property changes on the General page.
        /// Delegates to <see cref="XLanguagePropertyPageViewModel.SetDialectOptions"/> so the
        /// ViewModel can enable/disable the appropriate checkboxes and apply dialect defaults —
        /// matching the WinForms behaviour exactly.
        /// </summary>
        /// <param name="newDialect">The new dialect name (e.g. <c>"Core"</c>, <c>"FoxPro"</c>).</param>
        public void NotifyDialectChanged(string newDialect)
        {
            Microsoft.VisualStudio.Shell.ThreadHelper.ThrowIfNotOnUIThread();
            _viewModel.SetDialectOptions(newDialect);
        }
    }
}
