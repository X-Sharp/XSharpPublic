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
    /// WinForms/WPF bridge host for the General (Application) property page.
    /// Used for SDK-style XSharp projects.
    /// </summary>
    /// <remarks>
    /// <para>
    /// This class extends <see cref="XPropertyPageXamlHost"/>, which wraps an
    /// <c>ElementHost</c> inside a WinForms <c>UserControl</c> so that Visual Studio's
    /// COM property-sheet frame can host WPF content.
    /// </para>
    /// <para>
    /// The WPF content is a <see cref="XGeneralPropertyPageView"/> (<c>UserControl</c>)
    /// whose <c>DataContext</c> is a <see cref="XGeneralPropertyPageViewModel"/> that
    /// reads from and writes back to the MSBuild project via the owning
    /// <see cref="XSharpGeneralPropertyPage"/>.
    /// </para>
    /// </remarks>
    internal sealed class XGeneralPropertyPageXamlHost : XPropertyPageXamlHost
    {
        // =========================================================================================
        // Member Variables
        // =========================================================================================

        /// <summary>The ViewModel that backs the WPF UI.</summary>
        private readonly XGeneralPropertyPageViewModel _viewModel;

        // =========================================================================================
        // Constructor
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="XGeneralPropertyPageXamlHost"/> class.
        /// </summary>
        /// <param name="parentPropertyPage">
        /// The <see cref="XPropertyPage"/> that owns this panel.
        /// </param>
        public XGeneralPropertyPageXamlHost(XPropertyPage parentPropertyPage)
            : base(parentPropertyPage)
        {
            _viewModel = new XGeneralPropertyPageViewModel(parentPropertyPage);
            SetViewModel(_viewModel);
        }

        // =========================================================================================
        // XPropertyPageXamlHost — Abstract overrides
        // =========================================================================================

        /// <summary>
        /// Creates the WPF <see cref="FrameworkElement"/> (the
        /// <see cref="XGeneralPropertyPageView"/> <c>UserControl</c>) that will be
        /// hosted inside the <c>ElementHost</c>.
        /// </summary>
        /// <returns>A new <see cref="XGeneralPropertyPageView"/> instance.</returns>
        protected override FrameworkElement CreateXamlContent()
            => new XGeneralPropertyPageView();

        // =========================================================================================
        // IPropertyPagePanel — Lifecycle  (delegates to ViewModel)
        // =========================================================================================

        /// <summary>
        /// Wires up change-notification handlers in the ViewModel so that edits propagate
        /// the dirty flag to the owning <see cref="XPropertyPage"/>.
        /// </summary>
        public override void HookupEvents()
        {
            Microsoft.VisualStudio.Shell.ThreadHelper.ThrowIfNotOnUIThread();
            _viewModel.HookupEvents();
        }

        /// <summary>
        /// Reads all project properties from MSBuild and populates the ViewModel's
        /// observable properties so that the bound XAML controls display them.
        /// </summary>
        public override void BindProperties()
        {
            Microsoft.VisualStudio.Shell.ThreadHelper.ThrowIfNotOnUIThread();
            _viewModel.BindProperties();
        }

        /// <summary>
        /// Reads all current ViewModel values and writes them back to the MSBuild project.
        /// Called when the user clicks <em>Apply</em> or <em>OK</em>.
        /// </summary>
        protected override void ApplyChangesCore()
        {
            Microsoft.VisualStudio.Shell.ThreadHelper.ThrowIfNotOnUIThread();
            _viewModel.ApplyChanges();
        }
    }
}
