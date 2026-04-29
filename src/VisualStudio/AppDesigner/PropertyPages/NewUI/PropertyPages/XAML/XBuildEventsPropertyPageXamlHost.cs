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
    /// WinForms/WPF bridge host for the Build Events property page.
    /// Used for SDK-style XSharp projects.
    /// </summary>
    /// <remarks>
    /// <para>
    /// This class extends <see cref="XPropertyPageXamlHost"/>, which wraps an
    /// <c>ElementHost</c> inside a WinForms <c>UserControl</c> so that Visual Studio's
    /// COM property-sheet frame can host WPF content.
    /// </para>
    /// <para>
    /// The WPF content is a <see cref="XBuildEventsPropertyPageView"/> (<c>UserControl</c>)
    /// whose <c>DataContext</c> is an <see cref="XBuildEventsPropertyPageViewModel"/> that
    /// reads from and writes back to the MSBuild project via the owning
    /// <see cref="XSharpBuildEventsPropertyPage"/>.
    /// </para>
    /// </remarks>
    internal sealed class XBuildEventsPropertyPageXamlHost : XPropertyPageXamlHost
    {
        // =========================================================================================
        // Member Variables
        // =========================================================================================

        /// <summary>The ViewModel that backs the WPF UI.</summary>
        private readonly XBuildEventsPropertyPageViewModel _viewModel;

        // =========================================================================================
        // Constructor
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="XBuildEventsPropertyPageXamlHost"/> class.
        /// </summary>
        /// <param name="parentPropertyPage">The <see cref="XPropertyPage"/> that owns this panel.</param>
        /// <param name="runPostBuildNames">
        /// The three human-readable display strings for <c>RunPostBuildEvent</c>,
        /// forwarded from <see cref="XSharpBuildEventsPropertyPage"/>.
        /// </param>
        public XBuildEventsPropertyPageXamlHost(XPropertyPage parentPropertyPage, string[] runPostBuildNames)
            : base(parentPropertyPage)
        {
            _viewModel = new XBuildEventsPropertyPageViewModel(parentPropertyPage, runPostBuildNames);
            SetViewModel(_viewModel);
        }

        // =========================================================================================
        // XPropertyPageXamlHost — Abstract overrides
        // =========================================================================================

        /// <summary>
        /// Creates the WPF <see cref="FrameworkElement"/> (the
        /// <see cref="XBuildEventsPropertyPageView"/> <c>UserControl</c>) that will be
        /// hosted inside the <c>ElementHost</c>.
        /// </summary>
        protected override FrameworkElement CreateXamlContent()
            => new XBuildEventsPropertyPageView();

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
    }
}
