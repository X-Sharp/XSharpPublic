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
    /// WinForms/WPF bridge host for the Build property page.
    /// Used for SDK-style XSharp projects.
    /// </summary>
    /// <remarks>
    /// <para>
    /// This class extends <see cref="XPropertyPageXamlHost"/>, which wraps an
    /// <c>ElementHost</c> inside a WinForms <c>UserControl</c> so that Visual Studio's
    /// COM property-sheet frame can host WPF content.
    /// </para>
    /// <para>
    /// The WPF content is a <see cref="XBuildPropertyPageView"/> (<c>UserControl</c>)
    /// whose <c>DataContext</c> is an <see cref="XBuildPropertyPageViewModel"/> that
    /// reads from and writes back to the MSBuild project via the owning
    /// <see cref="XSharpBuildPropertyPage"/>.
    /// </para>
    /// <para>
    /// When the platform target changes externally (detected by
    /// <see cref="XSharpBuildPropertyPage.Project_OnProjectPropertyChanged"/>),
    /// <see cref="NotifyPlatformTargetChanged"/> is called so the ViewModel can
    /// update the <c>Prefer32BitEnabled</c> flag — mirroring the WinForms
    /// <c>XBuildPropertyPagePanel.Project_OnProjectPropertyChanged</c> behaviour.
    /// </para>
    /// </remarks>
    internal sealed class XBuildPropertyPageXamlHost : XPropertyPageXamlHost
    {
        // =========================================================================================
        // Member Variables
        // =========================================================================================

        /// <summary>The ViewModel that backs the WPF UI.</summary>
        private readonly XBuildPropertyPageViewModel _viewModel;

        // =========================================================================================
        // Constructor
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="XBuildPropertyPageXamlHost"/> class.
        /// </summary>
        /// <param name="parentPropertyPage">
        /// The <see cref="XPropertyPage"/> that owns this panel.
        /// </param>
        public XBuildPropertyPageXamlHost(XPropertyPage parentPropertyPage)
            : base(parentPropertyPage)
        {
            _viewModel = new XBuildPropertyPageViewModel(parentPropertyPage);
            SetViewModel(_viewModel);
        }

        // =========================================================================================
        // XPropertyPageXamlHost — Abstract overrides
        // =========================================================================================

        /// <summary>
        /// Creates the WPF <see cref="FrameworkElement"/> (the
        /// <see cref="XBuildPropertyPageView"/> <c>UserControl</c>) that will be
        /// hosted inside the <c>ElementHost</c>.
        /// </summary>
        protected override FrameworkElement CreateXamlContent()
            => new XBuildPropertyPageView();

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
        // Platform target change notification
        // =========================================================================================

        /// <summary>
        /// Called by <see cref="XSharpBuildPropertyPage.Project_OnProjectPropertyChanged"/>
        /// when the <c>PlatformTarget</c> property changes externally.
        /// Delegates to <see cref="XBuildPropertyPageViewModel.NotifyPlatformTargetChanged"/>
        /// so the ViewModel can update the <c>Prefer32BitEnabled</c> flag and, if necessary,
        /// uncheck <c>Prefer32Bit</c> — matching the WinForms behaviour exactly.
        /// </summary>
        /// <param name="newPlatform">The new platform target value (e.g. <c>"AnyCPU"</c>).</param>
        public void NotifyPlatformTargetChanged(string newPlatform)
        {
            Microsoft.VisualStudio.Shell.ThreadHelper.ThrowIfNotOnUIThread();
            _viewModel.NotifyPlatformTargetChanged(newPlatform);
        }
    }
}
