//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

namespace XSharp.Project
{
    using System;
    using System.Runtime.InteropServices;
    using Microsoft.VisualStudio.Project;
    using Microsoft.VisualStudio.Shell;

    /// <summary>
    /// Property page for the Build settings.
    /// </summary>
    [ComVisible(true)]
    [Guid(XSharpConstants.BuildPropertiesPage)]
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProvideObject(typeof(XSharpBuildPropertyPage))]
    public class XSharpBuildPropertyPage : XPropertyPage
    {
        // =========================================================================================
        // Constructor
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="XSharpBuildPropertyPage"/> class.
        /// </summary>
        public XSharpBuildPropertyPage()
        {
            this.PageName = "Build";
            this.PerConfig = false;
        }

        // =========================================================================================
        // XPropertyPage overrides
        // =========================================================================================

        /// <summary>
        /// Sets a project property, forwarding to the base implementation.
        /// </summary>
        /// <param name="propertyName">Name of the property to set.</param>
        /// <param name="value">Value of the property.</param>
        public override void SetProperty(string propertyName, string value)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            base.SetProperty(propertyName, value);
        }

        /// <summary>
        /// Creates the UI panel for this property page.
        /// SDK-style projects get the XAML/WPF host; legacy projects get the WinForms panel.
        /// </summary>
        /// <returns>An <see cref="IPropertyPagePanel"/> implementation.</returns>
        protected override IPropertyPagePanel CreatePropertyPagePanel()
        {
            if (IsSdkProject)
                return new XBuildPropertyPageXamlHost(this);
            return new XBuildPropertyPagePanelWinForms(this);
        }

        /// <summary>
        /// Notifies the active panel when a project property changes externally.
        /// On the WinForms path this propagates <c>PlatformTarget</c> changes to enable/disable
        /// <c>Prefer32Bit</c>.  On the XAML path the same is handled via
        /// <see cref="XBuildPropertyPageXamlHost.NotifyPlatformTargetChanged"/>.
        /// </summary>
        protected override void Project_OnProjectPropertyChanged(object sender, ProjectPropertyChangedArgs e)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            if (e.OldValue == e.NewValue)
                return;

            if (string.Compare(e.PropertyName, XSharpProjectFileConstants.PlatformTarget, StringComparison.OrdinalIgnoreCase) == 0)
            {
                // WinForms path
                (PropertyPagePanel as XBuildPropertyPagePanel)?.Project_OnProjectPropertyChanged(sender, e);

                // XAML path
                (PropertyPagePanel as XBuildPropertyPageXamlHost)?.NotifyPlatformTargetChanged(e.NewValue);
            }
        }
    }
}
