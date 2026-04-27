//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

namespace XSharp.Project
{
    using System.Runtime.InteropServices;
    using Microsoft.VisualStudio.Project;
    using Microsoft.VisualStudio.Shell;

    /// <summary>
    /// Property page for the Debug settings.
    /// </summary>
    [Guid(XSharpConstants.DebugPropertiesPage)]
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProvideObject(typeof(XSharpDebugPropertyPage))]
    public class XSharpDebugPropertyPage : XPropertyPage
    {
        // =========================================================================================
        // Constructor
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="XSharpDebugPropertyPage"/> class.
        /// </summary>
        public XSharpDebugPropertyPage()
        {
            this.PageName  = "Debug";
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
                return new XDebugPropertyPageXamlHost(this);
            return new XDebugPropertyPagePanelWinForms(this);
        }
    }
}
