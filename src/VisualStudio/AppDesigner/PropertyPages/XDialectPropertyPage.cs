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
    /// COM-visible property page for the Dialect settings tab.
    /// </summary>
    /// <remarks>
    /// <para>
    /// For SDK-style projects this page returns an <see cref="XDialectPropertyPageXamlHost"/>
    /// (a WPF/XAML panel embedded in an <c>ElementHost</c>).  For legacy .NET Framework
    /// projects it returns an <see cref="XDialectPropertyPagePanelWinForms"/> (the original
    /// WinForms panel).  All business logic and UI state are delegated to the appropriate
    /// panel through the <see cref="IPropertyPagePanel"/> interface.
    /// </para>
    /// </remarks>
    [Guid(XSharpConstants.DialectPropertiesPage)]
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProvideObject(typeof(XSharpDialectPropertyPage))]
    public class XSharpDialectPropertyPage : XPropertyPage
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="XSharpDialectPropertyPage"/> class.
        /// </summary>
        public XSharpDialectPropertyPage()
        {
            this.PageName = "Dialect";
            this.PerConfig = false;
        }

        /// <summary>
        /// Creates the panel that hosts the Dialect page controls.
        /// </summary>
        /// <returns>
        /// An <see cref="XDialectPropertyPageXamlHost"/> for SDK-style projects, or an
        /// <see cref="XDialectPropertyPagePanelWinForms"/> for legacy projects.
        /// </returns>
        protected override IPropertyPagePanel CreatePropertyPagePanel()
        {
            if (IsSdkProject)
                return new XDialectPropertyPageXamlHost(this);
            return new XDialectPropertyPagePanelWinForms(this);
        }

        /// <summary>
        /// Called by the project system when a project property changes externally
        /// (e.g. from another property page).  Forwards dialect changes to the active panel
        /// so it can re-apply dialect-specific enabling logic.
        /// </summary>
        protected override void Project_OnProjectPropertyChanged(object sender, ProjectPropertyChangedArgs e)
        {
            ThreadHelper.ThrowIfNotOnUIThread();

            if (string.Compare(e.PropertyName, XSharpProjectFileConstants.Dialect, true) == 0)
            {
                // XAML path — notify the host which delegates to the ViewModel.
                (PropertyPagePanel as XDialectPropertyPageXamlHost)?.NotifyDialectChanged(e.NewValue);

                // WinForms path — delegate directly to the panel (null-safe).
                (PropertyPagePanel as XDialectPropertyPagePanel)?.Project_OnProjectPropertyChanged(sender, e);
            }
        }
    }
}
