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
    /// COM-visible property page for the Language settings tab.
    /// </summary>
    /// <remarks>
    /// <para>
    /// For SDK-style projects this page returns an <see cref="XLanguagePropertyPageXamlHost"/>
    /// (a WPF/XAML panel embedded in an <c>ElementHost</c>).  For legacy .NET Framework
    /// projects it returns an <see cref="XLanguagePropertyPagePanelWinForms"/> (the original
    /// WinForms panel).  All business logic and UI state are delegated to the appropriate
    /// panel through the <see cref="IPropertyPagePanel"/> interface.
    /// </para>
    /// </remarks>
    [ComVisible(true)]
    [Guid(XSharpConstants.LanguagePropertiesPage)]
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProvideObject(typeof(XSharpLanguagePropertyPage))]
    public class XSharpLanguagePropertyPage : XPropertyPage
    {

        /// <summary>
        /// Initializes a new instance of the <see cref="XSharpLanguagePropertyPage"/> class.
        /// </summary>
        public XSharpLanguagePropertyPage()
        {
            this.PageName = "Language";
            this.PerConfig = false;
        }

        /// <summary>
        /// Creates the panel that hosts the Language page controls.
        /// </summary>
        /// <returns>
        /// An <see cref="XLanguagePropertyPageXamlHost"/> for SDK-style projects, or an
        /// <see cref="XLanguagePropertyPagePanelWinForms"/> for legacy projects.
        /// </returns>
        protected override IPropertyPagePanel CreatePropertyPagePanel()
        {
            if (IsSdkProject)
                return new XLanguagePropertyPageXamlHost(this);
            return new XLanguagePropertyPagePanelWinForms(this);
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
                (PropertyPagePanel as XLanguagePropertyPageXamlHost)?.NotifyDialectChanged(e.NewValue);

                // WinForms path — delegate directly to the panel (null-safe).
                (PropertyPagePanel as XLanguagePropertyPagePanel)?.Project_OnProjectPropertyChanged(sender, e);
            }
        }
    }
}
