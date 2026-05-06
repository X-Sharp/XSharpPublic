//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using Microsoft.VisualStudio.Project;

namespace XSharp.Project
{
    /// <summary>
    /// WinForms implementation of the General (Application) property page panel used for
    /// legacy .NET Framework (non-SDK-style) projects.
    /// </summary>
    /// <remarks>
    /// <para>
    /// This class is a thin subclass of the existing <see cref="XGeneralPropertyPagePanel"/>
    /// WinForms <c>UserControl</c>.  It exists so that the routing logic in
    /// <see cref="XSharpGeneralPropertyPage.CreatePropertyPagePanel"/> can return either this
    /// WinForms wrapper or a new <see cref="XGeneralPropertyPageXamlHost"/> (XAML) instance
    /// through the common <see cref="IPropertyPagePanel"/> interface, without changing the
    /// WinForms UI code at all.
    /// </para>
    /// <para>
    /// No new behaviour is added here.  All UI, binding, and apply logic remains in
    /// <see cref="XGeneralPropertyPagePanel"/>.
    /// </para>
    /// </remarks>
    internal sealed class XGeneralPropertyPagePanelWinForms : XGeneralPropertyPagePanel
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="XGeneralPropertyPagePanelWinForms"/>
        /// class.
        /// </summary>
        /// <param name="parentPropertyPage">
        /// The <see cref="XPropertyPage"/> that owns this panel.
        /// </param>
        public XGeneralPropertyPagePanelWinForms(XPropertyPage parentPropertyPage)
            : base(parentPropertyPage)
        {
        }
    }
}
