//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using Microsoft.VisualStudio.Project;

namespace XSharp.Project
{
    /// <summary>
    /// WinForms thin subclass of <see cref="XLanguagePropertyPagePanel"/> used for the
    /// legacy (.NET Framework) routing path.
    /// </summary>
    /// <remarks>
    /// All logic lives in the base <see cref="XLanguagePropertyPagePanel"/>.
    /// This subclass exists so that <see cref="XSharpLanguagePropertyPage.CreatePropertyPagePanel"/>
    /// can return an <see cref="IPropertyPagePanel"/> without depending on the concrete
    /// WinForms panel type on the XAML (SDK) path.
    /// </remarks>
    internal sealed class XLanguagePropertyPagePanelWinForms : XLanguagePropertyPagePanel
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="XLanguagePropertyPagePanelWinForms"/> class.
        /// </summary>
        /// <param name="parentPropertyPage">The owning property page.</param>
        public XLanguagePropertyPagePanelWinForms(XPropertyPage parentPropertyPage)
            : base(parentPropertyPage) { }
    }
}
