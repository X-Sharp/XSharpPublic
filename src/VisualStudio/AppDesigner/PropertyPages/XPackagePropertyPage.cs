//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using Microsoft.VisualStudio.Project;
namespace XSharp.Project
{
    using System.Runtime.InteropServices;
    using Microsoft.VisualStudio.Shell;

    /// <summary>
    /// Property page that exposes NuGet / assembly-info package metadata for
    /// SDK-style XSharp projects.
    /// </summary>
    /// <remarks>
    /// This page is configuration-independent (<see cref="XPropertyPage.PerConfig"/> = false)
    /// and is only surfaced for SDK-style projects — it is not added to the legacy project
    /// page list in <c>XSharpProjectNode.GetConfigurationIndependentPropertyPages()</c>.
    /// </remarks>
    [ComVisible(true)]
    [Guid(XSharpConstants.PackagePropertiesPage)]
    [ProvideObject(typeof(XSharpPackagePropertyPage))]
    public class XSharpPackagePropertyPage : XPropertyPage
    {
        public XSharpPackagePropertyPage()
        {
            this.PageName = "Package";
            this.PerConfig = false;
        }

        protected override IPropertyPagePanel CreatePropertyPagePanel()
        {
            // This page is only ever shown for SDK-style projects; the XAML host is
            // always used.  A WinForms fallback is not needed because the page is not
            // added to the legacy project page list.
            return new XPackagePropertyPageXamlHost(this);
        }
    }
}
