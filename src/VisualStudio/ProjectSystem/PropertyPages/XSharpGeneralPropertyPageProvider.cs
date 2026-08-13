//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System.ComponentModel.Composition;
using System.Runtime.InteropServices;

using Microsoft.VisualStudio.ProjectSystem;
using Microsoft.VisualStudio.ProjectSystem.VS.Properties;
using Microsoft.VisualStudio.Shell;

namespace XSharp.VisualStudio.ProjectSystem.PropertyPages
{
    /// <summary>
    /// CPS property page metadata for the XSharp "Application" (General) page.
    ///
    /// This page surfaces the MSBuild properties defined in
    /// <c>Rules/XSharp.xaml</c> under the <c>Application</c> rule category.
    /// It mirrors the information shown by <c>XSharpGeneralPropertyPage</c>
    /// in the MPF-based AppDesigner.
    /// </summary>
    [Export(typeof(IPageMetadata))]
    [AppliesTo(XSharpProjectCapabilities.XSharp)]
    internal sealed class XSharpGeneralPropertyPageProvider : IPageMetadata
    {
        /// <inheritdoc />
        public string Name => "Application";

        /// <inheritdoc />
        public System.Guid PageGuid =>
            new System.Guid(XSharpConstants.GeneralPropertiesPage);

        /// <inheritdoc />
        public int PageOrder => 100;

        /// <inheritdoc />
        public bool HasConfigurationCondition => false;
    }
}
