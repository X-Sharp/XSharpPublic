//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System.ComponentModel.Composition;

using Microsoft.VisualStudio.ProjectSystem;
using Microsoft.VisualStudio.ProjectSystem.VS.Properties;

namespace XSharp.VisualStudio.ProjectSystem.PropertyPages
{
    /// <summary>
    /// CPS property page metadata for the XSharp "Build" page.
    ///
    /// This page surfaces the MSBuild properties defined in
    /// <c>Rules/XSharp.xaml</c> under the <c>Build</c> rule category.
    /// It mirrors <c>XSharpBuildPropertyPage</c> in the MPF AppDesigner.
    /// </summary>
    [Export(typeof(IPageMetadata))]
    [AppliesTo(XSharpProjectCapabilities.XSharp)]
    internal sealed class XSharpBuildPropertyPageProvider : IPageMetadata
    {
        /// <inheritdoc />
        public string Name => "Build";

        /// <inheritdoc />
        public System.Guid PageGuid =>
            new System.Guid(XSharpConstants.BuildPropertiesPage);

        /// <inheritdoc />
        public int PageOrder => 200;

        /// <inheritdoc />
        public bool HasConfigurationCondition => true;
    }
}
