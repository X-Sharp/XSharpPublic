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
    /// CPS property page metadata for the XSharp "Debug" page.
    ///
    /// This page surfaces the MSBuild properties defined in
    /// <c>Rules/XSharp.xaml</c> under the <c>Debug</c> rule category.
    /// It mirrors <c>XSharpDebugPropertyPage</c> in the MPF AppDesigner.
    /// </summary>
    [Export(typeof(IPageMetadata))]
    [AppliesTo(XSharpProjectCapabilities.XSharp)]
    internal sealed class XSharpDebugPropertyPageProvider : IPageMetadata
    {
        /// <inheritdoc />
        public string Name => "Debug";

        /// <inheritdoc />
        public System.Guid PageGuid =>
            new System.Guid(XSharpConstants.DebugPropertiesPage);

        /// <inheritdoc />
        public int PageOrder => 300;

        /// <inheritdoc />
        public bool HasConfigurationCondition => true;
    }
}
