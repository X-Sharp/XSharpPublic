//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Threading.Tasks;
using Microsoft.VisualStudio.ProjectSystem;
using Microsoft.VisualStudio.ProjectSystem.VS.Properties;

namespace XSharp.ProjectSystem.PropertyPages
{
    /// <summary>
    /// Makes the project designer (Project > Properties, double click on the Properties folder) available for
    /// X# CPS projects.
    /// </summary>
    /// <remarks>
    /// CPS only offers the project designer when at least one IVsProjectDesignerPageProvider applies to the
    /// project (VsProjectDesignerPageService.IsProjectDesignerSupported). The managed project system only has
    /// providers for C#, VB and F#.
    /// Because X# CPS projects have the ProjectPropertiesEditor capability, the AppDesigner editor factory opens
    /// the new property editor, which shows the XAML rules with Context "Project" (the managed pages plus the X#
    /// pages from XSharpBuildTask\Rules). The classic COM property pages listed by this provider are not used
    /// in that case, so there are none: the X# COM property pages (AppDesigner2022) depend on MPFproj.
    /// </remarks>
    [Export(typeof(IVsProjectDesignerPageProvider))]
    [AppliesTo(XSharpCapabilities.XSharpCps + " & AppDesigner")]
    internal sealed class XSharpProjectDesignerPageProvider : IVsProjectDesignerPageProvider
    {
        private static readonly Task<IReadOnlyCollection<IPageMetadata>> noPages =
            Task.FromResult<IReadOnlyCollection<IPageMetadata>>(Array.Empty<IPageMetadata>());

        public Task<IReadOnlyCollection<IPageMetadata>> GetPagesAsync()
        {
            return noPages;
        }
    }
}
