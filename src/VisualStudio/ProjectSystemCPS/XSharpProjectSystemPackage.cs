//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Runtime.InteropServices;
using System.Threading;
using Microsoft.VisualStudio.Shell;
#if XSHARPCPS
using Microsoft.VisualStudio.ProjectSystem.VS;
#endif
using Task = System.Threading.Tasks.Task;

namespace XSharp.ProjectSystem
{
    /// <summary>
    /// Package for the CPS based X# project system.
    /// </summary>
    /// <remarks>
    /// The CPS project type is only registered when XSHARPCPS is defined (Debug builds). The MPFproj project
    /// system registers the same .xsproj extension, so during the side-by-side phase the CPS project type uses
    /// its own project type GUID and its own template language: a solution selects it explicitly through the
    /// project type GUID, and the existing X# project templates keep creating MPFproj projects.
    /// </remarks>
    [PackageRegistration(UseManagedResourcesOnly = true, AllowsBackgroundLoading = true)]
    [Guid(XSharpConstants.guidXSharpCpsProjectPkgString)]
#if XSHARPCPS
    [ProjectTypeRegistration(
        projectTypeGuid: XSharpConstants.guidCpsProjectTypeString,
        displayName: "XSharp (CPS)",
        displayProjectFileExtensions: "XSharp CPS Project Files (*." + XSharpConstants.ProjectExtension + ");*." + XSharpConstants.ProjectExtension,
        defaultProjectExtension: XSharpConstants.ProjectExtension,
        language: LanguageVsTemplate,
        resourcePackageGuid: XSharpConstants.guidXSharpCpsProjectPkgString,
        Capabilities = XSharpCapabilities.XSharpCps,
        PossibleProjectExtensions = XSharpConstants.ProjectExtension)]
#endif
    public sealed class XSharpProjectSystemPackage : AsyncPackage
    {
        /// <summary>
        /// Template language of the CPS project type. Deliberately different from the MPFproj "XSharp" language,
        /// so that X# project templates are not matched to both project types.
        /// </summary>
        internal const string LanguageVsTemplate = "XSharpCps";

        protected override async Task InitializeAsync(CancellationToken cancellationToken, IProgress<ServiceProgressData> progress)
        {
            await base.InitializeAsync(cancellationToken, progress);
        }
    }
}
