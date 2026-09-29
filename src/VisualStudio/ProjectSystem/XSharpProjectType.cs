//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System.ComponentModel.Composition;

using Microsoft.VisualStudio.ProjectSystem;

namespace XSharp.VisualStudio.ProjectSystem
{
    /// <summary>
    /// Binds the XSharp project type (GUID <c>aa6c8d78-22ff-423a-9c7c-5f2393824e04</c>)
    /// and the <c>.xsproj</c> file extension to this CPS-based project system.
    ///
    /// The <see cref="ExportProjectTypeAttribute"/> causes CPS to hand any
    /// <c>.xsproj</c> file that declares the XSharp project-type GUID to this
    /// project system rather than the legacy MPF factory.  For VS2019 support,
    /// the registry key <c>ProjectSystemPackage</c> under the project-type root
    /// is only written for VS2022+ installations (see <c>XSharpCpsPackage.pkgdef</c>).
    /// </summary>
    [Export]
    [ExportProjectType(
        projectTypeGuid: XSharpConstants.guidXSharpProjectFactoryString,
        displayName: XSharpConstants.LanguageName,
        projectFileExtension: XSharpConstants.ProjectExtension,
        defaultProjectCapabilities: XSharpProjectCapabilities.XSharp)]
    internal sealed class XSharpProjectType
    {
    }
}
