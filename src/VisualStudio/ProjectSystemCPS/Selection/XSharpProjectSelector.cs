//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio.Shell.Interop;
using XSharp.Settings;

namespace XSharp.ProjectSystem.Selection
{
    /// <summary>
    /// Chooses the project system for projects with the X# (MPFproj) project type GUID {AA6C8D78-...} or the CPS
    /// project type GUID {AB494DCE-...}: SDK-style projects are loaded by the CPS project type, all other (legacy)
    /// projects by MPFproj, which has no SDK-style support. VS rewrites {AA6C8D78} to {AB494DCE} in the .sln when it
    /// saves the solution, so both GUIDs are routed the same way.
    /// </summary>
    /// <remarks>
    /// Same mechanism as the managed project system uses for C#/VB/F# (FSharpProjectSelector): the selector is
    /// registered for the legacy project type GUID (pkgdef "Projects\{guid}\ProjectSelector", see
    /// <see cref="ProvideProjectSelectorAttribute"/>), so existing solution files keep working. Unlike the managed
    /// project system it is registered for the CPS project type GUID as well: VS persists the GUID of the factory
    /// the selector returned, so an SDK project's .sln entry flips to {AB494DCE} once (C# has the same flip,
    /// {FAE04EC0} -> {9A19103F}); with the second registration a legacy project behind such an entry still loads
    /// with MPFproj.
    /// SDK-style projects go to MPFproj only when the installed X# MSBuild support files lack the CPS design-time
    /// import (<see cref="XSharpMsBuildSupport"/>) or the project file cannot be read; MPFproj then reports that
    /// the project cannot be loaded (XSharpProjectFactory) instead of CPS failing with less clear errors.
    /// </remarks>
    [Guid(XSharpConstants.guidProjectSelectorString)]
    internal sealed class XSharpProjectSelector : IVsProjectSelector
    {
        public void GetProjectFactoryGuid(Guid guidProjectType, string pszFilename, out Guid guidProjectFactory)
        {
            guidProjectFactory = XSharpConstants.guidXSharpProjectFactory;
            try
            {
                if (SdkProjectFile.IsSdkProject(pszFilename) && XSharpMsBuildSupport.SupportsCps)
                {
                    guidProjectFactory = XSharpConstants.guidCpsProjectType;
                }
            }
            catch (Exception e)
            {
                XSettings.Exception(e);
                guidProjectFactory = XSharpConstants.guidXSharpProjectFactory;
            }
            XSettings.Information("XSharpProjectSelector: " + pszFilename + " -> " +
                (guidProjectFactory == XSharpConstants.guidCpsProjectType ? "CPS" : "MPFproj"));
        }
    }
}
