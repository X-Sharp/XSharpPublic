//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Runtime.InteropServices;
using System.Threading;
using Microsoft.VisualStudio.ProjectSystem.VS;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using XSharp.ProjectSystem.Selection;
using Task = System.Threading.Tasks.Task;

namespace XSharp.ProjectSystem
{
    /// <summary>
    /// Package for the CPS based X# project system.
    /// </summary>
    /// <remarks>
    /// Two X# project systems run side by side, like the legacy and the SDK project systems of C#/VB/F#:
    /// <list type="bullet">
    /// <item>legacy .xsproj: MPFproj (ProjectPackage), project type {AA6C8D78-...}</item>
    /// <item>SDK-style .xsproj: CPS (this package), project type {AB494DCE-...}</item>
    /// </list>
    /// <see cref="XSharpProjectSelector"/> is registered for the MPFproj project type and sends SDK-style projects
    /// to the CPS project type, so existing solution files keep working. MPFproj no longer loads SDK-style projects. The CPS project type keeps its own template language, so the X# project templates keep
    /// creating projects through the MPFproj project type (and the selector).
    /// </remarks>
    [PackageRegistration(UseManagedResourcesOnly = true, AllowsBackgroundLoading = true)]
    [Guid(XSharpConstants.guidXSharpCpsProjectPkgString)]
    [ProjectTypeRegistration(
        projectTypeGuid: XSharpConstants.guidCpsProjectTypeString,
        displayName: "XSharp",
        displayProjectFileExtensions: "XSharp Project Files (*." + XSharpConstants.ProjectExtension + ");*." + XSharpConstants.ProjectExtension,
        defaultProjectExtension: XSharpConstants.ProjectExtension,
        language: XSharpConstants.LanguageName,
        resourcePackageGuid: XSharpConstants.guidXSharpCpsProjectPkgString,
        Capabilities = XSharpCapabilities.ProjectTypeCapabilities,
        PossibleProjectExtensions = XSharpConstants.ProjectExtension)]
    [ProvideProjectSelector(XSharpConstants.guidXSharpProjectFactoryString, XSharpConstants.guidProjectSelectorString)]
    // Visual Studio writes the GUID of the project system that actually loaded a project into the .sln when it
    // saves the solution ({AA6C8D78} -> {AB494DCE}, the same happens for C#: {FAE04EC0} -> {9A19103F}). The
    // selector on the CPS project type makes such entries behave like the original ones: legacy projects and the
    // X# MSBuild support check still send projects to MPFproj.
    [ProvideProjectSelector(XSharpConstants.guidCpsProjectTypeString, XSharpConstants.guidProjectSelectorString)]
    // The image manifest (XSharp.ProjectSystemCPS.imagemanifest) refers to its PNGs with pack URIs that name this
    // assembly by its short name. The image library resolves them when it builds its cache at startup, usually before
    // a CPS project has loaded the assembly: without the extension folder as binding path the lookup failed, the
    // manifest was dropped from the cache and the project and .prg icons stayed empty (unless a CPS project happened
    // to load first). The codeBase entry (ProvideCodeBase in ProjectPackage/ExternalAssemblies.cs) only helps loads
    // with the full identity.
    [ProvideBindingPath]
    public sealed class XSharpProjectSystemPackage : AsyncPackage
    {
        /// <summary>
        /// Template language of the CPS project type. Deliberately different from the MPFproj "XSharp" language,
        /// so that X# project templates are not matched to both project types.
        /// </summary>

        private IVsRegisterProjectSelector projectSelectorRegistration;
        private uint projectSelectorCookie;

        protected override async Task InitializeAsync(CancellationToken cancellationToken, IProgress<ServiceProgressData> progress)
        {
            await base.InitializeAsync(cancellationToken, progress);
            await JoinableTaskFactory.SwitchToMainThreadAsync(cancellationToken);
            projectSelectorRegistration = await GetServiceAsync(typeof(SVsRegisterProjectTypes)) as IVsRegisterProjectSelector;
            if (projectSelectorRegistration != null)
            {
                var selectorGuid = new Guid(XSharpConstants.guidProjectSelectorString);
                var selector = new XSharpProjectSelector();
                projectSelectorRegistration.RegisterProjectSelector(ref selectorGuid, selector, out projectSelectorCookie);
            }
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing && projectSelectorCookie != 0)
            {
                JoinableTaskFactory.Run(async () =>
                {
                    await JoinableTaskFactory.SwitchToMainThreadAsync();
                    UnregisterProjectSelector();
                });
            }
            base.Dispose(disposing);
        }

        private void UnregisterProjectSelector()
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            if (projectSelectorRegistration != null && projectSelectorCookie != 0)
            {
                projectSelectorRegistration.UnregisterProjectSelector(projectSelectorCookie);
                projectSelectorCookie = 0;
            }
        }
    }
}
