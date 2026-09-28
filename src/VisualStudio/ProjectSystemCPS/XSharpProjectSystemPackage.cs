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
using XSharp.ProjectSystem.Options;
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
    /// to the CPS project type (switchable in Tools > Options > X# > Project System), so existing solution files
    /// do not change. The CPS project type keeps its own template language, so the X# project templates keep
    /// creating projects through the MPFproj project type (and the selector).
    /// </remarks>
    [PackageRegistration(UseManagedResourcesOnly = true, AllowsBackgroundLoading = true)]
    [Guid(XSharpConstants.guidXSharpCpsProjectPkgString)]
    [ProjectTypeRegistration(
        projectTypeGuid: XSharpConstants.guidCpsProjectTypeString,
        displayName: "XSharp (CPS)",
        displayProjectFileExtensions: "XSharp CPS Project Files (*." + XSharpConstants.ProjectExtension + ");*." + XSharpConstants.ProjectExtension,
        defaultProjectExtension: XSharpConstants.ProjectExtension,
        language: LanguageVsTemplate,
        resourcePackageGuid: XSharpConstants.guidXSharpCpsProjectPkgString,
        Capabilities = XSharpCapabilities.ProjectTypeCapabilities,
        PossibleProjectExtensions = XSharpConstants.ProjectExtension)]
    [ProvideProjectSelector(XSharpConstants.guidXSharpProjectFactoryString, XSharpConstants.guidProjectSelectorString)]
    // Visual Studio writes the GUID of the project system that actually loaded a project into the .sln when it
    // saves the solution ({AA6C8D78} -> {AB494DCE}, the same happens for C#: {FAE04EC0} -> {9A19103F}). The
    // selector on the CPS project type makes such entries behave like the original ones: legacy projects, the
    // Tools > Options switch and the X# MSBuild support check still send projects to MPFproj.
    [ProvideProjectSelector(XSharpConstants.guidCpsProjectTypeString, XSharpConstants.guidProjectSelectorString)]
    [ProvideOptionPage(typeof(ProjectSystemOptionsPage), ProjectSystemOptionsPage.CategoryName, ProjectSystemOptionsPage.PageName, 0, 0, true)]
    public sealed class XSharpProjectSystemPackage : AsyncPackage
    {
        /// <summary>
        /// Template language of the CPS project type. Deliberately different from the MPFproj "XSharp" language,
        /// so that X# project templates are not matched to both project types.
        /// </summary>
        internal const string LanguageVsTemplate = "XSharpCps";

        private IVsRegisterProjectSelector projectSelectorRegistration;
        private uint projectSelectorCookie;
        private ProjectSystemOptionsPage options;

        protected override async Task InitializeAsync(CancellationToken cancellationToken, IProgress<ServiceProgressData> progress)
        {
            await base.InitializeAsync(cancellationToken, progress);
            await JoinableTaskFactory.SwitchToMainThreadAsync(cancellationToken);
            options = (ProjectSystemOptionsPage)GetDialogPage(typeof(ProjectSystemOptionsPage));
            projectSelectorRegistration = await GetServiceAsync(typeof(SVsRegisterProjectTypes)) as IVsRegisterProjectSelector;
            if (projectSelectorRegistration != null)
            {
                var selectorGuid = new Guid(XSharpConstants.guidProjectSelectorString);
                var selector = new XSharpProjectSelector(() => options?.UseCpsForSdkProjects ?? true);
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
