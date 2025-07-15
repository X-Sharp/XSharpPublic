﻿// Licensed to the .NET Foundation under one or more agreements. The .NET Foundation licenses this file to you under the MIT license. See the LICENSE.md file in the project root for more information.

using System.Runtime.InteropServices;
using Microsoft.VisualStudio.ComponentModelHost;
using Microsoft.VisualStudio.ProjectSystem;
using Microsoft.VisualStudio.ProjectSystem.VS;
using Microsoft.VisualStudio.ProjectSystem.VS.Xproj;
using Microsoft.VisualStudio.Shell;

namespace Microsoft.VisualStudio.Packaging;

[Guid(PackageGuid)]
[PackageRegistration(AllowsBackgroundLoading = true, RegisterUsing = RegistrationMethod.Assembly, UseManagedResourcesOnly = true)]
[ProvideProjectFactory(typeof(XprojProjectFactory), null, "#27", "xproj", "xproj", null)]
[ProvideAutoLoad(ActivationContextGuid, PackageAutoLoadFlags.BackgroundLoad)]
[ProvideUIContextRule(
    contextGuid: ActivationContextGuid,
    name: "Load Managed Project Package",
    expression: "dotnetcore",
    termNames: ["dotnetcore"],
    termValues: ["SolutionHasProjectCapability:.NET & CPS"])]
[ProvideMenuResource("Menus.ctmenu", 5)]
    internal sealed class XSharpManagedProjectSystemPackage : AsyncPackage
{
    public const string ActivationContextGuid = "E7DF1626-44DD-4E8C-A8A0-92EAB6DDC16E";
#if XSHARP		
    public const string PackageGuid = "93bc6a0e-d5ad-455e-a9b9-01d09153707d";
#else		
    public const string PackageGuid = "860A27C0-B665-47F3-BC12-637E16A1050A";
#endif
    protected override async Task InitializeAsync(CancellationToken cancellationToken, IProgress<ServiceProgressData> progress)
    {
        // Here we initialize our internal IXPackageService implementations, both in global and project services scope.

        // Get access to global MEF services.
        IComponentModel componentModel = await this.GetServiceAsync<SComponentModel, IComponentModel>();

        // Get access to project services scope services.
        IProjectServiceAccessor projectServiceAccessor = componentModel.GetService<IProjectServiceAccessor>();

        // Find package services in global scope.
        IEnumerable<IXPackageService> globalPackageServices = componentModel.GetExtensions<IXPackageService>();

        // Find package services in project service scope.
        IEnumerable<IXPackageService> projectServicesPackageServices = projectServiceAccessor.GetProjectService().Services.ExportProvider.GetExportedValues<IXPackageService>(ExportContractNames.Scopes.ProjectService);

        // We initialize these on the main thread.
        await JoinableTaskFactory.SwitchToMainThreadAsync();

        // Initialize all services concurrently.
        await Task.WhenAll(globalPackageServices.Concat(projectServicesPackageServices).Select(s => s.InitializeAsync(this)));
    }
}
