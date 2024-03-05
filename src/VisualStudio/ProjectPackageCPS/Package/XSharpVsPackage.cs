//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.ComponentModel;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio.Shell;
using System.Threading;
using Microsoft.VisualStudio.ProjectSystem;
using Community.VisualStudio.Toolkit;
using System.Threading.Tasks;
using Microsoft.VisualStudio.Threading;

using Microsoft.VisualStudio.ComponentModelHost;
using System.Collections.Generic;
using Microsoft.VisualStudio;
using System.Linq;

namespace XSharp.VisualStudio.ProjectSystem
{
    /// <summary>
    /// This class implements the package exposed by this assembly.
    /// </summary>
    [PackageRegistration(UseManagedResourcesOnly = true, AllowsBackgroundLoading = true)]
    [Description("XSharp CPS based Project System")]
    [Guid(XSharpConstants.guidCpsProjectTypeString)]
    [ProvideAutoLoad(VSConstants.UICONTEXT.SolutionExists_string, PackageAutoLoadFlags.BackgroundLoad)]
    [ProvideUIContextRule(ActivationContextGuid,
        name: "Load X# Managed Project Package",
        expression: "dotnetcore",
        termNames: new[] { "dotnetcore" },
        termValues: new[] { "SolutionHasProjectCapability:.NET & CPS & XSharp" }
        )]
    [ProvideMenuResource("Menus.ctmenu", 5)]
    public sealed class XSharpCPSPackage : AsyncPackage
    {
        public const string ActivationContextGuid = "6634b40e-66e3-4f8d-af0f-b860354a9132";
        protected override async Task InitializeAsync(CancellationToken cancellationToken, IProgress<ServiceProgressData> progress)
        {
		
            // Here we initialize our internal IPackageService implementations, both in global and project services scope.
            await base.InitializeAsync(cancellationToken, progress);

            // Get access to global MEF services.
            IComponentModel componentModel = await this.GetServiceAsync<SComponentModel, IComponentModel>();

            // Find package services in global scope.
            IEnumerable<IPackageService> globalPackageServices = componentModel.GetExtensions<IPackageService>();

            await JoinableTaskFactory.SwitchToMainThreadAsync();

 			await Task.WhenAll(globalPackageServices.Select(s => s.InitializeAsync(this)));

        }
    }
  
}



