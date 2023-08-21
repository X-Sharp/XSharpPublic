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

namespace XSharp.ProjectSystem
{
    /// <summary>
    /// This class implements the package exposed by this assembly.
    /// </summary>
    /// <remarks>
    /// This package is required if you want to define adds custom commands (ctmenu)
    /// or localized resources for the strings that appear in the New Project and Open Project dialogs.
    /// Creating project extensions or project types does not actually require a VSPackage.
    /// </remarks>
    [PackageRegistration(UseManagedResourcesOnly = true, AllowsBackgroundLoading = true, RegisterUsing = RegistrationMethod.Assembly)]
    [Description("XSharp CPS based Project System")]
    [Guid(XSharpConstants.guidCpsProjectTypeString)]
    [ProvideAutoLoad(VSConstants.UICONTEXT.SolutionExists_string, PackageAutoLoadFlags.BackgroundLoad)]
    //[ProvideAutoLoad(ActivationContextGuid, PackageAutoLoadFlags.BackgroundLoad)]
    //[ProvideUIContextRule(ActivationContextGuid,
    //    name: "Load X# Managed Project Package",
    //    expression: "dotnetcore",
    //    termNames: new[] { "dotnetcore" },
    //    termValues: new[] { "SolutionHasProjectCapability:.NET & CPS & XSharp" }
    //    )]
    public sealed class XSharpCPSPackage : ToolkitPackage
    {
        public const string ActivationContextGuid = "6634b40e-66e3-4f8d-af0f-b860354a9132";
        protected override async Task InitializeAsync(CancellationToken cancellationToken, IProgress<ServiceProgressData> progress)
        {
            // Suspend walking until Solution is opened.
            await base.InitializeAsync(cancellationToken, progress);
            // The project selector helps to choose between MPF and CPS projects
            await this.RegisterCommandsAsync();
            // Register services that export IPackageService, such as the ProjectSelector
            await this.RegisterServicesAsync();
            return;
        }
        async Task RegisterServicesAsync()
        {
            IComponentModel componentModel = await this.GetServiceAsync<SComponentModel, IComponentModel>();

            IEnumerable<IPackageService> packageServices = componentModel.GetExtensions<IPackageService>();

            await JoinableTaskFactory.SwitchToMainThreadAsync();

            foreach (IPackageService packageService in packageServices)
            {
                await packageService.InitializeAsync(this);
            }
        }
    }
    internal static class ProjectTypeCapabilities
    {
        public const string Default = ProjectCapability.AppDesigner + "; " +
                                      ProjectCapability.DependenciesTree + "; " +
                                      ProjectCapability.EditAndContinue + "; " +
                                      ProjectCapability.OpenProjectFile + "; " +
                                      ProjectCapability.PreserveFormatting + "; " +
                                      /*ProjectCapability.ProjectConfigurationsDeclaredDimensions + "; " +*/
                                      ProjectCapability.LanguageService + "; " +
                                    ProjectCapabilities.SdkReferences + "; " +
                                    ProjectCapabilities.ProjectReferences + "; " +
                                    ProjectCapabilities.AssemblyReferences + "; " +
                                    ProjectCapabilities.ComReferences + "; " +
                                    ProjectCapabilities.WinRTReferences + "; " +
                                      ProjectCapability.DotNet;
        public const string XSharp = Default + "; " +
                                     ProjectCapability.XSharp + "; " +
                                     ProjectCapability.SortByDisplayOrder;

    }

    internal static class ProjectCapability
    {
        public const string XSharp = nameof(XSharp);
        public const string XSharpAppDesigner = XSharp + " & " + AppDesigner;

        public const string AlwaysAvailable = ProjectCapabilities.AlwaysApplicable;
        public const string AppDesigner = nameof(AppDesigner);
        public const string AppSettings = nameof(AppSettings);
        public const string DependenciesTree = nameof(DependenciesTree);
        public const string ProjectImportsTree = nameof(ProjectImportsTree);
        public const string EditAndContinue = nameof(EditAndContinue);
        public const string LaunchProfiles = nameof(LaunchProfiles);
        public const string OpenProjectFile = nameof(OpenProjectFile);
        public const string HandlesOwnReload = ProjectCapabilities.HandlesOwnReload;
        public const string Pack = nameof(Pack); // Keep this in sync with Microsoft.VisualStudio.Editors.ProjectCapability.Pack
        public const string PackageReferences = ProjectCapabilities.PackageReferences;
        public const string PreserveFormatting = nameof(PreserveFormatting);
        public const string ProjectConfigurationsDeclaredDimensions = ProjectCapabilities.ProjectConfigurationsDeclaredDimensions;
        public const string LanguageService = nameof(LanguageService);
        public const string DotNetLanguageService = DotNet + " & " + LanguageService;

        /// <summary>
        /// Instructs CPS to order tree items according to the <see cref="IProjectTree2.DisplayOrder"/> property first.
        /// This is in addition to the default ordering by <see cref="ProjectTreeFlags.Common.BubbleUp"/>, then by
        /// <see cref="ProjectTreeFlags.Common.Folder"/> or <see cref="ProjectTreeFlags.Common.VirtualFolder"/>, and finally
        /// alphabetical.
        /// </summary>
        public const string SortByDisplayOrder = ProjectCapabilities.SortByDisplayOrder;


        public const string DotNet = ".NET";
        public const string WindowsForms = nameof(WindowsForms);
        public const string WPF = nameof(WPF);
    }
}



