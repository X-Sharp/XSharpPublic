//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.ComponentModel;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio;
using XSharp.ProjectSystem;
using System.Threading;
using System.ComponentModel.Composition;
//using Microsoft.VisualStudio.LanguageServices;
using XSharp;
using Microsoft.VisualStudio.ProjectSystem;
using Microsoft.VisualStudio.ProjectSystem.VS;

[assembly: ProjectTypeRegistration(
  projectTypeGuid: GuidStrings.guidCpsProjectType,
    displayName: "#1",                      // "XSharp"
    displayProjectFileExtensions: "#2",     // "XSharp Project Files (*.xsproj);*.xsproj"
    defaultProjectExtension: XSharpConstants.ProjectExtension,
    language: XSharpConstants.LanguageName,
    resourcePackageGuid: GuidStrings.guidCpsProjectType,
    Capabilities = ProjectTypeCapabilities.XSharp,
    DisableAsynchronousProjectTreeLoad = true,
    PossibleProjectExtensions = XSharpConstants.ProjectExtension,
    NewProjectRequireNewFolderVsTemplate = true)]
[assembly: ProvideDiffSupportedContentType(XSharpConstants.ProjectExtension, "")]   // Empty string because content type is not important, we just want to tell the diff that the file type is supported
[assembly: ProvideEditorFactoryMapping("{f6819a78-a205-47b5-be1c-675b3c7f0b8e}", XSharpConstants.ProjectExtension)] // Use the XML editor

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
    [PackageRegistration(UseManagedResourcesOnly = true, AllowsBackgroundLoading = true)]
    [Description("XSharp project type based on CPS")]
    [Guid(GuidStrings.guidCpsProjectType)]

    public sealed class XSharpCPSPackage : AsyncPackage
    {
        /// <summary>
        /// The file extension of this project type.  No preceding period.
        /// </summary>
        public const string ProjectExtension = XSharpConstants.ProjectExtension;

        /// <summary>
        /// The default namespace this project compiles with, so that manifest
        /// resource names can be calculated for embedded resources.
        /// </summary>
        internal const string DefaultNamespace = XSharpConstants.LanguageName;

        //[Import(typeof(VisualStudioWorkspace))]
        //public VisualStudioWorkspace myWorkspace { get; set; }
        protected override async System.Threading.Tasks.Task InitializeAsync(CancellationToken cancellationToken, IProgress<ServiceProgressData> progress)
        {
            // Suspend walking until Solution is opened.
            await base.InitializeAsync(cancellationToken, progress);

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
                                    ProjectCapabilities.SdkReferences + "; "+
                                    ProjectCapabilities.ProjectReferences + "; " +
                                    ProjectCapabilities.AssemblyReferences + "; " +
                                    ProjectCapabilities.ComReferences + "; " +
                                    ProjectCapabilities.WinRTReferences+ "; " +
                                      ProjectCapability.DotNet;
        public const string XSharp = Default + "; " +
                                     ProjectCapability.XSharp + "; " +
                                     ProjectCapability.SortByDisplayOrder ;

    }

    internal static class ProjectCapability
    {
        public const string XSharp = nameof(XSharp);
        public const string XSharpAppDesigner = XSharp + " & " + AppDesigner;
        public const string CSharp = "XXCSharp";
        public const string VB = "XXVisualBasic";
        public const string VisualBasic = "XXVisualBasic";

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



