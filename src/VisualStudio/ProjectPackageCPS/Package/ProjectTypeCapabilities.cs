// Licensed to the .NET Foundation under one or more agreements. The .NET Foundation licenses this file to you under the MIT license. See the LICENSE.md file in the project root for more information.

using Microsoft.VisualStudio.ProjectSystem;

namespace XSharp.VisualStudio.ProjectSystem
{
    /// <summary>
    ///     Represents set of capabilities for .NET-based projects that are always present ("fixed").
    /// </summary>
    /// <remarks>
    ///     These capabilities (along with any active IProjectCapabilitiesProvider) are combined with
    ///     the "dynamic" capabilities inherited from the active configuration. These are typically
    ///     defined in Microsoft.Managed.DesignTime.targets, but could come from other locations such
    ///     as packages or other target files.
    /// </remarks>
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
