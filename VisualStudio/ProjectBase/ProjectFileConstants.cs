/* ****************************************************************************
 *
 * Copyright (c) Microsoft Corporation.
 *
 * This source code is subject to terms and conditions of the Apache License, Version 2.0. A
 * copy of the license can be found in the License.txt file at the root of this distribution.
 *
 * You must not remove this notice, or any other, from this software.
 *
 * ***************************************************************************/

using System.Diagnostics.CodeAnalysis;

namespace Microsoft.VisualStudio.Project
{
    /// <summary>
    /// Defines the constant strings for various msbuild targets
    /// </summary>
    public static class MsBuildTarget
    {
        public const string ResolveProjectReferences = nameof(ResolveProjectReferences);
        public const string ResolveAssemblyReferences = nameof(ResolveAssemblyReferences);
        public const string ResolveComReferences = nameof(ResolveComReferences);
        public const string Build = nameof(Build);
        public const string Rebuild = nameof(Rebuild);
        public const string Clean = nameof(Clean);
    }

    public static class MsBuildGeneratedItemType
    {
        public const string ReferenceCopyLocalPaths = nameof(ReferenceCopyLocalPaths);
        public const string ComReferenceWrappers = nameof(ComReferenceWrappers);
    }

    /// <summary>
    /// Defines the constant strings used with project files.
    /// </summary>
    public static class ProjectFileConstants
    {
        public const string Include = nameof(Include);
        public const string Name = nameof(Name);
        public const string HintPath = nameof(HintPath);
        public const string AssemblyName = nameof(AssemblyName);
        public const string FinalOutputPath = nameof(FinalOutputPath);
        public const string Project = nameof(Project);
        public const string LinkedIntoProjectAt = nameof(LinkedIntoProjectAt);
		public const string Link = nameof(Link);
        public const string TypeGuid = nameof(TypeGuid);
        public const string InstanceGuid = nameof(InstanceGuid);
        public const string Private = nameof(Private);
        public const string EmbedInteropTypes = nameof(EmbedInteropTypes);
        public const string PackageReference = nameof(PackageReference);
        public const string ProjectReference = nameof(ProjectReference);
        public const string Reference = nameof(Reference);
        public const string WebPiReference = nameof(WebPiReference);
        public const string WebReference = nameof(WebReference);
        public const string WebReferenceFolder = nameof(WebReferenceFolder);
        public const string Folder = nameof(Folder);
        public const string Content = nameof(Content);
		public const string None = nameof(None);
        public const string EmbeddedResource = nameof(EmbeddedResource);
        public const string RootNamespace = nameof(RootNamespace);
        public const string OutputType = nameof(OutputType);
        public const string SubType = nameof(SubType);
        public const string DependentUpon = nameof(DependentUpon);
        public const string Compile = nameof(Compile);
        public const string ReferencePath = nameof(ReferencePath);
        public const string ResolvedProjectReferencePaths = nameof(ResolvedProjectReferencePaths);
        public const string Configuration = nameof(Configuration);
        public const string Platform = nameof(Platform);
        public const string AvailablePlatforms = nameof(AvailablePlatforms);
		public const string AvailableItemName = nameof(AvailableItemName);
        public const string BuildVerbosity = nameof(BuildVerbosity);
        public const string Template = nameof(Template);
        public const string SubProject = nameof(SubProject);
        public const string BuildAction = nameof(BuildAction);
		public const string CopyToOutputDirectory = nameof(CopyToOutputDirectory);
		public const string SpecificVersion = nameof(SpecificVersion);
        public const string COMReference = nameof(COMReference);
        public const string Guid = nameof(Guid);
        public const string VersionMajor = nameof(VersionMajor);
        public const string VersionMinor = nameof(VersionMinor);
        public const string Lcid = nameof(Lcid);
        public const string Isolated = nameof(Isolated);
        public const string WrapperTool = nameof(WrapperTool);
        public const string BuildingInsideVisualStudio = nameof(BuildingInsideVisualStudio);
        public const string SccProjectName = nameof(SccProjectName);
        public const string SccLocalPath = nameof(SccLocalPath);
        public const string SccAuxPath = nameof(SccAuxPath);
        public const string SccProvider = nameof(SccProvider);
        public const string ProjectGuid = nameof(ProjectGuid);
        public const string ProjectTypeGuids = nameof(ProjectTypeGuids);
        public const string Generator = nameof(Generator);
        public const string CustomToolNamespace = nameof(CustomToolNamespace);
        public const string FlavorProperties = nameof(FlavorProperties);
        public const string VisualStudio = nameof(VisualStudio);
        public const string User = nameof(User);
		public const string StartURL = nameof(StartURL);
		public const string StartArguments = nameof(StartArguments);
		public const string StartWorkingDirectory = nameof(StartWorkingDirectory);
		public const string StartProgram = nameof(StartProgram);
		public const string StartAction = nameof(StartAction);
		public const string OutputPath = nameof(OutputPath);
		public const string OtherFlags = nameof(OtherFlags);
		public const string PlatformTarget = nameof(PlatformTarget);

        public const string ApplicationDefinition = nameof(ApplicationDefinition);
        public const string Page = nameof(Page);
        public const string Resource = nameof(Resource);
        public const string ApplicationIcon = nameof(ApplicationIcon);
        public const string StartupObject = nameof(StartupObject);
        public const string TargetPlatform = nameof(TargetPlatform);
	    public const string TargetPlatformLocation = nameof(TargetPlatformLocation);
        public const string PlatformAware = nameof(PlatformAware);
        public const string AppxPackage = nameof(AppxPackage);
        public const string WindowsAppContainer = nameof(WindowsAppContainer);
		public const string AllProjectOutputGroups = nameof(AllProjectOutputGroups);
		public const string TargetPath = nameof(TargetPath);
		public const string TargetDir = nameof(TargetDir);
        public const string Aliases = nameof(Aliases);
        public const string CurrentSolutionConfigurationContents = nameof(CurrentSolutionConfigurationContents);

        // XSharp additions
        public const string NativeResource = nameof(NativeResource);
        public const string VOBinary = nameof(VOBinary);
        public const string Settings = nameof(Settings);

    }

    public static class ProjectFileAttributeValue
    {
        public const string Code = nameof(Code);
        public const string Form = nameof(Form);
        public const string Component = nameof(Component);
        public const string Designer = nameof(Designer);
        public const string UserControl = nameof(UserControl);
      	public const string Visible = nameof(Visible);
    }

    internal static class ProjectFileValues
    {
        internal const string AnyCPU = nameof(AnyCPU);
    }

    public enum WrapperToolAttributeValue
    {
        Primary,
        TlbImp,
		AxImp
    }

    /// <summary>
    /// A set of constants that specify the default sort order for different types of hierarchy nodes.
    /// </summary>
    public static class DefaultSortOrderNode
    {
        public const int ProjectProperties = 100;
        public const int ProjectIncludeFiles = 150;
        public const int NestedProjectNode = 200;
        public const int ReferenceContainerNode = 300;
        public const int NuGetPackagesNode = 400;
        public const int FolderNode = 500;
        public const int VOBinaryNode = 750;
        public const int HierarchyNode = 1000;
    }
    public static class XSharpImageListIndex
    {
        public const int Project = 0;
        public const int Source = 1;
        public const int Form = 2;
        public const int Server = 3;
        public const int FieldSpec = 4;
        public const int Menu = 5;
        public const int VO = 6;
        public const int Grid = 7;
        public const int Test = 8;
        public const int Properties = 9;
        public const int Reference = 10;
        public const int DanglingReference = 11;
        public const int TabOrder = 12;

    }
}
