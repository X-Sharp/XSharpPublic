//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;

namespace XSharp
{
    internal static class GuidStrings
    {
        internal const string guidXSharpProjectPkgString = "e299fb7b-d273-4678-9acf-b37b4af04a12";
        internal const string guidXSharpProjectFactoryString = "aa6c8d78-22ff-423a-9c7c-5f2393824e04";
        internal const string guidXSharpProjectFactoryStringCurly = "{"+ guidXSharpProjectFactoryString+"}";
        internal const string guidXSharpVOMenuEditor = "e4ae4582-98ae-40c8-9e48-6f3be61ebf79";
        internal const string guidXSharpVODbServerEditor = "c7e4c5f6-98b8-4826-9000-6b9b94dc2d97";
        internal const string guidXSharpVOFieldSpecEditor = "8c5d0bae-7a69-437b-ad0e-1e1b89721ebd";
        internal const string guidVSXmlEditorString = "{fa3cd31e-987b-443a-9b81-186104e8dac1}";
        internal const string guidXSharpLanguageServicePkgString = "e9b6ee0f-9bfa-4cff-a60b-51a08bbc5050";
        internal const string EditorFactoryGuidString = "B4829761-2BFA-44B7-8F8F-D2625EBCF218";
        internal static readonly Guid guidXSharpProjectFactory = new Guid(guidXSharpProjectFactoryString);
        internal static readonly Guid guidVOMenuEditorFactory = new Guid(guidXSharpVOMenuEditor);
        internal static readonly Guid guidVODbServerEditorFactory = new Guid(guidXSharpVODbServerEditor);
        internal static readonly Guid guidVOFieldSpecEditorFactory = new Guid(guidXSharpVOFieldSpecEditor);
        internal static readonly Guid guidSourcecodeEditorFactory = new Guid(EditorFactoryGuidString);
        internal static readonly Guid guidLanguageService = new Guid(guidXSharpLanguageServicePkgString);
        internal static readonly Guid guidVSXmlEditor = new Guid(guidVSXmlEditorString);
    }

    internal static class XSharpConstants
    {

        internal const string LanguageName = "XSharp";
        internal const string LanguageServiceName = "XSharp Language Service";
        internal const string EditorName = "XSharp Source Code Editor";
        internal const string ProjectSystemName = "XSharp Project System";

        internal const string ProjectExtension = "xsproj";
        internal const string ProjectExtensions = "xsproj;xsprj";   // the first version of X# has .xsprj (without o) as extension
        internal const string ProjectFileMask = LanguageName + " Project Files (*." + ProjectExtension + ");*." + ProjectExtension;

        internal const string EditorFactoryGuidString = GuidStrings.EditorFactoryGuidString;

        internal const string IntellisenseOptionsPageGuidString = "FDE6E4C9-FA8A-4B93-8B6D-88D9D2A5063E";
        internal const string FormattingOptionsPageGuidString = "5A086A53-64B8-410A-BDC0-7D8D573F9B41";
        internal const string OtherOptionsPageGuidString = "FC6059E3-9762-407B-83BB-A90D2C57D710";
        internal const string GeneratorOptionsPageGuidString = "A9C3281D-F068-4639-B573-82A2491E839F";
        internal const string CompletionOptionsPageGuidString = "E37D51D4-30AA-4A28-81CA-1A486397640C";
        internal const string FileNodePropertiesGuidString = "B7971A68-EA46-4814-AC67-1424A59DC7EB";
        internal const string IndentingOptionsPageGuidString = "A8545AC1-4DD9-4A18-890C-C96CDC790950";

        internal const string WPFProjectFactory = "5ADB76EC-7017-476A-A8E0-25D4202FFCF0";
        internal const string WPFFlavor = "14989543-69A4-4C47-A31C-74B6A6DB719B";

        internal const string LibraryManagerService = "93F79240-85A9-4697-9A1C-71DE150BA363";
        internal const string LibraryManager = "1A36F2B9-EB46-42C7-8421-D5DF35653ED4";
        internal const string Library = "3AB768D9-DF41-443F-BECE-497EB5C234DB";

        internal const string GeneralPropertiesPage = "53651BEA-799A-45EB-B58C-C884F5417219";
        internal const string BuildPropertiesPage = "E994C210-9D6D-4CF4-A061-EBBEA2BC626B";
        internal const string DebugPropertiesPage = "2955A638-C389-4675-BB1C-6B2BC173C1E7";
        internal const string DialectPropertiesPage = "2652FCA6-1C45-4D25-942D-4C5D5EDE9539";
        internal const string LanguagePropertiesPage = "0DFC7EF7-3F1A-4ACB-AFD8-DF56AEF9467A";
        internal const string BuildEventsPropertiesPage = "49306259-9119-466E-8780-486CFBE2597D";

    }

    /// <summary>
    /// Indexes to the embedded ImageList .bmp
    /// </summary>
    internal static class XSharpImageListIndex
    {
        // Commented out entries are now handled with an ImageMoniker
        internal const int Project = 0;
        internal const int Source = 1;
        //public const int Form = 2;
        //public const int Server = 3;
        //public const int FieldSpec = 4;
        //public const int Menu = 5;
        internal const int VO = 6;
        //public const int Grid = 7;
        //public const int Test = 8;
        //public const int Properties = 9;
        //public const int Reference = 10;
        //public const int DanglingReference = 11;
        internal const int TabOrder = 12;
        internal const int FoxPro = 13;
        //public const int NuGet = 14;
        //public const int Header = 15;
        //public const int IncludeFolder = 16;
        internal const int ReferenceGroup = 17;   // VS2022 has an imagemoniker for this
        internal const int MissingFile = 18;      // VS2022 has an imagemoniker for this
    }
    internal static class XSharpProjectFileConstants
    {
        internal const string Allowdot = nameof(Allowdot);
        internal const string AllowOldStyleAssignments = nameof(AllowOldStyleAssignments);
        internal const string AssemblyName = nameof(AssemblyName);
        internal const string AssemblyOriginatorKeyFile = nameof(AssemblyOriginatorKeyFile);
        internal const string AutoGenerateBindingRedirects = nameof(AutoGenerateBindingRedirects);
        internal const string AZ = nameof(AZ);
        internal const string BaseInputPaths = nameof(BaseInputPaths);
        internal const string CommandLineOption = nameof(CommandLineOption);
        internal const string CS = nameof(CS);
        internal const string Cultures = nameof(Cultures);
        internal const string DefineConstants = nameof(DefineConstants);
        internal const string DefineDebugConstant = nameof(DefineDebugConstant);
        internal const string DelaySign = nameof(DelaySign);
        internal const string DevEnvDir = nameof(DevEnvDir);
        internal const string Dialect = nameof(Dialect);
        internal const string DialectCore = "Core";
        internal const string DialectFoxPro = "FoxPro";
        internal const string DialectHarbour = "Harbour";
        internal const string DialectVO = "VO";
        internal const string DialectVulcan = "Vulcan.NET";
        internal const string DialectXPP = "Xbase++";
        internal const string DisabledWarnings = nameof(DisabledWarnings);
        internal const string DocumentationFile = nameof(DocumentationFile);
        internal const string EnforceSelf = nameof(EnforceSelf);
        internal const string EnforceOverride = nameof(EnforceOverride);
        internal const string Fox1 = nameof(Fox1);
        internal const string Fox2 = nameof(Fox2);
        internal const string GetTargetPath = nameof(GetTargetPath);
        internal const string IncludePaths = nameof(IncludePaths);
        internal const string IncludeSearchPaths = nameof(IncludeSearchPaths);
        internal const string InitLocals = nameof(InitLocals);
        internal const string INS = nameof(INS);
        internal const string IntermediateOutputPath = nameof(IntermediateOutputPath);
        internal const string LB = nameof(LB);
        internal const string MemVar = nameof(MemVar);
        internal const string NamedArgs = nameof(NamedArgs);
        internal const string NativeResource = nameof(NativeResource);
        internal const string NoStandardDefs = nameof(NoStandardDefs);
        internal const string NoWin32Manifest = nameof(NoWin32Manifest);
        internal const string NS = nameof(NS);
        internal const string Optimize = nameof(Optimize);
        internal const string OutputName = nameof(OutputName);
        internal const string OutputPath = nameof(OutputPath);
        internal const string OutputType = nameof(OutputType);
        internal const string OVF = nameof(OVF);
        internal const string PackageReference = nameof(PackageReference);
        internal const string PlatformTarget = nameof(PlatformTarget);
        internal const string PostBuildEvent = nameof(PostBuildEvent);
        internal const string PreBuildEvent = nameof(PreBuildEvent);
        internal const string Prefer32Bit = nameof(Prefer32Bit);
        internal const string Project = nameof(Project);
        internal const string ProjectFiles = nameof(ProjectFiles);
        internal const string ProjectView = nameof(ProjectView);
        internal const string PropertyGroup = nameof(PropertyGroup);
        internal const string ReferencePaths = nameof(ReferencePaths);
        internal const string RegisterForComInterop = nameof(RegisterForComInterop);
        internal const string RunPostBuildEvent = nameof(RunPostBuildEvent);
        internal const string SccAuxPath = nameof(SccAuxPath);
        internal const string SccLocalPath = nameof(SccLocalPath);
        internal const string SccProjectName = nameof(SccProjectName);
        internal const string SccProvider = nameof(SccProvider);
        internal const string Settings = nameof(Settings);
        internal const string ShowAllFiles = nameof(ShowAllFiles);
        internal const string ShowSourceTrace = nameof(ShowSourceTrace);
        internal const string SignAssembly = nameof(SignAssembly);
        internal const string SolutionDir = nameof(SolutionDir);
        internal const string SolutionExt = nameof(SolutionExt);
        internal const string SolutionFileName = nameof(SolutionFileName);
        internal const string SolutionName = nameof(SolutionName);
        internal const string SolutionPath = nameof(SolutionPath);
        internal const string SpecificWarnings = nameof(SpecificWarnings);
        internal const string StandardDefs = nameof(StandardDefs);
        internal const string SuppressRCWarnings = nameof(SuppressRCWarnings);
        internal const string TargetFrameworkVersion = nameof(TargetFrameworkVersion);
        internal const string ToolsVersion = nameof(ToolsVersion);
        internal const string TreatWarningsAsErrors = nameof(TreatWarningsAsErrors);
        internal const string Undeclared = nameof(Undeclared);
        internal const string Unsafe = nameof(Unsafe);
        internal const string UseNativeVersion = nameof(UseNativeVersion);
        internal const string UseSharedCompilation = nameof(UseSharedCompilation);
        internal const string VerboseOutput = nameof(VerboseOutput);
        internal const string WarningsAsErrors = nameof(WarningsAsErrors);
        internal const string WarningsNotAsErrors = nameof(WarningsNotAsErrors);
        internal const string Vo1 = nameof(Vo1);
        internal const string Vo2 = nameof(Vo2);
        internal const string Vo3 = nameof(Vo3);
        internal const string Vo4 = nameof(Vo4);
        internal const string Vo5 = nameof(Vo5);
        internal const string Vo6 = nameof(Vo6);
        internal const string Vo7 = nameof(Vo7);
        internal const string Vo8 = nameof(Vo8);
        internal const string Vo9 = nameof(Vo9);
        internal const string Vo10 = nameof(Vo10);
        internal const string Vo11 = nameof(Vo11);
        internal const string Vo12 = nameof(Vo12);
        internal const string Vo13 = nameof(Vo13);
        internal const string Vo14 = nameof(Vo14);
        internal const string Vo15 = nameof(Vo15);
        internal const string Vo16 = nameof(Vo16);
        internal const string Vo17 = nameof(Vo17);
        internal const string VOBinary = nameof(VOBinary);
        internal const string VulcanCompatibleResources = nameof(VulcanCompatibleResources);
        internal const string WarningLevel = nameof(WarningLevel);
        internal const string Xpp1 = nameof(Xpp1);
        internal const string Xpp2 = nameof(Xpp2);
        internal const string RootNamespace = nameof(RootNamespace);
        internal const string StartupObject = nameof(StartupObject);
        internal const string ApplicationIcon = nameof(ApplicationIcon);
        internal const string DebuggerCommand = nameof(DebuggerCommand);
        internal const string DebuggerCommandArguments = nameof(DebuggerCommandArguments);
        internal const string DebugType = nameof(DebugType);
        internal const string DebuggerWorkingDirectory = nameof(DebuggerWorkingDirectory);
        internal const string EnableUnmanagedDebugging = nameof(EnableUnmanagedDebugging);
    }
}
