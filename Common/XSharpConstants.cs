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
        public const string guidXSharpProjectPkgString = "e299fb7b-d273-4678-9acf-b37b4af04a12";
        public const string guidXSharpProjectCmdSetString = "32a7b1cd-3663-4a70-b855-486671f5839c";
        public const string guidXSharpProjectFactoryString = "aa6c8d78-22ff-423a-9c7c-5f2393824e04";
        //public const string guidCpsProjectType = "3FBE2663-470C-4908-A199-55B65642DFDB";
        public const string guidCpsProjectType = guidXSharpProjectFactoryString;
        public const string guidXSharpProjectFactoryStringCurly = "{"+ guidXSharpProjectFactoryString+"}";
        public const string guidXSharpVOFormEditor = "fc9f8e69-f338-4fa6-aca3-aa41e445849a";
        public const string guidXSharpVOFormEditorCmdSet = "b9ce6f12-e66c-4e77-9be6-0491dfcdc2d3";

        public const string guidXSharpVOMenuEditor = "e4ae4582-98ae-40c8-9e48-6f3be61ebf79";
        public const string guidXSharpVOMenuEditorCmdSet = "416d760b-7d71-4362-b4a3-97048db8f65f";

        public const string guidXSharpVODbServerEditor = "c7e4c5f6-98b8-4826-9000-6b9b94dc2d97";
        public const string guidXSharpVODbServerEditorCmdSet = "15e7094c-8202-4b0a-a276-50a0d76136d4";

        public const string guidXSharpVOFieldSpecEditor = "8c5d0bae-7a69-437b-ad0e-1e1b89721ebd";
        public const string guidXSharpVOFieldSpecEditorCmdSet = "24ea5441-eb10-45e7-9a44-797df84f8775";

        public const string guidVulcanLanguageServiceString = "{8d3f6d25-c81c-4fd8-9599-2f72b5d4b0c9}";
        public const string guidVulcanSourceCodeEditor = "{e6787d5e-718e-4810-9c26-7cc920baa335}";
        public const string guidVulcanFormEditor = "{e9eecf7e-7aa2-490e-affc-c55fa2acc5a3}";
        public const string guidVulcanMenuEditor = "{adee1755-5ac3-485b-b857-f82d902362ca}";
        public const string guidVulcanDbEditor = "{5325db94-5d6c-41fd-be44-c5b277612ce6}";
        public const string guidVulcanFsEditor = "{4849278c-aacb-4bbe-9a15-d96da837aeb7}";

        public const string guidVSXmlEditorString = "{fa3cd31e-987b-443a-9b81-186104e8dac1}";


        public const string guidXSharpLanguageServicePkgString = "e9b6ee0f-9bfa-4cff-a60b-51a08bbc5050";
        public const string guidXSharpLanguageServiceCmdSetString = "6511ea00-4558-4ae7-84ee-0e2aebd40d88";

        public const string EditorFactoryGuidString = "B4829761-2BFA-44B7-8F8F-D2625EBCF218";

        public static readonly Guid guidXSharpProjectCmdSet = new Guid(guidXSharpProjectCmdSetString);
        public static readonly Guid guidXSharpProjectFactory = new Guid(guidXSharpProjectFactoryString);

        public static readonly Guid guidVOFormEditorFactory = new Guid(guidXSharpVOFormEditor);
        public static readonly Guid guidVOFormEditorCmdSet = new Guid(guidXSharpVOFormEditorCmdSet);

        public static readonly Guid guidVOMenuEditorFactory = new Guid(guidXSharpVOMenuEditor);
        public static readonly Guid guidVOMenuEditorCmdSet = new Guid(guidXSharpVOMenuEditorCmdSet);

        public static readonly Guid guidVODbServerEditorFactory = new Guid(guidXSharpVODbServerEditor);
        public static readonly Guid guidVODbServerEditorCmdSet = new Guid(guidXSharpVODbServerEditorCmdSet);

        public static readonly Guid guidVOFieldSpecEditorFactory = new Guid(guidXSharpVOFieldSpecEditor);
        public static readonly Guid guidVOFieldSpecEditorCmdSet = new Guid(guidXSharpVOFieldSpecEditorCmdSet);
        public static readonly Guid guidSourcecodeEditorFactory = new Guid(EditorFactoryGuidString);
        public static readonly Guid guidLanguageService = new Guid(guidXSharpLanguageServicePkgString);
        public static readonly Guid guidXSharpLanguageServiceCmdSet = new Guid(guidXSharpLanguageServiceCmdSetString);
        public static readonly Guid guidVulcanLanguageService = new Guid(guidVulcanLanguageServiceString);

        public static readonly Guid guidVSXmlEditor = new Guid(guidVSXmlEditorString);

        public const int cmdidShowGrid = 0x6001;
        public const int cmdidTestDialog = 0x6002;
        public const int VOFormEditorToolbar = 0x6003;
        public const int VOFormEditorToolbarGroup = 0x6004;
        public const int cmdidTabOrder = 0x6005;
    }

    internal static class XSharpConstants
    {

        public const string LanguageName = "XSharp";
        internal const string LanguageServiceName = "XSharp Language Service";
        internal const string EditorName = "XSharp Source Code Editor";
        internal const string ProjectSystemName = "XSharp Project System";

        internal const string ProjectExtension = "xsproj";
        internal const string ProjectExtensions = "xsproj;xsprj";   // the first version of X# has .xsprj (without o) as extension
        internal const string ProjectFileMask = LanguageName + " Project Files (*." + ProjectExtension + ");*." + ProjectExtension;

        public const string EditorFactoryGuidString = GuidStrings.EditorFactoryGuidString;

        public const string IntellisenseOptionsPageGuidString = "FDE6E4C9-FA8A-4B93-8B6D-88D9D2A5063E";
        public const string FormattingOptionsPageGuidString = "5A086A53-64B8-410A-BDC0-7D8D573F9B41";
        public const string OtherOptionsPageGuidString = "FC6059E3-9762-407B-83BB-A90D2C57D710";
        public const string GeneratorOptionsPageGuidString = "A9C3281D-F068-4639-B573-82A2491E839F";
        public const string CompletionOptionsPageGuidString = "E37D51D4-30AA-4A28-81CA-1A486397640C";
        public const string FileNodePropertiesGuidString = "B7971A68-EA46-4814-AC67-1424A59DC7EB";
        public const string IndentingOptionsPageGuidString = "A8545AC1-4DD9-4A18-890C-C96CDC790950";

        public const string WPFProjectFactory = "5ADB76EC-7017-476A-A8E0-25D4202FFCF0";
        public const string WPFFlavor = "14989543-69A4-4C47-A31C-74B6A6DB719B";

        public const string LibraryManagerService = "93F79240-85A9-4697-9A1C-71DE150BA363";
        public const string LibraryManager = "1A36F2B9-EB46-42C7-8421-D5DF35653ED4";
        public const string Library = "3AB768D9-DF41-443F-BECE-497EB5C234DB";

        public const string GeneralPropertiesPage = "53651BEA-799A-45EB-B58C-C884F5417219";
        public const string BuildPropertiesPage = "E994C210-9D6D-4CF4-A061-EBBEA2BC626B";
        public const string DebugPropertiesPage = "2955A638-C389-4675-BB1C-6B2BC173C1E7";
        public const string DialectPropertiesPage = "2652FCA6-1C45-4D25-942D-4C5D5EDE9539";
        public const string LanguagePropertiesPage = "0DFC7EF7-3F1A-4ACB-AFD8-DF56AEF9467A";
        public const string BuildEventsPropertiesPage = "49306259-9119-466E-8780-486CFBE2597D";

    }

    /// <summary>
    /// Indexes to the embedded ImageList .bmp
    /// </summary>
    internal static class XSharpImageListIndex
    {
        // Commented out entries are now handled with an ImageMoniker
        public const int Project = 0;
        public const int Source = 1;
        //public const int Form = 2;
        //public const int Server = 3;
        //public const int FieldSpec = 4;
        //public const int Menu = 5;
        public const int VO = 6;
        //public const int Grid = 7;
        //public const int Test = 8;
        //public const int Properties = 9;
        //public const int Reference = 10;
        //public const int DanglingReference = 11;
        public const int TabOrder = 12;
        public const int FoxPro = 13;
        //public const int NuGet = 14;
        //public const int Header = 15;
        //public const int IncludeFolder = 16;
        public const int ReferenceGroup = 17;   // VS2022 has an imagemoniker for this
        public const int MissingFile = 18;      // VS2022 has an imagemoniker for this
    }
    internal static class XSharpProjectFileConstants
    {
        public const string Allowdot = nameof(Allowdot);
        public const string AllowOldStyleAssignments = nameof(AllowOldStyleAssignments);
        public const string AssemblyName = nameof(AssemblyName);
        public const string AssemblyOriginatorKeyFile = nameof(AssemblyOriginatorKeyFile);
        public const string AutoGenerateBindingRedirects = nameof(AutoGenerateBindingRedirects);
        public const string AZ = nameof(AZ);
        public const string BaseInputPaths = nameof(BaseInputPaths);
        public const string CommandLineOption = nameof(CommandLineOption);
        public const string CS = nameof(CS);
        public const string Cultures = nameof(Cultures);
        public const string DefineConstants = nameof(DefineConstants);
        public const string DefineDebugConstant = nameof(DefineDebugConstant);
        public const string DelaySign = nameof(DelaySign);
        public const string DevEnvDir = nameof(DevEnvDir);
        public const string Dialect = nameof(Dialect);
        public const string DialectCore = "Core";
        public const string DialectFoxPro = "FoxPro";
        public const string DialectHarbour = "Harbour";
        public const string DialectVO = "VO";
        public const string DialectVulcan = "Vulcan.NET";
        public const string DialectXPP = "Xbase++";
        public const string DisabledWarnings = nameof(DisabledWarnings);
        public const string DocumentationFile = nameof(DocumentationFile);
        public const string EnforceSelf = nameof(EnforceSelf);
        public const string EnforceOverride = nameof(EnforceOverride);
        public const string Fox1 = nameof(Fox1);
        public const string Fox2 = nameof(Fox2);
        public const string GetTargetPath = nameof(GetTargetPath);
        public const string IncludePaths = nameof(IncludePaths);
        public const string IncludeSearchPaths = nameof(IncludeSearchPaths);
        public const string InitLocals = nameof(InitLocals);
        public const string INS = nameof(INS);
        public const string IntermediateOutputPath = nameof(IntermediateOutputPath);
        public const string LB = nameof(LB);
        public const string MemVar = nameof(MemVar);
        public const string NamedArgs = nameof(NamedArgs);
        public const string NativeResource = nameof(NativeResource);
        public const string NoStandardDefs = nameof(NoStandardDefs);
        public const string NoWin32Manifest = nameof(NoWin32Manifest);
        public const string NS = nameof(NS);
        public const string Optimize = nameof(Optimize);
        public const string OutputName = nameof(OutputName);
        public const string OutputPath = nameof(OutputPath);
        public const string OutputType = nameof(OutputType);
        public const string OVF = nameof(OVF);
        public const string PackageReference = nameof(PackageReference);
        public const string PlatformTarget = nameof(PlatformTarget);
        public const string PostBuildEvent = nameof(PostBuildEvent);
        public const string PreBuildEvent = nameof(PreBuildEvent);
        public const string Prefer32Bit = nameof(Prefer32Bit);
        public const string Project = nameof(Project);
        public const string ProjectFiles = nameof(ProjectFiles);
        public const string ProjectView = nameof(ProjectView);
        public const string PropertyGroup = nameof(PropertyGroup);
        public const string ReferencePaths = nameof(ReferencePaths);
        public const string RegisterForComInterop = nameof(RegisterForComInterop);
        public const string RunPostBuildEvent = nameof(RunPostBuildEvent);
        public const string SccAuxPath = nameof(SccAuxPath);
        public const string SccLocalPath = nameof(SccLocalPath);
        public const string SccProjectName = nameof(SccProjectName);
        public const string SccProvider = nameof(SccProvider);
        public const string Settings = nameof(Settings);
        public const string ShowAllFiles = nameof(ShowAllFiles);
        public const string ShowSourceTrace = nameof(ShowSourceTrace);
        public const string SignAssembly = nameof(SignAssembly);
        public const string SolutionDir = nameof(SolutionDir);
        public const string SolutionExt = nameof(SolutionExt);
        public const string SolutionFileName = nameof(SolutionFileName);
        public const string SolutionName = nameof(SolutionName);
        public const string SolutionPath = nameof(SolutionPath);
        public const string SpecificWarnings = nameof(SpecificWarnings);
        public const string StandardDefs = nameof(StandardDefs);
        public const string SuppressRCWarnings = nameof(SuppressRCWarnings);
        public const string TargetFrameworkVersion = nameof(TargetFrameworkVersion);
        public const string ToolsVersion = nameof(ToolsVersion);
        public const string TreatWarningsAsErrors = nameof(TreatWarningsAsErrors);
        public const string Undeclared = nameof(Undeclared);
        public const string Unsafe = nameof(Unsafe);
        public const string UseNativeVersion = nameof(UseNativeVersion);
        public const string UseSharedCompilation = nameof(UseSharedCompilation);
        public const string VerboseOutput = nameof(VerboseOutput);
        public const string WarningsAsErrors = nameof(WarningsAsErrors);
        public const string WarningsNotAsErrors = nameof(WarningsNotAsErrors);
        public const string Vo1 = nameof(Vo1);
        public const string Vo2 = nameof(Vo2);
        public const string Vo3 = nameof(Vo3);
        public const string Vo4 = nameof(Vo4);
        public const string Vo5 = nameof(Vo5);
        public const string Vo6 = nameof(Vo6);
        public const string Vo7 = nameof(Vo7);
        public const string Vo8 = nameof(Vo8);
        public const string Vo9 = nameof(Vo9);
        public const string Vo10 = nameof(Vo10);
        public const string Vo11 = nameof(Vo11);
        public const string Vo12 = nameof(Vo12);
        public const string Vo13 = nameof(Vo13);
        public const string Vo14 = nameof(Vo14);
        public const string Vo15 = nameof(Vo15);
        public const string Vo16 = nameof(Vo16);
        public const string Vo17 = nameof(Vo17);
        public const string VOBinary = nameof(VOBinary);
        public const string VulcanCompatibleResources = nameof(VulcanCompatibleResources);
        public const string WarningLevel = nameof(WarningLevel);
        public const string Xpp1 = nameof(Xpp1);
        public const string Xpp2 = nameof(Xpp2);
        public const string RootNamespace = nameof(RootNamespace);
        public const string StartupObject = nameof(StartupObject);
        public const string ApplicationIcon = nameof(ApplicationIcon);
        public const string DebuggerCommand = nameof(DebuggerCommand);
        public const string DebuggerCommandArguments = nameof(DebuggerCommandArguments);
        public const string DebugType = nameof(DebugType);
        public const string DebuggerWorkingDirectory = nameof(DebuggerWorkingDirectory);
        public const string EnableUnmanagedDebugging = nameof(EnableUnmanagedDebugging);

    }
}
