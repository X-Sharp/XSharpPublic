//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharp.Project
{
    public static class XSharpConstants
    {

        public const string FileExtension1 = ".prg";
        public const string FileExtension2 = ".xs";
        public const string LanguageName = "XSharp";
        public const string ProjectExtension = "xsproj";
        public const string ProjectExtensions = "xsproj;xsprj";

        public const string EditorFactoryGuidString = "B4829761-2BFA-44B7-8F8F-D2625EBCF218";

        public const string FileNodePropertiesGuidString = "B7971A68-EA46-4814-AC67-1424A59DC7EB";

        public const string WPFProjectFactory = "5ADB76EC-7017-476A-A8E0-25D4202FFCF0";
        public const string WPFFlavor = "14989543-69A4-4C47-A31C-74B6A6DB719B";
    }
    public static class XSharpProjectFileConstants
    {

        public const string NativeResource = "NativeResource";
		// Some properties that are in the projectfiles
        public const string DevEnvDir = "DevEnvDir";
        public const string IntermediateOutputPath = "IntermediateOutputPath";
        public const string OutputName = "OutputName";
        public const string OutputPath = "OutputPath";
        public const string OutputType = "OutputType";
        public const string PdbOutputFile = "PdbOutputFile";
        public const string PostBuildEvent = "PostBuildEvent";
        public const string Project = "Project";
        public const string ProjectFiles = "ProjectFiles";
        public const string ProjectView = "ProjectView";
        public const string PropertyGroup = "PropertyGroup";
        public const string PreBuildEvent = "PreBuildEvent";
        public const string ReferencePaths = "ReferencePaths";
        public const string RunPostBuildEvent = "RunPostBuildEvent";
        public const string SccProjectName = "SccProjectName";
        public const string SccLocalPath = "SccLocalPath";
        public const string SccAuxPath = "SccAuxPath";
        public const string SccProvider = "SccProvider";
        public const string ShowAllFiles = "ShowAllFiles";
        public const string ShowSourceTrace = "ShowSourceTrace";
        public const string SolutionDir = "SolutionDir";
        public const string SolutionExt = "SolutionExt";
        public const string SolutionName = "SolutionName";
        public const string SolutionFileName = "SolutionFileName";
        public const string SolutionPath = "SolutionPath";
        public const string SuppressAclReset = "SuppressAclReset";
        public const string SuppressAllWarnings = "SuppressAllWarnings";
        public const string SuppressAssemblies = "SuppressAssemblies";
        public const string SuppressDefaultAdminSequenceActions = "SuppressDefaultAdminSequenceActions";
        public const string SuppressDefaultAdvSequenceActions = "SuppressDefaultAdvSequenceActions";
        public const string SuppressDefaultUISequenceActions = "SuppressDefaultUISequenceActions";
        public const string SuppressFileHashAndInfo = "SuppressFileHashAndInfo";
        public const string SuppressFiles = "SuppressFiles";
        public const string SuppressIces = "SuppressIces";
        public const string SuppressLayout = "SuppressLayout";
        public const string SuppressMsiAssemblyTableProcessing = "SuppressMsiAssemblyTableProcessing";
        public const string SuppressPdbOutput = "SuppressPdbOutput";
        public const string SuppressSchemaValidation = "SuppressSchemaValidation";
        public const string SuppressSpecificWarnings = "SuppressSpecificWarnings";
        public const string SuppressTagSectionIdAttributeOnTuples = "SuppressTagSectionIdAttributeOnTuples";
        public const string SuppressValidation = "SuppressValidation";
        public const string ToolsVersion = "ToolsVersion";
        public const string TreatWarningsAsErrors = "TreatWarningsAsErrors";
        public const string VerboseOutput = "VerboseOutput";
        public const string WarningLevel = "WarningLevel";
        public const string VOBinary = "VOBinary";
        public const string Settings = "Settings";

        public static class MsBuildTarget
        {
            public const string GetTargetPath = "GetTargetPath";
            public const string ResolveWixLibraryReferences = "ResolveWixLibraryReferences";
        }


    }
	  /// <summary>
        /// Indexes to the embedded ImageList .bmp
        /// </summary>
    public static class XSharpImageListIndex
    {
        public const int Project = 0;
        public const int Source = 1;
        public const int Form = 2;
        public const int Server = 3;
        public const int FieldSpec = 4;
        public const int Menu = 5;

    }
}
