//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

namespace Microsoft.VisualStudio.Project
{
    using Microsoft.Build.Tasks;
    using System;

    /// <summary>
    /// Contains constants for MSBuild .xsproj and .user files.
    /// </summary>
    //// Keep these in alphabetical order for easy referencing.
    public static class XProjectFileConstants
    {
        public const int UnspecifiedValue = -1;

        public const string BaseInputPaths = "BaseInputPaths";
        public const string Cultures = "Cultures";
        public const string DefineDebugConstant = "DefineDebugConstant";
        public const string DefineConstants = "DefineConstants";
        public const string DevEnvDir = "DevEnvDir";
        public const string GetTargetPath = "GetTargetPath";
        public const string IncludeSearchPaths = "IncludeSearchPaths";
        public const string IntermediateOutputPath = "IntermediateOutputPath";
        public const string OutputName = "OutputName";
        public const string OutputPath = "OutputPath";
        public const string OutputType = "OutputType";
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
        public const string ToolsVersion = "ToolsVersion";
        public const string TreatWarningsAsErrors = "TreatWarningsAsErrors";
        public const string VerboseOutput = "VerboseOutput";
        public const string WarningLevel = "WarningLevel";

    }
}
