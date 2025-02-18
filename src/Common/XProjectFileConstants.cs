//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#pragma warning disable RS0016
namespace XSharp.VisualStudio.Project
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

        public const string BaseInputPaths = nameof(BaseInputPaths);
        public const string Cultures = nameof(Cultures);
        public const string DefineDebugConstant = nameof(DefineDebugConstant);
        public const string DefineConstants = nameof(DefineConstants);
        public const string DevEnvDir = nameof(DevEnvDir);
        public const string GetTargetPath = nameof(GetTargetPath);
        public const string IncludeSearchPaths = nameof(IncludeSearchPaths);
        public const string IntermediateOutputPath = nameof(IntermediateOutputPath);
        public const string OutputName = nameof(OutputName);
        public const string OutputPath = nameof(OutputPath);
        public const string OutputType = nameof(OutputType);
        public const string PostBuildEvent = nameof(PostBuildEvent);
        public const string Project = nameof(Project);
        public const string ProjectFiles = nameof(ProjectFiles);
        public const string ProjectView = nameof(ProjectView);
        public const string PropertyGroup = nameof(PropertyGroup);
        public const string PreBuildEvent = nameof(PreBuildEvent);
        public const string ReferencePaths = nameof(ReferencePaths);
        public const string RunPostBuildEvent = nameof(RunPostBuildEvent);
        public const string SccProjectName = nameof(SccProjectName);
        public const string SccLocalPath = nameof(SccLocalPath);
        public const string SccAuxPath = nameof(SccAuxPath);
        public const string SccProvider = nameof(SccProvider);
        public const string ShowAllFiles = nameof(ShowAllFiles);
        public const string ShowSourceTrace = nameof(ShowSourceTrace);
        public const string SolutionDir = nameof(SolutionDir);
        public const string SolutionExt = nameof(SolutionExt);
        public const string SolutionName = nameof(SolutionName);
        public const string SolutionFileName = nameof(SolutionFileName);
        public const string SolutionPath = nameof(SolutionPath);
        public const string ToolsVersion = nameof(ToolsVersion);
        public const string TreatWarningsAsErrors = nameof(TreatWarningsAsErrors);
        public const string VerboseOutput = nameof(VerboseOutput);
        public const string WarningLevel = nameof(WarningLevel);

    }
}
