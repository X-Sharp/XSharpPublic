# XSharpPublic/src

The Roslyn folder contains (modified) source from Roslyn
The Compiler folder contains our own source for the compiler, documentation  and other components
The Runtime folder contains the source code to our Runtime
The VsIntegration folder contains the source code to our VS Integration
The Tools folder contains the source code to some tools

#Changes to the Roslyn code for the compiler 
We have tried to minimize the changes to the Roslyn code. 
All code changes are marked with #if XSHARP

For the build process of the compiler we create our own "specialized" version of the CSharp Compiler. 
The source for this compiler and codeanalysis.dll is in the Tools folder.
This compiler will translate some of the Roslyn Namespaces from <something>CSharp into <Something>XSharp to
prevent name conflicts when assemblies of both origin are in memory at the same time

After retrieving this source code, you need to perform the following steps to be able to compile your XSharp Compiler:

- Make sure you have VS 2022 installed. The preferred edition is Enterprise. It also works on VS 2019
- Make sure you have Java JDK on your machine and that you have the Java binaries folder in your Path. Java can be downloaded from https://jdk.java.net/.
- Goto the root of the repository
- If you do not have Vs 2022 Enterprise you can set 2 environment variables to the VS version and edition you have installed
  SET VSVERSION=2019  (2017 also works)
  SET VSEDITION=Enterprise (or whichever edition you have installed i.e. Professional, Community, BuildTools)
- Run the cmd file ContinuousIntegrationBuild.cmd 
  This will:
  - download a .Net Core version in the Roslyn\Binaries folder
  - restore missing nuget packages
  - switch to the XSharp Folder
  - Restore the nuget packages for the X# projects
  - Build all configurations for the X# projects.
  - The output will be in the Binaries folder
  - The log file of the build process will be written into build-debug.log, build-public.log  and/or build-release.log 
  
- Later you can build a specific version of the X# compiler by running the build.cmd file in the XSharp folder. You will have  
  to pass it an argument. Valid arguments are Debug, Release, Public or All.  
  
The source code is available under the Apache 2 license that you can find in the root of this repository:
https://github.com/X-Sharp/XSharpDev/blob/master/license.txt.

Of course we welcome all additions, bug fixes etc.

Additional notes:
- to open the projects in Visual Studio, you MUST have the correct .Net Core version loaded on your machine 
  The version number for this SDK is specified in the global.json files inside the Roslyn and XSharp folders. 
  At this moment this version is 5.0.102
  This version number matches the version number in Roslyn\build\Targets\Tools.props and Roslyn\build\Targets\Tools.props.
- The CI build process for Roslyn does not use this setting. This process uses the version defined in Roslyn\build\Targets\Tools.props 
  and downloads a copy of .Net core into the Roslyn\Binaries\Tools\dotnet folder. The dotnet.exe file in this folder is used for the CI build.
- The Build scripts in the XSharp subfolder add the Roslyn\Binaries\Tools\dotnet folder to the path and then call dotnet to restore the files.
  This will make sure that the SDK of the right version is found.
  If this somehow fails, then you can set an environment variable SET COREHOST_TRACE=1. This will show where dotnet.exe is looking for the sdk.
    
To open the current projects in Visual Studio you need to install .Net 5.0.102 (this is also the version in global.json in the XSharp and Roslyn folders)


Results of automated builds:
![ContinuousBuild](https://github.com/X-Sharp/XSharpPublic/workflows/ContinuousBuild/badge.svg?branch=master)  

