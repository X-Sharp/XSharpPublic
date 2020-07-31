# XSharpDev
XSharp Compiler repository

This repository contains the source code to the X# compiler
Apart from the compiler this repository also has the source to:
- Documentation

The source code for the Runtime, VsIntegration and tools is on https://github.com/X-Sharp/XSharpPublic/

The Roslyn folder contains (modified) source from Roslyn
The XSharp folder contains our own source for the compiler, documentation  and other components

We have tried to minimize the changes to the Roslyn code. 
All code changes are marked with #if XSHARP

For the build process of the compiler we create our own "specialized" version of the CSharp Compiler. 
The source for this compiler and codeanalysis.dll is in the Tools folder.
This compiler will translate some of the Roslyn Namespaces from <something>CSharp into <Something>XSharp to
prevent name conflicts when assemblies of both origin are in memory at the same time

After retrieving this source code, you need to perform the following steps to be able to compile your XSharp Compiler:

- Open a VS (2017 or 2019) developers command prompt
- Goto the Roslyn subfolder
- Run Restore.Cmd to restore the nuget packages
- (Optionally) build the Roslyn binaries. You can run
  - MsBuild Compilers.sln to build just the compilers
  - MsBuild Workspaces.sln to build the VS integration
  - MsBuild Roslyn.sln to build everything
- Then navigate to the XSharp folder
- Run Restore.cmd to restore the nuget packages. This will also call Rebuild.Cmd that will build a Debug AND Release version.
- If you want to you can also build either for Debug, Public or for Release with the build.cmd. Add the configuration name: Build Debug.
  The separate Build.cmd will not clean old results and will only build changed code.
  In all cases the log file of the build process will be written into build-debug.log  or build-release.log 
  
The source code is available under the Apache 2 license that you can find in the root of this repository:
https://github.com/X-Sharp/XSharpDev/blob/master/license.txt.

Of course we welcome all additions, bug fixes etc.


Additional notes:
- to open the projects in Visual Studio, you MUST have the correct .Net Core version loaded on your machine 
  The version number for this SDK is specified in the global.json files inside the Roslyn and XSharp folders. 
  At this moment this version is 2.1.500
  This version number matches the version number in Roslyn\build\Targets\Tools.props and Roslyn\build\Targets\Tools.props.
- The CI build process for Roslyn does not use this setting. This process uses the version defined in Roslyn\build\Targets\Tools.props 
  and downloads a copy of .Net core into the Roslyn\Binaries\Tools\dotnet folder. The dotnet.exe file in this folder is used for the CI build.
- The Build scripts in the XSharp subfolder add the Roslyn\Binaries\Tools\dotnet folder to the path and then call dotnet to restore the files.
  This will make sure that the SDK of the right version is found.
  If this somehow fails, then you can set an environment variable SET COREHOST_TRACE=1. This will show where dotnet.exe is looking for the sdk.
    
To open the current projects in Visual Studio you need to install .Net Core 2.1.500 (this is also the version in global.json) from:
https://dotnet.microsoft.com/download/dotnet-core/2.1  

  
  
  

