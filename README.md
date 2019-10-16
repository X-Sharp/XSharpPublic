# XSharpDev
XSharp Development branch

This is the development branch of the Compiler.
Apart from the compiler this repository also has the source to:
- Documentation

The Roslyn folder contains (modified) source from Roslyn
The XSharp folder contains our own source for the compiler, documentation  and other components

We have tried to minimize the changes to the Roslyn code. 
All code changes are marked with #if XSHARP

For the build process of the compiler we create our own "specialized" version of the CSharp Compiler. 
The source for this compiler and codeanalysis.dll is in the Tools folder.
This compiler will translate some of the Roslyn Namespaces from <something>CSharp into <Something>XSharp to
prevent name conflicts when assemblies of both origin are in memory at the same time

After retrieving this source code, you need to perform the following steps to be able to compile your XSharp Compiler:

- Open a VS (2015 or 2017) developers command prompt
- Goto the Roslyn subfolder
- Run Restore.Cmd to restore the nuget packages
- (Optionally) build the Roslyn binaries. You can run
  x MsBuild Compilers.sln to build just the compilers
  x MsBuild Workspaces.sln to build the VS integration
  x MsBuild Roslyn.sln to build everything
- Then navigate to the XSharp folder
- Run Restore.cmd to restore the nuget packages. This will also call Rebuild.Cmd that will build a Debug AND Release version.
- If you want to you can also build either for Debug, Public or for Release with the build.cmd. Add the configuration name: Build Debug.
  The separate Build.cmd will not clean old results and will only build changed code.
  In all cases the log file of the build process will be written into build-debug.log  or build-release.log 
  


