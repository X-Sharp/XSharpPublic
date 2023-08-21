# XSharpPublic/VisualStudio
The Visual Studio integration in this version of XSharp is based on the MPF source. That source can be found on CodePlex.
All the code in the ProjectBase folder comes from this source.
Some of this source has been altered to handle:
- The 'Show All Files' option in the menu
- Linked files
- Better support for dependent items

We also looked at and were inspired by the Visual Studio integration from:
- the Nemerle Project
- Python Tools for VS
- NodeJs
- The VS Integration for WIX (Votive)
- VFPX and others.

For our Build System we have also looked at the Microsoft Build System source (https://github.com/Microsoft/msbuild).
Some elements from that code have been reused in our code.

The CodeDom provider and Colorizer use the XSharp.CodeAnalysis.DLL as an external reference.
They use this DLL to parse the source code and build an in-memory representation (CodeDomProvider) or color the tokens in
the editor buffer in the right color. This DLL is included in Binary Form and will be updated by the Development team from
time to time, when the compiler changes.


#Code formatting
We have added an .EditorConfig in the root of this folder. This helps to ensure consistent code formatting .
For Visual Studio 2017 and earlier you will need to load the EditorConfig LanguageService extension to enable this.
For more info see https://editorconfig.org/
We intend to move the case synchronization setting to this file as well so you can easily ensure consistent case synchronization as well.


External references
- We use the VisualStudio.Community.Toolkit versions 15 and 17. This toolkit automatically references the Visual Studio SDK package which brings in most of what we need.
- The NuGet packages for VS are either compatible with VS 2017 (version numbers that start with 15) or with VS 2022 (version numbers that start with 17)
  For Vs2022 we try to not use the "latest and greatest".
  there are Two exceptions:
  1) The VsLangProj150 package is used inside the VS 2017 compatible project but it has version 16.7.30328.74. The reason for this is that the package with number 
  15.0.26229 is incorrect and contains an interface definition for package references that is broken.
  2) The VSSDK.Debugger.VSDConfigTool is version 16.10.1051404. This however only runs on the developer machine, so this is harmless
  