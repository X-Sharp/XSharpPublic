# XSharpPublic/VisualStudio
The Visual Studio integration in this version of XSharp is based on the MPF source. That source can be found on CodePlex.
All the code in the ProjectBase folder comes from this source.
Some of this source has been altered to handle:
- The 'Show All Files' option in the menu
- Linked files
- Better support for dependent items

We also looked at and were inspired by the Visual Studio integration from:
- the Nemerle Project
- Python Toolk for VS
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
For Visual Studio 2017 you will need to load the EditorConfig LanguageService extension to enable this.
For Visual Studio 2015 you will need to load the EditorConfig extension to enable this.
For more info see https://editorconfig.org/
We intend to move the case synchronization setting to this file as well so you can easily ensure consistent case synchronization as well.