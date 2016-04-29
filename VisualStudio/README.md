# XSharpPublic
The Visual Studio integration in this version of XSharp is based on the MPF source. That source can be found on CodePlex.
All the code in the ProjectBase folder comes from this source.

We also looked at and were inspired by the Visual Studio integration from the Nemerle Project, IronPython, VFPX and others.

For our Build System we have also looked at the Microsoft Build System source (https://github.com/Microsoft/msbuild).
Some elements from that code have been reused in our code.

The CodeDom provider and Colorizer use the XSharp.CodeAnalysis.DLL as an external reference. 
They use this DLL to parse the source code and build an in-memory representation (CodeDomProvider) or color the tokens in 
the editor buffer in the right color. This DLL is included in Binary Form and will be updated by the Development team from
time to time, when the compiler changes.

