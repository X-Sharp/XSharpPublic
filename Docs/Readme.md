This repository consists of seperate folder structures with sourcecode:
* The **Roslyn** folders contain a copy of the Roslyn source code, in which we have made a few modifications
* The **XSharp** folders contain the project files that are used to build the X# compiler and its tools and some **extra** source files that are used to add or change functionality from the Roslyn compiler.
* The **Test** folders contain source code to several X# compiler tests

All changes the the Roslyn folder tree are marked in the source files with an `#if XSHARP` or `#if !XSHARP` region. One or two files also have an `#if XSHARPPRE` region.
When updating this source tree from the Roslyn tree you can safely replace / delete all CS files that do not contain the word XSHARP.
The Roslyn tree also contains Visual Basic files and translation fles (xlf files). These files are not used by us.

There are a few files from the Roslyn tree that are duplicated in the **XSharp** tree because many changes were made to them. These files are:
in the XSharp\src\Compiler\XSharpCodeAnalysis folder:

* ErrorCode.cs
* XSharpResources.resx 
* CodeAnalysisResources.resx 


From the **Roslyn** tree we are using the following folders in the source for the compiler:
* Roslyn\src\Compilers\Core
* Roslyn\src\Compilers\CSharp
* Roslyn\src\Compilers\Server
* Roslyn\src\Compilers\Shared
* 