# XSharpDev
XSharp Development branch

This is the development branch of the Compiler.
Apart from the compiler this repository also has the source to:
- Documentation
- Installer

The Roslyn folder contains (modified) source from Roslyn
The XSharp folder contains our own source for the compiler, documentation  and other components

We have tried to minimize the changes to the Roslyn code. 
All code changes are marked with #if XSHARP

For the build process of the compiler we create our own "specialized" version of the CSharp Compiler. 
The source for this compiler and codeanalysis.dll is in the Tools folder.
This compiler will translate some of the Roslyn Namespaces from <something>CSharp into <Something>XSharp to
prevent name conflicts when assemblies of both origin are in memory at the same time

