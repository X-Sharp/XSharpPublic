/*
   Copyright 2016-2017 XSharp B.V.

Licensed under the X# compiler source code License, Version 1.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.xsharp.info/licenses

Unless required by applicable law or agreed to in writing, software
Distributed under the License is distributed on an "as is" basis,
without warranties or conditions of any kind, either express or implied.
See the License for the specific language governing permissions and   
limitations under the License.
*/
using System.Reflection;

// General Information about an assembly is controlled through the following 
// set of attributes. Change these attribute values to modify the information
// associated with an assembly.
#if DEBUG
[assembly: AssemblyConfiguration("Debug")]
#else
[assembly: AssemblyConfiguration("Release")]
#endif	
[assembly: AssemblyProduct(XSharp.Constants.ProductName)]
[assembly: AssemblyCompany(XSharp.Constants.Company)]
[assembly: AssemblyCopyright(XSharp.Constants.Copyright)]
[assembly: AssemblyCulture("")]

[assembly: AssemblyVersion(XSharp.Constants.Version)]
[assembly: AssemblyFileVersion(XSharp.Constants.Version)]
