//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System.Reflection;

// General Information about an assembly is controlled through the following
// set of attributes. Change these attribute values to modify the information
// associated with an assembly.
#if DEBUG
[assembly: AssemblyConfiguration("Debug")]
#else
[assembly: AssemblyConfiguration("Release")]
#endif
[assembly: AssemblyProduct(XSharp.Constants.Product)]
[assembly: AssemblyCompany(XSharp.Constants.Company)]
[assembly: AssemblyCopyright(XSharp.Constants.Copyright)]
[assembly: AssemblyCulture("")]

[assembly: AssemblyVersion(XSharp.Constants.Version)]
[assembly: AssemblyFileVersion(XSharp.Constants.Version)]
