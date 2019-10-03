//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Reflection
USING System.Runtime.CompilerServices

// General Information about an assembly is controlled through the following
// set of attributes. Change these attribute values to modify the information
// associated with an assembly.
#ifdef DEBUG
    [Assembly: AssemblyConfiguration("Debug")]
#else
    [Assembly: AssemblyConfiguration("Release")]
#endif
[Assembly: AssemblyProduct(XSharp.Constants.Product)]
[Assembly: AssemblyCompany(XSharp.Constants.Company)]
[Assembly: AssemblyCopyright(XSharp.Constants.Copyright)]
[Assembly: AssemblyCulture("")]

[Assembly: AssemblyVersion(XSharp.Constants.Version)]
[Assembly: AssemblyFileVersion(XSharp.Constants.FileVersion)]
[Assembly: AssemblyInformationalVersion(XSharp.Constants.InformationalVersion)]
