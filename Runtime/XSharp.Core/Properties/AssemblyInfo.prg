//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System.Reflection
using System.Runtime.CompilerServices
using XSharp.Internal
using XSharp
//
// General Information about an assembly is controlled through the following
// set of attributes. Change these attribute values to modify the information
// associated with an assembly.
//
#ifndef NETNEXT
[assembly: AssemblyTitle("XSharp.Core")]
#endif
[assembly: AssemblyDescription("XSharp common runtime DLL (no XBase types)")]
// in the core dialect the next attributes are not included automatically
[assembly: ClassLibrary("XSharp.Core.Functions","XSharp")]
[assembly: ImplicitNamespace("XSharp")]
[assembly: ImplicitNamespace("XSharp.RDD")]
[assembly: CompilerVersionAttribute("X# "+__VERSION__ +" - dialect:"+__DIALECT__ )]
