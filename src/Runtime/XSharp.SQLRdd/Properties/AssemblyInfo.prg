//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System.Reflection
using System.Runtime.CompilerServices
USING XSharp.Internal
//
// General Information about an assembly is controlled through the following 
// set of attributes. Change these attribute values to modify the information
// associated with an assembly.
//
[assembly: AssemblyTitle("XSharp.SQLRDD")] 
[assembly: AssemblyDescription("XSharp SQLRDD DLL, holds the SQL RDD")]

// in the core dialect the next attributes are not included automatically
[assembly: ImplicitNamespace("XSharp")]
[assembly: ImplicitNamespace("XSharp.ADS")]
[assembly: CompilerVersionAttribute("X# "+__VERSION__ +" - dialect:"+__DIALECT__ )] 
[assembly: ClassLibrary("XSharp.RDD.Functions","XSharp")]
[assembly: RuntimeCompatibility(WrapNonExceptionThrows := TRUE)]

