//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
#include "buildnumber.h"
using System.Reflection
using System.Runtime.CompilerServices
USING XSharp.Internal
//
// General Information about an assembly is controlled through the following 
// set of attributes. Change these attribute values to modify the information
// associated with an assembly.
//
[assembly: AssemblyTitle("XSharp.RDD")] 
[assembly: AssemblyDescription("XSharp RDD DLL, holds all the standard RDDs as well as the Advantage RDDs")]

// in the core dialect the next attributes are not included automatically
[assembly: ImplicitNamespace("XSharp")]
[assembly: CompilerVersionAttribute("X# "+__VERSION__ +" - dialect:"+__DIALECT__ )] 
[assembly: ClassLibrary("XSharp.RDD.Functions","XSharp")]
[assembly: RuntimeCompatibility(WrapNonExceptionThrows := TRUE)]

