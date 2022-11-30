//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.

////////////////////////////////////////////////////////////////////////////////
// SDKAssemblyInfo.prg


using System.Reflection
using System.Runtime.InteropServices
using System.Security

#include "BuildNumber.h"    
#undef  COPYRIGHT_STR
#define COPYRIGHT_STR "Copyright © 1993-2022 Computer Associates & XSharp BV, All rights reserved"

[assembly: AssemblyConfigurationAttribute( ASSEMBLY_CONFIGURATION )] 
[assembly: AssemblyCompanyAttribute( COMPANY_NAME )]
[assembly: AssemblyProductAttribute( PRODUCT_NAME )]
[assembly: AssemblyCopyrightAttribute( COPYRIGHT_STR )]
[assembly: ComVisibleAttribute( FALSE )] 
[assembly: CLSCompliant( FALSE )]
//[assembly: AllowPartiallyTrustedCallersAttribute()]
[assembly: AssemblyVersionAttribute( VERSION_NUMBER )]
[assembly: AssemblyInformationalVersionAttribute( INFORMATIONAL_NUMBER )]
[assembly: AssemblyFileVersionAttribute( FILEVERSION_NUMBER )]
[assembly: ImplicitNamespaceAttribute( "XSharp.VO" )]
[assembly: AllowPartiallyTrustedCallersAttribute()]
[assembly: SecurityRulesAttribute (SecurityRuleSet.Level1)]

