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
#ifdef __XSHARP_RT__
[assembly: ImplicitNamespaceAttribute( "VO" )]
#else
[assembly: Vulcan.VulcanImplicitNamespaceAttribute( "VO" )]
#endif

[assembly: AllowPartiallyTrustedCallersAttribute()]
#ifdef __CLR4__
[assembly: SecurityRulesAttribute (SecurityRuleSet.Level1)]
#endif
