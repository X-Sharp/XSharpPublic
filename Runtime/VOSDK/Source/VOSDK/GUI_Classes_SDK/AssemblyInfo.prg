////////////////////////////////////////////////////////////////////////////////
// AssemblyInfo.prg


#using System.Reflection
#using System.Runtime.InteropServices
#using System.Security

#include "..\..\SharedSource\BuildNumber.h" 
#include "..\..\SharedSource\RuntimeNames.h"  

[assembly: AssemblyTitleAttribute( "VO-Compatible GUI Classes Library" )]
[assembly: AssemblyDescriptionAttribute( "VO-Compatible GUI Classes" )]
[assembly: AssemblyConfigurationAttribute( ASSEMBLY_CONFIGURATION )]
[assembly: AssemblyCompanyAttribute( COMPANY_NAME )]
[assembly: AssemblyProductAttribute( PRODUCT_NAME )]
[assembly: AssemblyCopyrightAttribute( COPYRIGHT_STR )]
[assembly: ComVisibleAttribute( FALSE )]
[assembly: CLSCompliant( FALSE )]
//[assembly: AllowPartiallyTrustedCallersAttribute()]
[assembly: AssemblyVersionAttribute( VERSION_NUMBER_STR )]
[assembly: AssemblyInformationalVersionAttribute( VERSION_NUMBER_STR )]
[assembly: AssemblyFileVersionAttribute( FILEVERSION_NUMBER_STR )]
#ifdef __XSHARP_RT__
[assembly: ImplicitNamespaceAttribute( VULCAN_VOSDK_NAMESPACE )]
#else
[assembly: Vulcan.VulcanImplicitNamespaceAttribute( VULCAN_VOSDK_NAMESPACE )]
#endif

[module: UnverifiableCodeAttribute()]
[assembly: AllowPartiallyTrustedCallersAttribute()]
#ifdef __CLR4__
[assembly: SecurityRulesAttribute (SecurityRuleSet.Level1)]
#endif
