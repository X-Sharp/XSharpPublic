////////////////////////////////////////////////////////////////////////////////
// AssemblyInfo.prg

#using System.Reflection
#using System.Runtime.InteropServices
#using System.Security

#include "..\..\SharedSource\BuildNumber.h" 
#include "..\..\SharedSource\RuntimeNames.h"  

[ASSEMBLY: AssemblyTitleAttribute( "VO-Compatible RDD Classes Library" )]
[ASSEMBLY: AssemblyDescriptionAttribute( "VO-Compatible RDD Classes" )]
[ASSEMBLY: AssemblyConfigurationAttribute( ASSEMBLY_CONFIGURATION )]
[ASSEMBLY: AssemblyCompanyAttribute( COMPANY_NAME )]
[ASSEMBLY: AssemblyProductAttribute( PRODUCT_NAME )]
[ASSEMBLY: AssemblyCopyrightAttribute( COPYRIGHT_STR )]
[ASSEMBLY: ComVisibleAttribute( FALSE )]
[ASSEMBLY: ClsCompliant( FALSE )]
//[assembly: AllowPartiallyTrustedCallersAttribute()]
[ASSEMBLY: AssemblyVersionAttribute( VERSION_NUMBER_STR )]
[ASSEMBLY: AssemblyFileVersionAttribute( FILEVERSION_NUMBER_STR )]
[ASSEMBLY: AssemblyInformationalVersionAttribute( INFORMATIONAL_NUMBER_STR )]
#ifdef __XSHARP_RT__
[ASSEMBLY: ImplicitNamespaceAttribute( VULCAN_VOSDK_NAMESPACE )]
#else
[ASSEMBLY: Vulcan.VulcanImplicitNamespaceAttribute( VULCAN_VOSDK_NAMESPACE )]
#endif

[MODULE: UnverifiableCodeAttribute()]
[ASSEMBLY: AllowPartiallyTrustedCallersAttribute()]
#ifdef __CLR4__
[ASSEMBLY: SecurityRulesAttribute (SecurityRuleSet.Level1)]
#endif
