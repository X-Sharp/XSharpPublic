////////////////////////////////////////////////////////////////////////////////
// SDKAssemblyInfo.prg


#using System.Reflection
#using System.Runtime.InteropServices
#using System.Security

#include "BuildNumber.h" 


[assembly: AssemblyConfigurationAttribute( ASSEMBLY_CONFIGURATION )]
[assembly: AssemblyCompanyAttribute( COMPANY_NAME )]
[assembly: AssemblyProductAttribute( PRODUCT_NAME )]
[assembly: AssemblyCopyrightAttribute( COPYRIGHT_STR )]
[assembly: ComVisibleAttribute( FALSE )]
[assembly: CLSCompliant( FALSE )]
//[assembly: AllowPartiallyTrustedCallersAttribute()]
[assembly: AssemblyVersionAttribute( VERSION_NUMBER_STR )]
[assembly: AssemblyInformationalVersionAttribute( INFORMATIONAL_NUMBER_STR )]
[assembly: AssemblyFileVersionAttribute( FILEVERSION_NUMBER_STR )]
#ifdef __XSHARP_RT__
[assembly: ImplicitNamespaceAttribute( "VO" )]
#else
[assembly: Vulcan.VulcanImplicitNamespaceAttribute( "VO" )]
#endif

[module: UnverifiableCodeAttribute()]
[assembly: AllowPartiallyTrustedCallersAttribute()]
#ifdef __CLR4__
[assembly: SecurityRulesAttribute (SecurityRuleSet.Level1)]
#endif
