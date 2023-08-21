// 774. Compiler crash with namespace called xSharp
#pragma warnings(219, off) //   assigned but not used
BEGIN NAMESPACE xSharp.Something // compiler crash. "XSharp" works ok

CLASS Nothing
END CLASS

END NAMESPACE

FUNCTION Start( ) AS VOID
LOCAL o AS XSharp.Something.Nothing
o := XSharp.Something.Nothing{}
RETURN


/*
---------------------------
Assertion Failed: Abort=Quit, Retry=Debug, Ignore=Continue
---------------------------
   at LanguageService.CodeAnalysis.XSharp.Symbols.NamespaceOrTypeSymbol.LookupMetadataType(MetadataTypeName& emittedTypeName)
   at LanguageService.CodeAnalysis.XSharp.Symbols.NonMissingModuleSymbol.LookupTopLevelMetadataType(MetadataTypeName& emittedName)
   at LanguageService.CodeAnalysis.XSharp.Symbols.NonMissingAssemblySymbol.LookupTopLevelMetadataTypeWithCycleDetection(MetadataTypeName& emittedName, ConsList`1 visitedAssemblies, Boolean digThroughForwardedTypes)
   at LanguageService.CodeAnalysis.XSharp.Symbols.AssemblySymbol.LookupTopLevelMetadataType(MetadataTypeName& emittedName, Boolean digThroughForwardedTypes)
   at LanguageService.CodeAnalysis.XSharp.Symbols.AssemblySymbol.GetTopLevelTypeByMetadataName(AssemblySymbol assembly, MetadataTypeName& metadataName, AssemblyIdentity assemblyOpt)
   at LanguageService.CodeAnalysis.XSharp.Symbols.AssemblySymbol.GetTopLevelTypeByMetadataName(MetadataTypeName& metadataName, AssemblyIdentity assemblyOpt, Boolean includeReferences, Boolean isWellKno......
<truncated>
---------------------------
Abort   Retry   Ignore
---------------------------
*/
