// 131. Assertion failed
// at LanguageService.CodeAnalysis.XSharp.Symbols.OverriddenMethodTypeParameterMap.GetOverriddenMethod(SourceMemberMethodSymbol overridingMethod)

CLASS AssertionClass
METHOD GenericMethod<T>() AS VOID
	
METHOD OriginalReport<T>() AS T WHERE T IS NEW()
RETURN Default(T) 	
END CLASS

FUNCTION Start() AS VOID

RETURN
