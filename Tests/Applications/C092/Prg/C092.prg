// 92. assertion failed at LanguageService.CodeAnalysis.XSharp.Symbols.SourceMemberContainerTypeSymbol.CheckNonOverrideMember, SourceMemberContainerSymbol_ImplementationChecks.cs:line 817

// also no warnings, while a warning should be reported about hiding parent property
#pragma warnings(108, off) // name hids inherited member
CLASS ParentClass
	PROPERTY Name AS INT GET 0
END CLASS

CLASS ChildClass INHERIT ParentClass
	VIRTUAL PROPERTY Name AS INT GET 0
END CLASS

