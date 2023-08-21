// 623. Incorrect syntax in LOCAL with generic arguments crasshes the compiler

USING System.Collections.Generic

FUNCTION Start() AS VOID
	LOCAL list := List<STRING> AS List<STRING>
	LOCAL o := TestClass<INT> AS TestClass<INT>
RETURN                                        

CLASS TestClass
END CLASS

/*
---------------------------
Assertion Failed: Abort=Quit, Retry=Debug, Ignore=Continue
---------------------------
   at LanguageService.CodeAnalysis.XSharp.Binder.BindXSIdentifier(SimpleNameSyntax node, Boolean invoked, DiagnosticBag diagnostics, Boolean bindMethod)
   at LanguageService.CodeAnalysis.XSharp.Binder.BindExpressionInternal(ExpressionSyntax node, DiagnosticBag diagnostics, Boolean invoked, Boolean indexed)
   at LanguageService.CodeAnalysis.XSharp.Binder.BindExpression(ExpressionSyntax node, DiagnosticBag diagnostics, Boolean invoked, Boolean indexed)
   at LanguageService.CodeAnalysis.XSharp.Binder.BindValue(ExpressionSyntax node, DiagnosticBag diagnostics, BindValueKind valueKind)
   at LanguageService.CodeAnalysis.XSharp.Binder.BindPossibleArrayInitializer(ExpressionSyntax node, TypeSymbol destinationType, BindValueKind valueKind, DiagnosticBag diagnostics)
   at LanguageService.CodeAnalysis.XSharp.Binder.BindVariableDeclaration(SourceLocalSymbol localSymbol, LocalDeclarationKind kind, Boolean isVar, VariableDeclaratorSyntax declarator, TypeSyntax typeSyntax, TypeSymbol declTypeOpt, AliasSymbo......
<truncated>
---------------------------
Abort   Retry   Ignore   
---------------------------
*/
