// 550. Assertion Failed: passing args byref should not clone them into temps
/*
When /vo7 is disabled, the following reports correctly:

error XS1510: A ref or out value must be an assignable variable

When /vo7+ is enabled though, an assertion failed dialog is shown.
selecting Ignore (or using the release version of the compiler) the compilation ends with
no errors reported, although it should had failed with the above error
*/
FUNCTION Start() AS VOID
	LOCAL n AS INT
	Default(@n , 123) // this should be an error
	? n
RETURN

/*
---------------------------
Assertion Failed: Abort=Quit, Retry=Debug, Ignore=Continue
---------------------------
passing args byref should not clone them into temps



   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitArgument(BoundExpression argument, RefKind refKind) in C:\XSharp\Dev\Roslyn\src\Compilers\CSharp\Portable\CodeGen\EmitExpression.cs:line 586

   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitArguments(ImmutableArray`1 arguments, ImmutableArray`1 parameters) in C:\XSharp\Dev\Roslyn\src\Compilers\CSharp\Portable\CodeGen\EmitExpression.cs:line 760

   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitCallExpression(BoundCall call, UseKind useKind) in C:\XSharp\Dev\Roslyn\src\Compilers\CSharp\Portable\CodeGen\EmitExpression.cs:line 1489

   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitExpressionCore(BoundExpression expression, Boolean used) in C:\XSharp\Dev\Roslyn\src\Compilers\CSharp\Portable\CodeGen\EmitExpression.cs:line 92

   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitExpressionCoreWithStackGuard(BoundExpressi......

<truncated>
---------------------------
Abort   Retry   Ignore   
---------------------------

*/
