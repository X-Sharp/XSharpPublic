// 945. Internal compiler error calling a method from interface with missing out param
// https://github.com/X-Sharp/XSharpPublic/issues/1766

CLASS TestClass
	METHOD RefMethod(test REF INT) AS VOID
		test := 42
	METHOD OutMethod(test OUT STRING) AS VOID
		test := "42"
END CLASS

FUNCTION Start() AS VOID
	VAR o := TestClass{}
	o:RefMethod() // correct error XS7036: There is no argument given that corresponds to the required formal parameter 'test' of 'TestClassNew.RefMethod(ref int)'
	o:OutMethod() // no error, ICE

/*
error XS9999: An internal compiler error has occurred: 'Index was outside the bounds of the array.',    at LanguageService.CodeAnalysis.XSharp.DiagnosticsPass.XsVisitCall(BoundCall node) in D:\a\XSharpPublic\XSharpPublic\src\Compiler\src\Compiler\XSharpCodeAnalysis\Lowering\DiagnosticsPass.cs:line 182
   at LanguageService.CodeAnalysis.XSharp.DiagnosticsPass.VisitCall(BoundCall node) in D:\a\XSharpPublic\XSharpPublic\src\Roslyn\Src\Compilers\CSharp\Portable\Lowering\DiagnosticsPass_ExpressionTrees.cs:line 446
   at LanguageService.CodeAnalysis.XSharp.BoundTreeWalkerWithStackGuard.VisitExpressionWithoutStackGuard(BoundExpression node) in D:\a\XSharpPublic\XSharpPublic\src\Roslyn\Src\Compilers\CSharp\Portable\BoundTree\BoundTreeWalker.cs:line 96
   at LanguageService.CodeAnalysis.XSharp.BoundTreeVisitor.VisitExpressionWithStackGuard(BoundExpression node) in D:\a\XSharpPublic\XSharpPublic\src\Roslyn\Src\Compilers\CSharp\Portable\BoundTree\BoundTreeVisitors.cs:line 232
   at LanguageService.CodeAnalysis.XSharp.BoundTreeVisitor.VisitExpressionWithStackGuard(Int32& recursionDepth, BoundExpression node) in D:\a\XSharpPublic\XSharpPublic\src\Roslyn\Src\Compilers\CSharp\Portable\BoundTree\BoundTreeVisitors.cs:line 217
   at LanguageService.CodeAnalysis.XSharp.BoundTreeWalker.VisitExpressionStatement(BoundExpressionStatement node) in D:\a\XSharpPublic\XSharpPublic\src\Roslyn\Src\Compilers\CSharp\Portable\Generated\BoundNodes.xml.Generated.cs:line 9110
   at LanguageService.CodeAnalysis.XSharp.BoundTreeWalkerWithStackGuard.Visit(BoundNode node) in D:\a\XSharpPublic\XSharpPublic\src\Roslyn\Src\Compilers\CSharp\Portable\BoundTree\BoundTreeWalker.cs:line 86
   at LanguageService.CodeAnalysis.XSharp.BoundTreeWalker.VisitList[T](ImmutableArray`1 list) in D:\a\XSharpPublic\XSharpPublic\src\Roslyn\Src\Compilers\CSharp\Portable\BoundTree\BoundTreeWalker.cs:line 20
   at LanguageService.CodeAnalysis.XSharp.BoundTreeWalker.VisitBlock(BoundBlock node) in D:\a\XSharpPublic\XSharpPublic\src\Roslyn\Src\Compilers\CSharp\Portable\Generated\BoundNodes.xml.Generated.cs:line 9054
   at LanguageService.CodeAnalysis.XSharp.BoundTreeWalkerWithStackGuard.Visit(BoundNode node) in D:\a\XSharpPublic\XSharpPublic\src\Roslyn\Src\Compilers\CSharp\Portable\BoundTree\BoundTreeWalker.cs:line 86
   at LanguageService.CodeAnalysis.XSharp.DiagnosticsPass.IssueDiagnostics(XSharpCompilation compilation, BoundNode node, DiagnosticBag diagnostics, MethodSymbol containingSymbol) in D:\a\XSharpPublic\XSharpPublic\src\Roslyn\Src\Compilers\CSharp\Portable\Lowering\DiagnosticsPass_ExpressionTrees.cs:line 39
   at LanguageService.CodeAnalysis.XSharp.MethodCompiler.CompileMethod(MethodSymbol methodSymbol, Int32 methodOrdinal, ProcessedFieldInitializers& processedInitializers, SynthesizedSubmissionFields previousSubmissionFields, TypeCompilationState compilationState) in D:\a\XSharpPublic\XSharpPublic\src\Roslyn\Src\Compilers\CSharp\Portable\Compiler\MethodCompiler.cs:line 1075
*/
