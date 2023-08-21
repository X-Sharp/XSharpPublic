// 737. error XS9999: An internal compiler error has occurred: 'Unexpected value 'Worst' of type 'LanguageService.CodeAnalysis.XSharp.MemberResolutionKind''
// The compiler has been changed now to make sure that /vo7 does not affect 
// argument - parameter matching when one or both methods have a PARAMS array
// /vo7+ must be enabled!
FUNCTION Start() AS VOID STRICT
	xAssert(Test.Exec("MyFunc", 1) == 42)
	RETURN
	
PUBLIC STATIC CLASS Test

	PUBLIC STATIC METHOD Exec(cName AS STRING, uReturnValue OUT USUAL, aParameters PARAMS OBJECT[]) AS LOGIC
		uReturnValue := NIL
		RETURN TRUE

	PUBLIC STATIC METHOD Exec(cName AS STRING, aParameters PARAMS OBJECT[]) AS USUAL
	    XAssert(cName == "MyFunc")
	    XAssert((INT) aParameters[1] == 1)
		RETURN 42

END CLASS
                 
PROC xAssert(l AS LOGIC) 
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"   
RETURN   

/*
error XS9999: An internal compiler error has occurred: 'Unexpected value 'Worst' of type 'LanguageService.CodeAnalysis.XSharp.MemberResolutionKind'',    at LanguageService.CodeAnalysis.XSharp.OverloadResolutionResult`1.ReportDiagnostics[T](Binder binder, Location location, SyntaxNode nodeOpt, DiagnosticBag diagnostics, String name, BoundExpression receiver, SyntaxNode invokedExpression, AnalyzedArguments arguments, ImmutableArray`1 memberGroup, NamedTypeSymbol typeContainingConstructor, NamedTypeSymbol delegateTypeBeingInvoked, XSharpSyntaxNode queryClause, Boolean isMethodGroupConversion, Nullable`1 returnRefKind, TypeSymbol delegateType) in D:\a\XSharpDev\XSharpDev\Roslyn\Src\Compilers\CSharp\Portable\Binder\Semantics\OverloadResolution\OverloadResolutionResult.cs:line 210
   at LanguageService.CodeAnalysis.XSharp.Binder.BindInvocationExpressionContinued(SyntaxNode node, SyntaxNode expression, String methodName, OverloadResolutionResult`1 result, AnalyzedArguments analyzedArguments, MethodGroup methodGroup, NamedTypeSymbol delegateTypeOpt, DiagnosticBag diagnostics, XSharpSyntaxNode queryClause) in D:\a\XSharpDev\XSharpDev\Roslyn\Src\Compilers\CSharp\Portable\Binder\Binder_Invocation.cs:line 1002
   at LanguageService.CodeAnalysis.XSharp.Binder.BindMethodGroupInvocation(SyntaxNode syntax, SyntaxNode expression, String methodName, BoundMethodGroup methodGroup, AnalyzedArguments analyzedArguments, DiagnosticBag diagnostics, XSharpSyntaxNode queryClause, Boolean allowUnexpandedForm) in D:\a\XSharpDev\XSharpDev\Roslyn\Src\Compilers\CSharp\Portable\Binder\Binder_Invocation.cs:line 709
   at LanguageService.CodeAnalysis.XSharp.Binder.BindInvocationExpression(SyntaxNode node, SyntaxNode expression, String methodName, BoundExpression boundExpression, AnalyzedArguments analyzedArguments, DiagnosticBag diagnostics, XSharpSyntaxNode queryClause, Boolean allowUnexpandedForm) in D:\a\XSharpDev\XSharpDev\Roslyn\Src\Compilers\CSharp\Portable\Binder\Binder_Invocation.cs:line 272
   at LanguageService.CodeAnalysis.XSharp.Binder.BindXsInvocationExpression(InvocationExpressionSyntax node, DiagnosticBag diagnostics) in D:\a\XSharpDev\XSharpDev\XSharp\src\Compiler\XSharpCodeAnalysis\Binder\Binder_Invocation.cs:line 63
   at LanguageService.CodeAnalysis.XSharp.Binder.BindExpressionInternal(ExpressionSyntax node, DiagnosticBag diagnostics, Boolean invoked, Boolean indexed) in D:\a\XSharpDev\XSharpDev\Roslyn\Src\Compilers\CSharp\Portable\Binder\Binder_Expressions.cs:line 397
   at LanguageService.CodeAnalysis.XSharp.Binder.BindExpression(ExpressionSyntax node, DiagnosticBag diagnostics, Boolean invoked, Boolean indexed) in D:\a\XSharpDev\XSharpDev\Roslyn\Src\Compilers\CSharp\Portable\Binder\Binder_Expressions.cs:line 334
   at LanguageService.CodeAnalysis.XSharp.Binder.BindExpressionStatement(XSharpSyntaxNode node, ExpressionSyntax syntax, Boolean allowsAnyExpression, DiagnosticBag diagnostics) in D:\a\XSharpDev\XSharpDev\Roslyn\Src\Compilers\CSharp\Portable\Binder\Binder_Statements.cs:line 577
   at LanguageService.CodeAnalysis.XSharp.Binder.BindExpressionStatement(ExpressionStatementSyntax node, DiagnosticBag diagnostics) in D:\a\XSharpDev\XSharpDev\Roslyn\Src\Compilers\CSharp\Portable\Binder\Binder_Statements.cs:line 570
   at LanguageService.CodeAnalysis.XSharp.Binder.BindStatement(StatementSyntax node, DiagnosticBag diagnostics) in D:\a\XSharpDev\XSharpDev\Roslyn\Src\Compilers\CSharp\Portable\Binder\Binder_Statements.cs:line 52
   at LanguageService.CodeAnalysis.XSharp.Binder.BindBlockParts(BlockSyntax node, DiagnosticBag diagnostics) in D:\a\XSharpDev\XSharpDev\Roslyn\Src\Compilers\CSharp\Portable\Binder\Binder_Statements.cs:line 1650
   at LanguageService.CodeAnalysis.XSharp.Binder.BindStatement(StatementSyntax node, DiagnosticBag diagnostics) in D:\a\XSharpDev\XSharpDev\Roslyn\Src\Compilers\CSharp\Portable\Binder\Binder_Statements.cs:line 43
   at LanguageService.CodeAnalysis.XSharp.Binder.BindBlockParts(BlockSyntax node, DiagnosticBag diagnostics) in D:\a\XSharpDev\XSharpDev\Roslyn\Src\Compilers\CSharp\Portable\Binder\Binder_Statements.cs:line 1650
   at LanguageService.CodeAnalysis.XSharp.Binder.BindTryStatement(TryStatementSyntax node, DiagnosticBag diagnostics) in D:\a\XSharpDev\XSharpDev\Roslyn\Src\Compilers\CSharp\Portable\Binder\Binder_Statements.cs:line 2739
   at LanguageService.CodeAnalysis.XSharp.Binder.BindStatement(StatementSyntax node, DiagnosticBag diagnostics) in D:\a\XSharpDev\XSharpDev\Roslyn\Src\Compilers\CSharp\Portable\Binder\Binder_Statements.cs:line 94
   at LanguageService.CodeAnalysis.XSharp.Binder.BindBlockParts(BlockSyntax node, DiagnosticBag diagnostics) in D:\a\XSharpDev\XSharpDev\Roslyn\Src\Compilers\CSharp\Portable\Binder\Binder_Statements.cs:line 1650
   at LanguageService.CodeAnalysis.XSharp.Binder.BindStatement(StatementSyntax node, DiagnosticBag diagnostics) in D:\a\XSharpDev\XSharpDev\Roslyn\Src\Compilers\CSharp\Portable\Binder\Binder_Statements.cs:line 43
   at LanguageService.CodeAnalysis.XSharp.Binder.BindMethodBody(XSharpSyntaxNode declaration, BlockSyntax blockBody, ArrowExpressionClauseSyntax expressionBody, DiagnosticBag diagnostics) in D:\a\XSharpDev\XSharpDev\Roslyn\Src\Compilers\CSharp\Portable\Binder\Binder_Statements.cs:line 3146
   at LanguageService.CodeAnalysis.XSharp.Binder.BindMethodBody(XSharpSyntaxNode syntax, DiagnosticBag diagnostics) in D:\a\XSharpDev\XSharpDev\Roslyn\Src\Compilers\CSharp\Portable\Binder\Binder_Statements.cs:line 3103
   at LanguageService.CodeAnalysis.XSharp.MethodCompiler.BindMethodBody(MethodSymbol method, TypeCompilationState compilationState, DiagnosticBag diagnostics, ImportChain& importChain, Boolean& originalBodyNested, ValueTuple`3& forSemanticModel) in D:\a\XSharpDev\XSharpDev\Roslyn\Src\Compilers\CSharp\Portable\Compiler\MethodCompiler.cs:line 1641
   at LanguageService.CodeAnalysis.XSharp.MethodCompiler.CompileMethod(MethodSymbol methodSymbol, Int32 methodOrdinal, ProcessedFieldInitializers& processedInitializers, SynthesizedSubmissionFields previousSubmissionFields, TypeCompilationState compilationState) in D:\a\XSharpDev\XSharpDev\Roslyn\Src\Compilers\CSharp\Portable\Compiler\MethodCompiler.cs:line 948



*/

