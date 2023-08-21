// 759. ICE with - iif() statement
// /vo10+ /vo14+
FUNCTION Start() AS VOID
	? Foo.Bar()
	xAssert(Foo.Bar() == 0.85)

CLASS Foo
	STATIC METHOD Bar() AS USUAL
		LOCAL lSmall := TRUE AS LOGIC
		LOCAL u AS OBJECT
		u := 1.0 - iif(lSmall,0.15,0)
	RETURN u
END CLASS

PROC xAssert(l AS LOGIC) 
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"   
RETURN   

/*

error XS9999: An internal compiler error has occurred: 'Unexpected value 'UnconvertedConditionalOperator' of type 'LanguageService.CodeAnalysis.XSharp.BoundKind'',    at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitExpressionCore(BoundExpression expression, Boolean used) in C:\XSharp\Devcs9\Roslyn\Src\Compilers\CSharp\Portable\CodeGen\EmitExpression.cs:line 92
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitExpression(BoundExpression expression, Boolean used) in C:\XSharp\Devcs9\Roslyn\Src\Compilers\CSharp\Portable\CodeGen\EmitExpression.cs:line 63
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitArguments(ImmutableArray`1 arguments, ImmutableArray`1 parameters, ImmutableArray`1 argRefKindsOpt) in C:\XSharp\Devcs9\Roslyn\Src\Compilers\CSharp\Portable\CodeGen\EmitExpression.cs:line 875
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitCallExpression(BoundCall call, UseKind useKind) in C:\XSharp\Devcs9\Roslyn\Src\Compilers\CSharp\Portable\CodeGen\EmitExpression.cs:line 1668
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitExpression(BoundExpression expression, Boolean used) in C:\XSharp\Devcs9\Roslyn\Src\Compilers\CSharp\Portable\CodeGen\EmitExpression.cs:line 63
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitArguments(ImmutableArray`1 arguments, ImmutableArray`1 parameters, ImmutableArray`1 argRefKindsOpt) in C:\XSharp\Devcs9\Roslyn\Src\Compilers\CSharp\Portable\CodeGen\EmitExpression.cs:line 875
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitCallExpression(BoundCall call, UseKind useKind) in C:\XSharp\Devcs9\Roslyn\Src\Compilers\CSharp\Portable\CodeGen\EmitExpression.cs:line 1668
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitExpression(BoundExpression expression, Boolean used) in C:\XSharp\Devcs9\Roslyn\Src\Compilers\CSharp\Portable\CodeGen\EmitExpression.cs:line 63
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitConversionExpression(BoundConversion conversion, Boolean used) in C:\XSharp\Devcs9\Roslyn\Src\Compilers\CSharp\Portable\CodeGen\EmitConversion.cs:line 63
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitExpression(BoundExpression expression, Boolean used) in C:\XSharp\Devcs9\Roslyn\Src\Compilers\CSharp\Portable\CodeGen\EmitExpression.cs:line 63
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitAssignmentExpression(BoundAssignmentOperator assignmentOperator, UseKind useKind) in C:\XSharp\Devcs9\Roslyn\Src\Compilers\CSharp\Portable\CodeGen\EmitExpression.cs:line 2063
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitExpressionCoreWithStackGuard(BoundExpression expression, Boolean used) in C:\XSharp\Devcs9\Roslyn\Src\Compilers\CSharp\Portable\CodeGen\EmitExpression.cs:line 79
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitExpression(BoundExpression expression, Boolean used) in C:\XSharp\Devcs9\Roslyn\Src\Compilers\CSharp\Portable\CodeGen\EmitExpression.cs:line 70
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitStatement(BoundStatement statement) in C:\XSharp\Devcs9\Roslyn\Src\Compilers\CSharp\Portable\CodeGen\EmitStatement.cs:line 59
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitStatementAndCountInstructions(BoundStatement statement) in C:\XSharp\Devcs9\Roslyn\Src\Compilers\CSharp\Portable\CodeGen\EmitStatement.cs:line 121
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitSequencePointStatement(BoundSequencePoint node) in C:\XSharp\Devcs9\Roslyn\Src\Compilers\CSharp\Portable\CodeGen\CodeGenerator.cs:line 381
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitStatement(BoundStatement statement) in C:\XSharp\Devcs9\Roslyn\Src\Compilers\CSharp\Portable\CodeGen\EmitStatement.cs:line 39
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitStatements(ImmutableArray`1 statements) in C:\XSharp\Devcs9\Roslyn\Src\Compilers\CSharp\Portable\CodeGen\EmitStatement.cs:line 663
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitBlock(BoundBlock block) in C:\XSharp\Devcs9\Roslyn\Src\Compilers\CSharp\Portable\CodeGen\EmitStatement.cs:line 644
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitStatement(BoundStatement statement) in C:\XSharp\Devcs9\Roslyn\Src\Compilers\CSharp\Portable\CodeGen\EmitStatement.cs:line 31
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitStatementList(BoundStatementList list) in C:\XSharp\Devcs9\Roslyn\Src\Compilers\CSharp\Portable\CodeGen\EmitStatement.cs:line 126
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitStatement(BoundStatement statement) in C:\XSharp\Devcs9\Roslyn\Src\Compilers\CSharp\Portable\CodeGen\EmitStatement.cs:line 63
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.GenerateImpl() in C:\XSharp\Devcs9\Roslyn\Src\Compilers\CSharp\Portable\CodeGen\CodeGenerator.cs:line 282
   at LanguageService.CodeAnalysis.XSharp.MethodCompiler.GenerateMethodBody(PEModuleBuilder moduleBuilder, MethodSymbol method, Int32 methodOrdinal, BoundStatement block, ImmutableArray`1 lambdaDebugInfo, ImmutableArray`1 closureDebugInfo, StateMachineTypeSymbol stateMachineTypeOpt, VariableSlotAllocator variableSlotAllocatorOpt, DiagnosticBag diagnostics, DebugDocumentProvider debugDocumentProvider, ImportChain importChainOpt, Boolean emittingPdb, Boolean emitTestCoverageData, ImmutableArray`1 dynamicAnalysisSpans, AsyncForwardEntryPoint entryPointOpt) in C:\XSharp\Devcs9\Roslyn\Src\Compilers\CSharp\Portable\Compiler\MethodCompiler.cs:line 1509
   at LanguageService.CodeAnalysis.XSharp.MethodCompiler.CompileMethod(MethodSymbol methodSymbol, Int32 methodOrdinal, ProcessedFieldInitializers& processedInitializers, SynthesizedSubmissionFields previousSubmissionFields, TypeCompilationState compilationState) in C:\XSharp\Devcs9\Roslyn\Src\Compilers\CSharp\Portable\Compiler\MethodCompiler.cs:line 1254


*/
