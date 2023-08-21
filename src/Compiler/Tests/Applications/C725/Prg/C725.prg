// error XS9999: An internal compiler error has occurred
// Problem happens only when both /ins and /lb are enabled

FUNCTION Start() AS INT 
? Functions.Test()	// ok
? C725.exe.Functions.Test() // ok
Foo{}:Show()
RETURN 0  

CLASS Foo
METHOD Show() AS VOID
	? Functions.Test() // <--- this line causes the XS9999 
	? C725.exe.Functions.Test()  // ok
END CLASS 	
	 
FUNCTION Test() AS STRING 
	RETURN "xyz" 
	
/*
error XS9999: An internal compiler error has occurred: 'Unexpected value 'TypeExpression' of type 'LanguageService.CodeAnalysis.XSharp.BoundKind'',    at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitExpressionCore(BoundExpression expression, Boolean used) in C:\XSharp\Dev\Roslyn\Src\Compilers\CSharp\Portable\CodeGen\EmitExpression.cs:line 88
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitExpression(BoundExpression expression, Boolean used) in C:\XSharp\Dev\Roslyn\Src\Compilers\CSharp\Portable\CodeGen\EmitExpression.cs:line 59
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitConversionExpression(BoundConversion conversion, Boolean used) in C:\XSharp\Dev\Roslyn\Src\Compilers\CSharp\Portable\CodeGen\EmitConversion.cs:line 41
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitExpression(BoundExpression expression, Boolean used) in C:\XSharp\Dev\Roslyn\Src\Compilers\CSharp\Portable\CodeGen\EmitExpression.cs:line 59
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitArguments(ImmutableArray`1 arguments, ImmutableArray`1 parameters, ImmutableArray`1 argRefKindsOpt) in C:\XSharp\Dev\Roslyn\Src\Compilers\CSharp\Portable\CodeGen\EmitExpression.cs:line 846
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitCallExpression(BoundCall call, UseKind useKind) in C:\XSharp\Dev\Roslyn\Src\Compilers\CSharp\Portable\CodeGen\EmitExpression.cs:line 1638
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitExpression(BoundExpression expression, Boolean used) in C:\XSharp\Dev\Roslyn\Src\Compilers\CSharp\Portable\CodeGen\EmitExpression.cs:line 59
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitArguments(ImmutableArray`1 arguments, ImmutableArray`1 parameters, ImmutableArray`1 argRefKindsOpt) in C:\XSharp\Dev\Roslyn\Src\Compilers\CSharp\Portable\CodeGen\EmitExpression.cs:line 846
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitCallExpression(BoundCall call, UseKind useKind) in C:\XSharp\Dev\Roslyn\Src\Compilers\CSharp\Portable\CodeGen\EmitExpression.cs:line 1638
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitExpression(BoundExpression expression, Boolean used) in C:\XSharp\Dev\Roslyn\Src\Compilers\CSharp\Portable\CodeGen\EmitExpression.cs:line 59
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitArguments(ImmutableArray`1 arguments, ImmutableArray`1 parameters, ImmutableArray`1 argRefKindsOpt) in C:\XSharp\Dev\Roslyn\Src\Compilers\CSharp\Portable\CodeGen\EmitExpression.cs:line 846
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitCallExpression(BoundCall call, UseKind useKind) in C:\XSharp\Dev\Roslyn\Src\Compilers\CSharp\Portable\CodeGen\EmitExpression.cs:line 1638
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitExpressionCoreWithStackGuard(BoundExpression expression, Boolean used) in C:\XSharp\Dev\Roslyn\Src\Compilers\CSharp\Portable\CodeGen\EmitExpression.cs:line 75
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitExpression(BoundExpression expression, Boolean used) in C:\XSharp\Dev\Roslyn\Src\Compilers\CSharp\Portable\CodeGen\EmitExpression.cs:line 66
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitStatement(BoundStatement statement) in C:\XSharp\Dev\Roslyn\Src\Compilers\CSharp\Portable\CodeGen\EmitStatement.cs:line 41
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitStatementAndCountInstructions(BoundStatement statement) in C:\XSharp\Dev\Roslyn\Src\Compilers\CSharp\Portable\CodeGen\EmitStatement.cs:line 103
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitSequencePointStatement(BoundSequencePoint node) in C:\XSharp\Dev\Roslyn\Src\Compilers\CSharp\Portable\CodeGen\CodeGenerator.cs:line 356
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitStatement(BoundStatement statement) in C:\XSharp\Dev\Roslyn\Src\Compilers\CSharp\Portable\CodeGen\EmitStatement.cs:line 33
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitStatements(ImmutableArray`1 statements) in C:\XSharp\Dev\Roslyn\Src\Compilers\CSharp\Portable\CodeGen\EmitStatement.cs:line 645
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitBlock(BoundBlock block) in C:\XSharp\Dev\Roslyn\Src\Compilers\CSharp\Portable\CodeGen\EmitStatement.cs:line 626
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitStatement(BoundStatement statement) in C:\XSharp\Dev\Roslyn\Src\Compilers\CSharp\Portable\CodeGen\EmitStatement.cs:line 25
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitStatementList(BoundStatementList list) in C:\XSharp\Dev\Roslyn\Src\Compilers\CSharp\Portable\CodeGen\EmitStatement.cs:line 108
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitStatement(BoundStatement statement) in C:\XSharp\Dev\Roslyn\Src\Compilers\CSharp\Portable\CodeGen\EmitStatement.cs:line 45
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.GenerateImpl() in C:\XSharp\Dev\Roslyn\Src\Compilers\CSharp\Portable\CodeGen\CodeGenerator.cs:line 263
   at LanguageService.CodeAnalysis.XSharp.MethodCompiler.GenerateMethodBody(PEModuleBuilder moduleBuilder, MethodSymbol method, Int32 methodOrdinal, BoundStatement block, ImmutableArray`1 lambdaDebugInfo, ImmutableArray`1 closureDebugInfo, StateMachineTypeSymbol stateMachineTypeOpt, VariableSlotAllocator variableSlotAllocatorOpt, DiagnosticBag diagnostics, DebugDocumentProvider debugDocumentProvider, ImportChain importChainOpt, Boolean emittingPdb, Boolean emitTestCoverageData, ImmutableArray`1 dynamicAnalysisSpans) in C:\XSharp\Dev\Roslyn\Src\Compilers\CSharp\Portable\Compiler\MethodCompiler.cs:line 1440
   at LanguageService.CodeAnalysis.XSharp.MethodCompiler.CompileMethod(MethodSymbol methodSymbol, Int32 methodOrdinal, ProcessedFieldInitializers& processedInitializers, SynthesizedSubmissionFields previousSubmissionFields, TypeCompilationState compilationState) in C:\XSharp\Dev\Roslyn\Src\Compilers\CSharp\Portable\Compiler\MethodCompiler.cs:line 1192
*/
