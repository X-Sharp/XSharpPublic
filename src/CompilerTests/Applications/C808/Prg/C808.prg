// 808. /fox2 switch causes an internal compiler error
// https://github.com/X-Sharp/XSharpPublic/issues/817
/*
error XS9999: An internal compiler error has occurred: 'Unexpected value 'ObjectInitializerMember' of type 'LanguageService.CodeAnalysis.XSharp.BoundKind'',    at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitExpressionCore(BoundExpression expression, Boolean used)
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitExpression(BoundExpression expression, Boolean used)
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitArguments(ImmutableArray`1 arguments, ImmutableArray`1 parameters, ImmutableArray`1 argRefKindsOpt)
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitCallExpression(BoundCall call, UseKind useKind)
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitExpression(BoundExpression expression, Boolean used)
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitAssignmentExpression(BoundAssignmentOperator assignmentOperator, UseKind useKind)
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitExpression(BoundExpression expression, Boolean used)
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitSideEffects(BoundSequence sequence)
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitSequenceExpression(BoundSequence sequence, Boolean used)
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitExpression(BoundExpression expression, Boolean used)
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitExpression(BoundExpression expression, Boolean used)
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitArguments(ImmutableArray`1 arguments, ImmutableArray`1 parameters, ImmutableArray`1 argRefKindsOpt)
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitCallExpression(BoundCall call, UseKind useKind)
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitExpressionCoreWithStackGuard(BoundExpression expression, Boolean used)
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitExpression(BoundExpression expression, Boolean used)
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitStatement(BoundStatement statement)
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitStatementAndCountInstructions(BoundStatement statement)
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitSequencePointStatement(BoundSequencePoint node)
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitStatement(BoundStatement statement)
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitStatements(ImmutableArray`1 statements)
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitBlock(BoundBlock block)
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitStatement(BoundStatement statement)
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitStatements(ImmutableArray`1 statements)
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitBlock(BoundBlock block)
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitTryStatement(BoundTryStatement statement, Boolean emitCatchesOnly)
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitStatement(BoundStatement statement)
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitStatements(ImmutableArray`1 statements)
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitBlock(BoundBlock block)
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitStatement(BoundStatement statement)
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitStatementList(BoundStatementList list)
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.EmitStatement(BoundStatement statement)
   at LanguageService.CodeAnalysis.XSharp.CodeGen.CodeGenerator.GenerateImpl()
   at LanguageService.CodeAnalysis.XSharp.MethodCompiler.GenerateMethodBody(PEModuleBuilder moduleBuilder, MethodSymbol method, Int32 methodOrdinal, BoundStatement block, ImmutableArray`1 lambdaDebugInfo, ImmutableArray`1 closureDebugInfo, StateMachineTypeSymbol stateMachineTypeOpt, VariableSlotAllocator variableSlotAllocatorOpt, DiagnosticBag diagnostics, DebugDocumentProvider debugDocumentProvider, ImportChain importChainOpt, Boolean emittingPdb, Boolean emitTestCoverageData, ImmutableArray`1 dynamicAnalysisSpans, AsyncForwardEntryPoint entryPointOpt)
   at LanguageService.CodeAnalysis.XSharp.MethodCompiler.CompileMethod(MethodSymbol methodSymbol, Int32 methodOrdinal, ProcessedFieldInitializers& processedInitializers, SynthesizedSubmissionFields previousSubmissionFields, TypeCompilationState compilationState)
*/

USING System.Collections.Generic
USING System.Diagnostics

// NameValuePair structure is part of the VFP GatherScatter.prg

[DebuggerDisplay("{Name,nq}={Value}")];
INTERNAL STRUCTURE NameValuePair
    INTERNAL Name  AS STRING
    INTERNAL @@Value AS USUAL
END STRUCTURE 


FUNCTION Start( ) AS VOID 
VAR values := List<NameValuePair>{}
LOCAL cName AS STRING 
LOCAL uValue AS USUAL

cName := "FIELD1"
uValue := 13.34

//  this line causes the XS9999 error
values:Add( NameValuePair{} {Name := cName, Value := uValue}) 

? values[0]:Name, values[0]:Value
xAssert(values[0]:Name == "FIELD1")
xAssert(values[0]:Value == 13.34)

RETURN 

PROC xAssert(l AS LOGIC) 
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"   
RETURN 		

