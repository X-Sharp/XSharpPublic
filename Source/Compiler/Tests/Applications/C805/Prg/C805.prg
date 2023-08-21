// 805. ICE with _Or() in DEFINE and combination of BYTE/DWORD conversions
// error XS9999: An internal compiler error has occurred: 'Specified cast is not valid.'
// problem happens in VO dialect only

DEFINE GRNUM_NONE				:= 0x00 AS BYTE
DEFINE GRNUM_ONE				:= 0x01 AS BYTE
DEFINE GRNUM_TWO				:= 0x02 AS BYTE
DEFINE GRNUM_ONE_TWO			:= _OR(GRNUM_ONE, GRNUM_TWO) AS BYTE
DEFINE GRNUM_ALL				:= _OR(GRNUM_NONE, GRNUM_ONE, GRNUM_TWO) AS BYTE
DEFINE GRNUM_DEFAULT			:= GRNUM_ALL AS BYTE

FUNCTION Start() AS VOID STRICT
	xAssert( DoSomething1(0) == 1 )
	xAssert( DoSomething2(0) == 3 )
	xAssert( DoSomething3(0) == 3 )
	xAssert( DoSomething4(0) == 3 )
RETURN

FUNCTION DoSomething1(nTN AS INT, nOptions := GRNUM_ONE AS DWORD) AS DWORD
	? nOptions
RETURN nOptions
FUNCTION DoSomething2(nTN AS INT, nOptions := GRNUM_ONE_TWO AS DWORD) AS DWORD
	? nOptions
RETURN nOptions
FUNCTION DoSomething3(nTN AS INT, nOptions := GRNUM_DEFAULT AS DWORD) AS DWORD
	? nOptions
RETURN nOptions
FUNCTION DoSomething4(nTN AS INT, nOptions := GRNUM_ALL AS DWORD) AS DWORD
	? nOptions
RETURN nOptions

PROC xAssert(l AS LOGIC) 
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"   
RETURN 		
/*
error XS9999: An internal compiler error has occurred: 'Specified cast is not valid.',    at LanguageService.CodeAnalysis.ConstantValue.Create(Object value, ConstantValueTypeDiscriminator discriminator)
   at LanguageService.CodeAnalysis.XSharp.Symbols.ParameterSymbolExtensions.GetVODefaultParameter(ParameterSymbol param, SyntaxNode syntax, XSharpCompilation compilation)
   at LanguageService.CodeAnalysis.XSharp.Binder.XsDefaultValue(ParameterSymbol parameter, SyntaxNode syntax, XSharpCompilation compilation)
   at LanguageService.CodeAnalysis.XSharp.Binder.<BindDefaultArguments>g__bindDefaultArgument|627_1(SyntaxNode syntax, ParameterSymbol parameter, Symbol containingMember, Boolean enableCallerInfo, DiagnosticBag diagnostics)
   at LanguageService.CodeAnalysis.XSharp.Binder.BindDefaultArguments(SyntaxNode node, ImmutableArray`1 parameters, ArrayBuilder`1 argumentsBuilder, ArrayBuilder`1 argumentRefKindsBuilder, ImmutableArray`1& argsToParamsOpt, BitVector& defaultArguments, Boolean expanded, Boolean enableCallerInfo, DiagnosticBag diagnostics, Boolean assertMissingParametersAreOptional)
   at LanguageService.CodeAnalysis.XSharp.Binder.BindInvocationExpressionContinued(SyntaxNode node, SyntaxNode expression, String methodName, OverloadResolutionResult`1 result, AnalyzedArguments analyzedArguments, MethodGroup methodGroup, NamedTypeSymbol delegateTypeOpt, DiagnosticBag diagnostics, XSharpSyntaxNode queryClause)
   at LanguageService.CodeAnalysis.XSharp.Binder.BindMethodGroupInvocation(SyntaxNode syntax, SyntaxNode expression, String methodName, BoundMethodGroup methodGroup, AnalyzedArguments analyzedArguments, DiagnosticBag diagnostics, XSharpSyntaxNode queryClause, Boolean allowUnexpandedForm, Boolean& anyApplicableCandidates)
   at LanguageService.CodeAnalysis.XSharp.Binder.BindInvocationExpression(SyntaxNode node, SyntaxNode expression, String methodName, BoundExpression boundExpression, AnalyzedArguments analyzedArguments, DiagnosticBag diagnostics, XSharpSyntaxNode queryClause, Boolean allowUnexpandedForm)
   at LanguageService.CodeAnalysis.XSharp.Binder.BindXsInvocationExpression(InvocationExpressionSyntax node, DiagnosticBag diagnostics)
   at LanguageService.CodeAnalysis.XSharp.Binder.BindExpressionInternal(ExpressionSyntax node, DiagnosticBag diagnostics, Boolean invoked, Boolean indexed)
   at LanguageService.CodeAnalysis.XSharp.Binder.BindExpression(ExpressionSyntax node, DiagnosticBag diagnostics, Boolean invoked, Boolean indexed)
   at LanguageService.CodeAnalysis.XSharp.Binder.BindExpressionStatement(XSharpSyntaxNode node, ExpressionSyntax syntax, Boolean allowsAnyExpression, DiagnosticBag diagnostics)
   at LanguageService.CodeAnalysis.XSharp.Binder.BindExpressionStatement(ExpressionStatementSyntax node, DiagnosticBag diagnostics)
   at LanguageService.CodeAnalysis.XSharp.Binder.BindStatement(StatementSyntax node, DiagnosticBag diagnostics)
   at LanguageService.CodeAnalysis.XSharp.Binder.BindBlockParts(BlockSyntax node, DiagnosticBag diagnostics)
   at LanguageService.CodeAnalysis.XSharp.Binder.BindBlock(BlockSyntax node, DiagnosticBag diagnostics)
   at LanguageService.CodeAnalysis.XSharp.Binder.BindStatement(StatementSyntax node, DiagnosticBag diagnostics)
   at LanguageService.CodeAnalysis.XSharp.Binder.BindBlockParts(BlockSyntax node, DiagnosticBag diagnostics)
   at LanguageService.CodeAnalysis.XSharp.Binder.BindBlock(BlockSyntax node, DiagnosticBag diagnostics)
   at LanguageService.CodeAnalysis.XSharp.Binder.BindTryStatement(TryStatementSyntax node, DiagnosticBag diagnostics)
   at LanguageService.CodeAnalysis.XSharp.Binder.BindStatement(StatementSyntax node, DiagnosticBag diagnostics)
   at LanguageService.CodeAnalysis.XSharp.Binder.BindBlockParts(BlockSyntax node, DiagnosticBag diagnostics)
   at LanguageService.CodeAnalysis.XSharp.Binder.BindBlock(BlockSyntax node, DiagnosticBag diagnostics)
   at LanguageService.CodeAnalysis.XSharp.Binder.BindStatement(StatementSyntax node, DiagnosticBag diagnostics)
   at LanguageService.CodeAnalysis.XSharp.Binder.BindMethodBody(XSharpSyntaxNode declaration, BlockSyntax blockBody, ArrowExpressionClauseSyntax expressionBody, DiagnosticBag diagnostics)
   at LanguageService.CodeAnalysis.XSharp.Binder.BindMethodBody(XSharpSyntaxNode syntax, DiagnosticBag diagnostics)
   at LanguageService.CodeAnalysis.XSharp.MethodCompiler.BindMethodBody(MethodSymbol method, TypeCompilationState compilationState, DiagnosticBag diagnostics, VariableState nullableInitialState, ImportChain& importChain, Boolean& originalBodyNested, InitialState& forSemanticModel)
   at LanguageService.CodeAnalysis.XSharp.MethodCompiler.CompileMethod(MethodSymbol methodSymbol, Int32 methodOrdinal, ProcessedFieldInitializers& processedInitializers, SynthesizedSubmissionFields previousSubmissionFields, TypeCompilationState compilationState)
*/
