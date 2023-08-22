// 749. ICE with incorrect assigning of NULL to numeric var
FUNCTION Start() AS VOID
LOCAL n AS BYTE
n := NULL
LOCAL l AS LOGIC
l := NULL // correct error:
// error XS0037: Cannot convert null to 'logic' because it is a non-nullable value type

PROCEDURE TestProc(n := NULL AS INT)
	
/*
Unhandled Exception: System.AggregateException: One or more errors occurred. ---> System.NullReferenceException: Object reference not set to an instance of an object.
   at LanguageService.CodeAnalysis.XSharp.Binder.XsCheckConversionForAssignment(TypeSymbol targetType, BoundExpression expression, DiagnosticBag diagnostics, Boolean isDefaultParameter, Boolean isRefAssignment) in D:\a\XSharpDev\XSharpDev\XSharp\src\Compiler\XSharpCodeAnalysis\Binder\Binder_Statements.cs:line 176
   at LanguageService.CodeAnalysis.XSharp.Binder.GenerateConversionForAssignment(TypeSymbol targetType, BoundExpression expression, DiagnosticBag diagnostics, Boolean isDefaultParameter, Boolean isRefAssignment) in D:\a\XSharpDev\XSharpDev\Roslyn\Src\Compilers\CSharp\Portable\Binder\Binder_Statements.cs:line 1711
   at LanguageService.CodeAnalysis.XSharp.Binder.BindParameterDefaultValue(EqualsValueClauseSyntax defaultValueSyntax, ParameterSymbol parameter, DiagnosticBag diagnostics, BoundExpression& valueBeforeConversion) in D:\a\XSharpDev\XSharpDev\Roslyn\Src\Compilers\CSharp\Portable\Binder\Binder_Expressions.cs:line 310
   at LanguageService.CodeAnalysis.XSharp.Symbols.SourceComplexParameterSymbol.MakeDefaultExpression(DiagnosticBag diagnostics, Binder binder) in D:\a\XSharpDev\XSharpDev\Roslyn\Src\Compilers\CSharp\Portable\Symbols\Source\SourceComplexParameterSymbol.cs:line 189
   at LanguageService.CodeAnalysis.XSharp.Symbols.SourceComplexParameterSymbol.get_DefaultSyntaxValue() in D:\a\XSharpDev\XSharpDev\Roslyn\Src\Compilers\CSharp\Portable\Symbols\Source\SourceComplexParameterSymbol.cs:line 138
   at LanguageService.CodeAnalysis.XSharp.Symbols.SourceComplexParameterSymbol.get_ExplicitDefaultConstantValue() in D:\a\XSharpDev\XSharpDev\Roslyn\Src\Compilers\CSharp\Portable\Symbols\Source\SourceComplexParameterSymbol.cs:line 94
   at LanguageService.CodeAnalysis.XSharp.Symbols.SourceComplexParameterSymbol.ForceComplete(SourceLocation locationOpt, CancellationToken cancellationToken) in D:\a\XSharpDev\XSharpDev\Roslyn\Src\Compilers\CSharp\Portable\Symbols\Source\SourceComplexParameterSymbol.cs:line 973
   at LanguageService.CodeAnalysis.XSharp.Symbols.SourceMemberMethodSymbol.ForceComplete(SourceLocation locationOpt, CancellationToken cancellationToken) in D:\a\XSharpDev\XSharpDev\Roslyn\Src\Compilers\CSharp\Portable\Symbols\Source\SourceMemberMethodSymbol.cs:line 790
   at LanguageService.CodeAnalysis.XSharp.Symbols.SourceOrdinaryMethodSymbol.ForceComplete(SourceLocation locationOpt, CancellationToken cancellationToken) in D:\a\XSharpDev\XSharpDev\Roslyn\Src\Compilers\CSharp\Portable\Symbols\Source\SourceOrdinaryMethodSymbol.cs:line 1034
   at LanguageService.CodeAnalysis.XSharp.Symbols.SourceMemberContainerTypeSymbol.ForceComplete(SourceLocation locationOpt, CancellationToken cancellationToken) in D:\a\XSharpDev\XSharpDev\Roslyn\Src\Compilers\CSharp\Portable\Symbols\Source\SourceMemberContainerSymbol.cs:line 529
   at LanguageService.CodeAnalysis.XSharp.Symbol.ForceCompleteMemberByLocation(SourceLocation locationOpt, Symbol member, CancellationToken cancellationToken) in D:\a\XSharpDev\XSharpDev\Roslyn\Src\Compilers\CSharp\Portable\Symbols\Symbol.cs:line 712
   at Roslyn.Utilities.UICultureUtilities.<>c__DisplayClass6_0`1.<WithCurrentUICulture>b__0(T param) in D:\a\XSharpDev\XSharpDev\Roslyn\Src\Compilers\Core\Portable\InternalUtilities\UICultureUtilities.cs:line 166
   at System.Threading.Tasks.Parallel.<>c__DisplayClass17_0`1.<ForWorker>b__1()
   at System.Threading.Tasks.Task.InnerInvokeWithArg(Task childTask)
   at System.Threading.Tasks.Task.<>c__DisplayClass176_0.<ExecuteSelfReplicating>b__0(Object <p0>)
   --- End of inner exception stack trace ---
   at System.Threading.Tasks.Task.ThrowIfExceptional(Boolean includeTaskCanceledExceptions)
   at System.Threading.Tasks.Task.Wait(Int32 millisecondsTimeout, CancellationToken cancellationToken)
   at System.Threading.Tasks.Parallel.ForWorker[TLocal](Int32 fromInclusive, Int32 toExclusive, ParallelOptions parallelOptions, Action`1 body, Action`2 bodyWithState, Func`4 bodyWithLocal, Func`1 localInit, Action`1 localFinally)
   at System.Threading.Tasks.Parallel.For(Int32 fromInclusive, Int32 toExclusive, ParallelOptions parallelOptions, Action`1 body)
   at LanguageService.CodeAnalysis.XSharp.Symbols.SourceNamespaceSymbol.ForceComplete(SourceLocation locationOpt, CancellationToken cancellationToken) in D:\a\XSharpDev\XSharpDev\Roslyn\Src\Compilers\CSharp\Portable\Symbols\Source\SourceNamespaceSymbol_Completion.cs:line 59
   at LanguageService.CodeAnalysis.XSharp.Symbols.SourceModuleSymbol.ForceComplete(SourceLocation locationOpt, CancellationToken cancellationToken) in D:\a\XSharpDev\XSharpDev\Roslyn\Src\Compilers\CSharp\Portable\Symbols\Source\SourceModuleSymbol.cs:line 260
   at LanguageService.CodeAnalysis.XSharp.Symbols.SourceAssemblySymbol.ForceComplete(SourceLocation locationOpt, CancellationToken cancellationToken) in D:\a\XSharpDev\XSharpDev\Roslyn\Src\Compilers\CSharp\Portable\Symbols\Source\SourceAssemblySymbol.cs:line 909
   at LanguageService.CodeAnalysis.XSharp.XSharpCompilation.GetSourceDeclarationDiagnostics(SyntaxTree syntaxTree, Nullable`1 filterSpanWithinTree, Func`4 locationFilterOpt, CancellationToken cancellationToken) in D:\a\XSharpDev\XSharpDev\Roslyn\Src\Compilers\CSharp\Portable\Compilation\CSharpCompilation.cs:line 2341
   at LanguageService.CodeAnalysis.XSharp.XSharpCompilation.GetDiagnostics(CompilationStage stage, Boolean includeEarlierStages, DiagnosticBag diagnostics, CancellationToken cancellationToken) in D:\a\XSharpDev\XSharpDev\Roslyn\Src\Compilers\CSharp\Portable\Compilation\CSharpCompilation.cs:line 2226
   at LanguageService.CodeAnalysis.CommonCompiler.CompileAndEmit(TouchedFileLogger touchedFilesLogger, Compilation& compilation, ImmutableArray`1 analyzers, ImmutableArray`1 additionalTextFiles, ImmutableArray`1 embeddedTexts, DiagnosticBag diagnostics, CancellationToken cancellationToken, CancellationTokenSource& analyzerCts, Boolean& reportAnalyzer, AnalyzerDriver& analyzerDriver) in D:\a\XSharpDev\XSharpDev\Roslyn\Src\Compilers\Core\Portable\CommandLine\CommonCompiler.cs:line 681
   at LanguageService.CodeAnalysis.CommonCompiler.RunCore(TextWriter consoleOutput, ErrorLogger errorLogger, CancellationToken cancellationToken) in D:\a\XSharpDev\XSharpDev\Roslyn\Src\Compilers\Core\Portable\CommandLine\CommonCompiler.cs:line 604
   at LanguageService.CodeAnalysis.CommonCompiler.Run(TextWriter consoleOutput, CancellationToken cancellationToken) in D:\a\XSharpDev\XSharpDev\Roslyn\Src\Compilers\Core\Portable\CommandLine\CommonCompiler.cs:line 510
   at LanguageService.CodeAnalysis.XSharp.CommandLine.Xsc.<>c__DisplayClass1_0.<Run>b__0(TextWriter tw) in D:\a\XSharpDev\XSharpDev\XSharp\src\Compiler\xsc\Xsc.cs:line 49
   at LanguageService.CodeAnalysis.CommandLine.ConsoleUtil.RunWithUtf8Output[T](Func`2 func) in D:\a\XSharpDev\XSharpDev\Roslyn\Src\Compilers\Core\CommandLine\ConsoleUtil.cs:line 24
   at LanguageService.CodeAnalysis.XSharp.CommandLine.Xsc.Run(String[] args, BuildPaths buildPaths, TextWriter textWriter, IAnalyzerAssemblyLoader analyzerLoader) in D:\a\XSharpDev\XSharpDev\XSharp\src\Compiler\xsc\Xsc.cs:line 49
   at LanguageService.CodeAnalysis.CommandLine.DesktopBuildClient.RunLocalCompilation(String[] arguments, BuildPaths buildPaths, TextWriter textWriter) in D:\a\XSharpDev\XSharpDev\Roslyn\Src\Compilers\Shared\DesktopBuildClient.cs:line 50
   at LanguageService.CodeAnalysis.CommandLine.BuildClient.RunCompilation(IEnumerable`1 originalArguments, BuildPaths buildPaths, TextWriter textWriter) in D:\a\XSharpDev\XSharpDev\Roslyn\Src\Compilers\Shared\BuildClient.cs:line 113
   at LanguageService.CodeAnalysis.CommandLine.DesktopBuildClient.Run(IEnumerable`1 arguments, RequestLanguage language, CompileFunc compileFunc, IAnalyzerAssemblyLoader analyzerAssemblyLoader) in D:\a\XSharpDev\XSharpDev\Roslyn\Src\Compilers\Shared\DesktopBuildClient.cs:line 31
   at LanguageService.CodeAnalysis.XSharp.CommandLine.Program.Main(String[] args) in D:\a\XSharpDev\XSharpDev\XSharp\src\Compiler\xsc\Program.cs:line 24
*/
