// 916. Some issues with indexed PROPERTY SET and ASSIGN
// https://github.com/X-Sharp/XSharpPublic/issues/1543
FUNCTION Start( ) AS VOID
	TestClass{}:TestProp[TRUE,"b",1] := 123
	TestClass{}:TestAssign[TRUE,"b",1] := 123 // No overload for method 'TestClass.TestAssign[string, int]' takes 3 arguments
RETURN

CLASS TestClass
	PROPERTY TestPropCrash[c1 AS LOGIC , c2 AS STRING, nValue AS INT] AS INT SET // compiler crash (if other errors are resolved)
	PROPERTY TestPropEmpty[c1 AS LOGIC , c2 AS STRING, nValue AS INT] AS INT SET NOP // error XS0103: The name 'NOP' does not exist in the current context
	
	PROPERTY TestProp[c1 AS LOGIC , c2 AS STRING, nValue AS INT] AS INT 
	SET 
		? c1,c2,nValue
	END SET
	END PROPERTY
	
	ASSIGN TestAssign(c1 AS LOGIC , c2 AS STRING, nValue AS INT) AS INT
		? c1,c2,nValue
	RETURN
END CLASS

/*
Unhandled Exception: System.Collections.Generic.KeyNotFoundException: The given key was not present in the dictionary.
   at System.ThrowHelper.ThrowKeyNotFoundException()
   at System.Collections.Generic.Dictionary`2.get_Item(TKey key)
   at LanguageService.Cci.FullMetadataWriter.GetFieldDefinitionHandle(IFieldDefinition def) in D:\a\XSharpPublic\XSharpPublic\src\Roslyn\Src\Compilers\Core\Portable\PEWriter\FullMetadataWriter.cs:line 164
   at LanguageService.Cci.MetadataWriter.GetFieldHandle(IFieldReference fieldReference) in D:\a\XSharpPublic\XSharpPublic\src\Roslyn\Src\Compilers\Core\Portable\PEWriter\MetadataWriter.cs:line 843
   at LanguageService.Cci.MetadataWriter.GetHandle(Object reference) in D:\a\XSharpPublic\XSharpPublic\src\Roslyn\Src\Compilers\Core\Portable\PEWriter\MetadataWriter.cs:line 3078
   at LanguageService.Cci.MetadataWriter.ResolveEntityHandleFromPseudoToken(Int32 pseudoSymbolToken) in D:\a\XSharpPublic\XSharpPublic\src\Roslyn\Src\Compilers\Core\Portable\PEWriter\MetadataWriter.cs:line 3104
   at LanguageService.Cci.MetadataWriter.WriteInstructions(Blob finalIL, ImmutableArray`1 generatedIL, UserStringHandle& mvidStringHandle, Blob& mvidStringFixup) in D:\a\XSharpPublic\XSharpPublic\src\Roslyn\Src\Compilers\Core\Portable\PEWriter\MetadataWriter.cs:line 3220
   at LanguageService.Cci.MetadataWriter.SerializeMethodBody(MethodBodyStreamEncoder encoder, IMethodBody methodBody, StandaloneSignatureHandle localSignatureHandleOpt, UserStringHandle& mvidStringHandle, Blob& mvidStringFixup) in D:\a\XSharpPublic\XSharpPublic\src\Roslyn\Src\Compilers\Core\Portable\PEWriter\MetadataWriter.cs:line 2990
   at LanguageService.Cci.MetadataWriter.SerializeMethodBodies(BlobBuilder ilBuilder, PdbWriter nativePdbWriterOpt, Blob& mvidStringFixup) in D:\a\XSharpPublic\XSharpPublic\src\Roslyn\Src\Compilers\Core\Portable\PEWriter\MetadataWriter.cs:line 2925
   at LanguageService.Cci.MetadataWriter.BuildMetadataAndIL(PdbWriter nativePdbWriterOpt, BlobBuilder ilBuilder, BlobBuilder mappedFieldDataBuilder, BlobBuilder managedResourceDataBuilder, Blob& mvidFixup, Blob& mvidStringFixup) in D:\a\XSharpPublic\XSharpPublic\src\Roslyn\Src\Compilers\Core\Portable\PEWriter\MetadataWriter.cs:line 1822
   at LanguageService.Cci.PeWriter.WritePeToStream(EmitContext context, CommonMessageProvider messageProvider, Func`1 getPeStream, Func`1 getPortablePdbStreamOpt, PdbWriter nativePdbWriterOpt, String pdbPathOpt, Boolean metadataOnly, Boolean isDeterministic, Boolean emitTestCoverageData, Nullable`1 privateKeyOpt, CancellationToken cancellationToken) in D:\a\XSharpPublic\XSharpPublic\src\Roslyn\Src\Compilers\Core\Portable\PEWriter\PeWriter.cs:line 81
   at LanguageService.CodeAnalysis.Compilation.SerializePeToStream(CommonPEModuleBuilder moduleBeingBuilt, DiagnosticBag metadataDiagnostics, CommonMessageProvider messageProvider, Func`1 getPeStream, Func`1 getMetadataPeStreamOpt, Func`1 getPortablePdbStreamOpt, PdbWriter nativePdbWriterOpt, String pdbPathOpt, Boolean metadataOnly, Boolean includePrivateMembers, Boolean isDeterministic, Boolean emitTestCoverageData, Nullable`1 privateKeyOpt, CancellationToken cancellationToken) in D:\a\XSharpPublic\XSharpPublic\src\Roslyn\Src\Compilers\Core\Portable\Compilation\Compilation.cs:line 2970
   at LanguageService.CodeAnalysis.Compilation.SerializeToPeStream(CommonPEModuleBuilder moduleBeingBuilt, EmitStreamProvider peStreamProvider, EmitStreamProvider metadataPEStreamProvider, EmitStreamProvider pdbStreamProvider, Func`2 testSymWriterFactory, DiagnosticBag diagnostics, EmitOptions emitOptions, Nullable`1 privateKeyOpt, CancellationToken cancellationToken) in D:\a\XSharpPublic\XSharpPublic\src\Roslyn\Src\Compilers\Core\Portable\Compilation\Compilation.cs:line 2869
   at LanguageService.CodeAnalysis.CommonCompiler.CompileAndEmit(TouchedFileLogger touchedFilesLogger, Compilation& compilation, ImmutableArray`1 analyzers, ImmutableArray`1 generators, ImmutableArray`1 additionalTextFiles, AnalyzerConfigSet analyzerConfigSet, ImmutableArray`1 sourceFileAnalyzerConfigOptions, ImmutableArray`1 embeddedTexts, DiagnosticBag diagnostics, CancellationToken cancellationToken, CancellationTokenSource& analyzerCts, Boolean& reportAnalyzer, AnalyzerDriver& analyzerDriver) in D:\a\XSharpPublic\XSharpPublic\src\Roslyn\Src\Compilers\Core\Portable\CommandLine\CommonCompiler.cs:line 1283
   at LanguageService.CodeAnalysis.CommonCompiler.RunCore(TextWriter consoleOutput, ErrorLogger errorLogger, CancellationToken cancellationToken) in D:\a\XSharpPublic\XSharpPublic\src\Roslyn\Src\Compilers\Core\Portable\CommandLine\CommonCompiler.cs:line 839
   at LanguageService.CodeAnalysis.CommonCompiler.Run(TextWriter consoleOutput, CancellationToken cancellationToken) in D:\a\XSharpPublic\XSharpPublic\src\Roslyn\Src\Compilers\Core\Portable\CommandLine\CommonCompiler.cs:line 706
   at LanguageService.CodeAnalysis.XSharp.CommandLine.Xsc.<>c__DisplayClass1_0.<Run>b__0(TextWriter tw) in D:\a\XSharpPublic\XSharpPublic\src\Compiler\src\Compiler\xsc\Xsc.cs:line 54
   at LanguageService.CodeAnalysis.CommandLine.ConsoleUtil.RunWithUtf8Output[T](Func`2 func) in D:\a\XSharpPublic\XSharpPublic\src\Roslyn\Src\Compilers\Core\CommandLine\ConsoleUtil.cs:line 26
   at LanguageService.CodeAnalysis.XSharp.CommandLine.Xsc.Run(String[] args, BuildPaths buildPaths, TextWriter textWriter, IAnalyzerAssemblyLoader analyzerLoader) in D:\a\XSharpPublic\XSharpPublic\src\Compiler\src\Compiler\xsc\Xsc.cs:line 56
   at LanguageService.CodeAnalysis.CommandLine.BuildClient.RunLocalCompilation(String[] arguments, BuildPaths buildPaths, TextWriter textWriter) in D:\a\XSharpPublic\XSharpPublic\src\Roslyn\Src\Compilers\Shared\BuildClient.cs:line 213
   at LanguageService.CodeAnalysis.CommandLine.BuildClient.RunCompilation(IEnumerable`1 originalArguments, BuildPaths buildPaths, TextWriter textWriter, String pipeName) in D:\a\XSharpPublic\XSharpPublic\src\Roslyn\Src\Compilers\Shared\BuildClient.cs:line 154
   at LanguageService.CodeAnalysis.CommandLine.BuildClient.Run(IEnumerable`1 arguments, RequestLanguage language, CompileFunc compileFunc, ICompilerServerLogger logger) in D:\a\XSharpPublic\XSharpPublic\src\Roslyn\Src\Compilers\Shared\BuildClient.cs:line 98
   at LanguageService.CodeAnalysis.XSharp.CommandLine.Program.MainCore(String[] args) in D:\a\XSharpPublic\XSharpPublic\src\Compiler\src\Compiler\xsc\Program.cs:line 37
   at LanguageService.CodeAnalysis.XSharp.CommandLine.Program.Main(String[] args) in D:\a\XSharpPublic\XSharpPublic\src\Compiler\src\Compiler\xsc\Program.cs:line 19
*/

