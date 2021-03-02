# Changes in the Roslyn code

This document describes the various changes that we had to make to the Roslyn source code, with the exception of the changes that are related to case sensitivity.
These are described globally in a separate document 

The document will list the various source files and the kind changes that were made.
In general we try to keep the # of changes in a file to a minimum. And when possible we have written an extra / new method
or property to implement different behavior in X#. Quite often you will see calls to code in the Roslyn branch that looks like this

```
#if XSHARP
  var xresult = XSharpResolveEqualSymbols(.......);
  if (xresult != null)
     return xresult;
#endif
```

This example shows that a helper method is called because X# has different rules to decide what to do when the binder finds 2 symbols
that could both be used for a method call.
We often use a `x` prefix for variable names so it is clear that this variable is used by the x# code and to 
prevent possible side effects of assigning to variables when someone changes other code in this file later.

The changes that we make in the Roslyn code are either this kind of method calls, or we add/ change visibility modifiers or we add 
a `partial` modifier to a type so we can add new methods to that type in our separate folder tree.


## Files in the Roslyn\src\Compilers\Core folder

File | Description
---- | -----------
CommandLine\BuildProtocol.cs  | X# passed 3 extra folder names to the BuildRequest.Create() method and the ArgumentId enum has 3 extra values for these folders
Portable\CaseInsensitiveComparison.cs | This file contains the XSharpString class which takes care of case (in) sensitive string comparisons
Portable\CodeGen\ILBuilder.cs | An assertion was disabled for X# because of the way how X# parse trees are created
Portable\CodeGen\ILBuilderEmit.cs | Added emitting of IntPtr values 
Portable\CommandLine\CommandLineParser.cs | Added commandline options /credits and /cs and set XSharpString.CaseSensitive to true when needed.
Portable\CommandLine\CommonCompiler.cs | Different handling of AssemblyProductVersion and AssemblyInformationalVersion. 
idem | Also handling of ParseLevel.  
idem | Also added Exception handler to catch Internal Exceptions.  
idem | Also added call to "Fix Resources" in .Net Assemblies (See separate description)
Portable\Compilation\Compilation.cs | Since X# can add native resources for forms and menus the handling of native resources has changed.
Portable\Compilation\CompilationOptions.cs | Changed visibility of MainType property
Portable\ConstantValue.cs | Added 2 members to the ConstantValueTypeDiscriminator enum (Intptr and Void) and added some code to handle these.
Portable\ConstantValueSpecialized.cs | ConstantValueDefault is now a partial class.
Portable\Diagnostic\DiagnosticInfo.cs | Changed some typenames in error messages from internal names to keywords (for example USUAL instead of XSharp.__Usual)
Portable\Emit\CommonPEModuleBuilder.cs | Vulcan.Net had chosen to add some code to the compiler generated `<Module>` class which is empty by default. We are doing the same to be as compatible as possible. This required us to replace the `<Module>` class with our own class.
Portable\PEWriter\DebugSourceDocument.cs | We have added new GUIDs for the company XSharp and the language XSharp. These are written to the PDB file so the debugger recognizes the language.
Portable\PEWriter\MetadataWriter.cs | When the compiler option to automatically generate constructors is chosen then we copy the attributes for the constructor from parent class to the constructor in the child class.
Portable\SpecialMember.cs | We have added a special member String_Compare. This is used for the `>` and `<` operators for string comparisons
Portable\SpecialMembers.cs | idem
Portable\Symbols\WellKnownMemberNames.cs | The EntryPointMethodName is "Start" and not "Main" for X#
Portable\Syntax\GreenNode.cs | Added "fictional" _fullWidth for generated Trivia nodes.
Portable\VersionHelper.cs | Restored the vulcan behavior to generate a version from `*` values in Assembly and File versions based on Date and Time.
Portable\WellKnownTypes.cs | Added "our" well known types, such as USUAL, FLOAT, ARRAY etc. both in Vulcan and X# variants.



## Files in the Roslyn\src\Compilers\CSharp folder

File | Description
---- | -----------
Portable\Binder\Binder.ValueChecks.cs | Allow to pass NULL as value to REF parameters (with a warning)
idem | Allow writing to ForEach loop variables
idem | Suppress a check for symbols of type XsVariableSymbol (MEMVAR, FIELD )
idem | Suppressed a compiler error for inaccessible code
Portable\Binder\Binder_Attributes.cs | Added code for our DefaultParameterValue attribute
Portable\Binder\Binder_Conversions.cs | Added code to handle "last minute" string to psz conversions. 
idem | Added code to Bind CodeBlocks
Portable\Binder\Binder_Crefs.cs | The X# SyntaxTree does not have the comments. So we had to suppress an assertion
Portable\Binder\Binder_Expressions.cs | C# does not allow SELF in initializers of class variables. X# does. 
idem | Allow the use of @ as alternative REF
idem | Call BindXsInvocationExpression to bind invocations (this takes care of resolving to functions )
idem | Call BindXSIdentifier to bind identifiers (this also takes care of MEMVARs and FIELDs)
idem | Handle binding of members inside VOStruct and Union
idem | Suppress warning for @ for managed types when compiling with /unsafe
idem | SizeOf() uses the size defined in the VoStructAttribute for VOStruct and Unions
idem | Special binding of _CAST, such as LOGIC(_CAST, numericvalue)
idem | Added check for conflicts between named arguments and local variables
idem | Added code to retrieve the "real" parameter types for _DLL declarations which contained PSZ types as parameters
idem | X# does not require the SUPER() call to be the first statement in the constructor body. We have added support for that.
idem | Call TryBindLateBoundCall for late bound expressions
idem | C# only allows to 'omit' the REF keyword for calls to COM components. X# allows this always.
idem | Added special handling for calling Indexed values in arrays and collections
Portable\Binder\Binder_Initializers.cs | Added code to initialize strings with "" when needed.
Portable\Binder\Binder_Invocation.cs | Call BindXSIdentifier instead of BindIdentifier
idem | Add call to BindXsInvocationExpression to resolve special cases.
idem | Added support for latebound calls
idem | Added support to get default argument values from the DefaultParameterValue attributes
Portable\Binder\Binder_Lookup.cs | Call XSLookupSymbolsWithFallback and XSLookupSymbolsInternal to lookup symbols
Portable\Binder\Binder_Operators.cs | Added support for "special" binary operators, such as String MINUS string and String `>` String
idem | Handle difference in constant folder and certain _CAST() operations.
idem | Handle + and - operations on Enum types
idem | Added support to Bind Addressof operations
idem | Adjusted handling of Unary operators
idem | Adjusted code to determine the result of an IIF() expression where the 2 arguments are of different type.
Portable\Binder\Binder_Statements.cs | Added support for REF locals (which we use for STATIC LOCAL)
idem | Added support to initialize local strings with ""
idem | Handle several differences in conversion operations
Portable\Binder\Binder_Symbols.cs | Handle different code generation for AS / IS with VOStruct and UNION types. (AS is translated to a Pointer to the symbol)
idem | Changed the way type names and namespace name conflicts are handled
idem | Call XSharpResolveEqualSymbols to handle 2 symbols with the same name from a different location
Portable\Binder\Binder_Unsafe.cs | The handling of unsafe types is a bit different in X#
Portable\Binder\BinderFactory.BinderFactoryVisitor.cs | Allow indexers with another name than "This"
Portable\Binder\ForEachLoopBinder.cs | Allow binding FOREACH to ARRAY or USUAL
Portable\Binder\Imports.cs | Has special handling for namespaces in X# created assemblies that have a ClassLibrary attribute. Also no warnings are generated for duplicate using statements.
Portable\Binder\InContainerBinder.cs | X# has an option to exclude namespace from the result
Portable\Binder\LocalScopeBinder.cs | Added option for Local Ref variables (STATIC LOCAL)
Portable\Binder\LookupOptions.cs | X# adds 3 enum values for lookups: ExcludeNameSpaces, DefinesOnly and MustNotBeMethod
Portable\Binder\Semantics\Conversions\ConversionKindExtensions.cs | ExplicitIntegerToPointer and IntPtr are considered to be "Implicit" in X#.
Portable\Binder\Semantics\Conversions\ConversionsBase.cs | Call ClassifyXSImplicitBuiltInConversionFromExpression for certain conversions. Some other methods are changed to virtual so they can be overridden in our code.
Portable\Binder\Semantics\Conversions\UserDefinedImplicitConversions.cs | Conversions from USUAL to a non supported type are mapped to the conversion from USUAL to OBJECT.
Portable\Binder\Semantics\Operators\BinaryOperatorOverloadResolution.cs | Handle binary comparison between VOSTRUCT types and several operators for Enums. Also added a filter to filter out an unwanted operator method from Vulcan Float types.   
idem | Added a call to VoBetterOperator for certain situations where Roslyn could not decide.
Portable\Binder\Semantics\Operators\OperatorKindExtensions.cs | Added IsUnaryMinus() extension method.
Portable\Binder\Semantics\OverloadResolution\AnalyzedArguments.cs | Added SetRefKind which is used when arguments are passed with @
Portable\Binder\Semantics\OverloadResolution\OverloadResolution.cs | Changed the way how parameters are resolved and how a method with CLIPPER calling convention is called.
idem | Changed handling of REF parameters
idem | Changed handling of skipped parameters
idem | allow passing NULL for REF parameters
Portable\Binder\Semantics\OverloadResolution\OverloadResolution_ArgsToParameters.cs | Changed handling of REF parameters and skipped parameters
Portable\BoundTree\UnboundLambda.cs | Added special handling for Lambda expressions in CodeBlocks
Portable\CodeGen\EmitConversion.cs | Allow numeric conversion for enum values
Portable\CodeGen\EmitExpression.cs | More changes for REF parameters and NULL values
Portable\CodeGen\EmitOperators.cs | Handling of add and subtract operations for Enum values 
Portable\CommandLine\CSharpCommandLineParser.cs | Add calls to  ParseXSharpArgument() that passes our arguments and also some changes in the code that copies options.
CSharp\Portable\CommandLine\CSharpCompiler.cs | Added a call to code that constructs a new tree with compiler generated code based on the trees generated from the input files. 
idem | We are also including .ppo files and header files to the "touchedFilesLogger".
Portable\Compilation\CSharpCompilation.cs | Added code that creates a "default SyntaxTree" with $Init1 etc.
Portable\Compilation\CSharpDiagnosticFilter.cs | X# does not store #pragma in the syntaxtree. We are therefore comparing line numbers and not positions when looking for a #pragma
Portable\Compiler\AnonymousTypeMethodBodySynthesizer.cs | We have made some changes to be able to process CodeBlocks
Portable\Compiler\DocumentationCommentCompiler.DocumentationCommentWalker.cs | Roslyn XML nodes are part of the SyntaxTree. We retrieve the XML from the Antlr Parse Tree node. XML before an entity is avaliable from a property on the Start token of the entity.
Portable\Compiler\MethodCompiler.cs | Added handler for Internal Errors
idem | Call RewriteXSharpMethod to rewrite Start() and the functions that call the _Init1, _Init2 and _Init3 procedures
Portable\CSharpCompilationOptions.cs | Type is now partial so we can add new fields and properties
Portable\CSharpParseOptions.cs | idem. We also changed the default LanguageVersion (which is "meaningless" in X#) to Latest
Portable\DocumentationComments\DocumentationCommentIDVisitor.PartVisitor.cs | Changed handling of parameters with Clipper Calling convention
Portable\DocumentationComments\SourceDocumentationCommentUtils.cs | Special handling of XML comments because our comments are not in the SyntaxTree
Portable\Emitter\Model\PEModuleBuilder.cs | X# allows private structs and classes and treat them as internal
Portable\FlowAnalysis\DefiniteAssignment.cs  | Make sure variables that are generated automatically by X# are not reported as unused
idem| C# does not report unused variables in quite some cases (when they get assigned the return value of an invocation for example). X# does that.
Portable\FlowAnalysis\NullableWalker.cs | DynamicInvocation includes late bound access to methods and properties
Portable\Lowering\DiagnosticsPass_ExpressionTrees.cs | Add check for Integer to Pointer conversions
Portable\Lowering\InitializerRewriter.cs | Handle initializing strings with ""
Portable\Lowering\Instrumentation\DebugInfoInjector.cs | Adjust location where sequence points for the debugger are inserted.
Portable\Lowering\LocalRewriter\LocalRewriter.cs  | Rewrite certain assignments
Portable\Lowering\LocalRewriter\LocalRewriter_AssignmentOperator.cs | Add support for late bound assignments and memvar and database field read/write
Portable\Lowering\LocalRewriter\LocalRewriter_Call.cs | Add support for latebound calls
Portable\Lowering\LocalRewriter\LocalRewriter_CompoundAssignmentOperator.cs | Make sure that `+=` and `-=` are not seen as eventhandler assigns for late bound code.
Portable\Lowering\LocalRewriter\LocalRewriter_Conversion.cs | Add handling of PSZ types
Portable\Lowering\LocalRewriter\LocalRewriter_ObjectCreationExpression.cs | Special handling for constructors of CLIPPER calling convention
Portable\Lowering\LocalRewriter\LocalRewriter_PropertyAccess.cs | Special handling of MEMVAR and FIELD
Portable\Lowering\LocalRewriter\LocalRewriter_StringConcat.cs | String concatenation with non string types now throws an error
Portable\Lowering\LocalRewriter\LocalRewriter_UnaryOperator.cs | Add support for latebound Unary operations
Portable\SymbolDisplay\SymbolDisplayVisitor.Types.cs | Make sure our type names are displayed correctly (short, dword etc)
Portable\Symbols\AnonymousTypes\AnonymousTypeManager.cs | Anonymous types for codeblocks do not have "dynamic" signatures. They always have a fixed number of properties and methods.
Portable\Symbols\AnonymousTypes\PublicSymbols\AnonymousType.TypePublicSymbol.cs | AnonymousTypePublicSymbol is not SEALED because we inherit codeblocks from it. Also added a special constructor for Codeblocks.
Portable\Symbols\AnonymousTypes\SynthesizedSymbols\AnonymousType.TemplateSymbol.cs | Added special constructor for CodeBlocks
Portable\Symbols\LocalDeclarationKind.cs | Added LocalDeclarationKind.RefVariable 
Portable\Symbols\MemberSignatureComparer.cs | Added some X# specific cases where we see signatures as equal
Portable\Symbols\Metadata\PE\PEFieldSymbol.cs | Added special handling for X# field fixed size buffers
Portable\Symbols\Metadata\PE\PENamespaceSymbol.cs | Added code to handle type names that are only different in case. They are called "siblings" in the code.
Portable\Symbols\Metadata\PE\PEPropertySymbol.cs | Added support for "named" indexers .
Portable\Symbols\ParameterSignature.cs | Added code to decode the "Actual Type" of a parameter
Portable\Symbols\Source\SourceConstructorSymbol.cs | Added support for generated constructors of CLIPPER calling convention.
Portable\Symbols\Source\SourceConstructorSymbolBase.cs | Made some properties virtual to support generated constructors of CLIPPER calling convention.
Portable\Symbols\Source\SourceCustomEventSymbol.cs | Fix weird VIRTUAL - OVERRIDE mixup that we inheritted from Vulcan
Portable\Symbols\Source\SourceDelegateMethodSymbol.cs | Allow compiler to mutate immutable object for PCALL() and PCALLNative()
Portable\Symbols\Source\SourceEventSymbol.cs | Fix weird VIRTUAL - OVERRIDE mixup that we inheritted from Vulcan
Portable\Symbols\Source\SourceFixedFieldSymbol.cs | Retrieve size of Fixed elements from the attributes for VOSTRUCT and UNION
Portable\Symbols\Source\SourceLocalSymbol.cs | Allow REF variables
Portable\Symbols\Source\SourceMemberContainerSymbol.cs | Add special handling for errors reported for ACCESS or ASSIGN
idem | Allow 'named' indexed properties
Portable\Symbols\Source\SourceMemberContainerSymbol_ImplementationChecks.cs | Fix weird VIRTUAL - OVERRIDE mixup that we inheritted from Vulcan
Portable\Symbols\Source\SourceMemberFieldSymbol.cs | Add code to retrieve the types for GLOBAL variables
Portable\Symbols\Source\SourceMemberMethodSymbol.cs | Fix weird VIRTUAL - OVERRIDE mixup that we inheritted from Vulcan
Portable\Symbols\Source\SourceNamedTypeSymbol.cs | Call code to generate an attribute for VOSTRUCT and UNION
Portable\Symbols\Source\SourceOrdinaryMethodSymbolBase.cs | Name property is Mutable so it can be adjusted for indexed properties
Portable\Symbols\Source\SourcePropertyAccessorSymbol.cs | Fix weird VIRTUAL - OVERRIDE mixup that we inheritted from Vulcan
idem | Added support so signature can be overridden if the parent class has a `[TypeChanged]` attribute
Portable\Symbols\Source\SourcePropertySymbol.cs | Added support for "named" indexers .
Portable\Symbols\Source\SourcePropertySymbolBase.cs | Fix weird VIRTUAL - OVERRIDE mixup that we inheritted from Vulcan
idem | Added support for "named" indexers .
Portable\Symbols\SpecialTypeExtensions.cs | Added IsNumericType() extension
Portable\Symbols\Symbol_Attributes.cs | Fix problem that Vulcan attribute ClipperCallingConvention does not have the Constructor target
Portable\Symbols\Synthesized\GeneratedNames.cs | Change name for DIM arrays inside VOStructs
Portable\Symbols\TypeSymbol.cs | Handle CLIPPER calling convention checks for types that implement an interface
Portable\Syntax\CSharpPragmaWarningStateMap.cs | Our pragmas are not in the SyntaxTree. 
Portable\Syntax\CSharpSyntaxNode.cs | Retrieve leading trivia (comments) from the XNode object.
Portable\Syntax\CSharpSyntaxTree.cs | In the internal compiler type names are translated.
idem | Call the X# Language Parser from ParseText()
idem | Changed the code that returns line span and similar values to read this from the Antlr tree.
Portable\Syntax\SyntaxFacts.cs | IsStatementExpression now also supports several other types of expressions.
Portable\Syntax\SyntaxTreeDiagnosticEnumerator.cs | Adjusted code to read the location of compiler errors.


## Files in the Roslyn\src\Compilers\Server folder

File | Description
---- | -----------
VBCSCompiler\BuildProtocolUtil.cs | GetCommandLineArguments() extracts the extra variables defaultIncludeDir, WinDir, SysDir
VBCSCompiler\BuildServerController.cs | Handle different pipename for case sensitive compile
VBCSCompiler\NamedPipeClientConnectionHost.cs | Detect case sensitivity from the pipe name

## Files in the Roslyn\src\Compilers\Shared folder

File | Description
---- | -----------
BuildClient.cs | Determine and encode SysDir, WinDir and IncludeDir in the libDirectory parameter
BuildServerConnection.cs | Handle special pipe name for case sensitive compilation


## Files in the Roslyn\src\Scripting\Core folder

File | Description
---- | -----------
ScriptBuilder.cs | XSharp returns all compiler errors in the exception and not just the first one
ScriptOptions.cs | XSharp has additional options in a XsOptions property. Therefore there is also a WithXSharpSpecificOptions method.
ScriptState.cs | We have added some helper methods that make it easier to find a method in the compiled script


## Files in the Roslyn\src\Scripting\CSharp folder

File | Description
---- | -----------
CSharpScriptCompiler.cs | Added extra parameter that contains the X# specific compilation options.

