// 696. error XS9999: An internal compiler error has occurred: 'Object reference not set to an instance of an object.'
FUNCTION Start() AS VOID
LOCAL o AS TestClass
o := TestClass{}
? o[123]
o[1] := 2
RETURN

CLASS TestClass
	PROPERTY SELF[n AS INT] AS INT
		GET
			? __ENTITY__
			RETURN n * 2
		END GET
		SET
			? __ENTITY__
		END SET
	END PROPERTY
END CLASS

/*
C:\xSharp\Dev\Tests\Applications\C696\Prg\C696.prg(1,1): error XS9999: An internal compiler error has occurred: 'Object reference not set to an instance of an object.',    at LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser.PropertyContext.get_Name() in C:\XSharp\Dev\XSharp\src\Compiler\XSharpCodeAnalysis\Parser\XSharpParserCode.cs:line 558
   at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.XSharpTreeTransformationCore.GetNestedName(IRuleNode ctx) in C:\XSharp\Dev\XSharp\src\Compiler\XSharpCodeAnalysis\Parser\XSharpTreeTransformationCore.cs:line 321
   at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.XSharpTreeTransformationCore.GetEntityName(Boolean Full, Boolean funcNameOnly) in C:\XSharp\Dev\XSharp\src\Compiler\XSharpCodeAnalysis\Parser\XSharpTreeTransformationCore.cs:line 352
   at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.XSharpTreeTransformationCore.ExitLiteralValue(LiteralValueContext context) in C:\XSharp\Dev\XSharp\src\Compiler\XSharpCodeAnalysis\Parser\XSharpTreeTransformationCore.cs:line 8038
   at LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser.LiteralValueContext.ExitRule(IParseTreeListener listener) in C:\XSharp\Dev\XSharp\src\Compiler\XSharpCodeAnalysis\Generated\XSharpParser.cs:line 18891
   at LanguageService.SyntaxTree.Tree.ParseTreeWalker.ExitRule(IParseTreeListener listener, IRuleNode r) in C:\XSharp\Dev\XSharp\src\Compiler\XSharpCodeAnalysis\Antlr4.Runtime\Tree\ParseTreeWalker.cs:line 95
   at LanguageService.SyntaxTree.Tree.ParseTreeWalker.Walk(IParseTreeListener listener, IParseTree t) in C:\XSharp\Dev\XSharp\src\Compiler\XSharpCodeAnalysis\Antlr4.Runtime\Tree\ParseTreeWalker.cs:line 54
   at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.XSharpLanguageParser.ParseCompilationUnitCore() in C:\XSharp\Dev\XSharp\src\Compiler\XSharpCodeAnalysis\Parser\XSharpLanguageParser.cs:line 394
*/
