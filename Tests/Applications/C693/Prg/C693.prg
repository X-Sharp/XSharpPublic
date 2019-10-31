// 693. error XS9999: An internal compiler error has occurred

// All following lines cause an ICE. Instead they should all report a normal compiler error.

// VO dialect, /vo1+ enabled
CLASS TestClass
	DESTRUCTOR()
	RETURN SUPER:Axit()
	DESTRUCTOR()
	RETURN SELF:Axit()
	DESTRUCTOR()
	? SELF:Axit()
	CONSTRUCTOR()
	RETURN SUPER:Axit()
	CONSTRUCTOR()
	RETURN SELF:Axit()
	METHOD Test() AS VOID
		LOCAL o AS OBJECT
		o := SELF:Axit()
	RETURN
END CLASS

/*
C:\xSharp\Dev\Tests\Applications\C693\Prg\C693.prg(1,1): error XS9999: An internal compiler error has occurred: 'Unable to cast object of type 'LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.EmptyStatementSyntax' to type 'LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.ExpressionSyntax'.',    at LanguageService.CodeAnalysis.XSharp.SyntaxParser.RuleExtensions.Get[T](IXParseTree t) in C:\XSharp\Dev\XSharp\src\Compiler\XSharpCodeAnalysis\Parser\XSharpParserCode.cs:line 951
   at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.XSharpTreeTransformationCore.ExitAssignmentExpression(AssignmentExpressionContext context) in C:\XSharp\Dev\XSharp\src\Compiler\XSharpCodeAnalysis\Parser\XSharpTreeTransformationCore.cs:line 6756
   at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.XSharpTreeTransformationRT.ExitAssignmentExpression(AssignmentExpressionContext context) in C:\XSharp\Dev\XSharp\src\Compiler\XSharpCodeAnalysis\Parser\XSharpTreeTransformationRT.cs:line 1801
   at LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser.AssignmentExpressionContext.ExitRule(IParseTreeListener listener) in C:\XSharp\Dev\XSharp\src\Compiler\XSharpCodeAnalysis\Generated\XSharpParser.cs:line 12524
   at LanguageService.SyntaxTree.Tree.ParseTreeWalker.ExitRule(IParseTreeListener listener, IRuleNode r) in C:\XSharp\Dev\XSharp\src\Compiler\XSharpCodeAnalysis\Antlr4.Runtime\Tree\ParseTreeWalker.cs:line 96
   at LanguageService.SyntaxTree.Tree.ParseTreeWalker.Walk(IParseTreeListener listener, IParseTree t) in C:\XSharp\Dev\XSharp\src\Compiler\XSharpCodeAnalysis\Antlr4.Runtime\Tree\ParseTreeWalker.cs:line 54
   at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.XSharpLanguageParser.ParseCompilationUnitCore() in C:\XSharp\Dev\XSharp\src\Compiler\XSharpCodeAnalysis\Parser\XSharpLanguageParser.cs:line 394


*/
