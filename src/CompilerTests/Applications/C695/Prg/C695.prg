// 695. error XS9999: An internal compiler error has occurred: 'Object reference not set to an instance of an object.'
FUNCTION Start() AS VOID
LOCAL cAlias AS STRING
alias->DbGoTop()
(cAlias)->DbGoTop()
RETURN

/*
C:\xSharp\Dev\Tests\Applications\C695\Prg\C695.prg(1,1): error XS9999: An internal compiler error has occurred: 'Object reference not set to an instance of an object.',    at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.XSharpTreeTransformationCore.GenerateExpressionStatement(ExpressionSyntax expr, Boolean markAsGenerated) in C:\XSharp\Dev\XSharp\src\Compiler\XSharpCodeAnalysis\Parser\XSharpTreeTransformationCore.cs:line 1338
   at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.XSharpTreeTransformationCore.HandleExpressionStmt(IList`1 expressions) in C:\XSharp\Dev\XSharp\src\Compiler\XSharpCodeAnalysis\Parser\XSharpTreeTransformationCore.cs:line 6321
   at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.XSharpTreeTransformationCore.ExitExpressionStmt(ExpressionStmtContext context) in C:\XSharp\Dev\XSharp\src\Compiler\XSharpCodeAnalysis\Parser\XSharpTreeTransformationCore.cs:line 6342
   at LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser.ExpressionStmtContext.ExitRule(IParseTreeListener listener) in C:\XSharp\Dev\XSharp\src\Compiler\XSharpCodeAnalysis\Generated\XSharpParser.cs:line 9216
   at LanguageService.SyntaxTree.Tree.ParseTreeWalker.ExitRule(IParseTreeListener listener, IRuleNode r) in C:\XSharp\Dev\XSharp\src\Compiler\XSharpCodeAnalysis\Antlr4.Runtime\Tree\ParseTreeWalker.cs:line 95
   at LanguageService.SyntaxTree.Tree.ParseTreeWalker.Walk(IParseTreeListener listener, IParseTree t) in C:\XSharp\Dev\XSharp\src\Compiler\XSharpCodeAnalysis\Antlr4.Runtime\Tree\ParseTreeWalker.cs:line 54
   at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.XSharpLanguageParser.ParseCompilationUnitCore() in C:\XSharp\Dev\XSharp\src\Compiler\XSharpCodeAnalysis\Parser\XSharpLanguageParser.cs:line 394
*/
