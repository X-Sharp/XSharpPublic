// 888. Internal Compiler Error with DO CASE statement missing CASE/OTHERWISE
//https://github.com/X-Sharp/XSharpPublic/issues/1281

/*
warning XS9076: Empty CASE statement
error XS9999: An internal compiler error has occurred: 'Object reference not set to an instance of an object.', at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.XSharpTreeTransformationCore.ExitConditionalStatement(XSharpParserRuleContext context, IList`1 conditions, StatementBlockContext elseBlock)
at LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser.CaseStmtContext.ExitRule(IParseTreeListener listener)
at LanguageService.SyntaxTree.Tree.ParseTreeWalker.ExitRule(IParseTreeListener listener, IRuleNode r)
at LanguageService.SyntaxTree.Tree.ParseTreeWalker.Walk(IParseTreeListener listener, IParseTree t)
at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.XSharpLanguageParser.ParseCompilationUnitCore()

*/
FUNCTION Start( ) AS VOID
DO CASE

ENDCASE
