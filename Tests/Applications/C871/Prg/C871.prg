// 871. ICE with DELEGATE with default value in parameter
// https://github.com/X-Sharp/XSharpPublic/issues/1129
/*
error XS9999: An internal compiler error has occurred: 'Object reference not set to an instance of an object.',    at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.XSharpTreeTransformationRT.ExitParameter(ParameterContext context)
   at LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser.ParameterContext.ExitRule(IParseTreeListener listener)
   at LanguageService.SyntaxTree.Tree.ParseTreeWalker.ExitRule(IParseTreeListener listener, IRuleNode r)
   at LanguageService.SyntaxTree.Tree.ParseTreeWalker.Walk(IParseTreeListener listener, IParseTree t)
   at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.XSharpLanguageParser.ParseCompilationUnitCore()
*/
DELEGATE Test(nParam := 0 AS INT) AS VOID

FUNCTION Start() AS VOID
RETURN
