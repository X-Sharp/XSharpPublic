// 933. ICE when using something like i"{ex:Message})"
// https://github.com/X-Sharp/XSharpPublic/issues/1672
FUNCTION Start() AS VOID
	LOCAL c AS STRING
	LOCAL IMPLIED a := System.Collections.ArrayList{}{1,2,3}
	
	#pragma options (allowdot , off)
	c := i"count is {a.Count}" // error XS0118: 'a' is a variable but is used like a type, OK
	? c
	c := i"count is {(a.Count)}" // error XS0118: 'a' is a variable but is used like a type, OK
	? c

	c := i"count is {a:Count}" // OK
	? c
	c := i"count is {(a:Count)}" // OK
	? c

	#pragma options (allowdot , on)
	c := i"count is {a.Count}" // OK
	? c
	c := i"count is {(a.Count)}" // OK
	? c

	c := i"count is {a:Count}" // no compiler error, maybe a warning should be isued?
	? c
	c := i"count is {(a:Count)}" // Internal compiler error
	? c
	c := i"count is {(a:Count}" // Internal compiler error (instead of syntax error)
	? c
/*
C:\xSharp\Dev\src\CompilerTests\Applications\C933\Prg\C933.prg(1,1): error XS9999: An internal compiler error has occurred: 'Object reference not set to an instance of an object.',    at LanguageService.CodeAnalysis.GreenNode.AdjustFlagsAndWidth(GreenNode node)
   at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.ArgumentSyntax..ctor(SyntaxKind kind, NameColonSyntax nameColon, SyntaxToken refKindKeyword, ExpressionSyntax expression, SyntaxFactoryContext context)
   at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.ContextAwareSyntax.Argument(NameColonSyntax nameColon, SyntaxToken refKindKeyword, ExpressionSyntax expression)
   at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.XSharpTreeTransformationCore.CreateInterPolatedStringExpression(IToken token, XSharpParserRuleContext context)
   at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.XSharpTreeTransformationCore.ExitLiteralValue(LiteralValueContext context)
   at LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser.LiteralValueContext.ExitRule(IParseTreeListener listener)
   at LanguageService.SyntaxTree.Tree.ParseTreeWalker.ExitRule(IParseTreeListener listener, IRuleNode r)
   at LanguageService.SyntaxTree.Tree.ParseTreeWalker.Walk(IParseTreeListener listener, IParseTree t)
   at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.XSharpLanguageParser.ParseCompilationUnitCore()
*/
