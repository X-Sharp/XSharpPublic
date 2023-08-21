// 822. Internal Compiler Error with CLIPPER delegate
// https://github.com/X-Sharp/XSharpPublic/issues/932
// this should reprot a compiler error I think, but of course not caause an internal compiler error

/*
error XS9999: An internal compiler error has occurred: 'Object reference not set to an instance of an object.',    at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.XSharpTreeTransformationRT._getParameterType(ParameterContext context)
   at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.XSharpTreeTransformationCore.ExitParameter(ParameterContext context)
   at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.XSharpTreeTransformationRT.ExitParameter(ParameterContext context)
   at LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser.ParameterContext.ExitRule(IParseTreeListener listener)
   at LanguageService.SyntaxTree.Tree.ParseTreeWalker.ExitRule(IParseTreeListener listener, IRuleNode r)
   at LanguageService.SyntaxTree.Tree.ParseTreeWalker.Walk(IParseTreeListener listener, IParseTree t)
   at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.XSharpLanguageParser.ParseCompilationUnitCore()
*/


FUNCTION Start() AS VOID
LOCAL d AS test_delegate
LOCAL d2 AS test_delegate_clippercall
d := test                            
d2 := test
d(1,"a", today())
d2(1,"a", today())

DELEGATE test_delegate(a,b,c,d) 
// this now generates an error because the delegate has a clipper calling convention.
// if you want a delegate with a clipper calling convention try this
DELEGATE test_delegate_clippercall( args PARAMS USUAL[]) AS USUAL



FUNCTION test(a,b,c,d)
? a,b,c,d
RETURN NIL
