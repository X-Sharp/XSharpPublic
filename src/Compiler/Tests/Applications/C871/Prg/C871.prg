// 871. ICE with DELEGATE with default value in parameter
// https://github.com/X-Sharp/XSharpPublic/issues/1129
/*
error XS9999: An internal compiler error has occurred: 'Object reference not set to an instance of an object.',    at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.XSharpTreeTransformationRT.ExitParameter(ParameterContext context)
   at LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser.ParameterContext.ExitRule(IParseTreeListener listener)
   at LanguageService.SyntaxTree.Tree.ParseTreeWalker.ExitRule(IParseTreeListener listener, IRuleNode r)
   at LanguageService.SyntaxTree.Tree.ParseTreeWalker.Walk(IParseTreeListener listener, IParseTree t)
   at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.XSharpLanguageParser.ParseCompilationUnitCore()
*/
DELEGATE Test(nParam := 0 AS INT) AS int
DELEGATE Add(a,b,c)
FUNCTION Start() AS VOID
    local delTest as Test
    local delAdd as Add
    delTest := Mul
    XAssert(delTest() == 0)
    XAssert(delTest(42) == 42*42)
    delAdd := Addme
    xAssert(delAdd(1,2,3) == 6)
    xAssert(delAdd('a','b','c') == "abc")

RETURN

Function Addme(a,b,c)
    return a+b+c

Function Mul(nParam as int) as int
    return nParam * nParam



PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF
