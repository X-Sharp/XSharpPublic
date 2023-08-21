// 802. Internal Compiler Error with multiple PRIVATE &varName commands
/*
error XS9999: An internal compiler error has occurred: 'An item with the same key has already been added.',    at System.ThrowHelper.ThrowArgumentException(ExceptionResource resource)
   at System.Collections.Generic.Dictionary`2.Insert(TKey key, TValue value, Boolean add)
   at LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser.EntityData.AddField(String Name, String Alias, XSharpParserRuleContext context)
   at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.XSharpTreeTransformationRT.addFieldOrMemvar(String name, String prefix, XSharpParserRuleContext context, Boolean isParameter)
   at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.XSharpTreeTransformationRT.AddAmpbasedMemvar(XSharpParserRuleContext context, String name, String alias, IToken amp)
   at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.XSharpTreeTransformationRT.EnterXbasedecl(XbasedeclContext context)
   at LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser.XbasedeclContext.EnterRule(IParseTreeListener listener)
   at LanguageService.SyntaxTree.Tree.ParseTreeWalker.Walk(IParseTreeListener listener, IParseTree t)
   at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.XSharpLanguageParser.ParseCompilationUnitCore()
*/
FUNCTION Start() AS VOID
	LOCAL cVarName AS STRING
	
	IF TRUE
	   cVarName := "myPrivate"
		PRIVATE &cVarName
		&cVarName := 1
		? &cVarName
		
		xAssert(&cVarName == 1)
	ENDIF
	IF TRUE
		PRIVATE &cVarName
		? &cVarName
		xAssert(&cVarName == 1)
		cVarName := "newtest"
		&cVarName := 2
		? &cVarName
		xAssert(&cVarName == 2)
		
		cVarName := "myPrivate"
		? &cVarName
		xAssert(&cVarName == 1)

		PRIVATE &cVarName
		xAssert(&cVarName == 1)
	ENDIF
	
RETURN


PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN	
