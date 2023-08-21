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

// warning XS9076: Empty CASE statement
DO CASE
END CASE

// no warnings at all below:
DO CASE
CASE FALSE
CASE TRUE
ENDCASE

IF TRUE
ELSEIF FALSE
ELSE
ENDIF

DO WHILE FALSE
ENDDO

TRY
    CATCH AS Exception
FINALLY
END TRY

FOR var i := 1 to 10
NEXT
FOREACH VAR x in System.Collections.Generic.List<INT>{}
NEXT

WITH Exception{}
END WITH
BEGIN SCOPE
END SCOPE
BEGIN CHECKED
END CHECKED
BEGIN UNCHECKED
END UNCHECKED
BEGIN LOCK Exception{}
END LOCK
local o as object
o := Exception{}
BEGIN USING (IDisposable) o
END USING
