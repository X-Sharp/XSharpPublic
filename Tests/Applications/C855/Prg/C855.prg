// 855. Problems with implicit numeric conversions
// Even though /vo4+ and /vo7+ are enabled, the following code reports a lot of error XS0266: Cannot implicitly convert type a to b
// Also, the syntax with ^= leads to an internal compiler error

/*
error XS9999: An internal compiler error has occurred: 'Operation is not valid due to the current state of the object.',    at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.TokenExtensions.ExpressionKindBinaryOp(IToken token)
   at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.XSharpTreeTransformationCore.ExitAssignmentExpression(AssignmentExpressionContext context)
   at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.XSharpTreeTransformationRT.ExitAssignmentExpression(AssignmentExpressionContext context)
   at LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser.AssignmentExpressionContext.ExitRule(IParseTreeListener listener)
   at LanguageService.SyntaxTree.Tree.ParseTreeWalker.ExitRule(IParseTreeListener listener, IRuleNode r)
   at LanguageService.SyntaxTree.Tree.ParseTreeWalker.Walk(IParseTreeListener listener, IParseTree t)
   at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.XSharpLanguageParser.ParseCompilationUnitCore()
*/

#pragma options ( "vo7", ON )
#pragma options ( "vo4", ON )
FUNCTION Start() AS VOID

	LOCAL i:=1 AS INT
	LOCAL d:=2 AS DWORD
	LOCAL w:=2 AS WORD
	LOCAL s:=3 AS SHORT
	LOCAL b:=3 AS BYTE

	b := 1

	// Internal Compiler Error with all the below
	? b ^= 1
	? b := b ^ 1
	? b ^= i
	? b := b ^ i
	? b ^= d
	? b := b ^ d
	? i ^= 1
	? i := i ^ 1
	? i ^= i
	? i := i ^ i
	? i ^= d
	? i := i ^ d


	?w ^= 1
	?w := i ^ 1
	?w ^= i
	?w := w ^ i
	?w ^= d
	?w := w ^ d


	?s ^= 1
	?s := i ^ 1
	?s ^= i
	?s := s ^ i
	?s ^= d
	?s := s ^ d
 ?

	// error XS0266: Cannot implicitly convert type a to b
	? b |= i
	? b |= d
	? b |= 2
	? b |= 2u

	? b += i
	? b += d
	? b += 2
	? b += 2u

	? b *= i
	? b *= d
	? b *= 2
	? b *= 2u

	? b &= i
	? b &= d
	? b &= 2
	? b &= 2u
//
	? b /= i
	? b /= d
	? b /= 2
	? b /= 2u
//
//
//
	? w |= i
	? w |= d
	? w |= 2
	? w |= 2u
//
	? w += i
	? w += d
	? w += 2
	? w += 2u

	? w *= i
	? w *= d
	? w *= 2
	? w *= 2u

	? w &= i
	? w &= d
	? w &= 2
	? w &= 2u

	? w /= i
	? w /= d
	? w /= 2
	? w /= 2u


	? s |= i
	? s |= d
	? s |= 2
	? s |= 2u

	? s += i
	? s += d
	? s += 2
	? s += 2u

	? s *= i
	? s *= d
	? s *= 2
	? s *= 2u

	? s &= i
	? s &= d
	? s &= 2
	? s &= 2u

	? s /= i
	? s /= d
	? s /= 2
	? s /= 2u
wait
RETURN


