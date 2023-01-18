// https://github.com/X-Sharp/XSharpPublic/issues/1184
FUNCTION start AS VOID
VAR teststring := ie"{1}}"
? testString

testString := ie"{}" // An internal compiler error here
// c# reports "error CS1733: Expected expression"
RETURN

/*
R882.prg(1,1): error XS9999: An internal compiler error has occurred: 'Object reference not set to an instance of an object.',    at LanguageService.CodeAnalysis.GreenNode.AdjustFlagsAndWidth(GreenNode node) in D:\a\XSharpDev\XSharpDev\Roslyn\Src\Compilers\Core\Portable\Syntax\GreenNode.cs:line 117
   at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.ArgumentSyntax..ctor(SyntaxKind kind, NameColonSyntax nameColon, SyntaxToken refKindKeyword, ExpressionSyntax expression, SyntaxFactoryContext context) in D:\a\XSharpDev\XSharpDev\Roslyn\Src\Compilers\CSharp\Portable\Generated\CSharpSyntaxGenerator\CSharpSyntaxGenerator.SourceGenerator\Syntax.xml.Internal.Generated.cs:line 5576
   at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.ContextAwareSyntax.Argument(NameColonSyntax nameColon, SyntaxToken refKindKeyword, ExpressionSyntax expression) in D:\a\XSharpDev\XSharpDev\Roslyn\Src\Compilers\CSharp\Portable\Generated\CSharpSyntaxGenerator\CSharpSyntaxGenerator.SourceGenerator\Syntax.xml.Internal.Generated.cs:line 35293
   at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.XSharpTreeTransformationCore.CreateInterPolatedStringExpression(IToken token, XSharpParserRuleContext context) in D:\a\XSharpDev\XSharpDev\XSharp\src\Compiler\XSharpCodeAnalysis\Parser\XSharpTreeTransformationCore.cs:line 8988
   at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.XSharpTreeTransformationCore.ExitLiteralValue(LiteralValueContext context) in D:\a\XSharpDev\XSharpDev\XSharp\src\Compiler\XSharpCodeAnalysis\Parser\XSharpTreeTransformationCore.cs:line 9106
   at LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser.LiteralValueContext.ExitRule(IParseTreeListener listener) in D:\a\XSharpDev\XSharpDev\XSharp\src\Compiler\XSharpCodeAnalysis\Generated\XSharpParser.cs:line 20941
   at LanguageService.SyntaxTree.Tree.ParseTreeWalker.ExitRule(IParseTreeListener listener, IRuleNode r) in D:\a\XSharpDev\XSharpDev\XSharp\src\Compiler\XSharpCodeAnalysis\Antlr4.Runtime\Tree\ParseTreeWalker.cs:line 95
   at LanguageService.SyntaxTree.Tree.ParseTreeWalker.Walk(IParseTreeListener listener, IParseTree t) in D:\a\XSharpDev\XSharpDev\XSharp\src\Compiler\XSharpCodeAnalysis\Antlr4.Runtime\Tree\ParseTreeWalker.cs:line 54
   at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.XSharpLanguageParser.ParseCompilationUnitCore() in D:\a\XSharpDev\XSharpDev\XSharp\src\Compiler\XSharpCodeAnalysis\Parser\XSharpLanguageParser.cs:line 395
*/
