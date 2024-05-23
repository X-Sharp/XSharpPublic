// 907. A single comma in a line of code causes an internal compiler error
// https://github.com/X-Sharp/XSharpPublic/issues/1462
/*
error XS9999: An internal compiler error has occurred: 'Index was out of range. Must be non-negative and less than the size of the collection.
Parameter name: index',    at System.ThrowHelper.ThrowArgumentOutOfRangeException(ExceptionArgument argument, ExceptionResource resource)
   at System.Collections.Generic.List`1.get_Item(Int32 index)
   at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.PPRule.matchRestrictedToken(PPMatchToken mToken, IList`1 tokens, Int32& iSource, PPMatchRange[] matchInfo, IList`1 matchedWithToken)
   at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.PPRule.matchToken(PPMatchToken mToken, Int32& iRule, Int32 iLastRule, Int32& iSource, IList`1 tokens, PPMatchRange[] matchInfo, IList`1 matchedWithToken)
   at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.PPRule.Matches(IList`1 tokens, PPMatchRange[]& matchInfo)
   at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.PPRuleDictionary.FindMatchingRule(IList`1 tokens, PPMatchRange[]& matchInfo)
   at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.XSharpPreprocessor.doProcessCommands(IList`1 line, IList`1& result)
   at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.XSharpPreprocessor.doNormalLine(IList`1 line, Boolean write2PPO)
   at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.XSharpPreprocessor.ProcessLine(IList`1 line, Boolean write2ppo)
   at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.XSharpPreprocessor.PreProcess()
   at LanguageService.CodeAnalysis.XSharp.Syntax.InternalSyntax.XSharpLanguageParser.ParseCompilationUnitCore()
*/

FUNCTION Start( ) AS VOID
,
RETURN


