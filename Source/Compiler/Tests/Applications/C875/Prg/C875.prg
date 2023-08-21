// 875. MemVars created inside macros do not continue to exist after macro execution
// https://github.com/X-Sharp/XSharpPublic/issues/1182
FUNCTION Start( ) AS VOID
//PUBLIC somevar // works with that
//Evaluate('__MemVarDecl("somevar", false)') // also with that
? Evaluate("somevar := 123")
? Evaluate("somevar") // XSharp.Error: Variable does not exist
? somevar // XSharp.Error: Variable does not exist

