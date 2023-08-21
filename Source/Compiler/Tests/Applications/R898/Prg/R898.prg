// See https://github.com/X-Sharp/XSharpPublic/issues/1229
// Debuggerhidden does not apply to the codeblock inside the method.
using System.Diagnostics

FUNCTION Start( ) AS VOID
	local cb as CODEBLOCK
	cb := GetBlock()
    ? Eval(cb,1,2)
    ? Eval(cb, Today(),1)
    wait
RETURN

[DebuggerHidden()][DebuggerStepThrough()];
FUNCTION GetBlock as CodeBlock STRICT
    return {|a,b| a+b}

