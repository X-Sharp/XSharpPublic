// 454. compiler crash with empty codeblock in command

#command TEST <expr> [OPTION <opt>] => DoTest(<expr> , <opt>)

FUNCTION DoTest(a,b)
	? a,b
RETURN NIL

FUNCTION Start() AS VOID
TEST {|n| n + 1} // ok
TEST {|n|} // compiler crash
RETURN
