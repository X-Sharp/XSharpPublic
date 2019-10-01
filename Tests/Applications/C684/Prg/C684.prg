// 684. error XS0121: The call is ambiguous between the following methods or properties: 'C684.Exe.Functions.Test(XSharp.__Float)' and 'C684.Exe.Functions.Test(logic)'
FUNCTION Start( ) AS VOID
	LOCAL n AS DWORD
	Test(n) // ok, float
	Test(DWORD(n)) // ok, float
	Test(DWORD(_CAST, n)) // ok, float
	Test((DWORD) n) // compiler error XS0121

	Test((INT) n) // compiler error XS0121
	Test(INT(n))
	Test(n)

	LOCAL u AS USUAL
	u := 1
	Test((INT) u) // compiler error XS0121
RETURN

FUNCTION Test(pfFloat AS FLOAT) AS VOID
? "float"
FUNCTION Test(plLogic AS LOGIC) AS VOID
? "logic"
