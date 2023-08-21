// 635. XSharp.Error: Conversion Error from USUAL (PTR)  to LONGINT
/*
The following compiler and runs fine when referencing the vulcan runtime.
When using the X# runtime, the conversions from PTR inside USUAL to INT/DWORD throw an exception

This is a cut down sample from the SDK where this is used, for example in Menu:AppendItem():

lRetVal := AppendMenu(hMenu, _OR(MF_BYCOMMAND, MF_POPUP, MF_ENABLED), DWORD(_CAST, nItemID:Handle()), String2Psz(cNewItem))
*/
FUNCTION Start() AS VOID
	LOCAL u AS USUAL
	LOCAL d AS DWORD
	LOCAL n AS INT
	
	u := @d
	? u

	n := u
	? AsHexString(n)
	xAssert( n == (INT)u )

	n := INT(u)
	? AsHexString(n)
	xAssert( n == (INT)u )

	d := u
	? AsHexString(d)
	xAssert( (INT)d == (INT)u ) // ok

	d := DWORD(u)
	? AsHexString(d)
	xAssert( (INT)d == (INT)u ) // ok

	n := INT(_CAST , u)
	? AsHexString(n)
	xAssert( n == (INT)u )

	d := DWORD(_CAST , u)
	? AsHexString(d)
	xAssert( d == (DWORD)u )
	
RETURN

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"

