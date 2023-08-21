// 668. Problem happens only when /vo4+ is enabled
// Following code is taken from the GUI SDK
FUNCTION Start() AS VOID
	xAssertFalse( DoTest(1) )// false, ok
	xAssertFalse( DoTest(65535) ) // false, ok
	xAssertFalse( DoTest(65536) ) // false, ok

	xAssertTrue( DoTest( 4287102976 ) ) // false, wrong!
	xAssertTrue( DoTest( uint32.MaxValue ) ) // false, wrong!
RETURN

FUNCTION DoTest(wParam AS DWORD) AS LOGIC
	? HiWord(wParam)
	? SHORTINT(_CAST, HiWord(wParam) )
	? (SHORTINT(_CAST, HiWord(wParam)) < 0)
RETURN (SHORTINT(_CAST, HiWord(wParam)) < 0)

PROC xAssertFalse(l AS LOGIC)
	xAssertTrue(.not. l)
PROC xAssertTrue(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF

