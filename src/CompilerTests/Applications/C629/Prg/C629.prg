//629. Incorrect PSZ returned from function returning PSZ

// problem exists with both the vulcan and xsharp runtime

FUNCTION Start() AS VOID
	DoTest()
RETURN

PROCEDURE DoTest() PASCAL
	LOCAL p AS PSZ

	p := Test1()
	? Psz2String(p)
	xAssertEquals(Psz2String(p) , "test1") // error

	p := Test2()
	? Psz2String(p)
	// same as in VO, PSZ is freed when exiting function Test2()
//	xAssertEquals(Psz2String(p) , "test2") // error
RETURN

FUNCTION Test1() AS PSZ PASCAL
	LOCAL p AS PSZ
	p := PSZ(_CAST,"test1")
	? p
	xAssertEquals(Psz2String(p) , "test1") // ok
RETURN p
FUNCTION Test2() AS PSZ PASCAL
	LOCAL p AS PSZ
	p := String2Psz("test2")
	? p
	xAssertEquals(Psz2String(p) , "test2") // ok
RETURN p


PROC xAssertEquals(o1 AS STRING, o2 AS STRING)
IF o1 == o2
	? "Assertion passed"
ELSE
	THROW Exception{String.Format("Incorrect result, expected {0}, returned {1}" , o2 , o1)}
END IF

