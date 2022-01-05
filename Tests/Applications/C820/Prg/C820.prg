// 820. Problem passing AddressOf and ByReference in the same call
// https://github.com/X-Sharp/XSharpPublic/issues/899
FUNCTION Start() AS VOID
LOCAL c := "test" AS STRING
LOCAL p AS PTR

p := 256
Test_A1(@p) 		// works ok
Test_A2(@p, @c) 	// System.NullReferenceException

p := 0x80000000
Test_B1(@p) 		// also works ok
Test_B2(@p, @c) 	// System.AccessViolationException: Attempted to read or write protected memory

FUNCTION Test_A1(p AS PTR) AS VOID 
? p
? PTR(p)
xAssert(PTR(p) == 256)

FUNCTION Test_A2(p AS PTR, c REF STRING) AS VOID 
? p
? PTR(p)
xAssert(PTR(p) == 256)

FUNCTION Test_B1(p AS PTR) AS VOID 
? p
? PTR(p)
xAssert(PTR(p) == 0x80000000)
FUNCTION Test_B2(p AS PTR, c REF STRING) AS VOID 
? p
? PTR(p)
xAssert(PTR(p) == 0x80000000)

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN	
