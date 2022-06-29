// 773. Problem with iif() return values in VO dialect
// https://github.com/X-Sharp/XSharpPublic/issues/606
#pragma warnings(219, off) //   assigned but not used
FUNCTION Start() AS VOID
	LOCAL lTrue := TRUE AS LOGIC
	LOCAL n := -1 AS INT
	LOCAL n16 := -1 AS SHORTINT
	LOCAL w := 256 AS WORD
	LOCAL dw := 65536 AS DWORD
	LOCAL nn := 1000 AS INT

	? iif(lTrue,1,1) * -1    // VO: -1    X#: 255
	? iif(lTrue,1,1) * n     // VO: -1    X#: 255
	? iif(lTrue,1,1) * n16   // VO: -1    X#: 255
	? iif(lTrue,1,1) * dw    // VO: 65536 X#: 0
	? iif(lTrue,1,1) * dw    // VO: 65536 X#: 0
	? iif(lTrue,1,1) * nn    // VO: 1000  X#: 232
	? iif(lTrue,256,1) * 256 // VO: 65536 X#: 0
?
	? iif(TRUE,1,1) * -1     // VO & X#: -1
	? iif(TRUE,1,1) * n      // VO & X#: -1
	? iif(TRUE,1,1) * n16    // VO & X#: -1
	? iif(TRUE,1,1) * dw     // VO & X#: 65536
	? iif(TRUE,1,1) * dw     // VO & X#: 65536
	? iif(TRUE,1,1) * nn     // VO & X#: 1000
	? iif(TRUE,256,1) * 256  // VO & X#: 65536
?
	? iif(lTrue,1,1):GetType():ToString() // BYTE
	? iif(TRUE,1,1):GetType():ToString()  // BYTE

	xAssert( iif(TRUE,1,1) * -1    == -1)
	xAssert( iif(TRUE,1,1) * n     == -1)
	xAssert( iif(TRUE,1,1) * n16   == -1)
	xAssert( iif(TRUE,1,1) * dw    == 65536)
	xAssert( iif(TRUE,1,1) * dw    == 65536)
	xAssert( iif(TRUE,1,1) * nn    == 1000)
	xAssert( iif(TRUE,256,1) * 256 == 65536)

	xAssert( iif(lTrue,1,1) * -1    == -1)
	xAssert( iif(lTrue,1,1) * n     == -1)
	xAssert( iif(lTrue,1,1) * n16   == -1)
	xAssert( iif(lTrue,1,1) * dw    == 65536)
	xAssert( iif(lTrue,1,1) * dw    == 65536)
	xAssert( iif(lTrue,1,1) * nn    == 1000)
	xAssert( iif(lTrue,256,1) * 256 == 65536)
RETURN

PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF

