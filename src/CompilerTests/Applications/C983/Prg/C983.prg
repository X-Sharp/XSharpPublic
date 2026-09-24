// 983. PTR problems in .Net 10
// https://github.com/X-Sharp/XSharpPublic/issues/2069

#pragma options("vo6", on)
#pragma options("unsafe", on)

DEFINE F_ERROR := IntPtr{-1} // Error value (all functions)

FUNCTION Start() AS VOID
LOCAL h AS PTR
h := NULL_PTR
IF h == NULL_PTR
    ? "null"
ENDIF
xAssert( h == NULL_PTR )

IF h != NULL_PTR
    ? "not null"
ENDIF

xAssert( .not. h != NULL_PTR )


LOCAL u AS USUAL
u := NULL_PTR

LOCAL p AS PTR
p := u

? p

xAssert( p == u )

LOCAL hFile AS PTR
? hFile == F_ERROR
? hFile != F_ERROR

xAssert( hFile != F_ERROR )
xAssert( .not. hFile == F_ERROR )

IF (hFile == F_ERROR)
ENDIF
IF (hFile != F_ERROR)
ENDIF


LOCAL pTest AS test PTR
IF pTest == NULL_PTR
    ? "null"
ENDIF
xAssert( pTest == NULL_PTR )


pTest := IntPtr{123}
IF pTest != NULL_PTR
    ? "not null"
ENDIF
xAssert( pTest != NULL_PTR )


STATIC FUNCTION test( ) AS PTR PASCAL
RETURN NULL_PTR

PROC xAssert(l AS LOGIC) AS VOID
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF

