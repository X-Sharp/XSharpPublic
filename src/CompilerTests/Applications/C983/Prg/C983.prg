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
IF h != NULL_PTR
    ? "not null"
ENDIF


LOCAL u AS USUAL
u := NULL_PTR

LOCAL p AS PTR
p := u

? p

LOCAL hFile AS PTR
? hFile == F_ERROR
? hFile != F_ERROR

IF (hFile == F_ERROR)
ENDIF
IF (hFile != F_ERROR)
ENDIF


LOCAL pTest AS test PTR
IF pTest == NULL_PTR
    ? "null"
ENDIF

IF pTest != NULL_PTR
    ? "not null"
ENDIF


STATIC FUNCTION test( ) AS PTR PASCAL
RETURN NULL_PTR

