#pragma warnings(9043, off)
FUNCTION Start() AS VOID
LOCAL u AS USUAL
u := "abc"
? FGetFileName(u)
RETURN

FUNCTION FGetFileName(cFullFileName) CLIPPER
RETURN 123
