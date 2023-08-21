// 592. error XS7038: Failed to emit module 'C592'.
FUNCTION Start() AS VOID
LOCAL u AS USUAL
u := NIL
u := Test(u)
//? u // crashes in vulcan!

u := TestPtr()
? u
u := NIL
u := TestPtr(u)
RETURN

FUNCTION Test(aRDDs:=NULL_ARRAY AS ARRAY) AS USUAL
// note that this crashes in vulcan with a null reference exception!
//	? AsString(aRDDs)
RETURN aRDDs

FUNCTION TestPtr(u := NULL_PTR AS PTR) AS USUAL
	? u
RETURN u

FUNCTION TestPsz(u := NULL_PSZ AS PSZ) AS USUAL
	? u
RETURN u

FUNCTION TestString1(u := NULL_STRING) AS USUAL
RETURN u
FUNCTION TestString2(u := NULL_STRING AS STRING) AS USUAL
RETURN u
FUNCTION TestString3(u := NULL_STRING AS USUAL) AS USUAL
RETURN u

