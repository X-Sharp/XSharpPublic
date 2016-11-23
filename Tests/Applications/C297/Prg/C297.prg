// 297. error XS0121: The call is ambiguous between the following methods or properties: 'Functions.Transform(__VOFloat, string)' and 'Functions.Transform(__Usual, string)'
// Vulcan resolves to the FLOAT overload
FUNCTION Start() AS VOID
LOCAL r := 1.2 AS REAL8
? Transform(r , "##.#")

FloatUsualFunc(r)

PROCEDURE FloatUsualFunc(u AS USUAL)
? "usual"
PROCEDURE FloatUsualFunc(f AS FLOAT)
? "float"

