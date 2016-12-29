// 368. cannot define DEFINE AS OBJECT/USUAL etc
// error XS9002: Parser: mismatched input 'USUAL'
// error XS0134: 'Functions.asobject' is of type 'object'. A const field of a reference type other than string can only be initialized with null.
DEFINE asint := 0 AS INT
DEFINE asusual := "usual" AS USUAL
DEFINE asobject := "object" AS OBJECT
DEFINE asptr := NULL_PTR AS PTR
DEFINE aspsz := NULL_PSZ AS PSZ
DEFINE asdate := NULL_DATE AS DATE
DEFINE asarray := NULL_ARRAY AS ARRAY

FUNCTION Start() AS VOID
? asint
? asusual
? asptr
