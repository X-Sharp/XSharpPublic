// 170. compiler crash
// vulcan dialect
GLOBAL g := Test() AS STRING
GLOBAL g1 := 1 AS INT
GLOBAL g2 := g1 AS INT
FUNCTION Test() AS STRING STRICT
RETURN "abc"
FUNCTION Start() AS VOID
? g,g1,g2

