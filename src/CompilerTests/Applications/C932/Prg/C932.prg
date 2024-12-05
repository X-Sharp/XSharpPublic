// 932. Compiler crash with default values
// https://github.com/X-Sharp/XSharpPublic/issues/1647

// All of the function definitions below cause an ICE (in non-core dialects) instead of a proper error message:

FUNCTION TestFunc1( a AS INT, b := 85, c AS INT) AS VOID STRICT
FUNCTION TestFunc2( a AS INT, b := 85, c := 1 AS INT) AS VOID STRICT
FUNCTION TestFunc3( b := 85, c AS INT) AS VOID STRICT
FUNCTION TestFunc4( b := 85, c := 1 AS INT) AS VOID STRICT
