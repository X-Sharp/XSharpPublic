// 932. Compiler crash with default values
// https://github.com/X-Sharp/XSharpPublic/issues/1647

// All of the function definitions below cause an ICE (in non-core dialects) instead of a proper error message:

FUNCTION TestFunc1( a AS INT, b := 85, c AS INT) AS INT STRICT
	? a,b,c
RETURN a+b+c
FUNCTION TestFunc2( a AS INT, b := 85, c := 1 AS INT) AS INT STRICT
	? a,b,c
RETURN a+b+c
FUNCTION TestFunc3( b := 85, c AS INT) AS INT STRICT
	? b,c
RETURN b+c
FUNCTION TestFunc4( b := 85, c := 1 AS INT) AS INT STRICT
	? b,c
RETURN b+c
	
FUNCTION Start() AS VOID
xAssert( TestFunc1(1,2,3) == 6)
xAssert( TestFunc1(1,,3) == 89)

xAssert( TestFunc2(1,2,3) == 6)
xAssert( TestFunc2(1) == 87)
xAssert( TestFunc2(1,1) == 3)
xAssert( TestFunc2(1,,10) == 96)

xAssert( TestFunc3(1,2) == 3)
xAssert( TestFunc3(,2) == 87)

xAssert( TestFunc4(1,2) == 3)
xAssert( TestFunc4() == 86)


PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF
RETURN
