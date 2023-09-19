// 510. DEFINE/GLOBAL incorrectly get precedence over CLASS member
#pragma warnings(219, off) // assigned not used

DEFINE dNAME := "abc"
GLOBAL gNAME := "abc" AS STRING

FUNCTION Start() AS VOID
	? dNAME
	? gNAME
	? SLen(dNAME)
	? SLen(gNAME)
	TestClass{}:Test()

	AnotherClass{}:Test()
RETURN


CLASS TestClass
	EXPORT dName := 1 AS INT
	EXPORT gName := 2 AS INT

	METHOD Test() AS VOID
		LOCAL n AS INT
		n := SELF:dName
		n := SELF:gName
		n := dName
		n := gName
		SELF:TestInt(dName)
		SELF:TestInt(gName)
	RETURN
	METHOD TestInt(n AS INT) AS VOID
		? n
	RETURN
END CLASS

CLASS AnotherClass
	PROTECT aSort := {1,2,30,4,5} AS ARRAY
	METHOD Test() AS VOID
		? aSort[3]
	RETURN
END CLASS
FUNCTION SLen(c AS STRING) AS DWORD
RETURN (DWORD)c:Length
