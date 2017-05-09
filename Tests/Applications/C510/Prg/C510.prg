// 510. DEFINE/GLOBAL incorrectly get precedence over CLASS member

DEFINE dNAME := "abc"
GLOBAL gNAME := "abc" AS STRING

FUNCTION Start() AS VOID
	? dNAME
	? gNAME
	? SLen(dNAME)
	? SLen(gNAME)
	TestClass{}:Test()
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
		TestInt(dName)
		TestInt(gName)
	RETURN
	METHOD TestInt(n AS INT) AS VOID
		? n
	RETURN
END CLASS

