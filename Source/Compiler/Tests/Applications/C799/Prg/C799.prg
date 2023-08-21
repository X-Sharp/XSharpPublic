// 799. Late-bound method calls and OUT parameters
// https://github.com/X-Sharp/XSharpPublic/issues/771
// lb+
#pragma warnings(165, off) //   unassigned local
FUNCTION Start() AS VOID STRICT

	LOCAL oOwner AS OBJECT
	oOwner := TestClass{}

	oOwner:OutParam(OUT cOut1 AS STRING)
	? cOut1 // empty, WRONG!
	xAssert(cOut1 == "out")

	LOCAL cOut2 AS STRING
	oOwner:OutParam(OUT cOut2)
	? cOut2 // empty, WRONG!
	xAssert(cOut2 == "out")

	LOCAL cRef AS STRING
	oOwner:RefParam(REF cRef)
	? cRef // ref, OK
	xAssert(cRef == "ref")

	RETURN

CLASS TestClass
	METHOD OutParam(cNum OUT STRING) AS VOID STRICT
		cNum := "out"
		RETURN
	METHOD RefParam(cNum REF STRING) AS VOID STRICT
		cNum := "ref"
		RETURN
END CLASS

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
