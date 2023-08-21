// 859. Incorrect evaluation of typed defines depending on declaration order
// https://github.com/X-Sharp/XSharpPublic/issues/1057

DEFINE TEST := PART_ALIAS AS DWORD // doesn't work if defined here
DEFINE PART := 5 AS DWORD
DEFINE PART_ALIAS := PART AS DWORD
//DEFINE TEST	:= PART_ALIAS // works here

FUNCTION Start() AS VOID STRICT
	? TEST // should be 5 and not 0
	xAssert(PART == 5)
	xAssert(PART_ALIAS == 5)
	xAssert(TEST == 5)
RETURN


PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
