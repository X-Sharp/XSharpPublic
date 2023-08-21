#pragma warnings(9066, off) // Ambiguous
FUNCTION Start( ) AS VOID
	LOCAL oTest AS Test
	oTest := Test{}
    xAssert(oTest:TestMe() == "Left Function")
	? oTest:TestMe()
	xAssert(Test.SubStr("a",1,2) == "Test.SubStr")
RETURN


CLASS Test
    METHOD TestMe() AS STRING
        VAR result := SubStr("Substr Function",1,20)
        ? result
        xAssert(result == "Substr Function")
        result := SELF:Left("abc",1)
        xAssert(result == "SELF:LEFT")
        RETURN Left("Left Function",13)

    METHOD Left(cString AS STRING, nLen AS DWORD) AS STRING
        RETURN "SELF:LEFT"
    STATIC METHOD SubStr(cString AS STRING, nStart AS DWORD, nLen AS DWORD) AS STRING
        RETURN "Test.SubStr"
END CLASS

PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
