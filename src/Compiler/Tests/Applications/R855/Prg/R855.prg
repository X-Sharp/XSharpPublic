// R855
// https://github.com/X-Sharp/XSharpPublic/issues/1017
FUNCTION Start() AS VOID

	? Test3()
    xAssert(Test3() == "Test3")
	RETURN


FUNCTION Test3() AS STRING

	? Test4()
    xAssert(Test4() == "Test4")
RETURN "Test3"

// ----------
LOCAL FUNCTION Test4() AS STRING
RETURN "Test4"
END FUNCTION
// ----------

// RETURN "Test3"

END FUNCTION

PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
