// https://github.com/X-Sharp/XSharpPublic/issues/1017
FUNCTION Start() AS VOID

	? Test3()

	RETURN


FUNCTION Test3() AS STRING

	? Test4()

RETURN "Test3"

// ----------
LOCAL FUNCTION Test4() AS STRING
RETURN "Test4"
END FUNCTION
// ----------

// RETURN "Test3"

END FUNCTION
