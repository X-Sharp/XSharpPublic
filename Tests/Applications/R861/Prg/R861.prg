
FUNCTION Start() AS VOID STRICT
Test{}
TestArrays()
CLASS Test
	PUBLIC s := "asd" AS STRING

	PROPERTY thisform AS Object GET SELF

	CONSTRUCTOR()
		? this.s:Length // OK
		? thisform:s:Length // OK
		? thisform.s:Length // error
		? thisform.s.Length // error
		? thisform.thisform.s.Length
		XAssert(this.s:Length == 3)
		XAssert(thisform:s:Length == 3)
		XAssert(thisform.s:Length == 3)
		XAssert(thisform.s.Length == 3)
		XAssert(thisform.thisform.s.Length == 3)
        s = "abcd" // old style assignments are allowed in the FoxPro dialect
        ? thisform.s.Length
        XAssert(thisform.s.Length == 4)

END CLASS



PROC xAssert(l AS LOGIC)  AS VOID
	IF l
		? "Assertion passed"
	ELSE
		THROW Exception{"Incorrect result"}
	END IF
RETURN


PROC TestArrays
LOCAL a1(10) AS ARRAY
LOCAL a2[10]  AS ARRAY
LOCAL ARRAY a3[10]
? ALen(a1)
? ALen(a2)
? ALen(a3)
RETURN
