// R789 - FoxPro Public arrays
// https://github.com/X-Sharp/XSharpPublic/issues/662

#pragma warnings(9073, off) // undeclared, field or memvar


FUNCTION Start( ) AS VOID
    CreateLocalArray()
	CreatePublicArray()

	IF IsArray ( pubarray )
        xAssert( pubarray IS __FOXARRAY )

		? "i am a public "
		IF pubarray IS __FOXARRAY
			?? "fox array - "
		ELSE
			?? "none fox array - "
		ENDIF

		?? ClassName ( pubarray )
		pubarray := 10
        ? pubarray[1]
	ENDIF

	RETURN

FUNCTION CreatePublicArray() AS VOID
    PUBLIC ARRAY pubarray [2,2]    // With ARRAY keyword and brackets
    PUBLIC pubarray2 (2,2)          // Without ARRAY keyword and with parentheses
	RETURN


PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN


FUNCTION CreateLocalArray() AS VOID
    LOCAL ARRAY localarray[1]
    LOCAL localvar
    DIMENSION localarray(10,10)
    ? localarray[1,1]
    ? localarray[2]
    ? localarray[3]
    localarray := 5
    localvar := 5
    xAssert(localarray[1] == 5)
    xAssert(localarray[2] == 5)
    ? localarray[1]
    ? localarray[2]
    ? localvar
    xAssert(localvar == 5)
    STORE 42 TO ARRAY localarray
    xAssert(localarray[1,1] == 42)
    xAssert(localarray[2,2] == 42)
    ? localarray[1,1]
    ? localarray[2,2]

