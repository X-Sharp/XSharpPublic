// R796 Incorrect structure size causes variable to become corruped
// https://github.com/X-Sharp/XSharpPublic/issues/735

FUNCTION Start() AS VOID STRICT

	LOCAL oObj AS OBJECT
	LOCAL st IS MYSTRUCT
	LOCAL oObj2 AS OBJECT

	oObj := 42 
	oObj2 := 424242
	MemClear(@st, _SIZEOF(MYSTRUCT))
	xAssert( (INT) oObj == 42)
	xAssert( (INT) oObj2 == 424242)
    ? oObj
    ? oObj2
	RETURN

INTERNAL VOSTRUCT MYSTRUCT
	MEMBER dwVal1 AS DWORD
	MEMBER dwVal2 AS DWORD
	MEMBER DIM bArray1[17] AS BYTE
	MEMBER dDate AS DATE
	MEMBER DIM bArray2[221] AS BYTE
	MEMBER dwVal4 AS DWORD
	
	
PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN	
