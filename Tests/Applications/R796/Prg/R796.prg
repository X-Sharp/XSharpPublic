// R796 Incorrect structure size causes variable to become corruped
// https://github.com/X-Sharp/XSharpPublic/issues/735

FUNCTION Start() AS VOID STRICT

	LOCAL oObj AS OBJECT
	LOCAL st1 IS MYSTRUCT
	LOCAL st2 IS MYSTRUCT
	LOCAL st3 IS MYSTRUCT
	LOCAL oObj2 AS OBJECT

	? "_SIZEOF(MYSTRUCT):", _SIZEOF(MYSTRUCT)

	oObj := 42 
	oObj2 := 424242
	st1:dwVal1 := Uint32.MaxValue
	st2:dwVal1 := Uint32.MaxValue
	st3:dwVal1 := Uint32.MaxValue

	MemClear(@st2, _SIZEOF(MYSTRUCT))

	? st1:dwVal1
	? st2:dwVal1
	? st3:dwVal1
	xAssert( st1:dwVal1 == Uint32.MaxValue)
	xAssert( st2:dwVal1 == 0)
	xAssert( st3:dwVal1 == Uint32.MaxValue)

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
