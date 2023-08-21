// 395. error XS0035: Operator '++' is ambiguous on an operand of type 'IntPtr'
// error XS0131: The left-hand side of an assignment must be a variable, property or indexer
FUNCTION Start() AS VOID
TestClass{}:Test()

CLASS TestClass
PROTECT ptrRTFChar AS BYTE PTR
METHOD Test() AS VOID
	LOCAL DIM bytes[100] AS BYTE
	bytes[1] := 1
	bytes[2] := 2
	bytes[3] := 4
	bytes[4] := 8

	SELF:ptrRTFChar := @bytes[1]
	? BYTE(SELF:ptrRTFChar)
	xAssert(BYTE(SELF:ptrRTFChar) == 1)
	
	++SELF:ptrRTFChar
	? BYTE(SELF:ptrRTFChar)
	xAssert(BYTE(SELF:ptrRTFChar) == 2)

	SELF:ptrRTFChar++
	? BYTE(SELF:ptrRTFChar)
	xAssert(BYTE(SELF:ptrRTFChar) == 4)

    BYTE(SELF:ptrRTFChar) := 10
	? BYTE(SELF:ptrRTFChar)
	xAssert(BYTE(SELF:ptrRTFChar) == 10)
END CLASS

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result"}
END IF

