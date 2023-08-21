// 853. Problem with the numeric shift operators, when /vo4 is disabled
FUNCTION Start() AS VOID
// error XS0019: Operator '<<' cannot be applied to operands of type 'dword' and 'dword'
// (in all lines)
xAssert( 1u << 2u == 4)
xAssert( 4u >> 1u == 2)
xAssert( 2u << 2u == 8)


PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
