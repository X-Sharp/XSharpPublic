// 445.error XS0208: Cannot take the address of, get the size of, or declare a pointer to a managed type ('__Usual')
FUNCTION Start() AS VOID
? SizeOf(USUAL)
? _SizeOf(USUAL)
xAssert(SizeOf(USUAL) == 28)
xAssert(_SizeOf(USUAL) == 28)

? SizeOf(DATE)
? _SizeOf(DATE)
xAssert(SizeOf(DATE) == 8)
xAssert(_SizeOf(DATE) == 8)

? SizeOf(FLOAT)
? _SizeOf(FLOAT)
xAssert(SizeOf(FLOAT) == 16)
xAssert(_SizeOf(FLOAT) == 16)

? SizeOf(SYMBOL)
? _SizeOf(SYMBOL)
xAssert(SizeOf(SYMBOL) == 4)
xAssert(_SizeOf(SYMBOL) == 4)

? SizeOf(BYTE)
? _SizeOf(BYTE)
xAssert(SizeOf(BYTE) == 1)
xAssert(_SizeOf(BYTE) == 1)

? SizeOf(WORD)
? _SizeOf(WORD)
xAssert(SizeOf(WORD) == 2)
xAssert(_SizeOf(WORD) == 2)

RETURN

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"

