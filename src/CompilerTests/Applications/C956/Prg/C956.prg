// 956. Problems with PTR type in Net 10 #1793
// https://github.com/X-Sharp/XSharpPublic/issues/1793

FUNCTION EmptyPtr(uValue AS PTR) AS LOGIC
    RETURN (uValue == NULL_PTR) // error XS0034: Operator '==' is ambiguous on operands of type 'ptr' and 'nint'

DEFINE F_ERROR :=  IntPtr{-1}

FUNCTION Start() AS VOID
? EmptyPtr(NULL_PTR)
xAssert(EmptyPtr(NULL_PTR))

LOCAL uValue AS USUAL
uValue := @uValue

local i64 := (UIntPtr)uValue as UIntPtr
? i64, uValue
? (PTR)i64, (PTR)uValue
xAssert((PTR)i64 == (PTR)uValue)

local u64 := i64:ToUInt64() as uint64
? (int64)u64, (int64)I64
xAssert((Uint64)u64 == (Uint64)I64)


LOCAL hfTo      AS PTR
hfTo := (IntPtr) F_ERROR
? hfTo
? (Int64)hfTo
xAssert((Int64)hfTo == -1)
xAssert(hfTo == F_ERROR)

PROC xAssert(l AS LOGIC) 
	IF .NOT. l
		THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
	END IF
	? "Assertion passed"   
RETURN
