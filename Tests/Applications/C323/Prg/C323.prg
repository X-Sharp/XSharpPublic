// 323. error XS0029: Cannot implicitly convert type 'logic*' to 'object'
// error XS0030: Cannot convert type 'object' to 'void*'
// LOGIC * to OBJECT does not compile in Vulcan too
// But we made it possible in X# when /vo7 is enabled
#pragma warnings(165, off) // variable not assigned
FUNCTION Start() AS VOID
LOCAL lMemoHandle AS LOGIC
LOCAL teststruct  IS test
LOCAL testPtr  AS PTR
LOCAL testIntPtr  AS IntPtr
LOCAL oMemoHandle AS OBJECT
oMemoHandle := @lMemoHandle
xAssert(oMemoHandle != NULL)
oMemoHandle := @teststruct
xAssert(oMemoHandle != NULL)
oMemoHandle := testptr
xAssert(oMemoHandle != NULL)
testPtr     	:= (PTR) oMemoHandle
xAssert(testPtr == NULL_PTR)
oMemoHandle  := TestIntptr
xAssert(oMemoHandle != NULL)
testIntPtr     := (IntPtr) oMemoHandle
xAssert(testIntPtr == Intptr.Zero)





VOSTRUCT test
	MEMBER A as long


PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
