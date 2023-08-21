// C750. Runtime error with AddressOf operator in parameter when calling CLIPPER method in x86 platform
// https://github.com/X-Sharp/XSharpPublic/issues/551

/*
System.Runtime.InteropServices.SEHException
External component has thrown an exception.
*/

// vo7- (disabled)
// platform:x86
FUNCTION Start() AS VOID
LOCAL o AS TestClass
LOCAL u AS USUAL
LOCAL n AS INT
LOCAL cRet AS STRING

o := TestClass{}
n := 1
? "old value", n
? "caller addressof:", AsHexString(@n)
cRet := o:UnTypedByRef(@n) // OK
? "callee addressof:", cRet
? "new value", n // 1, correct
?
xAssert(AsHexString(@n) == cRet)
xAssert(n == 1)

u := 2
? "old value", u
? "caller addressof:", AsHexString(@u)
cRet := o:UnTypedByRef(@u) // SEHException <-------------
xAssert(AsHexString(@u) == cRet)
? "callee addressof:", cRet
? "new value", u
xAssert(AsHexString(@u) == cRet)
xAssert(u == 2)


// same problem with OBJECT, STRING, ARRAY
LOCAL a := {} AS ARRAY
LOCAL c := "abc" AS STRING
LOCAL obj := 123 AS OBJECT
o:UnTypedByRef(@a,@obj)
o:UnTypedByRef(@a,@obj,@c)
o:UnTypedByRef(@c)
o:UnTypedByRef(@o)


CLASS TestClass
	METHOD UnTypedByRef(a)
		LOCAL cRet AS STRING
		cRet := AsHexString(a)
		a := 123
	RETURN cRet
END CLASS


PROC xAssert(l AS LOGIC) 
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"   
RETURN 	
