// 860. Runtime error with AScan() and OBJECT param passed
// /lb+
FUNCTION Start() AS VOID
LOCAL a AS ARRAY
LOCAL o AS OBJECT
LOCAL u AS USUAL

a := {}

xAssert( AScan(a,NIL) == 0)	// OK
xAssert(AScan(a,1) == 0)	// OK
xAssert(AScan(a,NULL_SYMBOL) == 0)	// OK
xAssert(AScan(a,#ABC) == 0)	// OK
xAssert(AScan(a,Today()) == 0)	// OK
xAssert(AScan(a,NULL_OBJECT) == 0)
xAssert(AScan(a,TestClass{})  == 0)// OK
o := TestClass{}
u := o
xAssert(AScan(a,u)  == 0)// OK
xAssert(AScan(a,o)  == 0)// Unable to cast object of type 'System.Collections.ArrayList' to type 'System.Func`2[XSharp.__Usual,System.Boolean]'.

CLASS TestClass
END CLASS

PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
