// 860. Runtime error with AScan() and OBJECT param passed
FUNCTION Start() AS VOID
Test1()
Test1_LB()
Test2()
Test2_LB()

#pragma options ("lb", false)
PROCEDURE Test1()
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

#pragma options ("lb", true)
PROCEDURE Test1_LB()
LOCAL a AS ARRAY
LOCAL o AS OBJECT
LOCAL u AS USUAL

a := {}

? AScan(a,NIL)	// OK
? AScan(a,1)	// OK
? AScan(a,NULL_SYMBOL)	// OK
? AScan(a,#ABC)	// OK
? AScan(a,Today())	// OK
? AScan(a,NULL_OBJECT)
? AScan(a,TestClass{}) // OK
o := TestClass{}
u := o
? AScan(a,u) // OK
? AScan(a,o) // Unable to cast object of type 'System.Collections.ArrayList' to type 'System.Func`2[XSharp.__Usual,System.Boolean]'.


#pragma options ("lb", false)
PROCEDURE Test2()
LOCAL aArray0 := {"SomeString1"} AS ARRAY
? AScan(aArray0, {|x|x=="1"})  // Works
? AScan(aArray0, {|x|x=="1"}, 1) // Fails - "Value does not fall within the expected range" 

#pragma options ("lb", true)
PROCEDURE Test2_LB()
LOCAL aArray0 := {"SomeString1"} AS ARRAY
? AScan(aArray0, {|x|x=="1"})  // Works
? AScan(aArray0, {|x|x=="1"}, 1) // Fails - "Value does not fall within the expected range" 

CLASS TestClass
END CLASS

PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
