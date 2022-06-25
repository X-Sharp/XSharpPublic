// 860. Runtime error with AScan() and OBJECT param passed
// /lb+
FUNCTION Start() AS VOID
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

CLASS TestClass
END CLASS

