// 743. Runtime error passing pointer to VOSTRUCT to untyped constructor
/*
Following throws a runtime error when calling the construtor with a pointer to a VOSTRUCT. 
Same scenario with a method seems to work ok. 
Code must be compiled with /vo7+

Description :   Conversion Error from USUAL (OBJECT)  to PTR
Subsystem :     BASE
GenCode :       EG_DATATYPE Data type error
FuncSym :       USUAL => PTR
Severity :      ES_ERROR
Can Default :   False
Can Retry :     False
Can Substitute :        False
Argument Type : PTR
Argument Number :       1
Argument :      USUAL
Arguments :     {strTest}
Expected Argument Type :        System.IntPtr
Stack Trace :
   at XSharp.__Usual.op_Implicit(__Usual u) in C:\xSharp\DevRt\Runtime\XSharp.RT\Types\Usual.prg:line 1978
   at TestClass..ctor(__Usual[] Xs$Args)
   at xRuntime.Exe.Functions.Start()
*/

VOSTRUCT strTest
MEMBER m AS INT
MEMBER n AS INT

FUNCTION Start( ) AS VOID
LOCAL u AS USUAL

LOCAL str IS strTest
str.m := 123
testFunc(@str) // ok
xAssert(str.m == -123)

str.m := 123
u := TestClass{} // ok
xAssert(str.m == 123)
u := TestClass{NULL_PTR} // ok
xAssert(str.m == 123)

str.m := 123
u:TestMethod(NULL_PTR) // ok
xAssert(str.m == 123)
u:TestMethod(@str) // ok
xAssert(str.m == -123)

str.m := 123
u := TestClass{@str} // runtime error
xAssert(str.m == -123)

CLASS TestClass
	CONSTRUCTOR(str)
		? str == null_ptr 
		IF str != null_ptr 
			testFunc(str)
		ENDIF
	RETURN
	
	METHOD TestMethod(str)
		? str == null_ptr 
		IF str != null_ptr 
			testFunc(str)
		ENDIF
	RETURN NIL
END CLASS

FUNCTION testFunc( str AS strTest) AS LOGIC
? str.m
str.m := -123
RETURN FALSE


PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN

