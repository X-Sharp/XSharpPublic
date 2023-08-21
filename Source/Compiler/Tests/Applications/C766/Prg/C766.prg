// 766. Problems with calling methods late bound in VFP dialect
/*
problem 1:

error XS9059: Cannot convert Array Index from 'string' to 'int'.

XSharp.Error
No exported variable


problem 2:

Callstack : 
static __Usual XSharp.RT.OOPHelpers.IVarGet(System.Object oObject, System.String cIVar, System.Boolean lSelf)()   :  C:\xSharp\DevRt\Runtime\XSharp.RT\Functions\OOP.prg  :  706
static __Usual XSharp.RT.Functions.IVarGet(System.Object oObject, System.String symInstanceVar)()   :  C:\xSharp\DevRt\Runtime\XSharp.RT\Functions\OOP.prg  :  1151
static System.Void C766.Exe.Functions.Start()()   :  C:\xSharp\Dev\Tests\Applications\C766\Prg\C766.prg  :  30
*/

FUNCTION Start() AS VOID
	LOCAL uTyped AS Foo
	uTyped := Foo{}
	? uTyped
	xAssert( uTyped:Test(123) == "123.F.")
	xAssert( uTyped:Test(,123) == ".F.123")
	xAssert( uTyped:Test(123,"abc") == "123abc")

	LOCAL u
	u := Foo{}
	? u
	xAssert( u:Test(123) == "123.F.")  // "No exported variable" in XSharp.RT.OOPHelpers.IVarGet()
	xAssert( u:Test(,123) == ".F.123") // same
	xAssert( u:Test(123,"abc") == "123abc")  // error XS9059: Cannot convert Array Index from 'string' to 'int'.
	
CLASS Foo
	METHOD Test(a,b)
	? a,b
	RETURN AsString(a) + AsString(b)
END METHOD
END CLASS


PROC xAssert(l AS LOGIC) 
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"   
RETURN   

