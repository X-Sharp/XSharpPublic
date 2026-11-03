// 988. Problem with /fox3 and custom properties #2092
// https://github.com/X-Sharp/XSharpPublic/issues/2092

#pragma options("fox3", enable)
#pragma options("allowdot", enable)
#pragma options("memvar", enable)
#pragma options("lb", enable)
#pragma options("undeclared", enable)

CLASS MyCustomClass INHERIT XSharp.VFP.Custom
	CONSTRUCTOR()
		SELF:AddProperty( "testprop", 123)
	
	METHOD Test() AS VOID
		? this.testprop // OK
		xAssert( this.testprop == 123 )

		this.testprop := 456 // OK
		? this.testprop // OK
		xAssert( this.testprop == 456 )
		
		? myself // OK
		? myself.testprop // Variable does not exist: MYSELF (only when /fox3 is enabled)
		xAssert( myself.testprop == 456 )

		myself.testprop := 123
		? myself.testprop
		xAssert( myself.testprop == 123 )
		
	PROPERTY myself AS USUAL GET SELF
END CLASS

FUNCTION Start() AS VOID
	LOCAL o AS MyCustomClass
	o := MyCustomClass{}
	o:Test()

PROC xAssert(l AS LOGIC) AS VOID
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"

