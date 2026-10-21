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
		this.testprop := 456 // OK
		? this.testprop // OK
		? myself // OK
		? myself.testprop // Variable does not exist: MYSELF (only when /fox3 is enabled)
		myself.testprop := 123
		? myself.testprop
		
	PROPERTY myself AS USUAL GET SELF
END CLASS

FUNCTION Start() AS VOID
	LOCAL o AS MyCustomClass
	o := MyCustomClass{}
	o:Test()
