// 915. Namespace vs. property confuses the compiler
// https://github.com/X-Sharp/XSharpPublic/issues/1515
// allowdot-
CLASS TestClass
PROPERTY System AS LOGIC AUTO
CONSTRUCTOR()

	System.Diagnostics.Debug.WriteLine( "Hi" )
	System := TRUE
	xAssert(system)
	
	? System.Int32.MaxValue
	xAssert(System.Int32.MaxValue == Int32.MaxValue)

	RETURN

END CLASS

FUNCTION Start() AS VOID
TestClass{}
RETURN

PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	? "Assertion FAILED"
	THROW Exception{"Incorrect result"}
END IF
RETURN
