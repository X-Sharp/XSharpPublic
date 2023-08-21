// See  https://github.com/X-Sharp/XSharpPublic/issues/922
#pragma options("allowdot", on)
CLASS TestClass
	PUBLIC PROPERTY Ret AS STRING GET "xyz"
	PUBLIC STATIC METHOD SMethod AS STRING
	    ? __FUNCTION__
	    RETURN __FUNCTION__
	PUBLIC METHOD Test() AS STRING
	    ? __FUNCTION__
	    RETURN __FUNCTION__
END CLASS

FUNCTION Start() AS VOID STRICT
        VAR TestClass := TestClass{}
        Console.WriteLine(i"{TestClass.Ret}")
	    xAssert(TestClass.Test() == "Test")
	    xAssert(TestClass.SMethod() == "SMethod")
        RETURN

PROC xAssert(l AS LOGIC) as VOID
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF
