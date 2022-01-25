// See  https://github.com/X-Sharp/XSharpPublic/issues/922
#pragma options("allowdot", on)
CLASS TestClass
	PUBLIC PROPERTY Ret AS STRING GET "xyz"
	PUBLIC METHOD Test() AS VOID
	    ? __FUNCTION__
	    RETURN 
END CLASS

FUNCTION Start() AS VOID STRICT
        VAR TestClass := TestClass{}
        Console.WriteLine(i"{TestClass.Ret}")
	    TestClass.Test()
        RETURN
