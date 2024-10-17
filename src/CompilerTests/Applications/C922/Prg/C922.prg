// 922. Problems with attributes and END markers in VFP dialect
// https://github.com/X-Sharp/XSharpPublic/issues/1566
// https://github.com/X-Sharp/XSharpPublic/issues/1568
FUNCTION Start() AS VOID
TestClass{}

[Obsolete];
DEFINE CLASS TestClass
	FUNCTION Test1() AS INT
    	RETURN 123
	END FUNCTION
	FUNCTION Test2() AS INT
    	RETURN 123
	ENDFUNC
	PROCEDURE Test3()
	ENDPROC
ENDDEFINE

