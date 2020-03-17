// 716. Macrocompiler problem with &cMacroToCompile syntax
// Following compiles and rns fine in VO, returning "3" both times. In X# it throws an exception
FUNCTION Start() AS VOID
	LOCAL c AS STRING
	c := "1 + 2"
	XAssert( &(c) == 3)
	XAssert( &c == 3)	
RETURN


PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF
