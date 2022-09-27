// 873. Incompatible behavior with VO when declaring global MEMVAR
// https://github.com/X-Sharp/XSharpPublic/issues/1144

FUNCTION Start() AS INT
	Func1()
	? test_value
	xAssert( test_value == 1 )

	Func2()
	? test_value
	xAssert( test_value == 1 )
RETURN 0

FUNCTION Func1()
	
	PUBLIC test_value
	? test_value
	xAssert( test_value == FALSE )

	IF Empty(test_value)
		test_value := 1
	ENDIF
	
RETURN NIL

FUNCTION Func2()
	
	PUBLIC test_value
	? test_value
	xAssert( test_value == 1 )

	IF Empty(test_value)
		test_value := 2
	ENDIF
	
RETURN NIL


PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
