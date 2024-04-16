// 903. Problem with the /vo9 (handle missing RETURN statements) option with ACCESS in PARTIAL classes
// https://github.com/X-Sharp/XSharpPublic/issues/1450

FUNCTION Start( ) AS VOID
	LOCAL o AS TestClass
	o := TestClass{}

	xAssert(o:AccessInt == 0)
	xAssert(o:AccessString == NULL_STRING)
	xAssert(o:AccessArray == NULL_ARRAY)
	xAssert(o:AccessLogic == FALSE)
	xAssert(o:AccessObject == NULL_OBJECT)

	xAssert(o:MethodInt() == 0)
	xAssert(o:MethodString() == NULL_STRING)
	xAssert(o:MethodArray() == NULL_ARRAY)
	xAssert(o:MethodLogic() == FALSE)
	xAssert(o:MethodObject() == NULL_OBJECT)
RETURN
    
// /vo9+
// error XS0161: 'TestClass.AccessXXX$Access()': not all code paths return a value
PARTIAL CLASS TestClass
	ACCESS AccessInt AS INT
	ACCESS AccessString AS STRING
	ACCESS AccessArray AS ARRAY
	ACCESS AccessLogic AS LOGIC
	ACCESS AccessObject AS TestClass

	METHOD MethodInt() AS INT
	METHOD MethodString() AS STRING
	METHOD MethodArray() AS ARRAY
	METHOD MethodLogic() AS LOGIC
	METHOD MethodObject() AS TestClass
END CLASS

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN	
