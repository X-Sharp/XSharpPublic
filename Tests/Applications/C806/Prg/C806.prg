GLOBAL global_string AS STRING

CLASS TestClass
	EXPORT eTest AS STRING , eInt AS INT
	PROTECT pTest AS STRING , pInt AS INT
	HIDDEN hTest AS STRING
CONSTRUCTOR()
	xAssert(SELF:eTest != NULL)
	xAssert(SELF:eTest == "")
	xAssert(SELF:pTest != NULL)
	xAssert(SELF:pTest == "")
	xAssert(SELF:hTest != NULL)
	xAssert(SELF:hTest == "")
RETURN
END CLASS

FUNCTION Start() AS VOID
	xAssert(global_string != NULL)
	xAssert(global_string == "")
	xAssert(global_string_Lib != NULL)
	xAssert(global_string_Lib == "")
	TestClass{}
	TestClass_Lib{}
RETURN

//PROC xAssert(l AS LOGIC)
//IF .NOT. l
//	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
//END IF
//? "Assertion passed"
//RETURN

