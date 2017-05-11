// 505. error XS1061: 'Class1' does not contain a definition for 'Test' and no extension method 'Test' accepting a first argument of type 'Class1' could be found 
// Core dialect
FUNCTION Start() AS VOID
	LOCAL o AS TestClass
	o := TestClass{}
	? o:Test
	xAssert(o:Test == 123)
RETURN

// without PARTIAL, it compiles and runs fine. Somehow it confuses the compiler?
PARTIAL CLASS HelpClass
	ACCESS Test AS INT
	RETURN 123
END CLASS

CLASS TestClass
	PROTECT oHelpClass := HelpClass{} AS HelpClass
	VIRTUAL ACCESS Test AS INT
	RETURN SELF:oHelpClass:Test
END CLASS

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"

