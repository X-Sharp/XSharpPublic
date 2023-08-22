//648. Problems using _FIELD with the X# runtime
/*
When compiling against the vulcan runtime, te following compiles and runs without errors
When compiling against the X# runtime, the following compiler errors are reported:
C648.prg(18,19): error XS1503: Argument 1: cannot convert from 'object' to 'string'
C648.prg(22,19): error XS1503: Argument 1: cannot convert from 'object' to 'string'
C648.prg(27,19): error XS0019: Operator '==' cannot be applied to operands of type 'object' and 'int'
C648.prg(31,19): error XS0019: Operator '==' cannot be applied to operands of type 'object' and 'int'
*/
FUNCTION Start( ) AS VOID
	LOCAL cDbf AS STRING
	cDbf := System.Environment.CurrentDirectory + "\C648.dbf"
	IF System.IO.File.Exists(cDbf)
		System.IO.File.Delete(cDbf)
	END IF
	? DbCreate(cDbf, {{"NFIELD", "N", 5, 0},{"CFIELD","C",10,0}}, "DBFNTX", TRUE)
	? DbCloseArea()
	
	? DbUseArea(,,cDbf)
	? DbAppend()
	FieldPut(1, 123)
	FieldPut(2, "ABC")
	? DbAppend()                 
	FieldPut(1, 456)
	FieldPut(2, "DEF")
	? DbSetFilter({||AllTrim(FIELD->CFIELD) == "ABC"})
	? DbGoTop()
	? FieldGet(1)
	xAssert(FieldGet(1) == 123)
	? DbSetFilter({||AllTrim(_FIELD->CFIELD) == "DEF"})
	? DbGoTop()
	? FieldGet(1)
	xAssert(FieldGet(1) == 456)

	? DbSetFilter({||FIELD->NFIELD == 123})
	? DbGoTop()
	? FieldGet(1)
	xAssert(FieldGet(1) == 123)
	? DbSetFilter({||_FIELD->NFIELD == 456})
	? DbGoTop()
	? FieldGet(1)
	xAssert(FieldGet(1) == 456)
	? DbCloseArea()
	

RETURN

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"

