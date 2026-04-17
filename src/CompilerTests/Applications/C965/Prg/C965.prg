// 965. COPY TO ARRAY command issues #1857
// https://github.com/X-Sharp/XSharpPublic/issues/1857#issuecomment-4191757341

FUNCTION Start( ) AS VOID
	
	LOCAL cDbf AS STRING
	cDbf := "test_copy_to_array"
	DbCreate(cDbf, {{"FLD1","N",10,0},{"FLD2","L",1,0}})
	DbUseArea(true, , cDbf)
	DbAppend(); FieldPut(1,1); FieldPut(2,TRUE)
	DbAppend(); FieldPut(1,2); FieldPut(2,FALSE)
	DbGoTop()
	
	COPY TO ARRAY testarray
	
	xAssert(testarray[1,1] == 1.0)
	xAssert(testarray[2,1] == 2.0)
	xAssert(testarray[1,2])
	xAssert(.not. testarray[2,2])
	
	DIMENSION arr(2,2)
	COPY TO ARRAY arr FIELDS LIKE FLD2
	
	xAssert(arr[1,1])
	xAssert(.not. arr[1,2])
	
	DIMENSION arr(2,2)
	COPY TO ARRAY arr
	xAssert(arr[1,1] == 1.0)
	xAssert(arr[1,2])
	xAssert(arr[2,1] == 2.0)
	xAssert(.not. arr[2,2])
	
RETURN

PROC xAssert(l AS LOGIC) AS VOID
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
