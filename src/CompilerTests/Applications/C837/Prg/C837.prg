// 837. SCATTER TO and APPEND FROM ARRAY
// https://github.com/X-Sharp/XSharpPublic/issues/821
#pragma warnings(219, off) // assigned but not used
FUNCTION Start() AS VOID
	LOCAL ARRAY aNewRec(3)  // to compile this line /fox2 must be enabled
//	Dimension aNewRec(3)
	LOCAL cFile AS STRING
	cFile := System.Environment.CurrentDirectory + "\C837"
	IF System.IO.File.Exists(cFile + ".cdx")
		System.IO.File.Delete(cFile + ".cdx")
	END IF

	DbCreate ( cFile , {{ "field1" , "C" , 10 , 0 } , { "field2" , "N" , 5,2 } , { "field3" , "L" , 1,0}}  )

	use (cFile ) EXCLUSIVE

	ZAP

	APPEND BLANK

	REPLACE field1 WITH "Test"
	REPLACE field2 WITH 11.11
	REPLACE field3 WITH .t.

	// -------------

	GO TOP


	SCATTER TO aNewRec

	ShowArray ( aNewRec , "Scatter I " )
	xAssert(aNewRec[1] == "Test      ")
	xAssert(aNewRec[2] == 11.11)
	xAssert(aNewRec[3] == TRUE)

	aNewRec[1] := "new"

	SCATTER TO aNewRec BLANK

	ShowArray ( aNewRec , "Scatter II " )
	xAssert(aNewRec[1] == "")
	xAssert(aNewRec[2] == 0.0)
	xAssert(aNewRec[3] == FALSE)

	// These are the values a SCATTER ... BLANK should create
	aNewRec[1] := "test"
	aNewRec[2] := 12.34
	aNewRec[3] := .t.

	// APPEND FROM throws an runtime error because
	// the underlying DBAppendFromArray() expects a multidimensioal array ?
	APPEND FROM ARRAY aNewRec

	GO TOP
	SKIP

	? a->field1, a->Field2 , a->Field3
	xAssert(a->field1 == "test      ")
	xAssert(a->field2 == 12.34)
	xAssert(a->field3 == TRUE)

	USE

PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
