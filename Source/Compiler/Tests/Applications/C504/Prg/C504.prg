// 504. GLOBALs/FIELDs incorrectly get presedence over same named LOCALs/params
// /warnaserror

GLOBAL oLocalAndGLobal := 1 AS INT

FUNCTION Start( ) AS VOID
	LOCAL o AS TestClass
	o := TestClass{"string"}
	o:Test(2)
RETURN


CLASS TestClass
	PROTECT oLocalAndField := 1 AS INT
	PROTECT oParamAndField := 1 AS INT
	CONSTRUCTOR(oParamAndField)
		LOCAL cTest := NULL AS STRING
		IF IsString(oParamAndField)
			cTest := oParamAndField
		END IF
		xAssert(cTest == oParamAndField)
		xAssert(cTest == "string")

		oParamAndField := NIL
		xAssert(oParamAndField == NIL)
		
		LOCAL oLocalAndGLobal AS STRING
		oLocalAndGLobal := "test"
		xAssert(oLocalAndGLobal == "test")
	RETURN
	
	METHOD Test(oParamAndField AS INT) AS VOID
		xAssert(SELF:oParamAndField == 1)
		xAssert(oParamAndField == 2)
		SELF:oParamAndField := oParamAndField
		xAssert(SELF:oParamAndField == 2)
		
		LOCAL oLocalAndField := 2 AS INT
		xAssert(SELF:oLocalAndField == 1)
		xAssert(oLocalAndField == 2)
		oLocalAndField := SELF:oLocalAndField
		xAssert(oLocalAndField == 1)
		
		xAssert(oLocalAndGLobal == 1)
	RETURN
	
	STATIC METHOD StaticTest() AS INT
		LOCAL oLocalAndField AS INT
		oLocalAndField := 123
	RETURN oLocalAndField
	
END CLASS

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"

