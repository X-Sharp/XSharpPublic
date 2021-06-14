// 781. Internal compiler error with DIM AS STRING and /vo2+
// https://github.com/X-Sharp/XSharpPublic/issues/638
// vo2+
STATIC DEFINE ARRAYLEN := 5 AS INT

FUNCTION Start() AS VOID STRICT
	LOCAL DIM cItems[ARRAYLEN] AS STRING
	? cItems:Length
	xAssert(cItems:Length == 5)
	xAssert(cItems[1] == "")
	xAssert(cItems[5] == "")
	
	RETURN

PROC xAssert(l AS LOGIC) 
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"   
RETURN       

