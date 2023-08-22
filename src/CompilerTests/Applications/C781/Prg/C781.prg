// 781. Internal compiler error with DIM AS STRING and /vo2+
// https://github.com/X-Sharp/XSharpPublic/issues/638
// vo2+                                  
STATIC DEFINE ARRAYLEN := 5 AS INT
//#define  ARRAYLEN 5
FUNCTION Start() AS VOID STRICT
	LOCAL DIM cItems[ARRAYLEN] AS STRING
	? cItems:Length
	xAssert(cItems:Length == ARRAYLEN)
	xAssert(cItems[1] == "")
	xAssert(cItems[ARRAYLEN] == "")

	LOCAL DIM cItems2[ARRAYLEN,ARRAYLEN] AS STRING
	? cItems2:Length
	xAssert(cItems2:Length == ARRAYLEN*ARRAYLEN)
	xAssert(cItems2[1,1] == "")
	xAssert(cItems2[ARRAYLEN,ARRAYLEN] == "")
	
	RETURN

PROC xAssert(l AS LOGIC) 
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"   
RETURN       

