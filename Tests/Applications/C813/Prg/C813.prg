// 813. ICE with Type() in FoxPro dialect with /fox2+ enabled
#pragma warnings(9025, off) // return statement
FUNCTION Start() AS VOID
MEMVAR testvar
LOCAL pVarId
testvar := DATE()
? Type(pVarId)
xAssert(Type(pVarId) == "U")

pVarId := "testvar"
? Type(pVarId)
? ValType(testvar)
xAssert( Type(pVarId)     == "D" )
xAssert( ValType(testvar) == "D" )

PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
ELSE
	? "Assertion passed"
END IF

