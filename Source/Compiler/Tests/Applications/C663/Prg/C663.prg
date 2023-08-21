// 663. There are some problems with DIM arrays and /vo2+:
/*
1. Elements of single-dim LOCAL DIM STRING arrays do not get initialized to ""
2. Elements of multi-dim LOCAL DIM STRING arrays do not get initialized to ""
3. Defining multi-dim CLASS STRING DIM arrays reports
error XS9002: Parser: DIM string arrays must have one dimension
*/
CLASS TestClass
	EXPORT DIM cDim[10] AS STRING
	EXPORT DIM cDim2[10,10] AS STRING
	EXPORT DIM cDim3[10,10,10] AS STRING
END CLASS
FUNCTION Start() AS VOID
	LOCAL DIM cDim[10] AS STRING
	LOCAL DIM cDim2[10,10] AS STRING
	LOCAL DIM cDim3[10,10,10] AS STRING
	xAssert(cDim[5] != NULL)
	xAssert(cDim2[5,5] != NULL)
	xAssert(cDim3[5,5,5] != NULL)
	
	LOCAL o AS TestClass
	o := TestClass{}
	xAssert(o:cDim[5] != NULL)
	xAssert(o:cDim2[5,5] != NULL)
	xAssert(o:cDim3[5,5,5] != NULL)
RETURN

PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF

