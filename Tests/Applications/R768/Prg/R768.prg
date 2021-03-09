// R768 Make sure that default value is inserted of the right type
#pragma options("lb", ON)
FUNCTION Start( ) AS VOID
	LOCAL oTest AS OBJECT
	oTest := Test{}
	xAssert(oTest:DoSomething(2,1) == 2)
	xAssert(oTest:DoSomething(2) == 2)
	xAssert(oTest:DoSomethingWithMoney(2) == 2.0m)
	xAssert(oTest:DoSomethingWithDouble(2) == 2.0)
RETURN


CLASS Test
    METHOD DoSomething (nInt AS LONG, nDword := 1 AS DWORD) AS DWORD
        RETURN (DWORD) nInt * nDword
    METHOD DoSomethingWithMoney (nInt AS LONG, nDword := 1.0m AS Decimal) AS Decimal
        RETURN (Decimal) nInt * nDword
    METHOD DoSomethingWithDouble (nInt AS LONG, nDword := 1.0 AS System.Double) AS REAL8
        RETURN (REAL8) nInt * nDword
END CLASS    



PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
