// 804. Problems with the new _WinDate structure    
// See https://github.com/X-Sharp/XSharpPublic/issues/773
// Extended the example to also include LOGIC values inside a VOSTRUCT

// Please note that the customers code assigns a LONG to a DWORD
// So /vo must be enabled.
#pragma options("vo4", ON)
FUNCTION Start() AS VOID STRICT
	LOCAL nDays AS DWORD
	LOCAL dNow AS DATE    
	LOCAL u AS USUAL
	LOCAL ts IS TEST_STRUCT
	
	dNow := Today()
	ts.dDate := Today()
	ts.lLogic := TRUE
	nDays := dNow - ts.dDate 
    xAssert (nDays == 0)
    xAssert(dNow == ts.dDate)
	ts.dDate := Today() -1
	nDays := dNow - ts.dDate 
    xAssert (nDays == 1)     
    xAssert(dNow == ts.dDate +1)
    u := TRUE
    xAssert(ts.lLogic == u)
    xAssert(ts.lLogic == !FALSE)
    xAssert(!ts.lLogic == FALSE)
    xAssert(!ts.lLogic == !u)

    u := 1
    ? ts.dDate + u
	ts.dDate := Today() +1
	nDays := dNow - ts.dDate 
    xAssert (nDays == -1)
    xAssert (nDays == UInt32.MaxValue)
	nDays := DWORD(_CAST, ts.dDate) 
	? ts.dDate:JulianValue
	? nDays
	xAssert(nDays == ts.dDate:JulianValue)
	
	u := Today()
	ts.dDate := u
	xAssert(ts.dDate == Today())

	AnotherTest()

PROCEDURE AnotherTest()
	LOCAL nDays AS DWORD
	LOCAL ts IS TEST_STRUCT   
	
	nDays := 10
	ts.dDate := Today()
	ts.dDate += ( nDays - 1 ) 
	XAssert(ts.dDate = Today() + 9)
	ts.dDate -= ( nDays - 1 )      
	XAssert(ts.dDate = Today() )
	ts.dDate += 1              
	XAssert(ts.dDate = Today() + 1)
	ts.dDate -= 1 
	XAssert(ts.dDate = Today() )
	ts.dDate++                 
    XAssert(ts.dDate = Today() + 1)
	ts.dDate-- 
    XAssert(ts.dDate = Today())
	
	xAssert(ts.dDate == Today())
	ts.dDate += 1
	xAssert(ts.dDate == Today() + 1)
	
	LOCAL dStart, dEnd AS DATE
	dStart := Today()
	dEnd := dStart + 1
	
	FOR ts.dDate := dStart UPTO dEnd 
		? ts.dDate
		IF ts.dDate == dStart
			xAssert(ts.dDate == Today())
		ELSE
			xAssert(ts.dDate == Today() + 1)
		END IF
	NEXT
	
VOSTRUCT TEST_STRUCT
	MEMBER dDate AS DATE     
	MEMBER lLogic AS LOGIC



PROC xAssert(l AS LOGIC) 
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN 	
