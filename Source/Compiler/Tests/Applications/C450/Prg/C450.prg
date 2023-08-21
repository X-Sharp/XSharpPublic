// 450. error XS0019: Operator '>' cannot be applied to operands of type 'logic' and 'logic'
// > and < operators on LOGICs are supported in VO and Vulcan and I have actually
// found them being used in existing VO code! (in RAMDBF)
// The behavior of <= and >= does not make sense, but at least it is the same in both VO and Vulcan
FUNCTION Start() AS VOID
	xAssert(TRUE > FALSE)
	xAssert(TRUE >= FALSE)
	
	xAssert(FALSE < TRUE)
	xAssert(FALSE <= TRUE)
	
//	 both VO and Vulcan return FALSE on comparing TRUE <= FALSE and FALSE >= TRUE!!!
	xAssert( .not. (TRUE <= FALSE) )
	xAssert( .not. (FALSE >= TRUE) )
	
	LOCAL lTrue := TRUE , lFalse := FALSE AS LOGIC
	xAssert(lTRUE > lFALSE)
	xAssert(lTRUE >= lFALSE)
	
	xAssert(lFALSE < lTRUE)
	xAssert(lFALSE <= lTRUE)
	
//	 both VO and Vulcan return FALSE on comparing TRUE <= FALSE and FALSE >= TRUE!!!
	xAssert( .not. (lTRUE <= lFALSE) )
	xAssert( .not. (lFALSE >= lTRUE) )
RETURN

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"

