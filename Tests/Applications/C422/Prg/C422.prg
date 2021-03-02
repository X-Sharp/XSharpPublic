// 422. Exception at runtime: Specified cast is not valid.
// note that this kind of (INT in)USUAL to PTR conversions are very common in the SDK
// 
/*
difference in code emitted by vulcan and x# is that vulcan uses

call void* [VulcanRTFuncs]Vulcan.__Usual::op_Implicit(valuetype [VulcanRTFuncs]Vulcan.__Usual)

while x#:

call native int [VulcanRTFuncs]Vulcan.__Usual::op_Implicit(valuetype [VulcanRTFuncs]Vulcan.__Usual)

(native int instead of void*)
*/
FUNCTION Start( ) AS VOID
	LOCAL nTest AS INT
	LOCAL pPtr AS PTR
	LOCAL iPtr AS INT
	LOCAL uPtr AS USUAL

	nTest := 123
	pPtr := @nTest
	iPtr := INT(_CAST,pPtr)

	uPtr := iPtr
	? pPtr , AsHexString(uPtr) , AsHexString(iPtr) // OK
	
	LOCAL p1,p2,p3 AS PTR
	// exception in all next 3 lines
	p1 := uPtr
	p2 := PTR(uPtr)
	p3 := PTR(_CAST,uPtr)
	? p1,p2,p3
	? INT(p1)
	IF p1 != p2 .or. p2 != p3 .or. p1 != pPtr .or. INT(p1) != nTest
		THROW Exception{"Incorrect conversion from (int in) usual to ptr"}
	END IF
	
	LOCAL dPtr AS DWORD
	dPtr := DWORD(_CAST,pPtr)
	uPtr := dPtr
//	#warning "following requires /vo7+, vulcan doesn't"
	p1 := dPtr // requires /vo7+ // Not any more :-)
	p2 := PTR(dPtr)
	p3 := PTR(_CAST,dPtr)
	? p1,p2,p3
	? DWORD(p1)
	IF p1 != p2 .or. p2 != p3 .or. p1 != pPtr .or. INT(p1) != nTest
		THROW Exception{"Incorrect conversion from (dword in) usual to ptr"}
	END IF
	
	StatusBar{}:DoTest()
	FTest()
RETURN




CLASS StatusBar
METHOD DoTest() AS VOID
	LOCAL oEvt AS @@Event
	LOCAL p1   AS _winDRAWITEMSTRUCT
	p1 := MemAlloc(SizeOf(_winDRAWITEMSTRUCT))
	p1:CtlID := 555
	oEvt := @@Event{p1}
	? PTR(oEvt:lParam) == p1
	IF PTR(oEvt:lParam) != p1
		THROW Exception{"Incorrect result"}
	END IF
	SELF:ODDrawItem(oEvt)
RETURN
METHOD ODDrawItem(oEvent) 
	LOCAL oEvt AS @@Event
	LOCAL p1   AS _winDRAWITEMSTRUCT
	oEvt     := oEvent
	p1       := PTR(_CAST, oEvt:lParam)
	? p1:CtlID
	IF p1:CtlID != 555
		THROW Exception{"Incorrect result"}
	END IF
	RETURN SELF

END CLASS

CLASS @@Event //inherit object
	EXPORT lParam 	AS LONGINT
	CONSTRUCTOR(l)
	SELF:lParam := l	
END CLASS

VOSTRUCT _winDRAWITEMSTRUCT
	MEMBER  CtlType AS DWORD
	MEMBER  CtlID AS DWORD


FUNCTION FTest() AS VOID
	LOCAL p AS PTR
	p := PTR(_CAST,GetPtr())
	? p
	IF INT(_CAST,p) != 1024
		THROW Exception{"Incorrect result"}
	END IF
RETURN 
FUNCTION GetPtr() AS USUAL
RETURN 1024

