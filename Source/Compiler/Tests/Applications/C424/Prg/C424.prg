// 424. error XS0034: Operator '==' is ambiguous on operands of type 'int' and '_winDRAWITEMSTRUCT*'
// vo7+ enabled
FUNCTION Start( ) AS VOID
	LOCAL oEvt AS @@Event
	LOCAL p AS _winDRAWITEMSTRUCT
	p := MemAlloc(SizeOf(_winDRAWITEMSTRUCT))
	p:CtlID := 555
	oEvt := @@Event{p}
	? oEvt:lParam == p
	IF oEvt:lParam != p
		THROW Exception{"Incorrect result"}
	END IF
	LOCAL n AS INT
	n := oEvt:lParam
	? n == p
	? p == n
	IF n != p .or. p != n
		THROW Exception{"Incorrect result"}
	END IF

	LOCAL d AS DWORD
	d := (DWORD)oEvt:lParam
	? d == p
	? p == n
	IF d != p .or. p != d
		THROW Exception{"Incorrect result"}
	END IF
RETURN

CLASS @@Event //inherit object
	EXPORT lParam 	AS LONGINT
	CONSTRUCTOR(l)
	SELF:lParam := l	
END CLASS

VOSTRUCT _winDRAWITEMSTRUCT
	MEMBER  CtlType AS DWORD
	MEMBER  CtlID AS DWORD

