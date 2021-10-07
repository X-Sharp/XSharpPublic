// 804. Problems with the new _WinDate structure
FUNCTION Start() AS VOID STRICT
	LOCAL nDays AS DWORD
	LOCAL dNow AS DATE
	LOCAL ts IS TEST_STRUCT
	
	dNow := Today()
	ts.dDate := Today()
	nDays := dNow - ts.dDate // error XS0034: Operator '-' is ambiguous on operands of type 'DATE' and 'XSharp.__WinDate'
	nDays := DWORD(_CAST, ts.dDate) // error XS0030: Cannot convert type 'XSharp.__WinDate' to 'dword'

VOSTRUCT TEST_STRUCT
	MEMBER dDate AS DATE

