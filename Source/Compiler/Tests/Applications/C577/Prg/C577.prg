// error XS7038: Failed to emit module 'C577'.
// something is weird here, reversing the order of defines results 
// to an error (correct) about mixing signed and signed. Same is 
// reported by laving the same order and removing the UNCHECKED statement
// also changing the define to either (DWORD)DTN_FIRST + 7 or (INT)DTN_FIRST + 7, compiles ok, too.
DEFINE DTN_CLOSEUP := DTN_FIRST + 7
DEFINE DTN_FIRST := 0U-760U

FUNCTION Start( ) AS VOID
	? DTN_CLOSEUP
	? DTN_FIRST
RETURN
