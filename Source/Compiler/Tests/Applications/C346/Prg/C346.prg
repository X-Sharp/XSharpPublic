// 346. error XS0034: Operator '==' is ambiguous on operands of type '_winFIELDINFO*' and '_winFIELDINFO*'
// compiles ok if /vo7 is not enabled, fails with /vo7+
// it's not new, just saw it now
VOSTRUCT _winFIELDINFO
	// ....
	MEMBER l AS LOGIC

FUNCTION Start() AS VOID
LOCAL s1,s2 AS _winFIELDINFO
s1 := MemAlloc(SizeOf(_winFIELDINFO))
s2 := MemAlloc(SizeOf(_winFIELDINFO))
? s1 == s2 // FALSE
? s1 != s2 // TRUE
? 
s1:l := TRUE
? s1 == s2 // FALSE
? s1 != s2 // TRUE

