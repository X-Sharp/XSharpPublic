// 347. error XS0019: Operator '==' cannot be applied to operands of type '_winFIELDINFO' and '_winFIELDINFO'
// same as C346, but with IS instead of AS
// have not seen this in code, but noticed that vulcan compiles it and returns expected results
#pragma warnings(165, off) // unassigned local
VOSTRUCT _winFIELDINFO
	// ....
	MEMBER l AS LOGIC

FUNCTION Start() AS VOID
LOCAL s1,s2 IS _winFIELDINFO

? s1 == s2 // TRUE
? s1 != s2 // FALSE
?
s1:l := TRUE
? s1 == s2 // FALSE
? s1 != s2 // TRUE

