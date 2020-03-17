// 711. Problem with signed / unsigned arithmetic results
// https://github.com/X-Sharp/XSharpPublic/issues/325
FUNCTION Start() AS VOID
	LOCAL n AS INT	
	LOCAL d AS DWORD
	LOCAL u AS USUAL   
	n := -2
	d := 1
	u := n + d
	? u // UInt32.MaxValue in ×#, -1 in VO & Vulcan
	xAssert(AsString(u) == "-1")
	
	u := d + n
	? u // -1 in ×#, UInt32.MaxValue in VO & Vulcan
	xAssert(AsString(u) == UInt32.MaxValue:ToString())
	
	?
	? (n + d) // UInt32.MaxValue in ×#, -1 in VO & Vulcan
	? (n + d):GetType():tostring() // DWORD in X#, INT in Vulcan (and in VO)
	xAssert(AsString(n + d) == "-1")
	
	? (d + n) // -1 in ×#, UInt32.MaxValue in VO & Vulcan
	? (d + n):GetType():tostring() // INT in X#, DWORD in Vulcan (and in VO)
	xAssert(AsString(d + n) == UInt32.MaxValue:ToString())
RETURN

PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF

