// 954. Error reported on Math.Max() with (U)Int64 value
// https://github.com/X-Sharp/XSharpPublic/issues/1791

FUNCTION Start( ) AS VOID
	LOCAL n64 := -123 AS INT64
	LOCAL u64 := 123 AS UINT64
	n64 := Math.Max(0,n64) // error XS0121: The call is ambiguous between the following methods or properties: 'System.Math.Max(int64, int64)' and 'System.Math.Max(nint, nint)'
	xAssert(n64 == 0)
		
	u64 := Math.Max(0,u64) // error XS0121: The call is ambiguous between the following methods or properties: 'System.Math.Max(nint, nint)' and 'System.Math.Max(uint64, uint64)'
	xAssert(u64 == 123)
	
PROC xAssert(l AS LOGIC) 
	IF .NOT. l
		THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
	END IF
	? "Assertion passed"   
RETURN
