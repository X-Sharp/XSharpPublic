// 817. Problem with Math.Round(float/int) when /vo12 is enabled (ambiguous call)
// not that the call Math.Round(f) works with no issues, it's only Math.Round(f/n) that causes trouble

// /vo11+ enabled
// /vo12+ enabled
FUNCTION Start() AS VOID
LOCAL f := 9.0 AS FLOAT
? Math.Round(f) 	// OK
xAssert( Math.Round(f)== 9.0)
? Math.Round(f/2) 	// error XS0121: The call is ambiguous between the following methods or properties: 'System.Math.Round(real8)' and 'System.Math.Round(decimal)'
xAssert( Math.Round(f/2)== 4.0)
//
LOCAL r AS REAL8 // error XS0121: The call is ambiguous between the following methods or properties: 'XSharp.Core.Functions.Mod(real8, real8)' and 'XSharp.Core.Functions.Mod(int, int)'
r := 150.0
? Mod(r,100)
xAssert( Mod(r,100)== 50.0)

LOCAL a := {1,2,3} AS ARRAY
LOCAL u := a AS USUAL
? AScan( u, { |n| n==2 } , 1 )
xAssert( AScan( u, { |n| n==2 } , 1 ) == 2)
// no compiler error, but exception at runtime:
// System.InvalidCastException: Unable to cast object of type 'XSharp.__Array' to type 'XSharp.__ArrayBase`1[XSharp.Codeblock]'.
//
? AScan( a, { |n| n==2 } , 1) // error XS0121: The call is ambiguous between the following methods or properties: 'XSharp.RT.Functions.AScan(ARRAY, USUAL, USUAL)' and 'XSharp.RT.Functions.AScan<T>(ARRAY OF<T>, T, int)'
xAssert(AScan( a, { |n| n==2 } ) == 2)
xAssert(AScan( a, { |n| n==2 } , 1) == 2)
xAssert(AScan( a, { |n| n==2 } , 2) == 2)
xAssert(AScan( a, { |n| n==2 } , 3) == 0) // cannot find it


PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF
