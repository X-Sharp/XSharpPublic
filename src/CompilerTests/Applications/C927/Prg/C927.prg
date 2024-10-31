// 927. Interpolated string literals problem with /allowdot+ enabled
// https://github.com/X-Sharp/XSharpPublic/issues/1597
FUNCTION Start( ) AS VOID
	TestEnabled()	
	TestDisabled()	
RETURN

#pragma options (allowdot, on)
PROCEDURE TestEnabled()
LOCAL IMPLIED d := DateTime.Now

? i"test {d:Year} end" // prints "test Year end", wrong

? i"test {d.Year} end" // "test 2024 end", OK

#pragma options (allowdot, off)
PROCEDURE TestDisabled()
LOCAL IMPLIED d := DateTime.Now

? i"test {d:Year} end" // "test 2024 end", OK

//? i"test {d.Year} end" // error XS0118: 'd' is a variable but is used like a type, OK

