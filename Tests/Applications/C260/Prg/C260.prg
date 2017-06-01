// 260. error XS0246: The type or namespace name 'TestFunc' could not be found
// vulcan compiles this with /vo6.
// The effect is that TestFunc is seen as some kind of delegate
// The Ptr can then only be used for Pcall and PCallNative
USING System.Runtime.InteropServices
GLOBAL p AS TestFunc PTR
FUNCTION TestFunc(n AS INT) AS INT
	? n
RETURN n+1
FUNCTION Start() AS VOID
LOCAL pFunc AS TestFunc PTR         
pFunc := NULL_PTR
p	  := NULL_PTR
? pFunc
? p     
RETURN


