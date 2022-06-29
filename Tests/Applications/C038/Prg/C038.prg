// 38. error XS0119: 'int' is a type, which is not valid in the given context
#pragma warnings(219, off)   // variable is assigned but never used
FUNCTION Start() AS VOID
LOCAL n AS INT
LOCAL d AS DWORD
d := 1
n := (INT)d // ok
n := INT(d) // error

