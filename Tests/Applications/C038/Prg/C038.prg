// 38. error XS0119: 'int' is a type, which is not valid in the given context
FUNCTION Start() AS VOID
LOCAL n AS INT
LOCAL d AS DWORD
d := 1
n := (INT)d // ok
n := INT(d) // error
	
