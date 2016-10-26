// 66. compiler crash
FUNCTION Start() AS VOID
SomeFunc( , , 1)

PROCEDURE SomeFunc(a,b,c)
	? a,b,c

