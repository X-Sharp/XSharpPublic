// 177. Compiler crash with xor operator
ENUM TestEnum AS INT
	MEMBER m1 := 1
	MEMBER m2 := 2
END ENUM

FUNCTION Start() AS VOID
LOCAL n AS INT
n := 1
n := n ~ 2

LOCAL e AS TestEnum
e := TestEnum.m1
e := e ~ TestEnum.m1

