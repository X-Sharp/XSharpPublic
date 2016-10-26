// 15. error XS0161: 'Xs$Globals.Test()': not all code paths return a value

FUNCTION Test() AS INT
LOCAL e AS MyEnum
e := MyEnum.NIL
RETURN 0

ENUM MyEnum AS INT
MEMBER @@NIL := 0
END ENUM

FUNCTION Start() AS VOID

RETURN
