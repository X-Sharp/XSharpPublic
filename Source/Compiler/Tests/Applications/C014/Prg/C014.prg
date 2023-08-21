// 14. compiler crash
FUNCTION Start() AS VOID
LOCAL e AS MyEnum
e := MyEnum.Method
? e
ENUM MyEnum AS INT
MEMBER @@Method := 0
END ENUM

