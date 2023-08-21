// 276. error XS0034: Operator '==' is ambiguous on operands of type 'short' and 'short'
// vulcan dialect
ENUM TestEnum
MEMBER m1
END ENUM

FUNCTION Start() AS VOID
LOCAL s := 0 AS SHORT
? s == (SHORT)TestEnum.m1

