// 194. Assertion failed at OverloadResolution.BetterOperator
ENUM TestEnum
MEMBER m1 := 1
MEMBER m2 := 2
END ENUM

FUNCTION Start() AS VOID
LOCAL n AS INT
n := 10
? n - TestEnum.m2

