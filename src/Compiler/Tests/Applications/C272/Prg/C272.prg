// error XS0121: The call is ambiguous between the following methods or properties: 
// 'Convert.ToChar(char)' and 'Convert.ToChar(sbyte)'
// should pick Convert.ToChar(int32)
ENUM TestEnum
	MEMBER m1
	MEMBER m2
END ENUM

FUNCTION Start() AS VOID
LOCAL e AS TestEnum
e := TestEnum.m1
? Convert.ToChar(e)

