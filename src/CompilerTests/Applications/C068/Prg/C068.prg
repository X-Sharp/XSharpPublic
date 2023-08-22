// 68. error XS0019: Operator '|' cannot be applied to operands of type 'int' and 'MyEnum'
// not sure I like it, but vulcan does compile this
FUNCTION Start() AS VOID
LOCAL e AS MyEnum
e := MyEnum.m2
? ((INT)e | MyEnum.m1) != 0
? ((INT)e & MyEnum.m2) != 0

ENUM MyEnum AS Int32
	MEMBER m1 := 0
	MEMBER m2 := 1
END ENUM

// 68. new version:
// error XS0034: Operator '==' is ambiguous on operands of type 'MyEnum' and 'int'

FUNCTION Startnew() AS VOID
? (123 | MyEnum.m1) == 0
? (MyEnum.m1 | 123) == 0
? (123 & MyEnum.m2) != 0

