// 81. error XS0266: Cannot implicitly convert type 'MyEnum' to 'int'. An explicit conversion exists (are you missing a cast?)
ENUM MyEnum AS INT
	MEMBER m1 := 1
END ENUM

FUNCTION Start() AS VOID
	LOCAL a AS INT[]
	a := INT[]{3}
	a[MyEnum.m1] := 1
RETURN

