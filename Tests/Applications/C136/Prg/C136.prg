// 136. error XS0674: Do not use 'System.ParamArrayAttribute'. Use the 'params' keyword instead.
FUNCTION Start() AS VOID
TestClass.ParamArrayTest(1,2,3)
CLASS TestClass
	STATIC METHOD ParamArrayTest([ParamArray] an AS INT[]) AS VOID
		? an:Length
END CLASS

