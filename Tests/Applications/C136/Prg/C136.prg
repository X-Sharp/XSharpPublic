// 136. error XS0674: Do not use 'System.ParamArrayAttribute'. Use the 'params' keyword instead.
#pragma warnings(674, off)    // do not use System.ParamArrayAttribute
FUNCTION Start() AS VOID
TestClass.ParamArrayTest(1,2,3)
TestClass.ParamArrayTest2(1,2,3,4)
CLASS TestClass
	STATIC METHOD ParamArrayTest([ParamArray] an AS CONST INT[]) AS VOID
		? an:Length
	STATIC METHOD ParamArrayTest2(an PARAMS CONST INT[]) AS VOID
		? an:Length

END CLASS

