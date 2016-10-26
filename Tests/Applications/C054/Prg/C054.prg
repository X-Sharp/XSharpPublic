// 54. error XS1003: Syntax error, 'Class_' expected
CLASS Test.TestClass
END CLASS

FUNCTION Start() AS VOID
LOCAL o AS Test.TestClass
o := Test.TestClass{}
? o
RETURN
