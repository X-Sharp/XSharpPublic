// 138. error XS0051: Inconsistent accessibility: parameter type 'IntClass' is less accessible than method 'TestClass.Test(IntClass)'
CLASS TestClass
PROTECTED METHOD Test(o AS IntClass) AS VOID
PUBLIC METHOD Test2(o AS IntClass) AS VOID
END CLASS

INTERNAL CLASS IntClass
END CLASS

