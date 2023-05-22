// 552. Compiler crash with CLIPPER method and /vo9+ with missing RETURN type
#pragma warnings(9026, off) //
#pragma warnings(9025, off) //
FUNCTION Start() AS VOID
LOCAL o AS TestClass
o := TestClass{}
? o:Test1()
? o:Test2()
? o:Test3()
? o:Test4()
? o:Test5()
? o:Test6(1)
? o:Test11()
? o:Test12()
? o:Test13()
? o:Test14()
? o:Test15()
? o:Test16(2)

? TestFunc1()
? TestFunc2()
? TestFunc3()
? TestFunc4()
? TestFunc21()
? TestFunc22()
? TestFunc23()
? TestFunc24()
RETURN

CLASS TestClass
METHOD Test1() CLIPPER
RETURN
METHOD Test2(a) CLIPPER
RETURN
METHOD Test3()
RETURN
METHOD Test4(a)
RETURN
METHOD Test5() PASCAL
RETURN
METHOD Test6(a AS USUAL) PASCAL
RETURN
METHOD Test11() CLIPPER
METHOD Test12(a) CLIPPER
METHOD Test13()
METHOD Test14(a)
METHOD Test15() PASCAL
METHOD Test16(a AS USUAL) PASCAL
END CLASS

FUNCTION TestFunc1() CLIPPER
RETURN
FUNCTION TestFunc2(a) CLIPPER
RETURN
FUNCTION TestFunc3()
RETURN
FUNCTION TestFunc4(a)
RETURN
FUNCTION TestFunc21() CLIPPER
FUNCTION TestFunc22(a) CLIPPER
FUNCTION TestFunc23()
FUNCTION TestFunc24(a)

