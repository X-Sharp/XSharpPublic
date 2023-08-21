// 592. Runtime error with Default(@param , NULL_OBJECT)
FUNCTION Start() AS VOID

Test()
xAssert(Test()==NULL_OBJECT)

Test(1)
xAssert(Test(1)==1)

LOCAL u AS USUAL
u := NIL
Test(u)
xAssert(Test(u)==NIL)

u := NULL_OBJECT
Test(u)
xAssert(Test(u)==NULL_OBJECT)

u := 123
Test(u)
xAssert(Test(u)==123)

u := TRUE
Test(u)
xAssert(Test(u)==TRUE)






TestNum()
xAssert(TestNum()==123)

TestNum(1976)
xAssert(TestNum(1976)==1976)

u := NIL
TestNum(u)
xAssert(TestNum(u)==123)

u := NULL_OBJECT
TestNum(u)
xAssert(TestNum(u)==123)

u := "asd"
TestNum(u)
xAssert(TestNum(u)=="asd")


RETURN

FUNCTION Test(u)
	Default(@u , NULL_OBJECT)
	? u
RETURN u

FUNCTION TestNum(u)
	Default(@u , 123)
	? u
RETURN u

PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF

