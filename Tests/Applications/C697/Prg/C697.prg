// 697. Cannot pass PUBLIC/PRIVATE by reference
#pragma warnings (9093, off) // undeclared, cursor
FUNCTION Start() AS VOID
	PRIVATE pr
	PUBLIC pu
	DbCreate("Test697",{{"Name","C",10,0}})
	DbUseArea(TRUE,,"TEST697","TEST")
	DbAppend()
	TEST->Name := "Robert"
	pr := 123
	pu := "abc"

	testproc(@pr,pu)
	? pr,pu
	xAssert(pr == -1)
	xAssert(pu == "abc")
    pr := 0
	testproc(pr,REF pu)
	? pr,pu
	xAssert(pr == 0)
	xAssert(pu == 2)
	pr := 0
	testproc2(@pr,@test.NAME)
	xAssert(pr == 42)
	xAssert(test.Name = "Chris")

	pr := 0
	TEST->Name := "Robert"
	testproc2(@pr,@TEST->NAME)
	xAssert(pr == 42)
	xAssert(test->Name = "Chris")
	DbCLoseArea()

RETURN

PROCEDURE testproc(a,b)
	a := -1
	b := 2

RETURN  FALSE

PROCEDURE testproc2(a,b)
	a := 42
	b := "Chris"

RETURN  FALSE


PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF
RETURN TRUE

