// 912. VO incompatibility resolving GLOBALs/DEFINEs
// https://www.xsharp.eu/forum/topic?p=30408#p30408

/*
In VO, when there's a name ambiguity, the compiler resolves identifiers used inside a class in this order:

1.Class fields
2.GLOBAL/DEFINE
3.Class ACCESS/ASSIGN
*/

/*
prg(25,3): error XS0154: The property or indexer 'NewTest.dTest2' cannot be used in this context because it lacks the get accessor
prg(26,3): error XS0200: Property or indexer 'NewTest.gTest2' cannot be assigned to -- it is read only
prg(32,3): error XS0154: The property or indexer 'NewTest.dTest2' cannot be used in this context because it lacks the get accessor
prg(33,3): error XS0200: Property or indexer 'NewTest.gTest2' cannot be assigned to -- it is read only
*/

GLOBAL gTest1 := "global"
// DEFINE dTest1 := "define" // defined in library

// GLOBAL gTest2 := "global"  // defined in library
DEFINE dTest2 := "define"

CLASS NewTest
	EXPORT gTest1 := "class"
	EXPORT dTest1 := "class"
	METHOD TestMethod()
		? gTest1
		xAssert(gTest1 == "class")

		? dTest1
		xAssert(dTest1 == "class")

		gTest1 := "class modified"
		? gTest1
		xAssert(gTest1 == "class modified")

		dTest1 := "class modified"
		? dTest1
		xAssert(dTest1 == "class modified")

//		dTest2 := "should report compiler error since it's a define" // error in VO, no error in X#
	RETURN NIL


	ACCESS gTest2()
		? gTest2
		xAssert(gTest2 == "global")

		? dTest2 // error in X#
		xAssert(dTest2 == "define")

		//gTest2 := "global modified" // error in X#
		? gTest2
		//xAssert(gTest2 == "global modified")
	RETURN "access"

	ASSIGN dTest2(u)
		? gTest2
		xAssert(gTest2 == "global")

		? dTest2 // error in X#
		xAssert(dTest2 == "define")

		//gTest2 := "global modified again" // error in X#
		? gTest2
		//xAssert(gTest2 == "global modified again")

//		dTest2 := "should report compiler error since it's a define" // error in VO, no error in X#
END CLASS

FUNCTION Start() AS VOID
	LOCAL o AS NewTest
	o := NewTest{}
	o:TestMethod()
	? "-----------"
	? o:gTest2
	? "-----------"
	gTest2 := "global"
	o:dTest2 := "test"



PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	? "Assertion FAILED"
	THROW Exception{"Incorrect result"}
END IF
RETURN
