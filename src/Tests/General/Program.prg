// 937. Late bound problem with enums
// https://github.com/X-Sharp/XSharpPublic/issues/1696

/*
System.InvalidCastException
Invalid cast from 'System.Int32' to 'TestEnum'.
*/

ENUM TestEnum
	MEMBER One
	MEMBER Two
	MEMBER Three
END ENUM

CLASS TestClass
	EXPORT IVar AS TestEnum
	PROPERTY Prop AS TestEnum AUTO
	METHOD TestMethod(e AS TestEnum) AS TestEnum
	RETURN e
	METHOD TestMethodDefault(n := 1 AS INT, e AS TestEnum) AS STRING
	RETURN e:ToString() + n:ToString()
	METHOD TestMethodDefault2(n AS INT, e := TestEnum.Two AS TestEnum) AS STRING
	RETURN e:ToString() + n:ToString()
END CLASS

FUNCTION Start() AS VOID
//	LOCAL u AS TestClass
	LOCAL u AS USUAL
	u := TestClass{}

	xAssert( u:TestMethod(TestEnum.Two) == TestEnum.Two)

	xAssert( u:TestMethodDefault(2,TestEnum.Three) == "Three2" )
	xAssert( u:TestMethodDefault(,TestEnum.Three)  == "Three1" )

	xAssert( u:TestMethodDefault2(1,TestEnum.One)  == "One1" )
	xAssert( u:TestMethodDefault2(2)               == "Two2" )

	u:IVar := TestEnum.Three
	xAssert( u:IVar == TestEnum.Three)
	u:Prop := TestEnum.Two
    xAssert( u:Prop == TestEnum.Two)
    wait

PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF
RETURN
