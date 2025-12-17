// 943. Default ENUM Parameters in x86 mode
// https://github.com/X-Sharp/XSharpPublic/issues/1760
// https://github.com/X-Sharp/XSharpPublic/issues/1761

enum IntEnum
	member m1 := 1
	member m2 := 2
end enum

enum dwordenum as dword
	member m1 := 1
	member m2 := 2
end enum

procedure TestProc1(c1 as string, c2 := "aaa" AS STRING, c3 AS STRING, l as logic, e := 2 as IntEnum)
	? c1, c2, c3, l , e:ToString()
	xAssert(c1 == "a")
	xAssert(c2 == "b")
	xAssert(c3 == "c")
	xAssert(l == true)
	xAssert(e == IntEnum.m2)

procedure TestProc2(c1 as string, c2 AS STRING, c3 := "test" AS STRING, l as logic, e := 1 as IntEnum)
	? c1, c2, c3, l , e:ToString()
	xAssert(c1 == "a")
	xAssert(c2 == "b")
	xAssert(c3 == "c")
	xAssert(l == false)
	xAssert(e == IntEnum.m1)


procedure TestProc3(c1 as string, c2 := "test2" AS STRING, c3 := "test3" AS STRING, l as logic, e := DwordEnum.m2 as DwordEnum)
	? c1, c2, c3, l , e:ToString()
	xAssert(c1 == "a")
	xAssert(c2 == "test2")
	xAssert(c3 == "c")
	xAssert(l == true)
	xAssert(e == IntEnum.m2)

function Start() as void
	TestProc1("a","b","c",true)
	TestProc2("a","b","c",false)
	TestProc3("a",,"c",true)

PROC xAssert(l AS LOGIC)  AS VOID
	IF l
		? "Assertion passed"
	ELSE
		THROW Exception{"Incorrect result"}
	END IF
RETURN
