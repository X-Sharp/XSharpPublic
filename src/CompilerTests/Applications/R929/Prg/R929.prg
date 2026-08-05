public static class Test
	public static method TestMethodNotWorking(a := 0 as decimal, b := 0 as decimal) as decimal
		return a + b

	public static method TestMethodWorking(a := 0.0m as decimal, b := 0.0m as decimal) as decimal
		return a + b
end class

function Start() as void strict
	var x := Test.TestMethodWorking()
	xAssert(x == 0)
	x := Test.TestMethodNotWorking() // crashing also in debugger with breakpoint in line before...never reached this line
	xAssert(x == 0)

	return

PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	? "Assertion FAILED"
	THROW Exception{"Incorrect result"}
END IF
RETURN
