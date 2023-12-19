// https://github.com/X-Sharp/XSharpPublic/issues/1343
// this should still work

delegate BrwRowCondition(val as int) as logic

class Test
	public method AddColorCondition(uCondition as BrwRowCondition) as logic
		var x := uCondition(1)
		xAssert(x)
		return x
end class



function Start() as void strict
	local test := Test{} as Test

	test:AddColorCondition({val => val > 0})

	return



PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
