// https://github.com/X-Sharp/XSharpPublic/issues/1343

delegate BrwRowCondition(val as int) as logic

class Test
	public method AddColorCondition(uCondition as BrwRowCondition) as logic
		var x := uCondition(1)
		return x
end class



function Start() as void strict
	local test := Test{} as usual

    // this now generates error XS9121: Cannot convert from Lambda Expression to Usual
	test:AddColorCondition({val => val > 0})

	return
