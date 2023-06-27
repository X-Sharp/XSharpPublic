// when a class inherits from Object then SUPER:Foo() should not generate a late bind call.
// See https://github.com/X-Sharp/XSharpPublic/issues/1285
function Start() as void strict
	var xx := NotOK{}
	xx:Foo()
	return


class NotOK

	method Foo() as void
		super:Foo()
		return

end class
