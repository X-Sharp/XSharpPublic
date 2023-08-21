// RETURN without expression causes crash in access in Vulcan mode
// when /vo9 is enabled
#pragma warnings(9026, off) // missing return value
CLASS Test
	ACCESS Foo
		RETURN
	ASSIGN Foo(u)
		RETURN
END CLASS
