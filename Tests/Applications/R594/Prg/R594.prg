// RETURN without expression causes crash in access in Vulcan mode
// when /vo9 is enabled

CLASS Test
	ACCESS Foo  
		RETURN 
	ASSIGN Foo(u)  
		RETURN  
END CLASS
