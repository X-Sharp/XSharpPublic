// the code below has a static class that inherits from a normal class
// the compiler throws a warning (713) but produces an exe with a strange
// reference in it with the name "Error<Guid>"
// This should throw an error in stead but we must also check what causes
// the strange reference
FUNCTION Start AS VOID
	Bar.Test()
	RETURN	
	
	
CLASS foo
END CLASS
STATIC CLASS bar INHERIT foo
	STATIC METHOD test AS VOID
		? "Test"
		RETURN
END CLASS	
