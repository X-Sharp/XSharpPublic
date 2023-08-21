// 803. Compiler error with two INSTANCE vars defined in the same line
CLASS TestClass
	// error XS0579: Duplicate 'global::XSharp.Internal.IsInstanceAttribute' attribute
	INSTANCE ins1,ins2 AS INT
	CONSTRUCTOR()
		SELF:ins1 := 123
		? SELF:ins1
	RETURN
	
END CLASS

FUNCTION Start() AS VOID
	LOCAL o AS TestClass
	o := TestClass{}
	? o
RETURN
