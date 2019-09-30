// 680. error XS0266: Cannot implicitly convert type 'int?' to 'int'. An explicit conversion exists (are you missing a cast?)
FUNCTION Start() AS VOID
	LOCAL n AS INT
	LOCAL o AS TestClass
	n := o?:Test()
	? n
RETURN

// small git test
CLASS TestClass
	METHOD Test() AS INT
	RETURN 0
END CLASS
