// 680. error XS0266: Cannot implicitly convert type 'int?' to 'int'. An explicit conversion exists (are you missing a cast?)
FUNCTION Start() AS VOID
	LOCAL n AS INT
	LOCAL o AS TestClass
	o := GetObject()
	n := o?:Test() DEFAULT 42
	? n
RETURN
function GetObject() as TestClass
	return null
// small git test
CLASS TestClass
	METHOD Test() AS INT
	RETURN 0
END CLASS
