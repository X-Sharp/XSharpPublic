// 434. error XS0127: Since 'TestClass.TestClass(params __Usual[])' returns void, a return keyword must not be followed by an object expression
// VO incompatibility - I think we should allow (and ignore) RETURN SELF for METHOD Init()s
// (obviously only when /vo1 is enabled)
FUNCTION Start( ) AS VOID
	LOCAL o AS TestClass
	o := TestClass{}
	IF .not. o:n == 1
		THROW Exception{"Init() not called"}
	END IF
RETURN

CLASS TestClass
	EXPORT n AS INT
	METHOD Init()
		SELF:n := 1
	RETURN SELF
END CLASS

