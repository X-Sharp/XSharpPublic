// 433. error XS0127: Since 'TestClass.TestClass(params __Usual[])' returns void, a return keyword must not be followed by an object expression
// problem happens when /vo9+ (implicit RETURN value) and of course /vo1+ are enabled
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
	RETURN
	METHOD Axit()
	RETURN
END CLASS

