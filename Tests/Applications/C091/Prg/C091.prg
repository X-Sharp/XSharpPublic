// 91. vo-incompatibility regarding virtual methods
// same code in vulcan compiled with no /vo options enabled
// vulcan calls child method, xsharp calls parent
CLASS ParentClass
	VIRTUAL METHOD OnTest() AS STRING
		? "parent"
	RETURN "parent"
END CLASS

CLASS ChildClass INHERIT ParentClass
	METHOD OnTest() AS STRING
		? "child"
	RETURN "child"
END CLASS

FUNCTION Start() AS VOID
	LOCAL o AS ParentClass
	o := ChildClass{}
	IF o:OnTest() == "parent"
		THROW System.Exception{"parent called, vulcan calls child"}
	END IF
RETURN

