// 579. When using /vo2, string class fieds get always reset to "", even if they have been
// assigned to a non empty value in a child class, before a call to super()
// As discussed with Arne in https://www.xsharp.info/forum/public-vo-vn/384-compiler-switch-initialize-strings-vo2?limitstart=0
// the compiler should emit code that checks for each string field if it is NULL and only then assign "" to it
// /vo2

#define sometext "assigned in child before super()"
FUNCTION Start() AS VOID
	LOCAL o AS ChildClass
	o := ChildClass{}
RETURN                  

CLASS ParentClass
	EXPORT cStr AS STRING
	CONSTRUCTOR() CLIPPER
		? "cStr in parent:", SELF:cStr
		xAssert(SELF:cStr == sometext)
	RETURN
END CLASS

CLASS ChildClass INHERIT ParentClass
	CONSTRUCTOR()
		SELF:cStr := sometext
		? "cStr before super():", SELF:cStr
		xAssert(SELF:cStr == sometext)
		SUPER()
		? "cStr after super():", SELF:cStr
		xAssert(SELF:cStr == sometext)
	RETURN
END CLASS

PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF
/*INTERFACE iTest
	EXPORT Name AS STRING	// should not get a default
END INTERFACE*/
STRUCTURE strTest
	EXPORT Name AS STRING   // should not get a default
END STRUCTURE
