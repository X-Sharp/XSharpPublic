// 93. error XS0506: 'ChildClass.OnTest()': cannot override inherited member 'ParentClass.OnTest()' because it is not marked virtual, abstract, or override

// vulcan only warning about hiding base method
#pragma warnings(108, off) // name hids inherited member
CLASS ParentClass
	METHOD OnTest() AS VOID
END CLASS

CLASS ChildClass INHERIT ParentClass
	VIRTUAL METHOD OnTest() AS VOID
END CLASS

