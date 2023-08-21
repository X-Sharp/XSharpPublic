// 210. Runtime exception, incorrect code generation for late bound calls on OBJECT vars
// vulcan dialect /lb+
FUNCTION Start() AS VOID
LOCAL o AS OBJECT // USUAL is ok
o := TestClass{}
? o:TestProp
? o:TestMethod()
IF .not. o:TestMethod() == "Called correctly"
	THROW Exception{"Bad method call"}
END IF

CLASS TestClass
	PROPERTY TestProp AS INT GET 123
	METHOD TestMethod() CLIPPER
	RETURN "Called correctly"
END CLASS

