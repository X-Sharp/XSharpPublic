// 210. Runtime exception, incorrect code generation for late bound calls on OBJECT vars
// vulcan dialect /lb+
FUNCTION Start() AS VOID
LOCAL o AS OBJECT // USUAL is ok
o := TestClass{}
? o:TestProp
? o:TestMethod()

CLASS TestClass
	PROPERTY TestProp AS INT GET 123
	METHOD TestMethod() CLIPPER
	RETURN "Called correctly"
END CLASS

