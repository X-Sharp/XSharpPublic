// 199. error XS0175: Use of keyword 'SUPER' is not valid in this context
// vulcan incompatibility, using SELF instead works fine
// not sure if we should allow this code and report a waring instead of an error
FUNCTION Start() AS VOID

CLASS TestClass
	METHOD Test() AS OBJECT
	RETURN SUPER
END CLASS

