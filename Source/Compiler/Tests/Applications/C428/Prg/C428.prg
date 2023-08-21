// 428. error XS9002: Parser: mismatched input 'Type'
#using System.Reflection
FUNCTION Start() AS VOID
	LOCAL oType AS Type
	oType := Assembly.GetExecutingAssembly():GetTypes()[1]
	? oType:Assembly
	? oType:BaseType
RETURN

CLASS TestClass
	EXPORT oType AS Type
END CLASS
