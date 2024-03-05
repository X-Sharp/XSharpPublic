// 900. INSTANCE field inconsistency with and without SELF
// https://github.com/X-Sharp/XSharpPublic/issues/1432

FUNCTION Start() AS VOID
	LOCAL o AS TestClass
	o := TestClass{}
	xAssert(o:Instance_member == "access" )
	xAssert(o:Protected_member == "access" )
	o:Test()

CLASS TestClass
	INSTANCE Instance_member := "field" AS STRING
	PROTECT Protected_member := "field" AS STRING
	ACCESS Instance_member AS STRING
	RETURN "access"
	ACCESS Protected_member AS STRING
	RETURN "access"
	EXPORT METHOD Test() AS VOID
		xAssert( SELF:Instance_member == "access")// access OK
		xAssert( Instance_member == "access") // field, wrong

		xAssert( SELF:Protected_member == "field") // field, OK
		xAssert( Protected_member == "field") // field, OK
	RETURN
	
END CLASS

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN	
