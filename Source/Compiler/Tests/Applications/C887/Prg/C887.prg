// 887. VO incompatibility with INSTANCE/PROTECT fields
// https://github.com/X-Sharp/XSharpPublic/issues/1253

FUNCTION Start() AS VOID
	LOCAL o := TestClass{} AS TestClass
	xAssert(o:pSameName == "access")
	xAssert(o:iSameName == "access" )
	o:DoTest()
	

CLASS TestClass
	PROTECT pSameName := "field" AS STRING
	INSTANCE iSameName := "field" AS STRING
	
ACCESS pSameName AS STRING
	? "Protect from access",SELF:pSameName
	xAssert(SELF:pSameName == "field")
RETURN "access"      
ACCESS iSameName AS STRING
	? "Instance from access",SELF:iSameName
	xAssert(SELF:pSameName == "field")
RETURN "access"      

METHOD DoTest() AS VOID
	? "Protect from method:", SELF:pSameName
	xAssert(SELF:pSameName == "field")
	
	? "Instance from method:", SELF:iSameName
	xAssert(SELF:iSameName == "access")
END CLASS

PROC xAssert(l AS LOGIC)  AS VOID
	IF l
		? "Assertion passed"
	ELSE
		? "Assertion failed!!!!!!!!!!!!!!!"
//		THROW Exception{"Incorrect result"}
	END IF
RETURN


