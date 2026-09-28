// 984. Problem with /fox3 when /fox2 is also enabled #2083
// https://github.com/X-Sharp/XSharpPublic/issues/2083

#pragma options("fox2", enable)

#pragma options("fox3", enable)
#pragma options("allowdot", enable)
#pragma options("lb", enable)
#pragma options("memvar", enable)
#pragma options("undeclared", enable)

CLASS SeparateClass
	PROPERTY OtherProp AS INT AUTO
	EXPORT OtherExport AS INT
END CLASS

CLASS MainClass
	EXPORT TestExport := SeparateClass{} AS SeparateClass
	PROPERTY TestProp AS SeparateClass GET SELF:TestExport
	
	METHOD DoTest() AS VOID
		TestExport.OtherProp := 123
		? TestExport.OtherProp
		xAssert( TestExport.OtherProp == 123 )

		TestProp.OtherProp := 456
		? TestProp.OtherProp
		xAssert( TestProp.OtherProp == 456 )

		TestExport.OtherExport := 1
		? TestExport.OtherExport
		xAssert( TestExport.OtherExport == 1 )

		TestProp.OtherExport := 2
		? TestProp.OtherExport
		xAssert( TestProp.OtherExport == 2 )
END CLASS

FUNCTION Start( ) AS VOID
	MainClass{}:DoTest()
	


PROC xAssert(l AS LOGIC) AS VOID
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF

