// 689. error XS1061: 'System.DateTime' does not contain a definition for 'yyyy
FUNCTION Start() AS VOID
	LOCAL s AS STRING

	LOCAL n AS INT
	n := 123
	s := i"{n:0000}" //ok
	? s
	
	LOCAL dData AS DateTime
	dData := DateTime{2019,11,09}
	s := i"{dData:yyyy-MM-dd}" // error XS0103
	? s
	xAssert(s == "2019-11-09")
	
	s := i"{dData:yyyy/MM/dd}"
	xAssert(s == "2019/11/09")
RETURN

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"

