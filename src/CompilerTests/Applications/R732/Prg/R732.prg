// Incorrect DATE/DateTime overload used with DATE param  
// Ticket https://github.com/X-Sharp/XSharpPublic/issues/181
FUNCTION Start() AS VOID
	LOCAL o AS ChildClass
	o := ChildClass{}

	xAssert(o:TestSameLevel(DateTime.Now) == "DateTime") // DateTime, ok
	xAssert(o:TestSameLevel(Today()) == "DATE") // DATE, ok
	?
	xAssert(o:TestDifferentLevel(DateTime.Now) == "DateTime")// DateTime, ok  
	xAssert(o:TestDifferentLevel(Today())== "DATE") // DateTime, wrong
RETURN

CLASS ParentClass
	VIRTUAL METHOD TestDifferentLevel(d AS DATE) AS STRING
	return "DATE"
END CLASS

CLASS ChildClass INHERIT ParentClass
	VIRTUAL METHOD TestDifferentLevel(d AS DateTime) AS STRING
	return "DateTime"
	// SELF:TestDifferentLevel(Today()) // this will cause a recursive call to same method

	VIRTUAL METHOD TestSameLevel(d AS DATE) AS STRING
	return "DATE"
	VIRTUAL METHOD TestSameLevel(d AS DateTime) AS STRING
	return "DateTime"
END CLASS


PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"

