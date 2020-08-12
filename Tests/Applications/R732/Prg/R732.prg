// Incorrect DATE/DateTime overload used with DATE param  
// Ticket https://github.com/X-Sharp/XSharpPublic/issues/181
FUNCTION Start() AS VOID
	LOCAL o AS ChildClass
	o := ChildClass{}

	o:TestSameLevel(DateTime.Now) // DateTime, ok
	o:TestSameLevel(Today()) // DATE, ok
	?
	o:TestDifferentLevel(DateTime.Now) // DateTime, ok
	o:TestDifferentLevel(Today()) // DateTime, wrong
RETURN

CLASS ParentClass
	VIRTUAL METHOD TestDifferentLevel(d AS DATE) AS VOID
	? "DATE"
END CLASS

CLASS ChildClass INHERIT ParentClass
	VIRTUAL METHOD TestDifferentLevel(d AS DateTime) AS VOID
	? "DateTime"
	// SELF:TestDifferentLevel(Today()) // this will cause a recursive call to same method

	VIRTUAL METHOD TestSameLevel(d AS DATE) AS VOID
	? "DATE"
	VIRTUAL METHOD TestSameLevel(d AS DateTime) AS VOID
	? "DateTime"
END CLASS
