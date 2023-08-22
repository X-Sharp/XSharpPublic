// 614. error XS0121: The call is ambiguous between the following methods or properties: 'Functions.TestFunc(int)' and 'Functions.TestFunc(ref int)'
/*
Problem only happens in Core dialect and when /vo7 is enabled. In VO dialect it works as expected.
Maybe it does not make sense to support /vo7 in core dialect at all anyway?
*/
FUNCTION Start() AS VOID
	LOCAL n AS INT
	n := 10
	TestFunc(n)
	IF TestFunc(n) == "AS"
		? "Test passed"
	ELSE
		THROW Exception{"REF overload was called"}
	END IF
RETURN

FUNCTION TestFunc(n AS INT) AS STRING
RETURN "AS"
FUNCTION TestFunc(n REF INT) AS STRING
RETURN "REF"


