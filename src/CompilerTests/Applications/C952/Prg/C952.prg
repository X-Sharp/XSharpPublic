// 952. Bogus error when concatenating string in .Net 10
// https://github.com/X-Sharp/XSharpPublic/issues/1787

FUNCTION Start() AS VOID
	? System.Environment.Version

	LOCAL s := "abcde" AS STRING
	LOCAL c := 'A' AS Char
	LOCAL n := 1 AS INT

	s := s:Substring(2,2) + c:ToString() // OK
	xAssert ( s == "cdA")
	
	s := c:ToString() + c:ToString()       // OK
	xAssert ( s == "AA")

	s := "abcde"
	s := s:Substring(1,3) + s:ToString() + c:ToString() // OK
	xAssert ( s == "bcdabcdeA")

	s := "abcde"
	s := s:Substring(2,2) + c:ToString() + s:ToString() // error XS9078: All elements of a string concatenation must be of type 'string'. Element 1 is of type 'char' 
	xAssert ( s == "cdAabcde")

	s := "abcde"
	s := s:Substring(2,2) + c:ToString() + s
	xAssert ( s == "cdAabcde")

	s := "abcde"
	s := s:Substring(2,2) + n:ToString() + s + c:ToString()
	xAssert ( s == "cd1abcdeA")

PROC xAssert(l AS LOGIC) 
	IF .NOT. l
		THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
	END IF
	? "Assertion passed"   
RETURN
