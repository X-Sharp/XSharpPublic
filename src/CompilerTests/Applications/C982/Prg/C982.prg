// 982. Incorrect scope of PUBLICS in the VFP dialect #2059
// https://github.com/X-Sharp/XSharpPublic/issues/2059

#pragma options("memvar", enable)
#pragma options("undeclared", enable)
FUNCTION Start() AS VOID
	PUBLIC public_test
	public_test := "public_test"

	PUBLIC private_test
	private_test := "private_test"
	
	SomeProc()
	
	? public_nested // XSharp.Error: Variable does not exist: public_nested
	xAssert( public_nested == "public_nested" ) 
	
	LOCAL lException := FALSE AS LOGIC
	TRY
		? private_nested
	CATCH
		lException := TRUE
	END TRY
	xAssert( lException )
	
RETURN

PROCEDURE SomeProc() AS VOID
	? public_test
	? private_test
	xAssert( public_test == "public_test" ) 
	xAssert( private_test == "private_test" ) 

	PUBLIC public_nested
	public_nested := "public_nested"
	? public_nested
	
	PRIVATE private_nested
	private_nested := "private_nested"


PROC xAssert(l AS LOGIC) AS VOID
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"

