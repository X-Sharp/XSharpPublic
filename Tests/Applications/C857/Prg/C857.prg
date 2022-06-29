// 857. Problem assigning function to delegate, when /undeclared+ is enabled
// https://github.com/X-Sharp/XSharpPublic/issues/1051
#pragma warnings(9073, off) // undeclared, field or memvar
DELEGATE TestDelegateType( par AS STRING ) AS INT

FUNCTION Start() AS VOID

abc := 123
? abc

LOCAL TestDelegate AS TestDelegateType
TestDelegate := Functions.MyFunction // Working statement
? TestDelegate( "Abcd" )
xAssert(TestDelegate( "Abcd" ) == 456)
TestDelegate := MyFunction // XSharp.Error: "Variable does not exist"
? TestDelegate( "Abcd" )
xAssert(TestDelegate( "Abcd" ) == 456)
RETURN

FUNCTION MyFunction( par AS STRING ) AS INT
? par
RETURN 456


PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN

