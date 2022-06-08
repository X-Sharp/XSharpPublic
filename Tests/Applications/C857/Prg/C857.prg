// 857. Problem assigning function to delegate, when /undeclared+ is enabled
// https://github.com/X-Sharp/XSharpPublic/issues/1051

DELEGATE TestDelegateType( par AS STRING ) AS INT

FUNCTION Start() AS VOID

abc := 123
? abc	
	
LOCAL TestDelegate AS TestDelegateType
TestDelegate := Functions.MyFunction // Working statement
? TestDelegate( "Abcd" )
TestDelegate := MyFunction // XSharp.Error: "Variable does not exist"
? TestDelegate( "Abcd" )
RETURN

FUNCTION MyFunction( par AS STRING ) AS INT
? par
RETURN 456

