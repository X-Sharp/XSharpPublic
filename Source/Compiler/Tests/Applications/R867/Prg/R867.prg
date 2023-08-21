// https://github.com/X-Sharp/XSharpPublic/issues/1096
// improve error message when multiple overloads are found
#pragma options("lb", on)
FUNCTION Start() AS VOID
LOCAL u AS OBJECT
u := TestClass{}

TRY
	u:Test("123")
CATCH e AS Exception
	? e:ToString()
END TRY
Console.ReadLine()
TRY
	u:Test(123)
CATCH e AS Exception
	? e:ToString()
END TRY

CLASS TestClass
	METHOD Test(o AS OBJECT) AS VOID
		? "object"

	METHOD Test(s AS string) AS VOID
		? "string", s
	METHOD Test(i AS int) AS VOID
		? "int", i

	METHOD Test(o AS USUAL , n := 1  AS INT) AS VOID
		? "usual and 2nd default param"
END CLASS
