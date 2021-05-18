// 778. Problem with Extension method not marked as STATIC
/*

- When /vo3 is enabled, the following reports an error:
error XS0112: A static member 'Extensions.NonStatic(string)' cannot be marked as override, virtual, or abstract

- When /vo3 is disabled, the code compiles with no errors, but when running an exception is thrown:
System.TypeLoadException: Virtual Static Method.

*/
FUNCTION Start() AS VOID
	LOCAL c := "abc" AS STRING
	? c:NonStatic()
RETURN


CLASS Extensions
	METHOD NonStatic(SELF c AS STRING) AS INT
	RETURN c:Length
END CLASS


