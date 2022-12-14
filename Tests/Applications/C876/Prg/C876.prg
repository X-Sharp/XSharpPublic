// 876. Bogus warning about ambiguous method
// https://github.com/X-Sharp/XSharpPublic/issues/1191
/*
warning XS9043: 'Equals' is ambiguous. Could be Method 'TestClass.Equals(int)' in C876 or Method 'object.Equals(object)' in mscorlib. Using the first one.
warning XS9043: 'Equals' is ambiguous. Could be Method 'AnotherClass.Equals(AnotherClass)' in C876 or Method 'object.Equals(object)' in mscorlib. Using the first one.
*/

// #pragma options ("wx", on)
FUNCTION Start() AS VOID
	LOCAL o := TestClass{} AS TestClass
	? o:Equals(123)
	xAssert(o:Equals(123))
	
	LOCAL a := AnotherClass{} AS AnotherClass
	? a:Equals(a)
	xAssert(a:Equals(a) == 999)
RETURN

CLASS TestClass
	METHOD Equals(n AS INT) AS LOGIC
		? "called child"
	RETURN TRUE
END CLASS

CLASS AnotherClass
	METHOD Equals(o AS AnotherClass) AS INT
		? "called child"
	RETURN 999
END CLASS


PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF
