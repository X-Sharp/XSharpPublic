// 593. (1,74): error XS0246: .....
// Error messages without file/line info

// uncomment to see different error messages without file/line info

/*CLASS FooBar
END CLASS
CLASS Bar
END CLASS
*/
PARTIAL CLASS Foo INHERIT Bar
	ACCESS Test1 AS FooBar
	RETURN 0
	ASSIGN Test2(n AS INT)
	LOCAL d AS DWORD
	SELF:Mmmmm(d)
	RETURN
	METHOD Mmmmm(n AS INT) AS VOID
END CLASS


