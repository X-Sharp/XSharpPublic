// 601. warning XS0219 (variable is assigned but its value is never used) not reported in some cases
FUNCTION Start( ) AS VOID
RETURN

CLASS TestClass
	PROTECT nStaticField AS INT

	METHOD Test1() AS VOID
		LOCAL nTest1 AS INT // warning
		LOCAL nTest2 AS INT // no warning, bug
		
		nTest1 := 1
		nTest2 := nStaticField
	RETURN

	METHOD Test2() AS VOID
		LOCAL nLocal := 1 AS INT
		LOCAL nTest1 AS INT // no warning, bug
		LOCAL nTest2 AS INT // warning
		
		nTest1 := nLocal
		nTest2 := 1
	RETURN

	METHOD Test3() AS VOID
		LOCAL nLocal := 1 AS INT
		LOCAL nTest1 AS INT // no warning, bug
		LOCAL nTest2 AS INT // no warning, bug
		
		nTest1 := nStaticField
		nTest2 := nLocal
	RETURN
	
END CLASS
