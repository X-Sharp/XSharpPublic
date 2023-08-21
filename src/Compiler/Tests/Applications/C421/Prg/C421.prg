// 421. RETURN ignored in Start() function?
// problem seems to exist only with Start()
#pragma warnings(162, off) // unreachable code
FUNCTION Start() AS INT
LOCAL n AS INT
Test1()
Test2()
TestClass{}:Test()
n := 1
? n
RETURN 1976
n ++
? n
THROW Exception{"This code should not execute!"}
RETURN 1

FUNCTION Test1() AS VOID
LOCAL n AS INT
n := 1
? n
RETURN // correct warning here and rest of the code does not get executed
n ++
? n
THROW Exception{"This code in Test1() should not execute!"}
RETURN

FUNCTION Test2() AS INT
LOCAL n AS INT
n := 1
? n
RETURN 1// correct warning here and rest of the code does not get executed
n ++
? n
THROW Exception{"This code in Test2() should not execute!"}
RETURN 2

CLASS TestClass
	CONSTRUCTOR()
		RETURN
		THROW Exception{"This code in ctor should not execute!"}
	RETURN
	METHOD Test() AS VOID
	RETURN
		THROW Exception{"This code in method should not execute!"}
	RETURN
END CLASS
