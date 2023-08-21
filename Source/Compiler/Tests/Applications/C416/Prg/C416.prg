// error XS0103: The name 'del' does not exist in the current context
DELEGATE MyDelegate(n AS INT) AS INT

FUNCTION Test(del AS MyDelegate) AS INT
	? del(20) // error
	? del:Invoke(20)  // OK

	LOCAL del2 AS MyDelegate
	del2 := handler
	? del2(30) // OK

RETURN del(10)

FUNCTION handler(n AS INT) AS INT
RETURN n + 1

CLASS TestClass
	METHOD DoTest(del AS MyDelegate) AS VOID
		? del(100)
	RETURN
END CLASS

FUNCTION Start( ) AS VOID
Test(MyDelegate{NULL , @handler()})

LOCAL oDel AS MyDelegate
oDel := handler
TestClass{}:DoTest(oDel)
RETURN

