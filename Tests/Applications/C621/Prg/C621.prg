// 621. Problems with same named functions and methods
/*
Compile reports several warnings like:

warning XS9043: Method 'Left' is ambiguous. Could be 'TestClass.Left(string, int)' or 'Functions.Left(string, int)'. Using the first one.

This is not correct, the function should be always used, not the same named static method.
Indeed at teh runtime the static method is (incorrectly) called.
*/
FUNCTION Start() AS VOID
	LOCAL o AS TestClass
	o := TestClass{}
	o:InstanceTest("abc")
	TestClass.StaticTest("def")
RETURN

FUNCTION Left(c AS STRING , dwLen AS INT) AS STRING
RETURN NULL
FUNCTION Left(c AS STRING , dwLen AS DWORD) AS STRING
RETURN NULL

FUNCTION SubStr(c AS STRING , nStart AS DWORD , dwLen AS DWORD) AS STRING
RETURN NULL
FUNCTION SubStr(c AS STRING , nStart AS INT , dwLen AS INT) AS STRING
RETURN NULL

FUNCTION SubStr(c AS STRING , nStart AS DWORD) AS STRING
RETURN NULL
FUNCTION SubStr(c AS STRING , nStart AS INT) AS STRING
RETURN NULL


CLASS TestClass
	METHOD InstanceTest(cPath AS STRING) AS STRING
		Left(cPath , 1)
		Left(cPath , cPath:Length - 1)
		SubStr(cPath , 2)
	RETURN cPath
	STATIC METHOD StaticTest(cPath AS STRING) AS STRING
		Left(cPath , 1)
		Left(cPath , cPath:Length - 1)
		SubStr(cPath , 2)
	RETURN cPath
	STATIC METHOD Left(c AS STRING,n AS Int32) AS STRING
		THROW Exception{"Should never be called"}
	RETURN NULL
	STATIC METHOD SubStr(c AS STRING,nPos AS Int32) AS STRING
		THROW Exception{"Should never be called"}
	RETURN NULL
END CLASS


