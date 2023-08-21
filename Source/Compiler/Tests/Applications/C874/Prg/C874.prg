// 874. Late bound problem resolving overloaded method
// https://github.com/X-Sharp/XSharpPublic/issues/1158

CLASS TestClass
	METHOD Seek(a AS INT) AS INT
	RETURN a
	METHOD Seek(a AS INT, b AS INT) AS INT
	RETURN a+b
	METHOD Seek(a AS INT, b AS INT, c AS INT) AS INT
	RETURN a+b+c
END CLASS

FUNCTION Start() AS VOID
LOCAL u AS USUAL
u := TestClass{}
u:Seek(1,2,3) // XSharp.Error: No exported method

xAssert( u:Seek(1) == 1 )
xAssert( u:Seek(1,2) == 3 )
xAssert( u:Seek(1,2,3) == 6 )

PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF
