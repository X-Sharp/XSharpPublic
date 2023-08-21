// 878. Preprocessor problem with the SUM UDC (with nested brackets)

FUNCTION Start() AS VOID
	LOCAL uSum1,uSum2 AS INT

	SUM 10 TO uSum1 WHILE 1==1 FOR "a"=="a"
	? uSum1
	xAssert(uSum1 == 10)
	SUM 100,200 TO uSum1 , uSum2 WHILE TRUE FOR TRUE

	? uSum1, uSum2
	xAssert(uSum1 == 100)
	xAssert(uSum2 == 200)

FUNCTION DbEval(cb)
	Eval(cb)
RETURN NIL

PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
