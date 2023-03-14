// 885. Problems with interpolated/escaped literal strings
// https://github.com/X-Sharp/XSharpPublic/issues/1218
FUNCTION Start() AS VOID
	LOCAL paramSetting := "xyz" AS STRING
	LOCAL test AS STRING
	test := ei"\"{paramSetting}\""
	? test
	xAssert(test == e"\"xyz\"")

	test := i" /\{paramSetting}/\ "
	xAssert(test == " /\xyz/\ ")
	? test

	test := i"/\{paramSetting}/\"
	xAssert(test == "/\xyz/\")
	? test

PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN

