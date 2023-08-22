// 448. at runtime System.NullReferenceException
// comparison is being implemented with a call to String.Compare(), instead of 
// a direct comparison between chars
FUNCTION Start() AS VOID
	LOCAL cChar AS Char
	LOCAL l AS LOGIC
	cChar := 'A'

//	#error runtime exception here
	l := cChar >= '0'
	? l
	xAssert(cChar >= '0')
	xAssert('0' < cChar)

//	#error runtime exception here
	cChar := 'A'
	l := cChar < '0'
	? l
	xAssert(.not. (cChar < '0'))
	xAssert(.not. ('0' >= cChar))
RETURN


PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"

