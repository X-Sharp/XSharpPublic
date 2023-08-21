FUNCTION Start( ) AS VOID
	LOCAL hFile AS PTR
	LOCAL cLine AS USUAL
	LOCAL cFile AS STRING
	LOCAL cText	AS STRING    
	cFile := "test.txt"
	hFile := FCreate(cFile)
	cLine := "Line1"              
	FWriteLine(hFile, cLine)
	cLine := "Line2"
	FWriteLine(hFile, cLine)
	FClose(hFile)
	cText := MemoRead(cFile)
	xAssertEquals(e"Line1\r\nLine2\r\n", cText)
	FErase(cFile) 
RETURN

PROC xAssertEquals(o1 AS STRING, o2 AS STRING)
IF o1 == o2
	? "Assertion passed"
ELSE
	THROW Exception{String.Format("Incorrect result, expected {0}, returned {1}" , o2 , o1)}
END IF
