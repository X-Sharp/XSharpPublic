// 437. error XS9002: Parser: unexpected input 'cChar'
FUNCTION Start() AS VOID
LOCAL cChar AS Char

cChar := '\''
DO CASE
CASE cChar == '\t'
	NOP
CASE cChar == '\r'
	NOP
CASE cChar == '\n'
	NOP
CASE cChar == '\''
	NOP
CASE cChar == '"'
	NOP
CASE cChar == '\\'
	NOP
END CASE

IF cChar == '\''
	DO WHILE cChar == '\t'
		? cChar
	END DO
END IF

cChar := '\t'
xAssert(cChar == '\t')
xAssert(cChar:ToString() == e"\t")

cChar := '\\'
xAssert(cChar == '\\')
xAssert(cChar:ToString() == "\")

cChar := '\''
? cChar == '\''
xAssert(cChar == '\'')
xAssert(.not. (cChar != '\''))
xAssert(cChar:ToString() == "'")
RETURN

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"

