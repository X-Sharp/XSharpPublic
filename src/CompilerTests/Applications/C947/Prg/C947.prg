// 947. Problem with text containing semicolon inside TEXT TO statement
// https://github.com/X-Sharp/XSharpPublic/issues/1605

FUNCTION Start( ) AS VOID
LOCAL cVar AS STRING

// OK:
TEXT TO cVar
some text;
other text;

ENDTEXT

? cVar

xAssert(cVar == e"some text;\r\nother text;\r\n\r\n")

TEXT TO cVar
some text;
other text;
ENDTEXT

xAssert(cVar == e"some text;\r\nother text;\r\n")


PROC xAssert(l AS LOGIC) AS VOID
	IF l
		? "Assertion passed"
	ELSE
		THROW Exception{"Incorrect result"}
	END IF
RETURN

