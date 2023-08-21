// 396. error XS1503: Argument 1: cannot convert from 'void*' to 'string'
FUNCTION Start() AS VOID
	LOCAL cFont AS STRING
	LOCAL ptrBuffer AS PTR

	ptrBuffer := String2Psz("abc")
	cFont:=Psz2String(PSZ(ptrBuffer))
	? cFont
	
	IF .not. (cFont == "abc")
		THROW Exception{"Incorrect result"}
	END IF
RETURN

