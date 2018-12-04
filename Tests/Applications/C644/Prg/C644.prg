// 644. Problems with && operator in VO dialect
/*
In Core and Vulcan dialect, all the following code compiles and tuns fine
In VO, a parser error is reported for all the code using &&
Additionally, it has wrong results:

? TRUE && !FALSE // TRUE
? FALSE && !TRUE // FALSE

this is only in VO dialect. In Core and Vulcan, correct results:

? TRUE && !FALSE // FALSE
? FALSE && !TRUE // FALSE

*/
FUNCTION Start( ) AS VOID

? TRUE && !FALSE // TRUE
? FALSE && !TRUE // FALSE

xAssert(TRUE .and. FALSE , FALSE)
xAssert(!TRUE .and. FALSE , FALSE)
xAssert(TRUE .and. !FALSE , TRUE)
xAssert(!TRUE .and. !FALSE , FALSE)

xAssert(TRUE && FALSE , FALSE) // error XS9002: Parser: unexpected CRLF, are you missing a  token ?
xAssert(!TRUE && FALSE , FALSE)
xAssert(TRUE && !FALSE , TRUE)
xAssert(!TRUE && !FALSE , FALSE)

xAssert(FALSE .and. TRUE , FALSE)
xAssert(!FALSE .and. TRUE , TRUE)
xAssert(FALSE .and. !TRUE , FALSE)
xAssert(!FALSE .and. !TRUE , FALSE)

xAssert(FALSE && TRUE , FALSE)
xAssert(!FALSE && TRUE , TRUE)
xAssert(FALSE && !TRUE , FALSE)
xAssert(!FALSE && !TRUE , FALSE)



xAssert(TRUE .or. FALSE , TRUE)
xAssert(!TRUE .or. FALSE , FALSE)
xAssert(TRUE .or. !FALSE , TRUE)
xAssert(!TRUE .or. !FALSE , TRUE)

xAssert(TRUE || FALSE , TRUE)
xAssert(!TRUE || FALSE , FALSE)
xAssert(TRUE || !FALSE , TRUE)
xAssert(!TRUE || !FALSE , TRUE)

RETURN


PROC xAssert(l AS LOGIC , lMustBe AS LOGIC)
IF l != lMustBe
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"

