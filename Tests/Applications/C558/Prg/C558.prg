// 558. error XS0037: Cannot convert null to 'logic' because it is a non-nullable value type
// also warning XS9025: Missing RETURN statement. A statement with a default 'empty' return value is returned.
// problem happens with /vo9 only (allow missing RETURN statements)
FUNCTION Start() AS VOID
	? Test(TRUE)
	xAssert(Test(TRUE))
	? Test(FALSE)
	xAssert(.not. Test(FALSE))
RETURN

FUNCTION Test(l AS LOGIC) AS LOGIC
	TRY
		RETURN l
	CATCH e AS Exception
		RETURN FALSE
	END TRY
//RETURN FALSE // this allow it to compile, but give a (correct) unreachable code warning


PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF

