// https://github.com/X-Sharp/XSharpPublic/issues/911
// test for # operator without whitespace

FUNCTION Start( ) AS VOID
LOCAL liALeft, liATop     
DIMENSION test(10)
IF liALeft#TRUE OR liATop#TRUE
    // name expression
    xAssert(TRUE)
NOP              
ELSE
    xAssert(FALSE)
ENDIF

IF (liALeft)#TRUE OR (liATop)#TRUE
    // parenthesized expression
    xAssert(TRUE)
NOP              
ELSE
    xAssert(FALSE)
ENDIF

test = FALSE
IF test[1]#TRUE OR test[2]#TRUE
    // compare array elements with logic
    xAssert(TRUE)
NOP              
ELSE
    xAssert(FALSE)
ENDIF
test[1] = #TRUE
test[2] = #FALSE

IF test[1]=#TRUE OR test[2]=#TRUE
    // compare symbols
    xAssert(TRUE)
NOP              
ELSE
    xAssert(FALSE)
ENDIF


RETURN
END FUNCTION



PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
