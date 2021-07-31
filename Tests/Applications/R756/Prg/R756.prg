DEFINE BUF_SIZE := _SIZEOF(_test) * 10

FUNCTION Start( ) AS VOID
	? _sizeof(_test)
	? BUF_SIZE
	xAssert(_sizeof(_test) == 12)
	xAssert(BUF_SIZE == 120)
RETURN


VOSTRUCT _test 
    MEMBER a AS LONG
    MEMBER b AS DWORD
    MEMBER c AS WORD
    MEMBER d AS SHORT
    
    
    
PROC xAssert(l AS LOGIC) 
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"   
RETURN       
