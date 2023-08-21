
FUNCTION Start( ) AS VOID
	LOCAL e AS USUAL
LOCAL cErrMsg AS STRING
	
BEGIN SEQUENCE
    IF TRUE
        THROW Error{"test"}
    ENDIF
RECOVER USING e
    IF e IS Error VAR oErr
        cErrMsg := oErr:Description // throw exception
      	xAssert(cErrMsg == "test")  
    ENDIF
END SEQUENCE
RETURN


PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
