#pragma options("vo17", on)

FUNCTION Start( ) AS VOID
    LOCAL n := 0 AS INT
    LOCAL uValue AS USUAL
    LOCAL lError AS LOGIC
    lError := TRUE
    FOR VAR i := 1 TO 2
        lError := i == 1
        ? "Iteration", i, "Generate error", lError
        
	BEGIN SEQUENCE
	    IF lError
	        ? 1 /n
	    ELSE
    	    BREAK 42
	    ENDIF
	// No Recover, so exception should call _SequenceError
	END
	? "After BEGIN SEQUENCE END 1"
	BEGIN SEQUENCE
	    IF lError
	        ? 1 /n
	    ELSE
    	    BREAK 42
	    ENDIF
	RECOVER
	    ? "In Recover"
	END
	? "After BEGIN SEQUENCE END 2"
	BEGIN SEQUENCE
	    IF lError
	        ? 1 /n
	    ELSE
    	    BREAK 42
	    ENDIF
	RECOVER USING uValue
	    ? "In Recover", uValue
	    IF uValue IS Exception VAR e
	        ? e:Message
	    ENDIF
	END
	? "After BEGIN SEQUENCE END 3"
	?
    NEXT


RETURN



FUNCTION _SequenceError(e AS Exception) AS USUAL
    ? __FUNCTION__
    xAssert(e IS DivideByZeroException)
    ? "Exception caught: "+e:Message
    RETURN  e:Message

FUNCTION _SequenceRecover(uBreakValue AS USUAL) AS USUAL
    ? __FUNCTION__
    xAssert(uBreakValue == 42)
    RETURN uBreakValue



PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF  
RETURN 
