#pragma options("vo17", on)

FUNCTION Start( ) AS VOID
    LOCAL n := 0 AS INT
    LOCAL uValue AS USUAL
    LOCAL lError AS LOGIC
    lError := TRUE
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


	WAIT
RETURN



FUNCTION _SequenceError(e AS Exception) AS USUAL
    Eval(Errorblock(), Error{e})
    RETURN "Exception caught: "+e:Message

FUNCTION _SequenceRecover(uBreakValue AS USUAL) AS USUAL
    ? __FUNCTION__
    ? uBreakValue
    RETURN uBreakValue


