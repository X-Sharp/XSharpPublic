#pragma options("vo17", on)

FUNCTION Start( ) AS VOID
    LOCAL n := 0 AS INT
    LOCAL uValue AS USUAL
    LOCAL lError AS LOGIC
    lError := TRUE
    
    FOR VAR I := 1 TO 10000
        IF i %100 == 0
            Qout(i)
        ENDIF
    NEXT    
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

#pragma options("vo17", off)
FUNCTION _ExecStatement( action AS @@Action) AS VOID
    LOCAL lRepeat := TRUE AS LOGIC
    DO WHILE lRepeat
    TRY
        lRepeat := FALSE
        action()
    CATCH e AS Exception
        LOCAL err AS Error
        IF e IS Error
            err := (Error) e
        ELSE
            err := Error{e}
            IF e:HResult == (INT) 0x80020012
                err:Gencode := EG_ZERODIV
            ENDIF
        ENDIF
        err:FuncSym := ProcName(1)
        err:Stack := ErrorStack(1)
        err:CanRetry := TRUE
        err:CanDefault := TRUE
        VAR x := Eval(ErrorBlock(), err)
        IF x == E_DEFAULT
            ? "Ignore error"
        ELSEIF x == E_RETRY
            ? "Retry"
            lRepeat := TRUE
        ENDIF
    END TRY
    ENDDO
    RETURN

FUNCTION _ExecExpression<T>( action AS @@Func<T>) AS T
    LOCAL lRepeat := TRUE AS LOGIC
    LOCAL result := default(T) AS T
    DO WHILE lRepeat
    TRY
        lRepeat := FALSE
        result := action()
    CATCH e AS Exception
        LOCAL err AS Error
        IF e IS Error
            err := (Error) e
        ELSE
            err := Error{e}
            IF e:HResult == (INT) 0x80020012
                err:Gencode := EG_ZERODIV
            ENDIF
        ENDIF
        err:FuncSym := ProcName(1)
        err:Stack := ErrorStack(1)
        err:CanRetry := TRUE
        err:CanDefault := TRUE
        VAR x := Eval(ErrorBlock(), err)
        IF x == E_DEFAULT
            ? "Ignore error"
        ELSEIF x == E_RETRY
            ? "Retry"
            lRepeat := TRUE
        ENDIF
    END TRY
    ENDDO
    RETURN result



