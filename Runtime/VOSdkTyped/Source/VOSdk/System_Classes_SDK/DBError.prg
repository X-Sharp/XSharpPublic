PARTIAL CLASS DbError   INHERIT Error

CONSTRUCTOR( oOriginator, symMethod, wErrorType, oHLErrorMessage, uMisc1, uMisc2 )  
    SUPER()
	SubSystem := "Database"
	IF oOriginator# NIL
		MethodSelf := oOriginator
	ENDIF
	IF symMethod# NIL
		FuncSym := symMethod
		CallFuncSym := symMethod
	ENDIF
	IF wErrorType# NIL
		Gencode := wErrorType
	ELSE
		Gencode := EG_NOTABLE
	ENDIF
	IF oHLErrorMessage# NIL
		IF IsObject(oHLErrorMessage) .and. __Usual.ToObject(oHLErrorMessage) IS HyperLabel 
			Description := ((HyperLabel)oHLErrorMessage):Description
		ELSE
			Description := oHLErrorMessage
		ENDIF
	ENDIF
	IF Gencode = EG_ARG .OR. uMisc1 != NIL .OR. uMisc2 != NIL
		Args := { uMisc1 }
		Arg := uMisc2
	ENDIF
	Severity := ES_ERROR
	RETURN 


METHOD Throw() AS VOID STRICT
	IF CanBreak()
		BREAK SELF
	ENDIF
	Eval(ErrorBlock(), SELF)
    RETURN
    
END CLASS

