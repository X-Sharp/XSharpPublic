//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


/// <include file="System.xml" path="doc/DbError/*" />
CLASS DbError   INHERIT Error


/// <include file="System.xml" path="doc/DbError.ctor/*" />
CONSTRUCTOR( oOriginator, symMethod, wErrorType, oHLErrorMessage, uMisc1, uMisc2 )  CLIPPER
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




/// <include file="System.xml" path="doc/DbError.Throw/*" />
METHOD Throw() AS VOID STRICT
	IF CanBreak()
		BREAK SELF
	ENDIF
	Eval(ErrorBlock(), SELF)
    RETURN
    
    
END CLASS


