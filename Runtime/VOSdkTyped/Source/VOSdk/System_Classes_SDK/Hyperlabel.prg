CLASS HyperLabel
	PROTECT symName AS SYMBOL       // A programmer's identifier
	PROTECT rCaption AS STRING      // A human-readable identifier, used for things such as field labels on forms
	PROTECT rDescription AS STRING  // A human-readable description, used for things such as status bar prompts
	PROTECT rHelpContext AS STRING	// A token used as an identifier for the context-sensitive help

METHOD AsString( )  AS STRING STRICT                            
	RETURN SELF:Caption

ACCESS Caption AS STRING                                	
	RETURN rCaption

ASSIGN Caption(u AS STRING )     
	SELF:rCaption := u
	RETURN 

ACCESS Description AS STRING                                 
	RETURN rDescription

ASSIGN Description(u AS STRING ) 
	SELF:rDescription  := u
	RETURN 

METHOD Error( oError AS Error, symMethod :=  #Unknown AS SYMBOL ) AS VOID
    LOCAL oErr AS Error
    oErr := oError
	oErr:MethodSelf := SELF    
    oErr:FuncSym := symMethod
	Eval( ErrorBlock( ), oErr)
	RETURN NIL


ACCESS HelpContext AS STRING                           
	RETURN rHelpContext

ASSIGN HelpContext(u  AS STRING) 
	SELF:rHelpContext :=u
	RETURN 


CONSTRUCTOR( uName AS SYMBOL, uCaption := "" AS STRING, uDescription := "" AS STRING, uHelpContext := ""  AS STRING)  
	symName := uName
	IF String.IsNullOrEmpty(uCaption)
		rCaption := Symbol2String( symName )
	ELSE 
		rCaption := uCaption
	ENDIF
	rDescription := uDescription
    rHelpContext := uHelpContext
	RETURN 


ACCESS Name  AS STRING
	RETURN Symbol2String( symName )

ACCESS NameSym AS SYMBOL                               	
	RETURN symName

ASSIGN NameSym  (x AS SYMBOL)                 			
	SELF:SymName := x
	RETURN SELF:SymName

END CLASS

PARTIAL CLASS HLError   INHERIT Error

CONSTRUCTOR( oOriginator, symMethod, wErrorType, oHLErrorMessage, uMisc1, uMisc2 )  
    //RvdH 080609 Added call to super:Init to correctly fill the callstack
    SUPER()
	SELF:SubSystem := "HyperLabel"
	IF oOriginator# NIL
		SELF:MethodSelf := oOriginator
	ENDIF
	IF symMethod# NIL
		SELF:FuncSym := symMethod
		SELF:CallFuncSym := symMethod
	ENDIF
	IF wErrorType# NIL
		SELF:GenCode := wErrorType
	ELSE
		SELF:GenCode := EG_NOTABLE
	ENDIF
	IF oHLErrorMessage# NIL
        IF IsObject( oHLErrorMessage) .and. __Usual.ToObject(oHLErrorMessage) IS HyperLabel 

			SELF:Description := ((HyperLabel)oHLErrorMessage):Description
		ELSE
			SELF:Description := oHLErrorMessage
		ENDIF
	ENDIF
	IF GenCode = EG_ARG
		SELF:Args := { uMisc1 }
		SELF:Arg := uMisc2
	ENDIF
	SELF:Severity := ES_ERROR
	RETURN 
END CLASS

