CLASS HyperLabel
	PROTECT symName AS SYMBOL       // A programmer's identifier
	PROTECT rCaption AS STRING      // A human-readable identifier, used for things such as field labels on forms
	PROTECT rDescription AS STRING  // A human-readable description, used for things such as status bar prompts
	PROTECT rHelpContext AS STRING	// A token used as an identifier for the context-sensitive help

METHOD AsString( )                              
	RETURN SELF:Caption

ACCESS Caption                                	
	RETURN rCaption

ASSIGN Caption(u)     
	SELF:rCaption := u
	RETURN 

ACCESS Description                                
	RETURN rDescription

ASSIGN Description(u) 
	SELF:rDescription  := u
	RETURN 

METHOD Error( oError, symMethod )                 
	LOCAL cErrorValType AS STRING
    LOCAL oErr AS Error
    IF IsObject(oError) .and. __Usual.ToObject(oError) IS Error 
        oErr := oError
    ELSE
		cErrorValType   := ValType(oError)
		oErr:= Error{}
		oErr:GenCode := EG_ERRORBUILD
		oErr:Description := VO_Sprintf(__CAVOSTR_SYSCLASS_BADERROROBJECT, cErrorValType)
	ENDIF
	oErr:MethodSelf := SELF    
	IF IsSymbol(symMethod)
		oErr:FuncSym := symMethod
	ELSE
		oErr:FuncSym := #Unknown
	ENDIF
	//oError:choice := EC_BREAK
	Eval( ErrorBlock( ), oErr)
	RETURN NIL


ACCESS HelpContext                            
	RETURN rHelpContext

ASSIGN HelpContext(u) 
	SELF:rHelpContext :=u
	RETURN 

CONSTRUCTOR( uName, uCaption, uDescription, uHelpContext )  

	IF IsSymbol( uName )
		symName := uName
	ELSEIF IsString( uName )
		symName := String2Symbol( uName )
	ELSE
		SELF:Error(HLError{ SELF, #Init, EG_ARG,__CavoStr(__CAVOSTR_SYSCLASS_BADNAME), uName, "uName" }, #Init) 
	ENDIF

	IF IsNil(uCaption)
		rCaption := Symbol2String( symName )
	ELSEIF IsString( uCaption )
		rCaption := uCaption
	ELSEIF IsSymbol( uCaption )
		rCaption := Symbol2String( uCaption )
	ELSEIF IsInstanceOfUsual( uCaption, #ResourceString )       // cannot change to IS because ResourceString is in GUI
		rCaption := uCaption:AsString()
	ELSE
		SELF:Error(HLError{ SELF, #Init, EG_ARG,__CavoStr(__CAVOSTR_SYSCLASS_BADCAPTION), uCaption, "uCaption" }, #Init) 
	ENDIF

	IF IsNil(uDescription)                    
		rDescription := " "                   
	ELSEIF IsString( uDescription )           
		rDescription := uDescription
	ELSEIF IsSymbol( uDescription )
		rDescription := Symbol2String( uDescription )
	ELSEIF IsInstanceOfUsual( uDescription, #ResourceString ) // cannot change to IS because ResourceString is in GUI
		rDescription := uDescription:AsString()
	ELSEIF !IsNil( uDescription )
		SELF:Error(HLError{ SELF, #Init, EG_ARG,__CavoStr(__CAVOSTR_SYSCLASS_BADDESCRIPTION), uDescription, "uDescription" }, #Init) 
	ENDIF

	IF IsString( uHelpContext )
		rHelpContext := uHelpContext
	ELSEIF IsSymbol( uHelpContext )
		rHelpContext := Symbol2String( uHelpContext )
	ELSEIF IsInstanceOfUsual( uHelpContext, #ResourceString ) // cannot change to IS because ResourceString is in GUI
		rHelpContext := uHelpContext:AsString()
	ELSEIF !IsNil( uHelpContext )
		SELF:Error(HLError{ SELF, #Init, EG_ARG,__CavoStr(__CAVOSTR_SYSCLASS_BADHELPCONTEXT), uHelpContext, "uHelpContext" }, #Init) 
	ENDIF

	RETURN 


ACCESS Name                                   	
	RETURN Symbol2String( symName )

ACCESS NameSym                                	
	RETURN symName

ASSIGN NameSym  (x)                 			
	IF IsSymbol(x)
		SELF:SymName := x
	ENDIF
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
        IF IsObject( oHLErrorMessage) .and. __USual.ToObject(oHLErrorMessage) IS HyperLabel 

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

