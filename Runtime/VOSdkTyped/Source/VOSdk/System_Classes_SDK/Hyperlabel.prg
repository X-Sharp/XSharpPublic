CLASS HyperLabel
	PROPERTY NameSym AS SYMBOL AUTO         // A programmer's identifier
	PROPERTY Caption AS STRING AUTO         // A human-readable identifier, used for things such as field labels on forms
	PROPERTY Description AS STRING  AUTO    // A human-readable description, used for things such as status bar prompts
	PROPERTY HelpContext AS STRING	AUTO    // A token used as an identifier for the context-sensitive help

METHOD AsString( )  AS STRING STRICT                            
	RETURN SELF:Caption

METHOD Error( oError AS Error) AS VOID
   SELF:Error(oError, #Unknown)
   
METHOD Error( oError AS Error, symMethod AS SYMBOL ) AS VOID
    LOCAL oErr AS Error
    oErr := oError
	oErr:MethodSelf := SELF    
    oErr:FuncSym := symMethod
	Eval( ErrorBlock( ), oErr)
	RETURN 



CONSTRUCTOR( uName AS SYMBOL, uCaption := "" AS STRING, uDescription := "" AS STRING, uHelpContext := ""  AS STRING)  
	SELF:NameSym := uName
	IF String.IsNullOrEmpty(uCaption)
		SELF:Caption := Symbol2String( NameSym )
	ELSE 
		SELF:Caption := uCaption
	ENDIF
	SELF:Description := uDescription
    SELF:HelpContext := uHelpContext
	RETURN 


PROPERTY Name  AS STRING GET Symbol2String( SELF:NameSym)


END CLASS

