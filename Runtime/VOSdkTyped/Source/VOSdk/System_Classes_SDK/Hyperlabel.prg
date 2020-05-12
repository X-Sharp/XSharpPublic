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
	RETURN 


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
	SELF:symName := x
	RETURN 

END CLASS

