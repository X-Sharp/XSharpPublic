//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


/// <include file="System.xml" path="doc/HyperLabel/*" />
[XSharp.Internal.TypesChanged];
CLASS HyperLabel
/// <include file="System.xml" path="doc/HyperLabel.NameSym/*" />
	PROPERTY NameSym AS SYMBOL AUTO         // A programmer's identifier
/// <include file="System.xml" path="doc/HyperLabel.Caption/*" />
	PROPERTY Caption AS STRING AUTO         // A human-readable identifier, used for things such as field labels on forms
/// <include file="System.xml" path="doc/HyperLabel.Description/*" />
	PROPERTY Description AS STRING  AUTO    // A human-readable description, used for things such as status bar prompts
/// <include file="System.xml" path="doc/HyperLabel.HelpContext/*" />
	PROPERTY HelpContext AS STRING	AUTO    // A token used as an identifier for the context-sensitive help


/// <include file="System.xml" path="doc/HyperLabel.AsString/*" />
METHOD AsString( )  AS STRING STRICT                            
	RETURN SELF:Caption


/// <include file="System.xml" path="doc/HyperLabel.Error/*" />
METHOD Error( oError AS Error) AS VOID
   SELF:Error(oError, #Unknown)
   
   
/// <include file="System.xml" path="doc/HyperLabel.Error/*" />
METHOD Error( oError AS Error, symMethod AS SYMBOL ) AS VOID
    LOCAL oErr AS Error
    oErr := oError
	oErr:MethodSelf := SELF    
    oErr:FuncSym := symMethod
	Eval( ErrorBlock( ), oErr)
	RETURN 


/// <include file="System.xml" path="doc/HyperLabel.ctor/*" />
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




/// <include file="System.xml" path="doc/HyperLabel.Name/*" />
PROPERTY Name  AS STRING GET Symbol2String( SELF:NameSym)




END CLASS


