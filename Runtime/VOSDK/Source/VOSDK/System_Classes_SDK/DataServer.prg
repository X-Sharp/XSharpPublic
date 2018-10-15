PARTIAL CLASS DataServer
   PROTECT oHyperLabel AS HyperLabel
   PROTECT oHLStatus		AS USUAL
	PROTECT wFieldCount	AS DWORD
	PROTECT aDataFields	AS ARRAY
	PROTECT aClients		AS ARRAY
	PROTECT nClients		AS DWORD
	PROTECT nCCMode		AS DWORD
	PROTECT nLastLock		AS DWORD
	//RvdH-030916 Strong typing
	METHOD __ClearLocks( ) AS VOID STRICT 
	//RvdH-030916 Strong typing

	DO CASE
	CASE nCCMode == ccStable
		SELF:Unlock( nLastLock )
	CASE nCCMode == ccRepeatable
		SELF:Unlock( )
	CASE nCCMode == ccFile
		SELF:Unlock( )
	ENDCASE

	RETURN

ACCESS __Clients AS ARRAY STRICT 
	//RvdH-030916 Strong typing
	RETURN SELF:aClients

METHOD __SetupLocks( ) AS VOID STRICT 
	//RvdH-030916 Strong typing

	nLastLock := 0

	DO CASE
	CASE nCCMode == ccNone .OR.  nCCMode == ccOptimistic
		//nothing to do
		NOP
	CASE nCCMode == ccStable .OR. nCCMode == ccRepeatable
		nLastLock := SELF:Recno
		// Do not free locks in case user has other locks
		IF ! SELF:RLOCK( nLastLock )
			nLastLock := 0
			oHLStatus := SELF:Status
		ENDIF
	CASE nCCMode == ccFile
		IF ! SELF:FLOCK( )
			oHLStatus := SELF:Status
		ENDIF
	OTHERWISE
		BREAK DbError{ SELF, #ConcurrencyControl, EG_ARG, ;
			__CavoStr(__CAVOSTR_DBFCLASS_BADCONCURRENCYASSIGN), nCCMode, "nCCMode" }
	ENDCASE

	RETURN

METHOD Append( ) 
	RETURN FALSE

METHOD AsString( ) 
	RETURN oHyperLabel:AsString( )

ACCESS BoF 
	RETURN FALSE

ACCESS Clients 
	// DHer: 18/12/2008
   RETURN SELF:aClients

ASSIGN Clients(aNewClients) 
	// DHer: 18/12/2008
	IF IsArray(aNewClients)
		SELF:aClients := aNewClients
	ENDIF
	SELF:nClients := ALen(SELF:aClients)
RETURN 

METHOD Close( ) 
	RETURN FALSE

METHOD Commit( ) 
	RETURN FALSE

ACCESS ConcurrencyControl( ) 
	RETURN SELF:nCCMode

ASSIGN ConcurrencyControl( nMode) 
	LOCAL newMode := nMode

	IF UsualType( newMode ) == STRING
		newMode := String2Symbol( nMode )
	ENDIF

	IF UsualType( newMode ) == SYMBOL
		DO CASE
		CASE newMode == #ccNone
			newMode := ccNone
		CASE newMode == #ccOptimistic
			newMode := ccOptimistic
		CASE newMode == #ccStable
			newMode := ccStable
		CASE newMode == #ccRepeatable
			newMode := ccRepeatable
		CASE newMode == #ccFile
			newMode := ccFile
		OTHERWISE
			BREAK DbError{ SELF, #ConcurrencyControl, EG_ARG, ;
				__CavoStr( __CAVOSTR_DBFCLASS_BADCONCURRENCYASSIGN ), nMode, "nMode" }
		ENDCASE
	ENDIF

	IF IsNumeric( newMode ) .AND. ( newMode != SELF:nCCMode )
		SELF:__ClearLocks( )
		SELF:nCCMode := newMode
		SELF:__SetupLocks( )
	ENDIF

	RETURN SELF:nCCMode

METHOD DataField( nFieldPosition ) 
	RETURN aDataFields[ nFieldPosition ]

ACCESS DBStruct 
	LOCAL aStruct	AS ARRAY
	LOCAL oDF		AS DataField
	LOCAL w			AS DWORD

	aStruct := ArrayNew( wFieldCount )
	FOR w := 1 UPTO wFieldCount
		oDF := aDataFields[ w ]
		aStruct[ w ] := { oDF:Name, oDF:FieldSpec:UsualType, oDF:FieldSpec:Length, oDF:FieldSpec:Decimals }
	NEXT

	RETURN aStruct

METHOD Delete( ) 
	RETURN FALSE

ACCESS EoF 
	RETURN FALSE

ACCESS FCount 
	RETURN wFieldCount

METHOD FIELDGET( nFieldPosition ) 
	RETURN NIL

METHOD FieldGetFormatted( nFieldPosition ) 
	RETURN NIL

METHOD FieldHyperLabel( nFieldPosition ) 
	RETURN aDataFields[ nFieldPosition ]:HyperLabel

METHOD FieldName( nFieldPosition ) 
	RETURN aDataFields[ nFieldPosition ]:HyperLabel:Name

METHOD FieldPos( nFieldPosition ) 
	RETURN NIL

METHOD FIELDPUT( nFieldPosition, uValue ) 
	RETURN NIL

METHOD FieldSpec( nFieldPosition ) 
	RETURN aDataFields[ nFieldPosition ]:FieldSpec

METHOD FieldStatus( nFieldPosition ) 
	RETURN aDataFields[ nFieldPosition ]:FieldSpec:Status

METHOD FieldSym( nFieldPosition ) 
	RETURN aDataFields[ nFieldPosition ]:HyperLabel:NameSym

METHOD FieldValidate( nFieldPosition, uValue ) 
	RETURN aDataFields[ nFieldPosition ]:FieldSpec:PerformValidations( uValue )

METHOD FLOCK( ) 
	RETURN FALSE

METHOD GoBottom( ) 
	RETURN FALSE

METHOD GoTo( nPosition ) 
	RETURN FALSE

METHOD GoTop( ) 
	RETURN FALSE

ACCESS HLStatus
// This always returns the status, regardless of the error flag	
RETURN SELF:oHLStatus	

ACCESS HyperLabel 
	RETURN oHyperLabel

ASSIGN HyperLabel( oHL ) 
	oHyperLabel := oHL
	RETURN 

CONSTRUCTOR( ) 
	SELF:aClients := { }
	RETURN 

ACCESS Name 

	IF oHyperlabel != NULL_OBJECT
		RETURN oHyperLabel:Name
	ENDIF
	RETURN NULL_STRING

ACCESS NameSym 

	IF oHyperlabel != NULL_OBJECT
		RETURN oHyperLabel:NameSym
	ENDIF
	RETURN NULL_STRING

METHOD NoIVarGet( symFieldName ) 
	RETURN SELF:FIELDGET( symFieldName )

METHOD NoIVarPut( symFieldName, uValue ) 

	SELF:FIELDPUT( symFieldName, uValue )
	SELF:Notify( NOTIFYFIELDCHANGE, symFieldName )
	RETURN uValue

METHOD Notify( kNotification, uDescription ) 
	//RvdH 070306 unsupported syntax under Vulcan.NET
	//aClients:Notify( kNotification, uDescription )
	ASend(aClients,#Notify, kNotification, uDescription )
	RETURN SELF

METHOD PostInit( ) 
	RETURN SELF

METHOD PreInit( ) 
	RETURN SELF

ACCESS RecCount 
	RETURN 0

ACCESS RecNo 
	RETURN NIL

ASSIGN RecNo( lRecNo ) 
	RETURN 

METHOD RegisterClient( oForm ) 

	IF AScan( aClients, oForm ) # 0
		RETURN FALSE
	ENDIF
	AAdd( aClients, oForm )
	nClients := ALen( aClients )

	RETURN TRUE

METHOD ResetNotification( ) 
	RETURN 0

METHOD RLOCK( nRecord ) 
	RETURN FALSE

METHOD RLockVerify( ) 
	RETURN FALSE

METHOD Rollback( ) 
	RETURN FALSE

METHOD Seek( uValue ) 
	RETURN FALSE

METHOD SetDataField( nFieldPosition, oDataField ) 
	LOCAL wFieldPosition := nFieldPosition	AS WORD
	LOCAL oField := oDataField				AS DataField
	LOCAL oDF								AS DataField

	IF aDataFields = NULL_ARRAY
		BREAK DbError{ SELF, #SetDataField, EG_SEQUENCE, ;
			__CavoStr( __CAVOSTR_DBFCLASS_NODATAFIELDSEXIST ) }
	ELSEIF IsNil( nFieldPosition) .OR. ! IsNumeric( nFieldPosition ) .OR.   ;
		wFieldPosition < 1 .OR. wFieldPosition > ALen( aDataFields )
		BREAK DbError{ SELF, #SetDataField, EG_ARG, ;
			__CavoStr(__CAVOSTR_DBFCLASS_BADFIELDPOSITION), nFieldPosition, "nFieldPosition" }
	ELSEIF /*IsNil( oDataField ) .OR. */! IsInstanceOfUsual( oDataField, #DataField )
		BREAK DbError{ SELF, #SetDataField, EG_ARG,   ;
			__CavoStr(__CAVOSTR_DBFCLASS_BADFIELDPOSITION), nFieldPosition, "nFieldPosition" }
	ELSE
		oDF := aDataFields[ wFieldPosition ]
		IF oField:Name == oDF:Name .AND.    ;
			oField:FieldSpec:UsualType == oDF:FieldSpec:UsualType .AND.     ;
			oField:FieldSpec:Length == oDF:FieldSpec:Length .AND.   ;
			oField:FieldSpec:Decimals == oDF:FieldSpec:Decimals
			aDataFields[ wFieldPosition ] := oField
			RETURN TRUE
		ELSE
			RETURN FALSE
		ENDIF
	ENDIF

METHOD Skip( nRelativePosition ) 
	RETURN FALSE

ACCESS Status 
	RETURN oHLStatus

ASSIGN Status(oHl) 
	// DHer: 18/12/2008
	SELF:oHLStatus := oHl
RETURN SELF:oHLStatus

METHOD SuspendNotification( ) 
	RETURN 0

METHOD UnLock( ) CLIPPER 
	RETURN FALSE

METHOD UnRegisterClient( oClient, lAllowClose ) 
	LOCAL w AS WORD

	IF ( w := AScan( aClients, oClient ) ) = 0
		RETURN FALSE
	ELSE
		ADel( aClients, w )
		nClients := ALen( aClients ) -1
		ASize( aClients, nClients )
		IF ( IsNil( lAllowClose ) .OR. lAllowClose) .AND. nClients = 0
			SELF:Close( )
		ENDIF
		RETURN TRUE
	ENDIF

METHOD Update( ) 
	RETURN FALSE
END CLASS

