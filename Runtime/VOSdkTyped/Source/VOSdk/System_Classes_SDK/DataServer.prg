
ABSTRACT CLASS DataServer
    PROTECT oHyperLabel 	AS HyperLabel
    PROTECT oHLStatus		AS HyperLabel
    PROTECT wFieldCount 	AS DWORD
	PROTECT aDataFields	    AS ARRAY
    PROTECT aClients		AS ARRAY
    PROTECT nClients		AS DWORD
    PROTECT nCCMode		    AS DWORD
    PROTECT nLastLock		AS LONG

CONSTRUCTOR( ) 
    SELF:aClients := { }
    SELF:aDataFields := {}
    RETURN
    
METHOD __ClearLocks( ) AS VOID STRICT 
    SWITCH nCCMode
    CASE ccStable
        SELF:UnLock( nLastLock )
    CASE ccRepeatable
        SELF:UnLock( )
    CASE ccFile
        SELF:UnLock( )
    END SWITCH

    RETURN

METHOD __SetupLocks( ) AS VOID STRICT 

    nLastLock := 0

    SWITCH nCCMode
    CASE ccNone 
    CASE ccOptimistic
        //nothing to do
        NOP
    CASE ccStable 
    CASE ccRepeatable
        nLastLock := SELF:RecNo
        // Do not free locks in case user has other locks
        IF ! SELF:RLock( nLastLock )
            nLastLock := 0
            oHLStatus := SELF:Status
        ENDIF
    CASE ccFile
        IF ! SELF:FLock( )
            oHLStatus := SELF:Status
        ENDIF
    OTHERWISE
        BREAK DbError{ SELF, #ConcurrencyControl, EG_ARG, ;
            __CavoStr(__CAVOSTR_DBFCLASS_BADCONCURRENCYASSIGN), nCCMode, "nCCMode" }
    END SWITCH

    RETURN

METHOD Append( )  AS LOGIC CLIPPER
    RETURN FALSE

METHOD AsString( ) AS STRING STRICT
    RETURN oHyperLabel:AsString( )

ACCESS BoF AS LOGIC
    RETURN FALSE

ACCESS Clients  AS ARRAY
   RETURN SELF:aClients

ASSIGN Clients(aNewClients AS ARRAY) 
    SELF:aClients := aNewClients
    SELF:nClients := ALen(SELF:aClients)
RETURN 

METHOD Close( ) AS LOGIC CLIPPER
    RETURN FALSE

METHOD Commit( ) AS LOGIC CLIPPER
    RETURN FALSE

ACCESS ConcurrencyControl( )  AS USUAL
    RETURN SELF:nCCMode

ASSIGN ConcurrencyControl( nMode  AS USUAL) 
    LOCAL newMode := nMode

    IF UsualType( newMode ) == STRING
        newMode := String2Symbol( nMode )
    ENDIF

    IF UsualType( newMode ) == SYMBOL
        SWITCH newMode:ToString()
        CASE "CCNONE"
            newMode := ccNone
        CASE "CCOPTIMISTIC"
            newMode := ccOptimistic
        CASE "CCSTABLE"
            newMode := ccStable
        CASE "CCREPEATABLE"
            newMode := ccRepeatable
        CASE "CCFILE"
            newMode := ccFile
        OTHERWISE 
            BREAK DbError{ SELF, #ConcurrencyControl, EG_ARG, ;
                __CavoStr( __CAVOSTR_DBFCLASS_BADCONCURRENCYASSIGN ), nMode, "nMode" }
        END SWITCH
    ENDIF

    IF IsNumeric( newMode ) .AND. ( newMode != SELF:nCCMode )
        SELF:__ClearLocks( )
        SELF:nCCMode := newMode
        SELF:__SetupLocks( )
    ENDIF

    RETURN 

METHOD DataField( nFieldPosition  AS USUAL) AS DataField
	LOCAL nPos := SELF:FieldPos(nFieldPosition) AS DWORD
	IF nPos > 0
		RETURN aDataFields[ nPos ]
	ENDIF
	RETURN NULL_OBJECT

ACCESS DBStruct AS ARRAY
    LOCAL aStruct	AS ARRAY
    LOCAL oDF		AS DataField
    LOCAL w			AS DWORD

    aStruct := ArrayNew( wFieldCount )
    FOR w := 1 UPTO wFieldCount
        oDF := aDataFields[ w ]
	    VAR oFS := oDF:FieldSpec
        aStruct[ w ] := { oDF:Name, oFS:UsualType, oFS:Length, oFS:Decimals }
    NEXT

    RETURN aStruct

	METHOD Delete( ) AS LOGIC CLIPPER
    RETURN FALSE

ACCESS EoF AS LOGIC
    RETURN FALSE

ACCESS FCount AS DWORD
    RETURN wFieldCount

METHOD FieldGet( nFieldPosition AS USUAL)  AS USUAL
    RETURN NIL

METHOD FieldGetFormatted( nFieldPosition  AS USUAL)  AS USUAL
    RETURN NIL

METHOD FieldHyperLabel( nFieldPosition  AS USUAL) AS HyperLabel
	LOCAL nPos := SELF:FieldPos(nFieldPosition) AS DWORD
	IF nPos > 0
		RETURN SELF:DataField(nPos):HyperLabel
	ENDIF
	RETURN NULL_OBJECT

METHOD FieldName( nFieldPosition AS USUAL )  AS STRING
	LOCAL nPos := SELF:FieldPos(nFieldPosition) AS DWORD
	IF nPos > 0
		RETURN SELF:DataField(nPos):HyperLabel:Name
	ENDIF
	RETURN NULL_STRING

	METHOD FieldPos( nFieldPosition AS USUAL) AS DWORD
		RETURN 0

METHOD FieldPut( nFieldPosition AS USUAL, uValue  AS USUAL) AS USUAL
    RETURN NIL

	METHOD FieldSpec( nFieldPosition AS USUAL)  AS FieldSpec
		LOCAL nPos := SELF:FieldPos(nFieldPosition) AS DWORD
		IF nPos > 0
			RETURN SELF:DataField(nPos):FieldSpec
		ENDIF
		RETURN NULL_OBJECT

	METHOD FieldStatus( nFieldPosition AS USUAL) AS HyperLabel
		LOCAL nPos := SELF:FieldPos(nFieldPosition) AS DWORD
		IF nPos > 0
			RETURN SELF:DataField(nPos):FieldSpec:Status
		ENDIF
		RETURN NULL_OBJECT

	METHOD FieldSym( nFieldPosition AS USUAL )  AS SYMBOL
		LOCAL nPos := SELF:FieldPos(nFieldPosition) AS DWORD
		IF nPos > 0
			RETURN SELF:DataField(nPos):HyperLabel:NameSym
		ENDIF
		RETURN NULL_SYMBOL

	METHOD FieldValidate( nFieldPosition AS USUAL, uValue AS USUAL) AS LOGIC
		LOCAL nPos := SELF:FieldPos(nFieldPosition) AS DWORD
		IF nPos > 0
			RETURN SELF:DataField(nPos):FieldSpec:Validate(uValue)
		ENDIF
		RETURN TRUE

	METHOD FLock( ) AS LOGIC STRICT
		RETURN FALSE

	METHOD GoBottom( ) AS LOGIC STRICT
		RETURN FALSE

	METHOD GoTo( nPosition ) AS LOGIC CLIPPER
		RETURN FALSE

	METHOD GoTop( ) AS LOGIC STRICT
		RETURN FALSE

	ACCESS HLStatus AS HyperLabel
		// This always returns the status, regardless of the error flag	
		RETURN SELF:oHLStatus	

	ACCESS HyperLabel AS HyperLabel
		RETURN oHyperLabel

	ASSIGN HyperLabel( oHL AS HyperLabel) 
		oHyperLabel := oHL
		RETURN 

	ACCESS LastRec AS LONG
		RETURN 0

	ACCESS Name AS STRING
		IF SELF:oHyperLabel != NULL_OBJECT
			RETURN oHyperLabel:Name
		ENDIF
		RETURN NULL_STRING

	ACCESS NameSym as symbol
        IF oHyperLabel != NULL_OBJECT
            RETURN oHyperLabel:NameSym
        ENDIF
        RETURN NULL_STRING

METHOD NoIVarGet( symFieldName ) 
    RETURN SELF:FieldGet( symFieldName )

METHOD NoIVarPut( symFieldName, uValue ) 

    SELF:FieldPut( symFieldName, uValue )
    SELF:Notify( NOTIFYFIELDCHANGE, symFieldName )
    RETURN uValue

METHOD Notify( kNotification, uDescription ) 
    FOREACH oClient AS OBJECT IN AClone(aClients)
        Send(oClient, #Notify, kNotification, uDescription )
    NEXT
    RETURN SELF

	METHOD PostInit( ) CLIPPER
		RETURN SELF

	METHOD PreInit( ) CLIPPER
		RETURN SELF

	ACCESS RecCount AS LONG
		RETURN 0

	ACCESS RecNo AS LONG
		RETURN 0L

	ASSIGN RecNo( lRecNo AS LONG) 
		RETURN 

METHOD RegisterClient( oForm ) AS LOGIC CLIPPER

    IF AScan( aClients, oForm ) # 0
        RETURN FALSE
    ENDIF
    AAdd( aClients, oForm )
    nClients := ALen( aClients )

    RETURN TRUE

	METHOD ResetNotification( ) AS LONG STRICT
    RETURN 0

	METHOD RLock( nRecord ) AS LOGIC CLIPPER
    RETURN FALSE

	METHOD RLockVerify( ) AS LOGIC STRICT
		RETURN FALSE

	METHOD Rollback( ) AS LOGIC STRICT
		RETURN FALSE

	METHOD Seek( uValue ) AS LOGIC CLIPPER
		RETURN FALSE

	METHOD SetDataField( nFieldPosition AS DWORD, oDataField AS DataField ) AS LOGIC

    IF aDataFields == NULL
        BREAK DbError{ SELF, #SetDataField, EG_SEQUENCE, ;
            __CavoStr( __CAVOSTR_DBFCLASS_NODATAFIELDSEXIST ) }
    ELSEIF nFieldPosition < 1 .OR. nFieldPosition > ALen(aDataFields)
        BREAK DbError{ SELF, #SetDataField, EG_ARG, ;
            __CavoStr(__CAVOSTR_DBFCLASS_BADFIELDPOSITION), nFieldPosition, "nFieldPosition" }
    ELSE
        LOCAL oExisting := aDataFields[ nFieldPosition ] AS DataField
        IF oDataField:Name == oExisting:Name .AND.    ;
            oDataField:FieldSpec:UsualType == oExisting:FieldSpec:UsualType .AND.     ;
            oDataField:FieldSpec:Length    == oExisting:FieldSpec:Length .AND.   ;
            oDataField:FieldSpec:Decimals  == oExisting:FieldSpec:Decimals
            aDataFields[ nFieldPosition ] := oDataField
            RETURN TRUE
        ELSE
            RETURN FALSE
        ENDIF
    ENDIF

	METHOD Skip( nRelativePosition ) AS LOGIC CLIPPER
		RETURN FALSE

	ACCESS Status AS HyperLabel
		// Usual because there may be a field Status as well
		RETURN oHLStatus

	ASSIGN Status(oHl AS HyperLabel) 
		// Usual because there may be a field Status as well
		IF IsInstanceOfUsual(oHl, #HyperLabel)
			SELF:oHLStatus := oHl
		ENDIF
		RETURN 
    
	METHOD SuspendNotification( ) AS LONG STRICT
		RETURN 0

	METHOD UnLock(nRecno) AS LOGIC CLIPPER 
		RETURN FALSE

	METHOD UnRegisterClient( oClient , lAllowClose )  AS LOGIC CLIPPER
    LOCAL w AS DWORD

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

	METHOD Update( ) AS LOGIC CLIPPER
    RETURN FALSE
END CLASS

