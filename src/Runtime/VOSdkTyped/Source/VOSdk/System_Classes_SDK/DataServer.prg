//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


[XSharp.Internal.TypesChanged];
ABSTRACT CLASS DataServer
    PROTECT oHyperLabel 	AS HyperLabel
    PROTECT oHLStatus		AS HyperLabel
    PROTECT wFieldCount 	AS DWORD
	PROTECT aDataFields	    AS ARRAY
    PROTECT aClients		AS ARRAY
    PROTECT nClients		AS DWORD
    PROTECT nCCMode		    AS DWORD
    PROTECT nLastLock		AS LONG


/// <include file="System.xml" path="doc/DataServer.ctor/*" />
CONSTRUCTOR( )
    SELF:aClients := { }
    SELF:aDataFields := {}
    RETURN


 /// <exclude />
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


 /// <exclude />
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


/// <include file="System.xml" path="doc/DataServer.Append/*" />
METHOD Append( lReleaseLocks AS LOGIC)  AS LOGIC
    RETURN FALSE


/// <include file="System.xml" path="doc/DataServer.AsString/*" />
METHOD AsString( ) AS STRING STRICT
    RETURN oHyperLabel:AsString( )


/// <include file="System.xml" path="doc/DataServer.BoF/*" />
ACCESS BoF AS LOGIC
    RETURN FALSE


/// <include file="System.xml" path="doc/DataServer.Clients/*" />
ACCESS Clients  AS ARRAY
   RETURN SELF:aClients


/// <include file="System.xml" path="doc/DataServer.Clients/*" />
ASSIGN Clients(aNewClients AS ARRAY)
    SELF:aClients := aNewClients
    SELF:nClients := ALen(SELF:aClients)
RETURN


/// <include file="System.xml" path="doc/DataServer.Close/*" />
METHOD Close( ) AS LOGIC STRICT
    RETURN FALSE


/// <include file="System.xml" path="doc/DataServer.Commit/*" />
METHOD Commit( ) AS LOGIC STRICT
    RETURN FALSE


/// <include file="System.xml" path="doc/DataServer.ConcurrencyControl/*" />
ACCESS ConcurrencyControl( )  AS USUAL
    RETURN SELF:nCCMode


/// <include file="System.xml" path="doc/DataServer.ConcurrencyControl/*" />
ASSIGN ConcurrencyControl( nMode  AS USUAL)
    LOCAL newMode := nMode AS USUAL


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


/// <include file="System.xml" path="doc/DataServer.DataField/*" />
METHOD DataField( nFieldPosition  AS USUAL) AS DataField
	LOCAL nPos := SELF:FieldPos(nFieldPosition) AS DWORD
	IF nPos > 0
		RETURN aDataFields[ nPos ]
	ENDIF
	RETURN NULL_OBJECT


/// <include file="System.xml" path="doc/DataServer.DBStruct/*" />
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


/// <include file="System.xml" path="doc/DataServer.Delete/*" />
METHOD Delete( ) AS LOGIC CLIPPER
    RETURN FALSE


/// <include file="System.xml" path="doc/DataServer.EoF/*" />
ACCESS EoF AS LOGIC
    RETURN FALSE


/// <include file="System.xml" path="doc/DataServer.FCount/*" />
ACCESS FCount AS DWORD
    RETURN wFieldCount


/// <include file="System.xml" path="doc/DataServer.FieldGet/*" />
METHOD FieldGet( nFieldPosition AS USUAL)  AS USUAL
    RETURN NIL


/// <include file="System.xml" path="doc/DataServer.FieldGetFormatted/*" />
METHOD FieldGetFormatted( nFieldPosition  AS USUAL)  AS USUAL
    RETURN NIL


/// <include file="System.xml" path="doc/DataServer.FieldHyperLabel/*" />
METHOD FieldHyperLabel( nFieldPosition  AS USUAL) AS HyperLabel
	LOCAL nPos := SELF:FieldPos(nFieldPosition) AS DWORD
	IF nPos > 0
		RETURN SELF:DataField(nPos):HyperLabel
	ENDIF
	RETURN NULL_OBJECT


/// <include file="System.xml" path="doc/DataServer.FieldName/*" />
METHOD FieldName( nFieldPosition AS USUAL )  AS STRING
	LOCAL nPos := SELF:FieldPos(nFieldPosition) AS DWORD
	IF nPos > 0
		RETURN SELF:DataField(nPos):HyperLabel:Name
	ENDIF
	RETURN NULL_STRING


/// <include file="System.xml" path="doc/DataServer.FieldPos/*" />
	METHOD FieldPos( nFieldPosition AS USUAL) AS DWORD
		RETURN 0


/// <include file="System.xml" path="doc/DataServer.FieldPut/*" />
METHOD FieldPut( nFieldPosition AS USUAL, uValue  AS USUAL) AS USUAL
    RETURN NIL


/// <include file="System.xml" path="doc/DataServer.FieldSpec/*" />
	METHOD FieldSpec( nFieldPosition AS USUAL)  AS FieldSpec
		LOCAL nPos := SELF:FieldPos(nFieldPosition) AS DWORD
		IF nPos > 0
			RETURN SELF:DataField(nPos):FieldSpec
		ENDIF
		RETURN NULL_OBJECT


/// <include file="System.xml" path="doc/DataServer.FieldStatus/*" />
	METHOD FieldStatus( nFieldPosition AS USUAL) AS HyperLabel
		LOCAL nPos := SELF:FieldPos(nFieldPosition) AS DWORD
		IF nPos > 0
			RETURN SELF:DataField(nPos):FieldSpec:Status
		ENDIF
		RETURN NULL_OBJECT


/// <include file="System.xml" path="doc/DataServer.FieldSym/*" />
	METHOD FieldSym( nFieldPosition AS USUAL )  AS SYMBOL
		LOCAL nPos := SELF:FieldPos(nFieldPosition) AS DWORD
		IF nPos > 0
			RETURN SELF:DataField(nPos):HyperLabel:NameSym
		ENDIF
		RETURN NULL_SYMBOL


/// <include file="System.xml" path="doc/DataServer.FieldValidate/*" />
	METHOD FieldValidate( nFieldPosition AS USUAL, uValue AS USUAL) AS LOGIC
		LOCAL nPos := SELF:FieldPos(nFieldPosition) AS DWORD
		IF nPos > 0
			RETURN SELF:DataField(nPos):FieldSpec:Validate(uValue)
		ENDIF
		RETURN TRUE


/// <include file="System.xml" path="doc/DataServer.FLOCK/*" />
	METHOD FLock( ) AS LOGIC STRICT
		RETURN FALSE


/// <include file="Rdd.xml" path="doc/DbServer.GetLookupTable/*" />
    abstract METHOD GetLookupTable( nMaxRows , uField1 , uField2 , uSearchValue )  AS ARRAY CLIPPER

/// <include file="System.xml" path="doc/DataServer.GoBottom/*" />
	METHOD GoBottom( ) AS LOGIC STRICT
		RETURN FALSE


/// <include file="System.xml" path="doc/DataServer.GoTo/*" />
	METHOD GoTo( nPosition AS LONG ) AS LOGIC
		RETURN FALSE


/// <include file="System.xml" path="doc/DataServer.GoTop/*" />
	METHOD GoTop( ) AS LOGIC STRICT
		RETURN FALSE


/// <include file="System.xml" path="doc/DataServer.HLStatus/*" />
	ACCESS HLStatus AS HyperLabel
		// This always returns the status, regardless of the error flag
		RETURN SELF:oHLStatus


/// <include file="System.xml" path="doc/DataServer.HyperLabel/*" />
	ACCESS HyperLabel AS HyperLabel
		RETURN oHyperLabel


/// <include file="System.xml" path="doc/DataServer.HyperLabel/*" />
	ASSIGN HyperLabel( oHL AS HyperLabel)
		oHyperLabel := oHL
		RETURN


/// <include file="System.xml" path="doc/DataServer.RecCount/*" />
	ACCESS LastRec AS LONG
		RETURN 0

    method LockCurrentRecord( ) as logic strict
        return self:RLock(self:RecNo)

/// <include file="System.xml" path="doc/DataServer.Name/*" />
	ACCESS Name AS STRING
		IF SELF:oHyperLabel != NULL_OBJECT
			RETURN oHyperLabel:Name
		ENDIF
		RETURN NULL_STRING


/// <include file="System.xml" path="doc/DataServer.NameSym/*" />
	ACCESS NameSym as symbol
        IF oHyperLabel != NULL_OBJECT
            RETURN oHyperLabel:NameSym
        ENDIF
        RETURN NULL_STRING


/// <include file="System.xml" path="doc/DataServer.NoIVarGet/*" />
METHOD NoIVarGet( symFieldName AS USUAL) AS USUAL
    RETURN SELF:FieldGet( symFieldName )


/// <include file="System.xml" path="doc/DataServer.NoIVarPut/*" />
METHOD NoIVarPut( symFieldName AS USUAL, uValue AS USUAL)  AS USUAL


    SELF:FieldPut( symFieldName, uValue )
    SELF:Notify( NOTIFYFIELDCHANGE, symFieldName )
    RETURN uValue


/// <include file="System.xml" path="doc/DataServer.Notify/*" />
METHOD Notify( kNotification AS LONG, uDescription := NIL AS USUAL) AS USUAL
    FOREACH oClient AS OBJECT IN AClone(aClients)
        Send(oClient, #Notify, kNotification, uDescription )
    NEXT
    RETURN SELF


/// <include file="System.xml" path="doc/DataServer.PostInit/*" />
	METHOD PostInit( ) AS USUAL CLIPPER
		RETURN SELF


/// <include file="System.xml" path="doc/DataServer.PreInit/*" />
	METHOD PreInit( ) AS USUAL CLIPPER
		RETURN SELF


/// <include file="System.xml" path="doc/DataServer.RecCount/*" />
	ACCESS RecCount AS LONG
		RETURN 0


/// <include file="System.xml" path="doc/DataServer.RecNo/*" />
	ACCESS RecNo AS LONG
		RETURN 0L


/// <include file="System.xml" path="doc/DataServer.RecNo/*" />
	ASSIGN RecNo( lRecNo AS LONG)
		RETURN


/// <include file="System.xml" path="doc/DataServer.RegisterClient/*" />
METHOD RegisterClient( oForm AS OBJECT) AS LOGIC
    IF AScan( aClients, oForm ) # 0
        RETURN FALSE
    ENDIF
    AAdd( aClients, oForm )
    nClients := ALen( aClients )


    RETURN TRUE


/// <include file="System.xml" path="doc/DataServer.ResetNotification/*" />
	METHOD ResetNotification( ) AS LONG STRICT
    RETURN 0


/// <include file="System.xml" path="doc/DataServer.RLOCK/*" />
	METHOD RLock( nRecord AS LONG ) AS LOGIC
        RETURN FALSE


/// <include file="System.xml" path="doc/DataServer.RLockVerify/*" />
	METHOD RLockVerify( ) AS LOGIC STRICT
		RETURN FALSE


/// <include file="System.xml" path="doc/DataServer.Rollback/*" />
	METHOD Rollback( ) AS LOGIC STRICT
		RETURN FALSE


/// <include file="System.xml" path="doc/DataServer.Seek/*" />
	METHOD Seek( ) AS LOGIC CLIPPER
		RETURN FALSE


/// <include file="System.xml" path="doc/DataServer.SetDataField/*" />
	METHOD SetDataField( nFieldPosition AS DWORD, oDataField AS DataField ) AS LOGIC


    IF aDataFields == NULL
        BREAK DbError{ SELF, #SetDataField, EG_SEQUENCE, ;
            __CavoStr( __CAVOSTR_DBFCLASS_NODATAFIELDSEXIST ) }


    ELSEIF nFieldPosition < 1 .OR. nFieldPosition > ALen(aDataFields)
        BREAK DbError{ SELF, #SetDataField, EG_ARG, ;
            __CavoStr(__CAVOSTR_DBFCLASS_BADFIELDPOSITION), nFieldPosition, "nFieldPosition" }
    ELSE
        LOCAL oExisting := aDataFields[ nFieldPosition ] AS DataField
        IF oExisting == NULL
            aDataFields[ nFieldPosition ] := oDataField
            RETURN TRUE
        ELSEIF oDataField:Name == oExisting:Name .AND.    ;
            oDataField:FieldSpec:UsualType == oExisting:FieldSpec:UsualType .AND.     ;
            oDataField:FieldSpec:Length    == oExisting:FieldSpec:Length .AND.   ;
            oDataField:FieldSpec:Decimals  == oExisting:FieldSpec:Decimals
            aDataFields[ nFieldPosition ] := oDataField
            RETURN TRUE
        ELSE
            RETURN FALSE
        ENDIF
    ENDIF


/// <include file="System.xml" path="doc/DataServer.Skip/*" />
	METHOD Skip( nRelativePosition := 1 AS LONG) AS LOGIC
		RETURN FALSE


/// <include file="System.xml" path="doc/DataServer.Status/*" />
	ACCESS Status AS HyperLabel
		// Usual because there may be a field Status as well
		RETURN oHLStatus


/// <include file="System.xml" path="doc/DataServer.Status/*" />
	ASSIGN Status(oHl AS HyperLabel)
		// Usual because there may be a field Status as well
		IF IsInstanceOfUsual(oHl, #HyperLabel)
			SELF:oHLStatus := oHl
		ENDIF
		RETURN


/// <include file="System.xml" path="doc/DataServer.SuspendNotification/*" />
	METHOD SuspendNotification( ) AS LONG STRICT
		RETURN 0


/// <include file="System.xml" path="doc/DataServer.UnLock/*" />
	METHOD UnLock(nRecno := 0  AS LONG) AS LOGIC
		RETURN FALSE


/// <include file="System.xml" path="doc/DataServer.UnRegisterClient/*" />
	METHOD UnRegisterClient( oClient AS OBJECT, lAllowClose := TRUE AS LOGIC)  AS LOGIC
    LOCAL w AS DWORD


    IF ( w := AScan( aClients, oClient ) ) = 0
        RETURN FALSE
    ELSE
        ADel( aClients, w )
        nClients := ALen( aClients ) -1
        ASize( aClients, nClients )
        IF lAllowClose .AND. nClients = 0
            SELF:Close( )
        ENDIF
        RETURN TRUE
    ENDIF


/// <include file="System.xml" path="doc/DataServer.Update/*" />
	METHOD Update( ) AS LOGIC CLIPPER
    RETURN FALSE
END CLASS


