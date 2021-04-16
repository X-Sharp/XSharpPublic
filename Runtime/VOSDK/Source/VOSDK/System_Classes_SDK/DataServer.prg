/// <include file="System.xml" path="doc/DataServer/*" />
CLASS DataServer
    PROTECT oHyperLabel AS HyperLabel
    PROTECT oHLStatus		AS USUAL
    PROTECT wFieldCount 	AS DWORD
    PROTECT aDataFields	    AS ARRAY
    PROTECT aClients		AS ARRAY
    PROTECT nClients		AS DWORD
    PROTECT nCCMode		    AS DWORD
    PROTECT nLastLock		AS DWORD


 /// <exclude />
METHOD __ClearLocks( ) AS VOID STRICT
    SWITCH nCCMode
    CASE ccStable
        SELF:Unlock( nLastLock )
    CASE ccRepeatable
        SELF:Unlock( )
    CASE ccFile
        SELF:Unlock( )
    END SWITCH


    RETURN


 /// <exclude />
METHOD __DataField (nFieldPosition AS DWORD) AS DataField
    RETURN aDataFields[ nFieldPosition ]


 /// <exclude />
ACCESS __Clients AS ARRAY STRICT
    RETURN SELF:aClients


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
        nLastLock := SELF:Recno
        // Do not free locks in case user has other locks
        IF ! SELF:RLOCK( nLastLock )
            nLastLock := 0
            oHLStatus := SELF:Status
        ENDIF
    CASE ccFile
        IF ! SELF:FLOCK( )
            oHLStatus := SELF:Status
        ENDIF
    OTHERWISE
        BREAK DbError{ SELF, #ConcurrencyControl, EG_ARG, ;
            __CavoStr(__CAVOSTR_DBFCLASS_BADCONCURRENCYASSIGN), nCCMode, "nCCMode" }
    END SWITCH


    RETURN


/// <include file="System.xml" path="doc/DataServer.Append/*" />
METHOD Append( )
    RETURN FALSE


/// <include file="System.xml" path="doc/DataServer.AsString/*" />
METHOD AsString( )
    RETURN oHyperLabel:AsString( )


/// <include file="System.xml" path="doc/DataServer.BoF/*" />
ACCESS BoF
    RETURN FALSE


/// <include file="System.xml" path="doc/DataServer.Clients/*" />
ACCESS Clients
    // DHer: 18/12/2008
   RETURN SELF:aClients


/// <include file="System.xml" path="doc/DataServer.Clients/*" />
ASSIGN Clients(aNewClients)
    // DHer: 18/12/2008
    IF IsArray(aNewClients)
        SELF:aClients := aNewClients
    ENDIF
    SELF:nClients := ALen(SELF:aClients)
RETURN


/// <include file="System.xml" path="doc/DataServer.Close/*" />
METHOD Close( )
    RETURN FALSE


/// <include file="System.xml" path="doc/DataServer.Commit/*" />
METHOD Commit( )
    RETURN FALSE


/// <include file="System.xml" path="doc/DataServer.ConcurrencyControl/*" />
ACCESS ConcurrencyControl( )
    RETURN SELF:nCCMode


/// <include file="System.xml" path="doc/DataServer.ConcurrencyControl/*" />
ASSIGN ConcurrencyControl( nMode)
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


/// <include file="System.xml" path="doc/DataServer.DataField/*" />
METHOD DataField( nFieldPosition )
    RETURN SELF:__DataField (nFieldPosition)


/// <include file="System.xml" path="doc/DataServer.DBStruct/*" />
ACCESS DBStruct
    LOCAL aStruct	AS ARRAY
    LOCAL oDF		AS DataField
    LOCAL w			AS DWORD


    aStruct := ArrayNew( wFieldCount )
    FOR w := 1 UPTO wFieldCount
        oDF := aDataFields[ w ]
        aStruct[ w ] := { oDF:Name, oDF:__FieldSpec:UsualType, oDF:__FieldSpec:Length, oDF:__FieldSpec:Decimals }
    NEXT


    RETURN aStruct


/// <include file="System.xml" path="doc/DataServer.Delete/*" />
METHOD Delete( )
    RETURN FALSE


/// <include file="System.xml" path="doc/DataServer.EoF/*" />
ACCESS EoF
    RETURN FALSE


/// <include file="System.xml" path="doc/DataServer.FCount/*" />
ACCESS FCount
    RETURN wFieldCount


/// <include file="System.xml" path="doc/DataServer.FieldGet/*" />
METHOD FIELDGET( nFieldPosition )
    RETURN NIL


/// <include file="System.xml" path="doc/DataServer.FieldGetFormatted/*" />
METHOD FieldGetFormatted( nFieldPosition )
    RETURN NIL


/// <include file="System.xml" path="doc/DataServer.FieldHyperLabel/*" />
METHOD FieldHyperLabel( nFieldPosition )
    RETURN SELF:__DataField (nFieldPosition):HyperLabel


/// <include file="System.xml" path="doc/DataServer.FieldName/*" />
METHOD FieldName( nFieldPosition )
    RETURN SELF:__DataField (nFieldPosition):__HyperLabel:Name


/// <include file="System.xml" path="doc/DataServer.FieldPos/*" />
METHOD FieldPos( nFieldPosition )
    RETURN NIL


/// <include file="System.xml" path="doc/DataServer.FieldPut/*" /> 
METHOD FIELDPUT( nFieldPosition, uValue )
    RETURN NIL


/// <include file="System.xml" path="doc/DataServer.FieldSpec/*" />
METHOD FieldSpec( nFieldPosition )
    RETURN SELF:__DataField (nFieldPosition):FieldSpec


/// <include file="System.xml" path="doc/DataServer.FieldStatus/*" />
METHOD FieldStatus( nFieldPosition )
    RETURN SELF:__DataField (nFieldPosition):__FieldSpec:Status


/// <include file="System.xml" path="doc/DataServer.FieldSym/*" />
METHOD FieldSym( nFieldPosition )
    RETURN SELF:__DataField (nFieldPosition):__HyperLabel:NameSym


/// <include file="System.xml" path="doc/DataServer.FieldValidate/*" />
METHOD FieldValidate( nFieldPosition, uValue )
    RETURN SELF:__DataField (nFieldPosition):__FieldSpec:PerformValidations( uValue )


/// <include file="System.xml" path="doc/DataServer.FLOCK/*" />
METHOD FLOCK( )
    RETURN FALSE


/// <include file="System.xml" path="doc/DataServer.GoBottom/*" />
METHOD GoBottom( )
    RETURN FALSE


/// <include file="System.xml" path="doc/DataServer.GoTo/*" />
METHOD GoTo( nPosition )
    RETURN FALSE


/// <include file="System.xml" path="doc/DataServer.GoTop/*" />
METHOD GoTop( )
    RETURN FALSE


/// <include file="System.xml" path="doc/DataServer.HLStatus/*" />
ACCESS HLStatus
    // This always returns the status, regardless of the error flag
    RETURN SELF:oHLStatus


/// <include file="System.xml" path="doc/DataServer.HyperLabel/*" />
ACCESS HyperLabel
    RETURN oHyperLabel


/// <include file="System.xml" path="doc/DataServer.HyperLabel/*" />
ASSIGN HyperLabel( oHL )
    oHyperLabel := oHL
    RETURN


/// <include file="System.xml" path="doc/DataServer.ctor/*" />
CONSTRUCTOR( )
    SELF:aClients := { }
    RETURN


/// <include file="System.xml" path="doc/DataServer.Name/*" />
ACCESS Name
    IF oHyperlabel != NULL_OBJECT
        RETURN oHyperLabel:Name
    ENDIF
    RETURN NULL_STRING


/// <include file="System.xml" path="doc/DataServer.NameSym/*" />
ACCESS NameSym
    IF oHyperlabel != NULL_OBJECT
        RETURN oHyperLabel:NameSym
    ENDIF
    RETURN NULL_STRING


/// <include file="System.xml" path="doc/DataServer.NoIVarGet/*" />
METHOD NoIVarGet( symFieldName )
    RETURN SELF:FieldGet( symFieldName )


/// <include file="System.xml" path="doc/DataServer.NoIVarPut/*" />
METHOD NoIVarPut( symFieldName, uValue )


    SELF:FieldPut( symFieldName, uValue )
    SELF:Notify( NOTIFYFIELDCHANGE, symFieldName )
    RETURN uValue


/// <include file="System.xml" path="doc/DataServer.Notify/*" />
METHOD Notify( kNotification, uDescription )
    //unsupported syntax under .NET
    //aClients:Notify( kNotification, uDescription )
    FOREACH oClient AS OBJECT IN AClone(aClients)
        Send(oClient, #Notify, kNotification, uDescription )
    NEXT
    RETURN SELF


/// <include file="System.xml" path="doc/DataServer.PostInit/*" />
METHOD PostInit( )
    RETURN SELF


/// <include file="System.xml" path="doc/DataServer.PreInit/*" />
METHOD PreInit( )
    RETURN SELF


/// <include file="System.xml" path="doc/DataServer.RecCount/*" />
ACCESS RecCount
    RETURN 0


/// <include file="System.xml" path="doc/DataServer.RecNo/*" />
ACCESS RecNo
    RETURN NIL


/// <include file="System.xml" path="doc/DataServer.RecNo/*" />
ASSIGN RecNo( lRecNo )
    RETURN


/// <include file="System.xml" path="doc/DataServer.RegisterClient/*" />
METHOD RegisterClient( oForm )


    IF AScan( aClients, oForm ) # 0
        RETURN FALSE
    ENDIF
    AAdd( aClients, oForm )
    nClients := ALen( aClients )


    RETURN TRUE


/// <include file="System.xml" path="doc/DataServer.ResetNotification/*" />
METHOD ResetNotification( )
    RETURN 0


/// <include file="System.xml" path="doc/DataServer.RLOCK/*" />
METHOD RLOCK( nRecord )
    RETURN FALSE


/// <include file="System.xml" path="doc/DataServer.RLockVerify/*" />
METHOD RLockVerify( )
    RETURN FALSE


/// <include file="System.xml" path="doc/DataServer.Rollback/*" />
METHOD Rollback( )
    RETURN FALSE


/// <include file="System.xml" path="doc/DataServer.Seek/*" />
METHOD Seek( uValue )
    RETURN FALSE


/// <include file="System.xml" path="doc/DataServer.SetDataField/*" />
METHOD SetDataField( nFieldPosition, oDataField )
    LOCAL wFieldPosition := nFieldPosition	AS WORD


    IF aDataFields = NULL_ARRAY
        BREAK DbError{ SELF, #SetDataField, EG_SEQUENCE, ;
            __CavoStr( __CAVOSTR_DBFCLASS_NODATAFIELDSEXIST ) }
    ELSEIF IsNil( nFieldPosition) .OR. ! IsNumeric( nFieldPosition ) .OR.   ;
        wFieldPosition < 1 .OR. wFieldPosition > ALen( aDataFields )
        BREAK DbError{ SELF, #SetDataField, EG_ARG, ;
            __CavoStr(__CAVOSTR_DBFCLASS_BADFIELDPOSITION), nFieldPosition, "nFieldPosition" }
    ELSEIF ! (__Usual.ToObject(oDataField) IS DataField)
        BREAK DbError{ SELF, #SetDataField, EG_ARG,   ;
            __CavoStr(__CAVOSTR_DBFCLASS_BADFIELDPOSITION), nFieldPosition, "nFieldPosition" }
    ELSE
        LOCAL oField := oDataField				AS DataField
        LOCAL oDF								AS DataField
        oDF := aDataFields[ wFieldPosition ]
        IF oField:Name == oDF:Name .AND.    ;
            oField:__FieldSpec:UsualType == oDF:__FieldSpec:UsualType .AND.     ;
            oField:__FieldSpec:Length    == oDF:__FieldSpec:Length .AND.   ;
            oField:__FieldSpec:Decimals  == oDF:__FieldSpec:Decimals
            aDataFields[ wFieldPosition ] := oField
            RETURN TRUE
        ELSE
            RETURN FALSE
        ENDIF
    ENDIF


/// <include file="System.xml" path="doc/DataServer.Skip/*" />
METHOD Skip( nRelativePosition )
    RETURN FALSE


/// <include file="System.xml" path="doc/DataServer.Status/*" />
ACCESS Status
    RETURN oHLStatus


/// <include file="System.xml" path="doc/DataServer.Status/*" />
ASSIGN Status(oHl)
    SELF:oHLStatus := oHl
    RETURN


/// <include file="System.xml" path="doc/DataServer.SuspendNotification/*" />
METHOD SuspendNotification( )
    RETURN 0


/// <include file="System.xml" path="doc/DataServer.UnLock/*" />
METHOD UnLock( ) CLIPPER
    RETURN FALSE


/// <include file="System.xml" path="doc/DataServer.UnRegisterClient/*" />
METHOD UnRegisterClient( oClient, lAllowClose )
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


/// <include file="System.xml" path="doc/DataServer.Update/*" />
METHOD Update( )
    RETURN FALSE
END CLASS


