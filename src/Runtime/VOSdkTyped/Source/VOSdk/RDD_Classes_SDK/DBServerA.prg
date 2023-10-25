//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


#pragma warnings(165, off)

PARTIAL CLASS DbServer


/// <include file="Rdd.xml" path="doc/DbServer.Alias/*" />
    PROPERTY Alias  AS STRING GET Symbol2String( symAlias )


/// <include file="Rdd.xml" path="doc/DbServer.AliasSym/*" />
    PROPERTY AliasSym AS SYMBOL GET symAlias




/// <include file="Rdd.xml" path="doc/DbServer.BoF/*" />
    PROPERTY BoF AS LOGIC
        GET


        LOCAL dwCurrentWorkArea := 0 AS DWORD
        LOCAL lRetVal AS LOGIC
        IF ! SELF:Used
            RETURN FALSE
        ENDIF


        IF lSelectionActive
            RETURN siSelectionStatus == DBSELECTIONBOF .OR. siSelectionStatus == DBSELECTIONEMPTY
        ENDIF
        VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
        lRetVal := VoDbBof()
        __DBSSetSelect( dwCurrentWorkArea )
        RETURN lRetVal
        END GET
    END PROPERTY


/// <include file="Rdd.xml" path="doc/DbServer.ConcurrencyControl/*" />
    ACCESS ConcurrencyControl AS USUAL
        RETURN SELF:nCCMode


/// <include file="Rdd.xml" path="doc/DbServer.ConcurrencyControl/*" />
    ASSIGN ConcurrencyControl( nMode AS USUAL)
        LOCAL newMode := nMode AS USUAL
        LOCAL dwCurrentWorkArea  := 0 AS DWORD
        LOCAL oError            AS USUAL
        IF IsString(newMode)
            newMode := String2Symbol(nMode)
        ENDIF


        IF IsSymbol(newMode)
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
            ENDCASE
        ENDIF


        IF !IsNumeric(newMode)
                oErrorInfo:=DbError{ SELF, #ConcurrencyControl, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADCONCURRENCYASSIGN), nMode, "nMode" }
            SELF:Error( oErrorInfo, #ConcurrencyControl )
        ELSEIF newMode != SELF:nCCMode
            BEGIN SEQUENCE
                    VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
                    SELF:__ClearLocks()
                    SELF:nEffectiveCCMode := newMode
                    IF lReadOnly .OR. ! lShared
                        SELF:nEffectiveCCMode := ccNone
                    ENDIF
                    SELF:nCCMode := newMode
                    SELF:__SetupLocks()
                __DBSSetSelect( dwCurrentWorkArea )
            RECOVER USING oError
                oErrorInfo := oError
                __DBSSetSelect( dwCurrentWorkArea )
                SELF:Error( oErrorInfo, #Average )
            END SEQUENCE
        ENDIF


        RETURN




/// <include file="Rdd.xml" path="doc/DbServer.DBStruct/*" />
    PROPERTY DBStruct  AS ARRAY
        GET
	    IF ! SELF:Used
	        RETURN {}
	    ENDIF
        IF ALen(aStruct) == 0
            SELF:Error( __MakeErrObj(0), #DBSTRUCT )
        ENDIF


        RETURN AClone(aStruct)
        END GET
    END PROPERTY


/// <include file="Rdd.xml" path="doc/DbServer.DbStructure/*" />
    PROPERTY DbStructure AS ARRAY GET SELF:aStruct
        // This returns the original structure array


/// <include file="Rdd.xml" path="doc/DbServer.Deleted/*" />
    PROPERTY Deleted AS LOGIC
        GET


        LOCAL dwCurrentWorkArea := 0 AS DWORD
        LOCAL uRetVal           AS USUAL
        LOCAL oError            AS USUAL
    IF ! SELF:Used
        RETURN FALSE
    ENDIF




        lErrorFlag := FALSE
        BEGIN SEQUENCE
                VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
                uRetVal := VoDbDeleted()


        RECOVER USING oError
            oHLStatus := SELF:__GenerateStatusHL( oError )
            oErrorInfo := oError
            uRetVal := FALSE
        END SEQUENCE


        __DBSSetSelect( dwCurrentWorkArea )




        RETURN uRetVal
        END GET
    END PROPERTY


/// <include file="Rdd.xml" path="doc/DbServer.Driver/*" />
    PROPERTY Driver AS STRING GET SELF:cRDDName




/// <include file="Rdd.xml" path="doc/DbServer.EoF/*" />
    PROPERTY EoF AS LOGIC
        GET
        LOCAL dwCurrentWorkArea := 0 AS DWORD
        LOCAL lRetVal AS LOGIC
	    IF ! SELF:Used
	        RETURN FALSE
	    ENDIF




        IF lSelectionActive
            RETURN siSelectionStatus == DBSELECTIONEOF .OR. siSelectionStatus == DBSELECTIONEMPTY
        ENDIF
        VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
        lRetVal := VoDbEof()
        __DBSSetSelect( dwCurrentWorkArea )
        RETURN lRetVal
        END GET
    END PROPERTY


/// <include file="Rdd.xml" path="doc/DbServer.ErrInfo/*" />
    PROPERTY ErrInfo AS Error GET IIF (lErrorFlag, oErrorInfo, NULL_OBJECT)
        // returns an Error object if last operation generated an error.


/// <include file="Rdd.xml" path="doc/DbServer.ErrorInfo/*" />
    PROPERTY ErrorInfo() AS Error GET SELF:oErrorInfo


/// <include file="Rdd.xml" path="doc/DbServer.FCount/*" />
    PROPERTY FCount AS DWORD GET SELF:wFieldCount


/// <include file="Rdd.xml" path="doc/DbServer.FieldDesc/*" />
    PROPERTY FieldDesc AS ARRAY GET {}


/// <include file="Rdd.xml" path="doc/DbServer.FileSpec/*" />
    PROPERTY FileSpec AS FileSpec GET oFileSpec




/// <include file="Rdd.xml" path="doc/DbServer.Filter/*" />
    ACCESS Filter AS USUAL
        LOCAL dwCurrentWorkArea := 0 AS DWORD
        LOCAL oError            AS USUAL
        LOCAL uInfo             AS USUAL
	    IF ! SELF:Used
	        RETURN ""
	    ENDIF

        lErrorFlag := FALSE
        BEGIN SEQUENCE
                VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
                IF !VoDbInfo(DBI_DBFILTER, REF uInfo)
                    BREAK ErrorBuild(_VoDbErrInfoPtr())
                ENDIF
            __DBSSetSelect( dwCurrentWorkArea )
        RECOVER USING oError
            oErrorInfo := oError
            __DBSSetSelect( dwCurrentWorkArea )
            SELF:Error( oErrorInfo, #Filter )
        END SEQUENCE




        RETURN uInfo




/// <include file="Rdd.xml" path="doc/DbServer.Filter/*" />
    ASSIGN Filter( uFilterBlock AS USUAL)
        SELF:SetFilter( uFilterBlock )

        RETURN




/// <include file="Rdd.xml" path="doc/DbServer.ForBlock/*" />
    ACCESS ForBlock AS USUAL
        RETURN cbStoredForBlock




/// <include file="Rdd.xml" path="doc/DbServer.ForBlock/*" />
    ASSIGN ForBlock( cbForBlock  AS USUAL)
        IF IsString(cbForBlock)
            cbStoredForBlock := &( "{ ||" + cbForBlock + " }" )
        ELSEIF IsSymbol(cbForBlock)
            cbStoredForBlock := &( "{ ||" + Symbol2String(cbForBlock) + " }" )
        ELSE
            cbStoredForBlock := cbForBlock
        ENDIF
        SELF:lActiveScope := cbStoredForBlock# NIL .OR. cbStoredWhileBlock# NIL ;
        .OR. uStoredScope# NIL
        RETURN




/// <include file="Rdd.xml" path="doc/DbServer.Found/*" />
    PROPERTY Found AS LOGIC
        GET
        LOCAL dwCurrentWorkArea := 0 AS DWORD
        LOCAL oError            AS USUAL
        LOCAL lRetCode          AS LOGIC
	    IF ! SELF:Used
	        RETURN FALSE
	    ENDIF
        IF lSelectionActive
            RETURN siSelectionStatus == DBSELECTIONFOUND
        ENDIF




        lErrorFlag := FALSE
        BEGIN SEQUENCE
                VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
            lRetCode:=VoDbFound()
        RECOVER USING oError
            oHLStatus := SELF:__GenerateStatusHL( oError )
            oErrorInfo := oError
            lRetCode := FALSE
        END SEQUENCE


        __DBSSetSelect( dwCurrentWorkArea )




        RETURN lRetCode
        END GET
    END PROPERTY


/// <include file="Rdd.xml" path="doc/DbServer.Header/*" />
    PROPERTY Header AS DWORD
        GET
        LOCAL dwCurrentWorkArea := 0 AS DWORD
        LOCAL oError            AS USUAL
        LOCAL uInfo             AS USUAL
	    IF ! SELF:Used
	        RETURN 0
	    ENDIF




        lErrorFlag := FALSE
        BEGIN SEQUENCE
            VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
            IF ! VoDbInfo(DBI_GETHEADERSIZE, REF uInfo)
               BREAK ErrorBuild(_VoDbErrInfoPtr())
            ENDIF
            __DBSSetSelect( dwCurrentWorkArea )
        RECOVER USING oError
            oErrorInfo := oError
            __DBSSetSelect( dwCurrentWorkArea )
            SELF:Error( oErrorInfo, #Info )
        END SEQUENCE




        RETURN uInfo
        END GET
    END PROPERTY




/// <include file="Rdd.xml" path="doc/DbServer.IndexExt/*" />
    PROPERTY IndexExt AS STRING
        GET
        LOCAL dwCurrentWorkArea := 0 AS DWORD
        LOCAL oError            AS USUAL
        LOCAL uOrdVal           AS USUAL
	    IF ! SELF:Used
	        RETURN ""
	    ENDIF




        lErrorFlag := FALSE
        BEGIN SEQUENCE
                VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
                IF ! VoDbOrderInfo(DBOI_BAGEXT, "", NIL, REF uOrdVal)
                    BREAK ErrorBuild(_VoDbErrInfoPtr())
                ENDIF
            __DBSSetSelect( dwCurrentWorkArea )
        RECOVER USING oError
            oErrorInfo := oError
            __DBSSetSelect( dwCurrentWorkArea )
            SELF:Error( oErrorInfo, #OrderInfo )
        END SEQUENCE




        RETURN uOrdVal
        END GET
   END PROPERTY


/// <include file="Rdd.xml" path="doc/DbServer.IndexList/*" />
    PROPERTY IndexList AS ARRAY GET {}




/// <include file="Rdd.xml" path="doc/DbServer.LastRec/*" />
    PROPERTY LastRec AS LONG
        GET
        LOCAL dwCurrentWorkArea := 0 AS DWORD
        LOCAL liRecno AS LONGINT
	    IF ! SELF:Used
	        RETURN 0
	    ENDIF



        VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
        liRecno := VoDbLastRec()
        __DBSSetSelect( dwCurrentWorkArea )
        RETURN liRecno
        END GET
   END PROPERTY


/// <include file="Rdd.xml" path="doc/DbServer.Lupdate/*" />
    PROPERTY Lupdate AS DATE
        GET
        LOCAL dwCurrentWorkArea := 0 AS DWORD
        LOCAL oError            AS USUAL
        LOCAL uInfo             AS USUAL
	    IF ! SELF:Used
	        RETURN NULL_DATE
	    ENDIF




        lErrorFlag := FALSE
        BEGIN SEQUENCE
            VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
            IF ! VoDbInfo(DBI_LASTUPDATE, REF uInfo)
                BREAK ErrorBuild(_VoDbErrInfoPtr())
            ENDIF
            __DBSSetSelect( dwCurrentWorkArea )
        RECOVER USING oError
            oErrorInfo := oError
            __DBSSetSelect( dwCurrentWorkArea )
            SELF:Error( oErrorInfo, #Info )
        END SEQUENCE




        RETURN uInfo
        END GET
   END PROPERTY


/// <include file="Rdd.xml" path="doc/DbServer.MemoExt/*" />
    PROPERTY MemoExt AS STRING
        GET
        IF SELF:Used
            RETURN SELF:Info(DBI_MEMOEXT)
        ENDIF
        RETURN ""
        END GET
    END PROPERTY


/// <include file="Rdd.xml" path="doc/DbServer.Name/*" />
    PROPERTY Name AS STRING GET SUPER:Name


/// <include file="Rdd.xml" path="doc/DbServer.OleExt/*" />
    PROPERTY OleExt AS STRING GET "DFL"


/// <include file="Rdd.xml" path="doc/DbServer.OrderBottomScope/*" />
    ACCESS OrderBottomScope AS USUAL


        LOCAL dwCurrentWorkArea := 0 AS DWORD
        LOCAL uRetVal := NIL AS USUAL
        LOCAL oError        AS USUAL

	    IF ! SELF:Used
	        RETURN uRetVal
	    ENDIF

        lErrorFlag := FALSE
        BEGIN SEQUENCE
            VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
            IF ! VoDbOrderInfo(DBOI_SCOPEBOTTOM, "", NIL, REF uRetVal)
                BREAK ErrorBuild(_VoDbErrInfoPtr())
            ENDIF
            __DBSSetSelect( dwCurrentWorkArea )
        RECOVER USING oError
            oErrorInfo := oError
            __DBSSetSelect( dwCurrentWorkArea )
            SELF:Error( oErrorInfo, #OrderBottomScope )
            uRetVal:=NIL
        END SEQUENCE


        RETURN uRetVal




/// <include file="Rdd.xml" path="doc/DbServer.OrderBottomScope/*" />
    ASSIGN OrderBottomScope(uValue  AS USUAL)
        LOCAL dwCurrentWorkArea := 0 AS DWORD
        LOCAL oError        AS USUAL
        LOCAL n             AS DWORD




        lErrorFlag := FALSE
        BEGIN SEQUENCE
                VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
                n := DBOI_SCOPEBOTTOM
                IF IsNil(uValue)
                    n := DBOI_SCOPEBOTTOMCLEAR
                ENDIF


                IF ! VoDbOrderInfo(n, "", NIL, REF uValue)
                    BREAK ErrorBuild(_VoDbErrInfoPtr())
                ENDIF
                __DBSSetSelect( dwCurrentWorkArea )


        RECOVER USING oError
            oErrorInfo := oError
            __DBSSetSelect( dwCurrentWorkArea )
            SELF:Error( oErrorInfo, #OrderBottomScope )
        END SEQUENCE


        RETURN


/// <include file="Rdd.xml" path="doc/DbServer.OrderKeyVal/*" />
    PROPERTY OrderKeyVal  AS USUAL
        GET
        LOCAL dwCurrentWorkArea := 0 AS DWORD
        LOCAL uRetVal := NIL AS USUAL
        LOCAL oError        AS USUAL
	    IF ! SELF:Used
	        RETURN uRetVal
	    ENDIF



        lErrorFlag := FALSE
        BEGIN SEQUENCE
            VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
            //uRetVal := OrdKeyVal()
            IF ! VoDbOrderInfo(DBOI_KEYVAL, "", NIL, REF uRetVal)
                BREAK ErrorBuild(_VoDbErrInfoPtr())
            ENDIF
            __DBSSetSelect( dwCurrentWorkArea )
        RECOVER USING oError
            oErrorInfo := oError
            __DBSSetSelect( dwCurrentWorkArea )
            SELF:Error( oErrorInfo, #OrderKeyVal )
            uRetVal:=NIL
        END SEQUENCE


        RETURN uRetVal
        END GET
    END PROPERTY


/// <include file="Rdd.xml" path="doc/DbServer.OrderTopScope/*" />
    ACCESS OrderTopScope  AS USUAL


        LOCAL dwCurrentWorkArea := 0 AS DWORD
        LOCAL uRetVal := NIL AS USUAL
        LOCAL oError        AS USUAL
	    IF ! SELF:Used
	        RETURN uRetVal
	    ENDIF



        lErrorFlag := FALSE
        BEGIN SEQUENCE
            VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
            //uRetVal := DbOrderInfo(DBOI_Scopetop)
            IF ! VoDbOrderInfo(DBOI_SCOPETOP, "", NIL, REF uRetVal)
                BREAK ErrorBuild(_VoDbErrInfoPtr())
            ENDIF
            __DBSSetSelect( dwCurrentWorkArea )
        RECOVER USING oError
            oErrorInfo := oError
            __DBSSetSelect( dwCurrentWorkArea )
            SELF:Error( oErrorInfo, #OrderTopScope )
            uRetVal:=NIL
        END SEQUENCE


        RETURN uRetVal


/// <include file="Rdd.xml" path="doc/DbServer.OrderTopScope/*" />
    ASSIGN OrderTopScope(uValue AS USUAL)


        LOCAL dwCurrentWorkArea := 0 AS DWORD
        LOCAL oError        AS USUAL
        LOCAL n             AS DWORD




        lErrorFlag := FALSE
        BEGIN SEQUENCE
            VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
            n := DBOI_SCOPETOP
            IF IsNil(uValue)
                n := DBOI_SCOPETOPCLEAR
            ENDIF


			IF ! VoDbOrderInfo(n, "", NIL, REF uValue)
				BREAK ErrorBuild(_VoDbErrInfoPtr())
            ENDIF
            __DBSSetSelect( dwCurrentWorkArea )
        RECOVER USING oError
            oErrorInfo := oError
            __DBSSetSelect( dwCurrentWorkArea )
            SELF:Error( oErrorInfo, #OrderTopScope )
        END SEQUENCE
        RETURN




/// <include file="Rdd.xml" path="doc/DbServer.PaintedStructure/*" />
    PROPERTY  PaintedStructure AS ARRAY
        GET
        LOCAL aRet       AS ARRAY
        LOCAL aFDesc     AS ARRAY
        LOCAL i,nField   AS DWORD


        aFDesc := SELF:FieldDesc
        nField := ALen(aFDesc)


        aRet := {}


        FOR i:=1 UPTO nField
            LOCAL oFs AS FieldSpec
            oFs := aFDesc[i][DBC_FIELDSPEC]
            AAdd( aRet, { aFDesc[i][DBC_NAME] , oFs:ValType, oFs:Length , oFs:Decimals } )
        NEXT


        RETURN aRet
        END GET
    END PROPERTY


/// <include file="Rdd.xml" path="doc/DbServer.RddName/*" />
    PROPERTY RddName  AS STRING GET cRDDName




/// <include file="Rdd.xml" path="doc/DbServer.Rdds/*" />
    PROPERTY Rdds AS ARRAY GET aRdds




/// <include file="Rdd.xml" path="doc/DbServer.ReadOnly/*" />
    PROPERTY ReadOnly AS LOGIC GET lReadOnly


/// <include file="Rdd.xml" path="doc/DbServer.RecCount/*" />
    PROPERTY RecCount  AS LONG
        GET
        LOCAL nCurrentRecord            AS DWORD
        LOCAL siCurrentSelectionStatus  AS SHORTINT
        LOCAL iRetVal                   AS INT
        LOCAL dwCurrentWorkArea := 0    AS DWORD




        LOCAL oError                    AS USUAL
	    IF ! SELF:Used
	        RETURN 0
	    ENDIF


        lErrorFlag := FALSE
        BEGIN SEQUENCE
                IF lSelectionActive
                    IF siSelectionStatus == DBSELECTIONEMPTY
                        iRetVal := 0
                    ELSE
                        VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
                        nCurrentRecord          := VoDbRecno( )
                        siCurrentSelectionStatus:= siSelectionStatus
                        iRetVal                 := SELF:Count( )
                        IF ! VoDbGoto( nCurrentRecord )
                            BREAK ErrorBuild(_VoDbErrInfoPtr())
                        ENDIF
                        __DBSSetSelect( dwCurrentWorkArea )
                        siSelectionStatus       := siCurrentSelectionStatus
                    ENDIF
                ELSE
                    VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
                    iRetVal := VoDbLastRec()
                    __DBSSetSelect( dwCurrentWorkArea )
                ENDIF


        RECOVER USING oError
            oErrorInfo := oError
            __DBSSetSelect( dwCurrentWorkArea )
            SELF:Error( oErrorInfo, #RECCOUNT )
        END SEQUENCE




        RETURN iRetVal
        END GET
    END PROPERTY


/// <include file="Rdd.xml" path="doc/DbServer.RecNo/*" />
    ACCESS RecNo AS LONG
        LOCAL dwCurrentWorkArea := 0 AS DWORD
        LOCAL wRetCode      AS DWORD
        LOCAL oError        AS USUAL

	    IF ! SELF:Used
	        RETURN 0
	    ENDIF

        lErrorFlag := FALSE
        BEGIN SEQUENCE
            VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
            wRetCode := VoDbRecno()
            __DBSSetSelect( dwCurrentWorkArea )
        RECOVER USING oError
            oErrorInfo := oError
            __DBSSetSelect( dwCurrentWorkArea )
            SELF:Error( oErrorInfo, #RECNO )
        END SEQUENCE




        RETURN (LONG) wRetCode




/// <include file="Rdd.xml" path="doc/DbServer.RecNo/*" />
    ASSIGN RecNo( nRecordNumber AS LONG)
        LOCAL oError        AS USUAL




        lErrorFlag := FALSE
        BEGIN SEQUENCE
            SELF:GoTo( nRecordNumber )
        RECOVER USING oError
            oErrorInfo := oError
            SELF:Error( oErrorInfo, #RECNO )
        END SEQUENCE




        RETURN




/// <include file="Rdd.xml" path="doc/DbServer.RecSize/*" />
    PROPERTY RecSize AS DWORD
        GET
        LOCAL dwCurrentWorkArea := 0 AS DWORD
        LOCAL oError        AS USUAL
        LOCAL uVoVal        AS USUAL




        lErrorFlag := FALSE
        BEGIN SEQUENCE
            VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
            IF ! VoDbRecordInfo(DBRI_RECSIZE, 0, REF uVoVal)
                BREAK ErrorBuild(_VoDbErrInfoPtr())
            ENDIF
            __DBSSetSelect( dwCurrentWorkArea )
        RECOVER USING oError
            oErrorInfo := oError
            __DBSSetSelect( dwCurrentWorkArea )
            SELF:Error( oErrorInfo, #RECSIZE )
        END SEQUENCE


        RETURN uVoVal
        END GET
     END PROPERTY




/// <include file="Rdd.xml" path="doc/DbServer.RelationChildren/*" />
    ACCESS RelationChildren AS ARRAY
        RETURN SELF:aRelationChildren


/// <include file="Rdd.xml" path="doc/DbServer.RelationChildren/*" />
    ASSIGN RelationChildren(aNewChildren AS ARRAY)
        IF IsArray(aNewChildren)
            SELF:aRelationChildren := aNewChildren
        ENDIF
        IF ALen(SELF:aRelationChildren)=0
            lRelationsActive := FALSE
        ELSE
            lRelationsActive := TRUE
        ENDIF


        RETURN


/// <include file="Rdd.xml" path="doc/DbServer.Retries/*" />
    ACCESS Retries AS DWORD
        RETURN SELF:nRetries


/// <include file="Rdd.xml" path="doc/DbServer.Retries/*" />
    ASSIGN Retries  (n AS DWORD)
        IF n > 0
            SELF:nRetries := n
        ENDIF
        RETURN




/// <include file="Rdd.xml" path="doc/DbServer.RLockList/*" />
    PROPERTY RLockList AS ARRAY
        GET
        LOCAL dwCurrentWorkArea := 0 AS DWORD
        LOCAL oError            AS USUAL
        LOCAL aLockList := { }  AS ARRAY
	    IF ! SELF:Used
	        RETURN aLockList
	    ENDIF


        lErrorFlag := FALSE
        BEGIN SEQUENCE
            VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
            aLockList := DbRLockList()
            __DBSSetSelect( dwCurrentWorkArea )
        RECOVER USING oError
            oErrorInfo := oError
            __DBSSetSelect( dwCurrentWorkArea )
            SELF:Error( oErrorInfo, #RLockList )
        END SEQUENCE


        RETURN aLockList
        END GET
    END PROPERTY
/// <include file="Rdd.xml" path="doc/DbServer.Scope/*" />
    ACCESS Scope AS USUAL
        RETURN uStoredScope


/// <include file="Rdd.xml" path="doc/DbServer.SelectionWorkArea/*" />
    PROPERTY SelectionWorkArea AS DWORD GET SELF:wSelectionWorkArea


/// <include file="Rdd.xml" path="doc/DbServer.Scope/*" />
    ASSIGN Scope( uScope AS USUAL)
        uStoredScope := uScope
        IF uScope == NIL
            lStoredAllRecords := FALSE
            lStoredRestOfFile := FALSE
            nStoredNextCount := 0
        ELSEIF IsNumeric(uScope)
            lStoredAllRecords := FALSE
            lStoredRestOfFile := FALSE
            nStoredNextCount := uScope
        ELSEIF uScope == DBSCOPEREST
            lStoredAllRecords := FALSE
            lStoredRestOfFile := TRUE
            nStoredNextCount := 0
        ELSE
            lStoredAllRecords := TRUE
            lStoredRestOfFile := FALSE
            nStoredNextCount := 0
        ENDIF
        lActiveScope := cbStoredForBlock# NIL .OR. cbStoredWhileBlock# NIL ;
        .OR. uStoredScope# NIL
        RETURN




/// <include file="Rdd.xml" path="doc/DbServer.Shared/*" />
    PROPERTY  Shared AS LOGIC GET lShared




/// <include file="Rdd.xml" path="doc/DbServer.Status/*" />
    ACCESS Status AS HyperLabel
        IF ( lErrorFlag )
            RETURN oHLStatus
        ENDIF
        RETURN NULL_OBJECT


/// <include file="Rdd.xml" path="doc/DbServer.Status/*" />
    ASSIGN Status(oHl AS HyperLabel)
        SUPER:Status := oHl
        SELF:lErrorFlag := TRUE


        RETURN


/// <include file="Rdd.xml" path="doc/DbServer.TableExt/*" />
PROPERTY TableExt as STRING
GET
	// DHer: 18/12/2008
    IF ! SELF:Used
        RETURN ""
    ENDIF
RETURN SELF:Info(DBI_TABLEEXT)
END GET
END PROPERTY

/// <include file="Rdd.xml" path="doc/DbServer.Used/*" />
    PROPERTY Used AS LOGIC
        GET
        LOCAL dwCurrentWorkArea := 0 AS DWORD
        LOCAL lRetVal AS LOGIC


        IF SELF:wWorkArea == 0
            RETURN FALSE
        ENDIF
        VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
        lRetVal := Used()
        __DBSSetSelect( dwCurrentWorkArea )
        RETURN lRetVal
        END GET
    END PROPERTY


/// <include file="Rdd.xml" path="doc/DbServer.WhileBlock/*" />
    ACCESS WhileBlock AS USUAL
        RETURN cbStoredWhileBlock




/// <include file="Rdd.xml" path="doc/DbServer.WhileBlock/*" />
    ASSIGN WhileBlock( cbWhileBlock  AS USUAL)
        IF IsString(cbWhileBlock)
            cbStoredWhileBlock := &( "{ ||" + cbWhileBlock + " }" )
        ELSEIF IsSymbol(cbWhileBlock)
            cbStoredWhileBlock := &( "{ ||" + Symbol2String(cbWhileBlock) + " }" )
        ELSE
            cbStoredWhileBlock := cbWhileBlock
        ENDIF
        lActiveScope := cbStoredForBlock# NIL .OR. cbStoredWhileBlock# NIL ;
        .OR. uStoredScope# NIL
        RETURN




/// <include file="Rdd.xml" path="doc/DbServer.WorkArea/*" />
    PROPERTY  WorkArea AS DWORD GET wWorkArea
    END CLASS


