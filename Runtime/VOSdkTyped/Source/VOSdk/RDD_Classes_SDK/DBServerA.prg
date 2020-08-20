#pragma warnings(165, off)
PARTIAL CLASS DbServer

    PROPERTY Alias  AS STRING GET Symbol2String( symAlias )
    
    PROPERTY AliasSym AS SYMBOL GET symAlias
    
    
    PROPERTY BoF AS LOGIC
        GET
        
        LOCAL dwCurrentWorkArea := 0 AS DWORD
        LOCAL lRetVal AS LOGIC
        
        IF lSelectionActive
            RETURN siSelectionStatus == DBSELECTIONBOF .OR. siSelectionStatus == DBSELECTIONEMPTY
        ENDIF
        VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
        lRetVal := VoDbBof()
        __DBSSetSelect( dwCurrentWorkArea )
        RETURN lRetVal
        END GET
    END PROPERTY
    
    ACCESS ConcurrencyControl AS USUAL
        RETURN SELF:nCCMode
        
    ASSIGN ConcurrencyControl( nMode AS USUAL) 
        LOCAL newMode := nMode
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
    
    
    PROPERTY DBStruct  AS ARRAY
        GET
        IF ALen(aStruct) == 0
            SELF:Error( __MakeErrObj(0), #DBSTRUCT )
        ENDIF
        
        RETURN AClone(aStruct)
        END GET
    END PROPERTY
        
    PROPERTY DbStructure AS ARRAY GET SELF:aStruct
        // This returns the original structure array
        
    PROPERTY Deleted AS LOGIC
        GET
        
        LOCAL dwCurrentWorkArea := 0 AS DWORD
        LOCAL uRetVal           AS USUAL
        LOCAL oError            AS USUAL
        
        
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
    
    PROPERTY Driver AS STRING GET SELF:cRDDName
    
    
    PROPERTY EoF AS LOGIC
        GET
        LOCAL dwCurrentWorkArea := 0 AS DWORD
        LOCAL lRetVal AS LOGIC
        
        
        IF lSelectionActive
            RETURN siSelectionStatus == DBSELECTIONEOF .OR. siSelectionStatus == DBSELECTIONEMPTY
        ENDIF
        VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
        lRetVal := VoDbEof()
        __DBSSetSelect( dwCurrentWorkArea )
        RETURN lRetVal
        END GET
    END PROPERTY
    
    PROPERTY ErrInfo AS Error GET IIF (lErrorFlag, oErrorInfo, NULL_OBJECT)
        // returns an Error object if last operation generated an error.

    PROPERTY ErrorInfo() AS Error GET SELF:oErrorInfo
    
    PROPERTY FCount AS DWORD GET SELF:wFieldCount
    
    PROPERTY FieldDesc AS ARRAY GET {}
    
    PROPERTY FileSpec AS FileSpec GET oFileSpec
    
    
    ACCESS Filter AS USUAL
        LOCAL dwCurrentWorkArea := 0 AS DWORD
        LOCAL oError            AS USUAL
        LOCAL uInfo             AS USUAL
        
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
    
    
    ASSIGN Filter( uFilterBlock AS USUAL) 
        SELF:SetFilter( uFilterBlock )
        
        RETURN 
    
    
    ACCESS ForBlock AS USUAL
        RETURN cbStoredForBlock
    
    
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
    
    
    PROPERTY Found AS LOGIC
        GET
        LOCAL dwCurrentWorkArea := 0 AS DWORD
        LOCAL oError            AS USUAL
        LOCAL lRetCode          AS LOGIC
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
    
    PROPERTY Header AS DWORD
        GET
        LOCAL dwCurrentWorkArea := 0 AS DWORD
        LOCAL oError            AS USUAL
        LOCAL uInfo             AS USUAL
        
        
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


    PROPERTY IndexExt AS STRING
        GET
        LOCAL dwCurrentWorkArea := 0 AS DWORD
        LOCAL oError            AS USUAL
        LOCAL uOrdVal           AS USUAL
        
        
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
    
    PROPERTY IndexList AS ARRAY GET {}
    
    
    PROPERTY LastRec AS LONG
        GET        
        LOCAL dwCurrentWorkArea := 0 AS DWORD
        LOCAL liRecno AS LONGINT
        
        
        VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
        liRecno := VoDbLastRec()
        __DBSSetSelect( dwCurrentWorkArea )
        RETURN liRecno
        END GET
   END PROPERTY
        
    PROPERTY Lupdate AS DATE
        GET
        LOCAL dwCurrentWorkArea := 0 AS DWORD
        LOCAL oError            AS USUAL
        LOCAL uInfo             AS USUAL
        
        
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
    
    PROPERTY MemoExt AS STRING
        GET
        IF SELF:Used
            RETURN SELF:Info(DBI_MEMOEXT)
        ENDIF
        RETURN ""
        END GET
    END PROPERTY
        
    PROPERTY Name AS STRING GET SUPER:Name
    
    PROPERTY OleExt AS STRING GET "DFL"	
        
    ACCESS OrderBottomScope AS USUAL
        
        LOCAL dwCurrentWorkArea := 0 AS DWORD
        LOCAL uRetVal := NIL AS USUAL
        LOCAL oError        AS USUAL
        
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
        
    PROPERTY OrderKeyVal  AS USUAL
        GET
        LOCAL dwCurrentWorkArea := 0 AS DWORD
        LOCAL uRetVal := NIL AS USUAL
        LOCAL oError        AS USUAL
        
        
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
    
    ACCESS OrderTopScope  AS USUAL
        
        LOCAL dwCurrentWorkArea := 0 AS DWORD
        LOCAL uRetVal := NIL AS USUAL
        LOCAL oError        AS USUAL
        
        
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
    
    PROPERTY RddName  AS STRING GET cRDDName
    
    
    PROPERTY Rdds AS ARRAY GET aRdds
    
    
    PROPERTY ReadOnly AS LOGIC GET lReadOnly
    
    PROPERTY RecCount  AS LONG
        GET
        LOCAL nCurrentRecord            AS LONGINT
        LOCAL siCurrentSelectionStatus  AS SHORTINT
        LOCAL iRetVal                   AS INT
        LOCAL dwCurrentWorkArea := 0    AS DWORD
         
         
        LOCAL oError                    AS USUAL
        
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
    
    ACCESS RecNo AS LONG
        LOCAL dwCurrentWorkArea := 0 AS DWORD
        LOCAL wRetCode      AS LONGINT
        LOCAL oError        AS USUAL
        
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
        
        
        RETURN wRetCode
    
    
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
    
     
    ACCESS RelationChildren AS ARRAY
        RETURN SELF:aRelationChildren
        
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
         
    ACCESS Retries AS DWORD
        RETURN SELF:nRetries
      
    ASSIGN Retries  (n AS DWORD) 
        IF n > 0
            SELF:nRetries := n
        ENDIF
        RETURN 
    
    
    PROPERTY RLockList AS ARRAY
        GET
        LOCAL dwCurrentWorkArea := 0 AS DWORD
        LOCAL oError            AS USUAL
        LOCAL aLockList := { }  AS ARRAY
        
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
    ACCESS Scope AS USUAL
        RETURN uStoredScope
    
    PROPERTY SelectionWorkArea AS DWORD GET SELF:wSelectionWorkArea	
        
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
    
    
    PROPERTY  Shared AS LOGIC GET lShared
    
    
    ACCESS Status AS HyperLabel
        IF ( lErrorFlag )
            RETURN oHLStatus
        ENDIF
        RETURN NULL_OBJECT
    
    ASSIGN Status(oHl AS HyperLabel) 
        SUPER:Status := oHl
        SELF:lErrorFlag := TRUE
        
        RETURN  
        
    PROPERTY TableExt AS STRING GET SELF:Info(DBI_TABLEEXT)
        
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
    
    ACCESS WhileBlock AS USUAL
        RETURN cbStoredWhileBlock
    
    
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
    
    
    PROPERTY  WorkArea AS DWORD GET wWorkArea
    END CLASS
        
