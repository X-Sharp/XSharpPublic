
PARTIAL CLASS DbServer

PROPERTY __FileSpec as FileSpec GET SELF:oFileSpec

ACCESS Alias  AS STRING
		RETURN Symbol2String( symAlias )
	

ACCESS AliasSym AS SYMBOL
		RETURN symAlias
	

ACCESS BoF AS LOGIC
		//SE-060601
		LOCAL dwCurrentWorkArea := 0 AS DWORD
		LOCAL lRetVal AS LOGIC
		
		IF lSelectionActive
			RETURN siSelectionStatus == DBSELECTIONBOF .OR. siSelectionStatus == DBSELECTIONEMPTY
		ENDIF
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		lRetVal := VoDbBof()
		__DBSSetSelect( dwCurrentWorkArea )
		RETURN lRetVal
	

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
			__DBSSetSelect( dwCurrentWorkArea )  //SE-060527
		RECOVER USING oError
			oErrorInfo := oError
			__DBSSetSelect( dwCurrentWorkArea )  //SE-060527
			SELF:Error( oErrorInfo, #Average )
		END SEQUENCE
	ENDIF
	
	RETURN 
	

ACCESS DBStruct  AS ARRAY
	
	IF ALen(aStruct) == 0
		SELF:Error( __MakeErrObj(0), #DBSTRUCT )
	ENDIF
	
	RETURN AClone(aStruct)

ACCESS DbStructure AS ARRAY
	// This returns the original structure array
    RETURN SELF:aStruct

ACCESS Deleted AS LOGIC
	//SE-060601
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
	

ACCESS Driver AS STRING
	
	RETURN SELF:cRDDName
	

ACCESS EoF AS LOGIC
	//SE-060601
	LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL lRetVal AS LOGIC
	
	
	IF lSelectionActive
		RETURN siSelectionStatus == DBSELECTIONEOF .OR. siSelectionStatus == DBSELECTIONEMPTY
	ENDIF
	VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
	lRetVal := VoDbEof()
	__DBSSetSelect( dwCurrentWorkArea )
	RETURN lRetVal
	

ACCESS ErrInfo AS Error
	// returns an Error object if last operation generated an error.
	IF lErrorFlag
		RETURN oErrorInfo
	ENDIF
	RETURN NULL_OBJECT

ACCESS ErrorInfo() AS Error
	
	// This returns ErrorInfo regardless of the ErrortFlag
    RETURN SELF:oErrorInfo
	

ACCESS FCount AS DWORD
	// ACCESS: like FCount( )
	RETURN SELF:wFieldCount
	

ACCESS FieldDesc AS ARRAY
	
	LOCAL   aRet AS ARRAY
	aRet := {}
	
	RETURN aRet
	

ACCESS FileSpec AS FileSpec
	RETURN oFileSpec
	

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
	

ACCESS Found AS LOGIC
	//SE-060601
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
	

ACCESS Header AS DWORD
	//SE-060601
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

ACCESS IndexExt AS STRING
	//SE-060601
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
	

ACCESS IndexList AS ARRAY
	LOCAL aRet  AS ARRAY
	aRet := {}
	RETURN aRet
	

ACCESS LastRec AS LONG
	//SE-060601
	LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL liRecno AS LONGINT
	
	
	VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
	liRecno := VoDbLastRec()
	__DBSSetSelect( dwCurrentWorkArea )
	RETURN liRecno

ACCESS Lupdate AS DATE
	//SE-060601
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
	
ACCESS MemoExt AS STRING
	IF SELF:Used
		RETURN SELF:Info(DBI_MEMOEXT)
	ENDIF
    RETURN ""

ACCESS Name AS STRING
	RETURN SUPER:Name
	
ACCESS OleExt AS STRING
	
    RETURN "DFL"	

ACCESS OrderBottomScope AS USUAL
	//SE-060601
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
	//PP-040416 uRetVal was LOGIC, should be USUAL
	//SE-060601
	LOCAL dwCurrentWorkArea := 0 AS DWORD
	//LOCAL uRetVal := NIL AS USUAL
	LOCAL oError        AS USUAL
	LOCAL n             AS DWORD
	

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		//uRetVal := OrdScope(BottomScope,uValue)
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
		//uRetVal:=NIL
	END SEQUENCE
	
	RETURN 
/*

This conflicts with METHOD OrderKeyNo !!!
	

ACCESS OrderKeyNo 
	//SE-060601
	LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL uRetVal := NIL AS USUAL
	LOCAL oError        AS USUAL
	
	
	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		//iRetVal := (OrdKeyNo()
		IF ! VoDbOrderInfo(DBOI_POSITION, "", NIL, REF uRetVal)
			BREAK ErrorBuild(_VoDbErrInfoPtr())
		ENDIF
		__DBSSetSelect( dwCurrentWorkArea )
	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oErrorInfo, #OrderKeyNo )
		uRetVal:=0
	END SEQUENCE
	
	RETURN uRetVal
*/	
	
	
	
//	RvdH 070323 This does not set the keyno, but retrieves the KeyVal...
// ASSIGN OrderKeyNo(uKeyValue) CLASS DbServer
// 	LOCAL uRetVal := NIL AS USUAL
// 	LOCAL oError        AS USUAL
// 	LOCAL dwCurrentWorkArea  AS DWORD
// 	
// 	lErrorFlag := FALSE
// 	BEGIN SEQUENCE
// 		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
// 		IF !SELF:Notify( NOTIFYINTENTTOMOVE )
// 			BREAK DbError{ SELF, #OrderKeyNo, 999, VO_SPrintF(__CAVOSTR_DBFCLASS_INTENTTOMOVE)}
// 		ENDIF
// 		//if !OrdKeyVal(uKeyValue)
// 		//  break ErrorBuild(_VoDbErrInfoPtr())
// 		//endif
// 		//VoDbOrderInfo(DBOI_KEYVAL, "", NIL, REF uRetVal)
// 		SELF:OrderKeyGoTo(nKeyPos)
// 		
// 		SELF:__ProcessConcurrency(TRUE)
// 		
// 		SELF:Notify( NOTIFYRECORDCHANGE )
// 		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527
// 	RECOVER USING oError
// 		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527
// 		oErrorInfo := oError
// 		SELF:Error( oErrorInfo, #OrderKeyNo )
// 		uRetVal:=0
//	END SEQUENCE
//	RETURN uRetVal                              
/*

This conflicts with METHOD OrderKeyNo !!!
	
ASSIGN OrderKeyNo(nKeyPos) 
	SELF:OrderKeyGoTo(nKeyPos)
	RETURN 
	
*/	

ACCESS OrderKeyVal  AS USUAL
	//SE-060601
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
	

ACCESS OrderTopScope  AS USUAL
	//SE-060601
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
	//PP-040416 uRetVal was LOGIC, should be USUAL
	//SE-060601
	LOCAL dwCurrentWorkArea := 0 AS DWORD
	//LOCAL uRetVal := NIL AS USUAL
	LOCAL oError        AS USUAL
	LOCAL n             AS DWORD
	
	
	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		//uRetVal := OrdScope(TopScope,uValue)
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
		//uRetVal:=NIL
	END SEQUENCE
	RETURN 
	

ACCESS  PaintedStructure AS ARRAY
	
	LOCAL aRet       AS ARRAY
	LOCAL aFDesc     AS ARRAY
	LOCAL i,nField   AS DWORD
	
	aFDesc := SELF:FieldDesc
	nField := ALen(aFDesc)
	
	aRet := {}
	
	FOR i:=1 UPTO nField
        LOCAL oFs as FieldSpec
        oFs := aFDesc[i][DBC_FIELDSPEC]
		AAdd( aRet, { aFDesc[i][DBC_NAME] , oFs:ValType, oFs:Length , oFs:Decimals } )
	NEXT
	
	RETURN aRet
	

ACCESS RddName  AS STRING
	RETURN cRDDName
	

ACCESS Rdds AS ARRAY
	RETURN aRdds
	

ACCESS ReadOnly AS LOGIC
	RETURN lReadOnly
	

ACCESS RecCount  AS LONG
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
				__DBSSetSelect( dwCurrentWorkArea )  //SE-060527
				siSelectionStatus       := siCurrentSelectionStatus
			ENDIF
		ELSE
			VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
			iRetVal := VoDbLastRec()
			__DBSSetSelect( dwCurrentWorkArea )
		ENDIF
		
	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527
		SELF:Error( oErrorInfo, #RECCOUNT )
	END SEQUENCE
	
	
	RETURN iRetVal
	

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
	

ACCESS RecSize AS DWORD
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
	

ACCESS Retries AS DWORD
	RETURN SELF:nRetries
	
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

ASSIGN Retries  (n AS DWORD) 
	IF IsNumeric(n) .AND. n > 0
		SELF:nRetries := n
	ENDIF
	RETURN 
	

ACCESS RLockList AS ARRAY
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
	
	

ACCESS Scope AS USUAL
	RETURN uStoredScope
	
ACCESS SelectionWorkArea AS DWORD
	
    RETURN SELF:wSelectionWorkArea	

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
	

ACCESS Shared AS LOGIC
	RETURN lShared
	

ACCESS Status AS HyperLabel
	IF ( lErrorFlag )
		RETURN oHLStatus
	ENDIF
	RETURN NULL_OBJECT
	
ASSIGN Status(oHl AS HyperLabel) 
	SUPER:Status := oHl
	SELF:lErrorFlag := TRUE

    RETURN  

ACCESS TableExt AS STRING
	
    RETURN SELF:Info(DBI_TABLEEXT)

ACCESS Used AS LOGIC
	LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL lRetVal AS LOGIC
	
	IF SELF:wWorkArea == 0
		RETURN FALSE
	ENDIF
	VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
	lRetVal := Used()
	__DBSSetSelect( dwCurrentWorkArea )
	RETURN lRetVal
	

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
	

ACCESS WorkArea AS DWORD
	RETURN wWorkArea
END CLASS

