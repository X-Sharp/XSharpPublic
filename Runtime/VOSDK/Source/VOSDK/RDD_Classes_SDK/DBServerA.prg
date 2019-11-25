#translate DBFDebug(<c1> [, <cn>]) =>

PARTIAL CLASS DbServer

PROPERTY __FileSpec as FileSpec GET SELF:oFileSpec

ACCESS Alias 
		#IFDEF __DEBUG__
			DBFDebug(__ENTITY__, Symbol2String( symAlias ))
		#ENDIF
		RETURN Symbol2String( symAlias )
	

ACCESS AliasSym 
		#IFDEF __DEBUG__
			DBFDebug(__ENTITY__, Symbol2String( symAlias ))
		#ENDIF
		RETURN symAlias
	

ACCESS BoF 
		//SE-060601
		LOCAL dwCurrentWorkArea AS DWORD
		LOCAL lRetVal AS LOGIC
		
		IF lSelectionActive
			RETURN siSelectionStatus == DBSELECTIONBOF .OR. siSelectionStatus == DBSELECTIONEMPTY
		ENDIF
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		lRetVal := VODBBof()
		__DBSSetSelect( dwCurrentWorkArea )
		#IFDEF __DEBUG__
			DBFDebug(__ENTITY__, AsString(lRetVal))
		#ENDIF
		RETURN lRetVal
	

ACCESS ConcurrencyControl 
		#IFDEF __DEBUG__
			DBFDebug(__ENTITY__, AsString(SELF:nCCMode))
		#ENDIF
		RETURN SELF:nCCMode

ASSIGN ConcurrencyControl( nMode) 
	LOCAL newMode := nMode
	LOCAL dwCurrentWorkArea  AS DWORD
	LOCAL oError            AS USUAL
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__, AsString(nMode))
	#ENDIF
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
			VODBSelect( wWorkArea, @dwCurrentWorkArea )
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
	
	RETURN SELF:nCCMode
	

ACCESS DBStruct    // dcaton 070307 changed case to match overridden parent property
	
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF
	IF ALen(aStruct) == 0
		SELF:Error( __MakeErrObj(0), #DBSTRUCT )
	ENDIF
	
	RETURN AClone(aStruct)

ACCESS DbStructure 
	// DHer: 18/12/2008
	// This returns the original structure array
RETURN SELF:aStruct
	
	// UH 09/13/1999
	/*ACCESS Name CLASS DBServer
	LOCAL xRet   AS USUAL
	LOCAL nPos   AS INT
	
	nPos := SELF:FieldPos(#NAME)
	
	IF nPos > 0
	xRet := SELF:FIELDGET(nPos)
	ELSE
	xRet := SUPER:Name
	ENDIF
	
	RETURN xRet
	
	*/
	

ACCESS Deleted 
	//SE-060601
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL uRetVal           AS USUAL
	LOCAL oError            AS USUAL
	
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF
	
	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		uRetVal := VODBDeleted()
		
	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		uRetVal := FALSE
	END SEQUENCE
	
	__DBSSetSelect( dwCurrentWorkArea )
	
	
	RETURN uRetVal
	

ACCESS Driver 
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF
	
	RETURN SELF:cRDDName
	

ACCESS EoF 
	//SE-060601
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL lRetVal AS LOGIC
	
	
	IF lSelectionActive
		RETURN siSelectionStatus == DBSELECTIONEOF .OR. siSelectionStatus == DBSELECTIONEMPTY
	ENDIF
	VODBSelect( wWorkArea, @dwCurrentWorkArea )
	lRetVal := VODBEof()
	__DBSSetSelect( dwCurrentWorkArea )
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__, AsString(lRetVal))
	#ENDIF
	RETURN lRetVal
	

ACCESS ErrInfo 
	// returns an Error object if last operation generated an error.
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF
	IF lErrorFlag
		RETURN oErrorInfo
	ENDIF
	RETURN NULL_OBJECT

ACCESS ErrorInfo() 
	
	// DHer: 18/12/2008
	// This returns ErrorInfo regardless of the ErrortFlag
RETURN SELF:oErrorInfo
	

ACCESS FCount 
	// ACCESS: like FCount( )
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__, AsString(SELF:wFieldCount))
	#ENDIF
	RETURN SELF:wFieldCount
	

ACCESS FieldDesc 
	
	LOCAL   aRet AS ARRAY
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF
	aRet := {}
	
	RETURN aRet
	

ACCESS FileSpec 
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__, AsString(oFileSpec))
	#ENDIF
	RETURN oFileSpec
	

ACCESS Filter 
	//SE-060601
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL oError            AS USUAL
	LOCAL uInfo             AS USUAL
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF
	
	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		IF !VODBInfo(DBI_DBFILTER, @uInfo)
			BREAK ErrorBuild(_VODBErrInfoPtr())
		ENDIF
		__DBSSetSelect( dwCurrentWorkArea )
	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oErrorInfo, #Filter )
	END SEQUENCE
	
	
	RETURN uInfo
	

ASSIGN Filter( uFilterBlock ) 
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__, AsString(uFilterBlock))
	#ENDIF
	SELF:SetFilter( uFilterBlock )
	
	RETURN 
	

ACCESS ForBlock 
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF
	RETURN cbStoredForBlock
	

ASSIGN ForBlock( cbForBlock ) 
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__, AsString(cbForBlock))
	#ENDIF
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
	

ACCESS Found 
	//SE-060601
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL oError            AS USUAL
	LOCAL lRetCode          AS LOGIC
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF
	IF lSelectionActive
		RETURN siSelectionStatus == DBSELECTIONFOUND
	ENDIF
	
	
	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		lRetCode:=VODBFound()
	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		lRetCode := FALSE
	END SEQUENCE
	
	__DBSSetSelect( dwCurrentWorkArea )
	
	
	RETURN lRetCode
	

ACCESS Header 
	//SE-060601
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL oError            AS USUAL
	LOCAL uInfo             AS USUAL
	
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF
	
	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		IF ! VODBInfo(DBI_GETHEADERSIZE, @uInfo)
			BREAK ErrorBuild(_VODBErrInfoPtr())
		ENDIF
		__DBSSetSelect( dwCurrentWorkArea )
	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oErrorInfo, #Info )
	END SEQUENCE
	
	
	RETURN uInfo

ACCESS IndexExt 
	//SE-060601
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL oError            AS USUAL
	LOCAL uOrdVal           AS USUAL
	
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF
	
	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		IF ! VODBOrderInfo(DBOI_BAGEXT, "", NIL, @uOrdVal)
			BREAK ErrorBuild(_VODBErrInfoPtr())
		ENDIF
		__DBSSetSelect( dwCurrentWorkArea )
	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oErrorInfo, #OrderInfo )
	END SEQUENCE
	
	
	RETURN uOrdVal
	

ACCESS IndexList 
	LOCAL aRet  AS ARRAY
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF
	aRet := {}
	RETURN aRet
	

ACCESS LastRec 
	//SE-060601
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL liRecno AS LONGINT
	
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF
	
	VODBSelect( wWorkArea, @dwCurrentWorkArea )
	liRecno := VODBLastRec()
	__DBSSetSelect( dwCurrentWorkArea )
	RETURN liRecno

ACCESS Lupdate 
	//SE-060601
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL oError            AS USUAL
	LOCAL uInfo             AS USUAL
	
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF
	
	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		IF ! VODBInfo(DBI_LASTUPDATE, @uInfo)
			BREAK ErrorBuild(_VODBErrInfoPtr())
		ENDIF
		__DBSSetSelect( dwCurrentWorkArea )
	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oErrorInfo, #Info )
	END SEQUENCE
	
	
	RETURN uInfo
	
ACCESS MemoExt 
	// DHer: 18/12/2008
	IF SELF:Used
		RETURN SELF:Info(DBI_MEMOEXT)
	ENDIF
RETURN ""

ACCESS Name 
	//  01/01/2000
	//  12/23/1999 Reactivated
	//  RETURN SUPER:Name()
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__, AsString(SUPER:Name))
	#ENDIF
	RETURN SUPER:Name
	
ACCESS OleExt 
	// DHer: 18/12/2008
RETURN "DFL"	

ACCESS OrderBottomScope 
	//SE-060601
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL uRetVal       AS USUAL
	LOCAL oError        AS USUAL
	
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF
	
	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		IF ! VODBOrderInfo(DBOI_SCOPEBOTTOM, "", NIL, @uRetVal)
			BREAK ErrorBuild(_VODBErrInfoPtr())
		ENDIF
		__DBSSetSelect( dwCurrentWorkArea )
	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oErrorInfo, #OrderBottomScope )
		uRetVal:=NIL
	END SEQUENCE
	
	RETURN uRetVal
	

ASSIGN OrderBottomScope(uValue) 
	//PP-040416 uRetVal was LOGIC, should be USUAL
	//SE-060601
	LOCAL dwCurrentWorkArea AS DWORD
	//LOCAL uRetVal       AS USUAL
	LOCAL oError        AS USUAL
	LOCAL n             AS DWORD
	
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__, AsString(uValue))
	#ENDIF
	
	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		//uRetVal := OrdScope(BottomScope,uValue)
		n := DBOI_SCOPEBOTTOM
		IF IsNil(uValue)
			n := DBOI_SCOPEBOTTOMCLEAR
		ENDIF
		
		IF ! VODBOrderInfo(n, "", NIL, @uValue)
			BREAK ErrorBuild(_VODBErrInfoPtr())
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
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL uRetVal       AS USUAL
	LOCAL oError        AS USUAL
	
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF
	
	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		//iRetVal := (OrdKeyNo()
		IF ! VODBOrderInfo(DBOI_POSITION, "", NIL, @uRetVal)
			BREAK ErrorBuild(_VODBErrInfoPtr())
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
// 	LOCAL uRetVal       AS USUAL
// 	LOCAL oError        AS USUAL
// 	LOCAL dwCurrentWorkArea  AS DWORD
// 	#IFDEF __DEBUG__
// 		DBFDebug(__ENTITY__,AsString(uKeyValue))
// 	#ENDIF
// 	
// 	lErrorFlag := FALSE
// 	BEGIN SEQUENCE
// 		VODBSelect( wWorkArea, @dwCurrentWorkArea )
// 		IF !SELF:Notify( NOTIFYINTENTTOMOVE )
// 			BREAK DbError{ SELF, #OrderKeyNo, 999, VO_SPrintF(__CAVOSTR_DBFCLASS_INTENTTOMOVE)}
// 		ENDIF
// 		//if !OrdKeyVal(uKeyValue)
// 		//  break ErrorBuild(_VODBErrInfoPtr())
// 		//endif
// 		//VODBOrderInfo(DBOI_KEYVAL, "", NIL, @uRetVal)
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

ACCESS OrderKeyVal 
	//SE-060601
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL uRetVal       AS USUAL
	LOCAL oError        AS USUAL
	
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF
	
	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		//uRetVal := OrdKeyVal()
		IF ! VODBOrderInfo(DBOI_KEYVAL, "", NIL, @uRetVal)
			BREAK ErrorBuild(_VODBErrInfoPtr())
		ENDIF
		__DBSSetSelect( dwCurrentWorkArea )
	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oErrorInfo, #OrderKeyVal )
		uRetVal:=NIL
	END SEQUENCE
	
	RETURN uRetVal
	

ACCESS OrderTopScope 
	//SE-060601
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL uRetVal       AS USUAL
	LOCAL oError        AS USUAL
	
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF
	
	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		//uRetVal := DBOrderInfo(DBOI_Scopetop)
		IF ! VODBOrderInfo(DBOI_SCOPETOP, "", NIL, @uRetVal)
			BREAK ErrorBuild(_VODBErrInfoPtr())
		ENDIF
		__DBSSetSelect( dwCurrentWorkArea )
	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oErrorInfo, #OrderTopScope )
		uRetVal:=NIL
	END SEQUENCE
	
	RETURN uRetVal
	
	

ASSIGN OrderTopScope(uValue) 
	//PP-040416 uRetVal was LOGIC, should be USUAL
	//SE-060601
	LOCAL dwCurrentWorkArea AS DWORD
	//LOCAL uRetVal       AS USUAL
	LOCAL oError        AS USUAL
	LOCAL n             AS DWORD
	
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__,AsString(uValue))
	#ENDIF
	
	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		//uRetVal := OrdScope(TopScope,uValue)
		n := DBOI_SCOPETOP
		IF IsNil(uValue)
			n := DBOI_SCOPETOPCLEAR
		ENDIF
		
		IF ! VODBOrderInfo(n, "", NIL, @uValue)
			BREAK ErrorBuild(_VODBErrInfoPtr())
		ENDIF
		__DBSSetSelect( dwCurrentWorkArea )
	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oErrorInfo, #OrderTopScope )
		//uRetVal:=NIL
	END SEQUENCE
	RETURN 
	

ACCESS  PaintedStructure 
	
	LOCAL aRet       AS ARRAY
	LOCAL aFDesc     AS ARRAY
	LOCAL i,nField   AS DWORD
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF
	
	aFDesc := SELF:FieldDesc
	nField := ALen(aFDesc)
	
	aRet := {}
	
	FOR i:=1 UPTO nField
        LOCAL oFs as FieldSpec
        oFs := aFDesc[i][DBC_FIELDSPEC]
		AAdd( aRet, { aFDesc[i][DBC_NAME] , oFS:valtype , oFS:length , oFS:Decimals } )
	NEXT
	
	RETURN aRet
	

ACCESS RddName 
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__, cRDDName)
	#ENDIF
	RETURN cRDDName
	

ACCESS Rdds 
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__, AsString(aRdds))
	#ENDIF
	RETURN aRdds
	

ACCESS ReadOnly 
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__, AsString(lReadOnly))
	#ENDIF
	RETURN lReadOnly
	

ACCESS RecCount  
	LOCAL nCurrentRecord            AS LONGINT
	LOCAL siCurrentSelectionStatus  AS SHORTINT
	LOCAL iRetVal                   AS INT
	LOCAL dwCurrentWorkArea          AS DWORD
	LOCAL oError                    AS USUAL
	
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF
	
	lErrorFlag := FALSE
	BEGIN SEQUENCE
		IF lSelectionActive
			IF siSelectionStatus == DBSELECTIONEMPTY
				iRetVal := 0
			ELSE
				VODBSelect( wWorkArea, @dwCurrentWorkArea )
				nCurrentRecord          := VODBRecno( )
				siCurrentSelectionStatus:= siSelectionStatus
				iRetVal                 := SELF:Count( )
				IF ! VODBGoTo( nCurrentRecord )
					BREAK ErrorBuild(_VODBErrInfoPtr())
				ENDIF
				__DBSSetSelect( dwCurrentWorkArea )  //SE-060527
				siSelectionStatus       := siCurrentSelectionStatus
			ENDIF
		ELSE
			VODBSelect( wWorkArea, @dwCurrentWorkArea )
			iRetVal := VODBLastRec()
			__DBSSetSelect( dwCurrentWorkArea )
		ENDIF
		
	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527
		SELF:Error( oErrorInfo, #RECCOUNT )
	END SEQUENCE
	
	
	RETURN iRetVal
	

ACCESS RecNo 
	//SE-060601
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL wRetCode      AS LONGINT
	LOCAL oError        AS USUAL
	
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF
	
	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		wRetCode := VODBRecno()
		__DBSSetSelect( dwCurrentWorkArea )
	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oErrorInfo, #RECNO )
	END SEQUENCE
	
	
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__, AsString(wRetCode))
	#ENDIF
	RETURN wRetCode
	

ASSIGN RecNo( nRecordNumber ) 
	LOCAL oError        AS USUAL
	
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__, AsString(nRecordNumber))
	#ENDIF
	
	lErrorFlag := FALSE
	BEGIN SEQUENCE
		SELF:GoTo( nRecordNumber )
	RECOVER USING oError
		oErrorInfo := oError
		SELF:Error( oErrorInfo, #RECNO )
	END SEQUENCE
	
	
	RETURN 
	

ACCESS RecSize 
	//SE-060601
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL oError        AS USUAL
	LOCAL uVoVal        AS USUAL
	
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF
	
	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		IF ! VODBRecordInfo(DBRI_RECSIZE, 0, @uVoVal)
			BREAK ErrorBuild(_VODBErrInfoPtr())
		ENDIF
		__DBSSetSelect( dwCurrentWorkArea )
	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oErrorInfo, #RECSIZE )
	END SEQUENCE
	
	
	RETURN uVoVal
	

ACCESS Retries 
	//  UH 01/05/2000
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF
	RETURN SELF:nRetries
	
ACCESS RelationChildren 
	// DHer: 18/12/2008
RETURN SELF:aRelationChildren

ASSIGN RelationChildren(aNewChildren) 
	// DHer: 18/12/2008
	IF IsArray(aNewChildren)
		SELF:aRelationChildren := aNewChildren
	ENDIF
	IF ALen(SELF:aRelationChildren)=0
		lRelationsActive := FALSE
	ELSE
		lRelationsActive := TRUE
	ENDIF

RETURN SELF:aRelationChildren

ASSIGN Retries  (n) 
	//  UH 01/05/2000
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__, AsString(n))
	#ENDIF
	IF IsNumeric(n) .AND. n > 0
		SELF:nRetries := n
	ENDIF
	RETURN SELF:nRetries
	

ACCESS RLockList 
	//SE-060601
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL oError            AS USUAL
	LOCAL aLockList := { }  AS ARRAY
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF
	
	
	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		aLockList := DBRLockList()
		__DBSSetSelect( dwCurrentWorkArea )
	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oErrorInfo, #RLockList )
	END SEQUENCE
	
	
	RETURN aLockList
	
	

ACCESS Scope 
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF
	RETURN uStoredScope
	
ACCESS SelectionWorkArea 
	// DHer: 18/12/2008
RETURN SELF:wSelectionWorkArea	

ASSIGN Scope( uScope ) 
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__, AsString(uScope))
	#ENDIF
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
	

ACCESS Shared 
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF
	RETURN lShared
	

ACCESS Status 
	// UH 08/30/1999
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF
	IF ( lErrorFlag )
		RETURN oHLStatus
	ENDIF
	RETURN NULL_OBJECT
	
ASSIGN Status(oHl) 
	// DHer: 18/12/2008
	SUPER:Status := oHl
	SELF:lErrorFlag := TRUE

RETURN SELF:oHLStatus

ACCESS TableExt 
	// DHer: 18/12/2008
RETURN SELF:Info(DBI_TABLEEXT)

ACCESS Used 
	//SE-060601
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL lRetVal AS LOGIC
	
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF
	IF SELF:wWorkArea == 0
		RETURN FALSE
	ENDIF
	VODBSelect( wWorkArea, @dwCurrentWorkArea )
	lRetVal := Used()
	__DBSSetSelect( dwCurrentWorkArea )
	RETURN lRetVal
	

ACCESS WhileBlock 
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF
	RETURN cbStoredWhileBlock
	

ASSIGN WhileBlock( cbWhileBlock ) 
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__, AsString(cbWhileBlock))
	#ENDIF
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
	

ACCESS WorkArea 
	//SE-060527
	RETURN wWorkArea
END CLASS

