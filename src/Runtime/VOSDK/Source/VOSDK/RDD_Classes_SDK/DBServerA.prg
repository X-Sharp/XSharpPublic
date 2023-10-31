#translate DBFDebug(<c1> [, <cn>]) =>



PARTIAL CLASS DbServer


 /// <exclude />
PROPERTY __FileSpec as FileSpec GET SELF:oFileSpec


/// <include file="Rdd.xml" path="doc/DbServer.Alias/*" />
ACCESS Alias
		#IFDEF __DEBUG__
			DBFDebug(__ENTITY__, Symbol2String( symAlias ))
		#ENDIF
       IF ! SELF:Used
            RETURN ""
        ENDIF
		RETURN Symbol2String( symAlias )




/// <include file="Rdd.xml" path="doc/DbServer.AliasSym/*" />
ACCESS AliasSym
		#IFDEF __DEBUG__
			DBFDebug(__ENTITY__, Symbol2String( symAlias ))
		#ENDIF
        IF ! SELF:Used
            RETURN NULL_SYMBOL
        ENDIF
		RETURN symAlias




/// <include file="Rdd.xml" path="doc/DbServer.BoF/*" />
ACCESS BoF
		//SE-060601
		LOCAL dwCurrentWorkArea AS DWORD
        LOCAL lRetVal AS LOGIC
        IF ! SELF:Used
            RETURN FALSE
        ENDIF



		IF lSelectionActive
			RETURN siSelectionStatus == DBSELECTIONBOF .OR. siSelectionStatus == DBSELECTIONEMPTY
		ENDIF
		VODBSelect( wWorkArea, out dwCurrentWorkArea )
		lRetVal := VODBBof()
		__DBSSetSelect( dwCurrentWorkArea )
		#IFDEF __DEBUG__
			DBFDebug(__ENTITY__, AsString(lRetVal))
		#ENDIF
		RETURN lRetVal




/// <include file="Rdd.xml" path="doc/DbServer.ConcurrencyControl/*" />
ACCESS ConcurrencyControl
		#IFDEF __DEBUG__
			DBFDebug(__ENTITY__, AsString(SELF:nCCMode))
		#ENDIF
		RETURN SELF:nCCMode


/// <include file="Rdd.xml" path="doc/DbServer.ConcurrencyControl/*" />
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
			VoDbSelect( wWorkArea, out dwCurrentWorkArea )
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




/// <include file="Rdd.xml" path="doc/DbServer.DBStruct/*" />
ACCESS DBStruct    // dcaton 070307 changed case to match overridden parent property

    IF ! SELF:Used
        RETURN {}
    ENDIF

	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF
	IF ALen(aStruct) == 0
		SELF:Error( __MakeErrObj(0), #DBSTRUCT )
	ENDIF


	RETURN AClone(aStruct)


/// <include file="Rdd.xml" path="doc/DbServer.DbStructure/*" />
ACCESS DbStructure
	// DHer: 18/12/2008
	// This returns the original structure array
    IF ! SELF:Used
        RETURN {}
    ENDIF
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




/// <include file="Rdd.xml" path="doc/DbServer.Deleted/*" />
ACCESS Deleted
	//SE-060601
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL uRetVal           AS USUAL
	LOCAL oError            AS USUAL
    IF ! SELF:Used
        RETURN FALSE
    ENDIF


	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, out dwCurrentWorkArea )
		uRetVal := VoDbDeleted()


	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		uRetVal := FALSE
	END SEQUENCE


	__DBSSetSelect( dwCurrentWorkArea )




	RETURN uRetVal




/// <include file="Rdd.xml" path="doc/DbServer.Driver/*" />
ACCESS Driver
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF
    IF ! SELF:Used
        RETURN ""
    ENDIF


	RETURN SELF:cRDDName




/// <include file="Rdd.xml" path="doc/DbServer.EoF/*" />
ACCESS EoF
	//SE-060601
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL lRetVal AS LOGIC
    IF ! SELF:Used
        RETURN FALSE
    ENDIF




	IF lSelectionActive
		RETURN siSelectionStatus == DBSELECTIONEOF .OR. siSelectionStatus == DBSELECTIONEMPTY
	ENDIF
	VoDbSelect( wWorkArea, out dwCurrentWorkArea )
	lRetVal := VoDbEof()
	__DBSSetSelect( dwCurrentWorkArea )
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__, AsString(lRetVal))
	#ENDIF
	RETURN lRetVal




/// <include file="Rdd.xml" path="doc/DbServer.ErrInfo/*" />
ACCESS ErrInfo
	// returns an Error object if last operation generated an error.
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF
	IF lErrorFlag
		RETURN oErrorInfo
	ENDIF
	RETURN NULL_OBJECT


/// <include file="Rdd.xml" path="doc/DbServer.ErrorInfo/*" />
ACCESS ErrorInfo()


	// DHer: 18/12/2008
	// This returns ErrorInfo regardless of the ErrortFlag
RETURN SELF:oErrorInfo




/// <include file="Rdd.xml" path="doc/DbServer.FCount/*" />
ACCESS FCount
	// ACCESS: like FCount( )
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__, AsString(SELF:wFieldCount))
    #ENDIF
	RETURN SELF:wFieldCount




/// <include file="Rdd.xml" path="doc/DbServer.FieldDesc/*" />
ACCESS FieldDesc

	LOCAL   aRet AS ARRAY
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF
	aRet := {}


	RETURN aRet




/// <include file="Rdd.xml" path="doc/DbServer.FileSpec/*" />
ACCESS FileSpec
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__, AsString(oFileSpec))
	#ENDIF
	RETURN oFileSpec




/// <include file="Rdd.xml" path="doc/DbServer.Filter/*" />
ACCESS Filter
	//SE-060601
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL oError            AS USUAL
	LOCAL uInfo             AS USUAL
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF
    IF ! SELF:Used
        RETURN ""
    ENDIF

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, out dwCurrentWorkArea )
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
ASSIGN Filter( uFilterBlock )
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__, AsString(uFilterBlock))
    #ENDIF
	SELF:SetFilter( uFilterBlock )

	RETURN




/// <include file="Rdd.xml" path="doc/DbServer.ForBlock/*" />
ACCESS ForBlock
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
    #ENDIF

	RETURN cbStoredForBlock




/// <include file="Rdd.xml" path="doc/DbServer.ForBlock/*" />
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




/// <include file="Rdd.xml" path="doc/DbServer.Found/*" />
ACCESS Found
	//SE-060601
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL oError            AS USUAL
	LOCAL lRetCode          AS LOGIC
    IF ! SELF:Used
        RETURN FALSE
    ENDIF
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF
	IF lSelectionActive
		RETURN siSelectionStatus == DBSELECTIONFOUND
	ENDIF




	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, out dwCurrentWorkArea )
		lRetCode:=VoDbFound()
	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		lRetCode := FALSE
	END SEQUENCE


	__DBSSetSelect( dwCurrentWorkArea )




	RETURN lRetCode




/// <include file="Rdd.xml" path="doc/DbServer.Header/*" />
ACCESS Header
	//SE-060601
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL oError            AS USUAL
	LOCAL uInfo             AS USUAL
    IF ! SELF:Used
        RETURN 0
    ENDIF


	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF


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


/// <include file="Rdd.xml" path="doc/DbServer.IndexExt/*" />
ACCESS IndexExt
	//SE-060601
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL oError            AS USUAL
	LOCAL uOrdVal           AS USUAL
    IF ! SELF:Used
        RETURN ""
    ENDIF


	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF


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




/// <include file="Rdd.xml" path="doc/DbServer.IndexList/*" />
ACCESS IndexList
    LOCAL aRet  AS ARRAY
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF
	aRet := {}
	RETURN aRet




/// <include file="Rdd.xml" path="doc/DbServer.LastRec/*" />
ACCESS LastRec
	//SE-060601
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL liRecno AS LONGINT
    IF ! SELF:Used
        RETURN 0
    ENDIF

	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF


    VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
    liRecno := VoDbLastRec()
	__DBSSetSelect( dwCurrentWorkArea )
	RETURN liRecno


/// <include file="Rdd.xml" path="doc/DbServer.Lupdate/*" />
ACCESS Lupdate
	//SE-060601
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL oError            AS USUAL
	LOCAL uInfo             AS USUAL
    IF ! SELF:Used
        RETURN NULL_DATE
    ENDIF


	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF


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


/// <include file="Rdd.xml" path="doc/DbServer.MemoExt/*" />
ACCESS MemoExt
	// DHer: 18/12/2008
	IF SELF:Used
		RETURN SELF:Info(DBI_MEMOEXT)
	ENDIF
RETURN ""


/// <include file="Rdd.xml" path="doc/DbServer.Name/*" />
ACCESS Name
	//  01/01/2000
	//  12/23/1999 Reactivated
	//  RETURN SUPER:Name()
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__, AsString(SUPER:Name))
	#ENDIF
	RETURN SUPER:Name


/// <include file="Rdd.xml" path="doc/DbServer.OleExt/*" />
ACCESS OleExt
	// DHer: 18/12/2008
RETURN "DFL"


/// <include file="Rdd.xml" path="doc/DbServer.OrderBottomScope/*" />
ACCESS OrderBottomScope
	//SE-060601
	LOCAL dwCurrentWorkArea AS DWORD
    LOCAL uRetVal := NIL AS USUAL
	LOCAL oError        AS USUAL

    IF ! SELF:Used
        RETURN uRetVal
    ENDIF

	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF


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
		VoDbSelect( wWorkArea, out dwCurrentWorkArea )
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




/// <include file="Rdd.xml" path="doc/DbServer.OrderKeyNo/*" />
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
		VODBSelect( wWorkArea, out dwCurrentWorkArea )
		//iRetVal := (OrdKeyNo()
		IF ! VODBOrderInfo(DBOI_POSITION, "", NIL, REF uRetVal)
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
// 		VODBSelect( wWorkArea, out dwCurrentWorkArea )
// 		IF !SELF:Notify( NOTIFYINTENTTOMOVE )
// 			BREAK DbError{ SELF, #OrderKeyNo, 999, VO_SPrintF(__CAVOSTR_DBFCLASS_INTENTTOMOVE)}
// 		ENDIF
// 		//if !OrdKeyVal(uKeyValue)
// 		//  break ErrorBuild(_VODBErrInfoPtr())
// 		//endif
// 		//VODBOrderInfo(DBOI_KEYVAL, "", NIL, REF uRetVal)
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


/// <include file="Rdd.xml" path="doc/DbServer.OrderKeyNo/*" />
ASSIGN OrderKeyNo(nKeyPos)
	SELF:OrderKeyGoTo(nKeyPos)
	RETURN


*/


/// <include file="Rdd.xml" path="doc/DbServer.OrderKeyVal/*" />
ACCESS OrderKeyVal
	//SE-060601
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL uRetVal := NIL AS USUAL
	LOCAL oError        AS USUAL
    IF ! SELF:Used
        RETURN uRetVal
    ENDIF

	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF


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




/// <include file="Rdd.xml" path="doc/DbServer.OrderTopScope/*" />
ACCESS OrderTopScope
	//SE-060601
	LOCAL dwCurrentWorkArea AS DWORD
    LOCAL uRetVal := NIL AS USUAL
	LOCAL oError        AS USUAL
    IF ! SELF:Used
        RETURN uRetVal
    ENDIF

	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF


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
		VoDbSelect( wWorkArea, out dwCurrentWorkArea )
		//uRetVal := OrdScope(TopScope,uValue)
		n := DBOI_SCOPETOP
		IF IsNil(uValue)
			n := DBOI_SCOPETOPCLEAR
		ENDIF


		IF ! VODBOrderInfo(n, "", NIL, REF uValue)
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




/// <include file="Rdd.xml" path="doc/DbServer.PaintedStructure/*" />
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




/// <include file="Rdd.xml" path="doc/DbServer.RddName/*" />
ACCESS RddName
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__, cRDDName)
	#ENDIF
    IF ! SELF:Used
        RETURN ""
    ENDIF
	RETURN cRDDName




/// <include file="Rdd.xml" path="doc/DbServer.Rdds/*" />
ACCESS Rdds
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__, AsString(aRdds))
	#ENDIF
    RETURN aRdds




/// <include file="Rdd.xml" path="doc/DbServer.ReadOnly/*" />
ACCESS ReadOnly
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__, AsString(lReadOnly))
	#ENDIF
	RETURN lReadOnly




/// <include file="Rdd.xml" path="doc/DbServer.RecCount/*" />
ACCESS RecCount
	LOCAL nCurrentRecord            AS DWORD
	LOCAL siCurrentSelectionStatus  AS SHORTINT
	LOCAL iRetVal                   AS INT
	LOCAL dwCurrentWorkArea          AS DWORD
	LOCAL oError                    AS USUAL
    IF ! SELF:Used
        RETURN 0
    ENDIF


	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF


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




/// <include file="Rdd.xml" path="doc/DbServer.RecNo/*" />
ACCESS RecNo
	//SE-060601
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL wRetCode      AS DWORD
	LOCAL oError        AS USUAL

    IF ! SELF:Used
        RETURN 0
    ENDIF

	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF


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




	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__, AsString(wRetCode))
	#ENDIF
	RETURN wRetCode




/// <include file="Rdd.xml" path="doc/DbServer.RecNo/*" />
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




/// <include file="Rdd.xml" path="doc/DbServer.RecSize/*" />
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






/// <include file="Rdd.xml" path="doc/DbServer.RelationChildren/*" />
ACCESS RelationChildren
	// DHer: 18/12/2008
RETURN SELF:aRelationChildren


/// <include file="Rdd.xml" path="doc/DbServer.RelationChildren/*" />
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


/// <include file="Rdd.xml" path="doc/DbServer.Retries/*" />
ACCESS Retries
	//  UH 01/05/2000
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
    #ENDIF
	RETURN SELF:nRetries


/// <include file="Rdd.xml" path="doc/DbServer.Retries/*" />
ASSIGN Retries  (n)
	//  UH 01/05/2000
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__, AsString(n))
	#ENDIF
	IF IsNumeric(n) .AND. n > 0
		SELF:nRetries := n
	ENDIF
	RETURN SELF:nRetries




/// <include file="Rdd.xml" path="doc/DbServer.RLockList/*" />
ACCESS RLockList
	//SE-060601
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL oError            AS USUAL
	LOCAL aLockList := { }  AS ARRAY
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF
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






/// <include file="Rdd.xml" path="doc/DbServer.Scope/*" />
ACCESS Scope
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF
	RETURN uStoredScope


/// <include file="Rdd.xml" path="doc/DbServer.SelectionWorkArea/*" />
ACCESS SelectionWorkArea
	// DHer: 18/12/2008
RETURN SELF:wSelectionWorkArea


/// <include file="Rdd.xml" path="doc/DbServer.Scope/*" />
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




/// <include file="Rdd.xml" path="doc/DbServer.Shared/*" />
ACCESS Shared
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF
	RETURN lShared




/// <include file="Rdd.xml" path="doc/DbServer.Status/*" />
ACCESS Status
	// UH 08/30/1999
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF
	IF ( lErrorFlag )
		RETURN oHLStatus
	ENDIF
	RETURN NULL_OBJECT


/// <include file="Rdd.xml" path="doc/DbServer.Status/*" />
ASSIGN Status(oHl)
	// DHer: 18/12/2008
	SUPER:Status := oHl
	SELF:lErrorFlag := TRUE


RETURN SELF:oHLStatus


/// <include file="Rdd.xml" path="doc/DbServer.TableExt/*" />
ACCESS TableExt
	// DHer: 18/12/2008
    IF ! SELF:Used
        RETURN ""
    ENDIF
RETURN SELF:Info(DBI_TABLEEXT)


/// <include file="Rdd.xml" path="doc/DbServer.Used/*" />
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
	VoDbSelect( wWorkArea, out dwCurrentWorkArea )
	lRetVal := Used()
	__DBSSetSelect( dwCurrentWorkArea )
	RETURN lRetVal




/// <include file="Rdd.xml" path="doc/DbServer.WhileBlock/*" />
ACCESS WhileBlock
	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__)
	#ENDIF
	RETURN cbStoredWhileBlock




/// <include file="Rdd.xml" path="doc/DbServer.WhileBlock/*" />
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




/// <include file="Rdd.xml" path="doc/DbServer.WorkArea/*" />
ACCESS WorkArea
	//SE-060527
	RETURN wWorkArea
END CLASS


