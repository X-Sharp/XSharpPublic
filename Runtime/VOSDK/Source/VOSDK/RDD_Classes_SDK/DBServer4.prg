#translate DBFDebug(<c1> [, <cn>]) =>

PARTIAL CLASS DbServer

METHOD RDDINFO( kRDDInfoType, uRDDVal )
	//SE-060601
   LOCAL dwCurrentWorkArea AS DWORD
	LOCAL oError AS USUAL

	#IFDEF __DEBUG__
		DBFDebug( "Entering "+__ENTITY__, AsString(kRDDInfoType), AsString(uRDDVal))
	#ENDIF

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		IF ! VODBRDDInfo( kRDDInfoType, @uRDDVal )
			BREAK ErrorBuild( _VODBErrInfoPtr() )
		ENDIF
		__DBSSetSelect( dwCurrentWorkArea )
	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oErrorInfo, #RDDINFO )
	END SEQUENCE


	#IFDEF __DEBUG__
		DBFDebug( "Leaving "+__ENTITY__, AsString(uRDDVal) )
	#ENDIF
	RETURN uRDDVal

METHOD Recall( cbForBlock, cbWhileBlock, uScope )
	LOCAL nNextCount AS LONGINT
	LOCAL lRestOfFile AS LOGIC
	LOCAL lRetCode AS LOGIC
	LOCAL oError AS USUAL
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL nCurrRec AS LONGINT
	LOCAL lFLock AS LOGIC
	LOCAL uVoRet AS USUAL

	#IFDEF __DEBUG__
		DBFDebug("Entering "+ __ENTITY__ )
	#ENDIF

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		IF ! IsNil( cbForBlock ) .OR. ! IsNil( cbWhileBlock ) .OR. ! IsNil( uScope )
			IF SELF:Notify( NOTIFYINTENTTOMOVE )
				IF IsString( cbForBlock )
					cbForBlock := &( "{ || " + cbForBlock + " }" )
				ENDIF

				IF IsString( cbWhileBlock )
					cbWhileBlock := &( "{ || " + cbWhileBlock + " }" )
				ENDIF

				IF ! IsNil( uScope )
					IF IsNumeric( uScope )
						nNextCount := uScope
					ELSE
						lRestOfFile := uScope
					ENDIF
				ENDIF
				lRetCode := SELF:__DBServerEval( { || VODBRecall() },  ;
					cbForBlock,  ;
					cbWhileBlock,  ;
					nNextCount,  ;
					NIL,  ;
					lRestOfFile,  ;
					DBCCON,  ;
					DBCCUPDATE )
				SELF:Notify( NOTIFYFILECHANGE )
			ELSE
				lRetCode := FALSE
				SELF:__SetStatusHL( #Recall, __CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE_CAPTION ),  ;
					__CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE ) )
			ENDIF

		ELSEIF lActiveScope
			IF SELF:Notify( NOTIFYINTENTTOMOVE )
				lRetCode := SELF:__DBServerEval( { || VODBRecall() },  ;
					cbStoredForBlock,  ;
					cbStoredWhileBlock,  ;
					nStoredNextCount,  ;
					NIL,  ;
					lStoredRestOfFile,  ;
					DBCCON,  ;
					DBCCUPDATE )
				SELF:Notify( NOTIFYFILECHANGE )
			ELSE
				lRetCode := FALSE
				SELF:__SetStatusHL( #Recall, __CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE_CAPTION ),  ;
					__CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE ) )
			ENDIF

		ELSE
			VODBInfo( DBI_ISFLOCK, @uVoRet )
			IF nEffectiveCCMode == ccOptimistic .AND.  ;
				( nCurrRec := VODBRecno() ) <= VODBLastRec() .AND.  ;
				! ( lFLock := uVoRet )
				nCurrRec := VODBRecno()
				IF ! VODBRLock( nCurrRec )
					BREAK DbError{ NIL, #Recall, EG_LOCK, __CavoStr( __CAVOSTR_DBFCLASS_LOCKFAILED ) }
				ENDIF
				IF ! VODBRecall()
					BREAK ErrorBuild( _VODBErrInfoPtr() )
				ENDIF
				lRetCode := TRUE
				IF ! lFLock
					VODBUnlock( nCurrRec )
				ENDIF
			ELSE
				IF ! VODBRecall()
					BREAK ErrorBuild( _VODBErrInfoPtr() )
				ENDIF
				lRetCode := TRUE
			ENDIF
			SELF:Notify( NOTIFYRECORDCHANGE )
		ENDIF

		SELF:__ProcessConcurrency( TRUE )
		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527

	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		SELF:__ProcessConcurrency( FALSE )
		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527
		lRetCode := FALSE
	END SEQUENCE


	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(lRetCode))
	#ENDIF
	RETURN lRetCode

METHOD RecallAll()
	LOCAL lRetCode AS LOGIC
	LOCAL uValue AS USUAL
	LOCAL cbKey AS USUAL
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL oError AS USUAL
	LOCAL lSetDeleted AS LOGIC

	#IFDEF __DEBUG__
		DBFDebug("Entering "+ __ENTITY__ )
	#ENDIF

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		IF SELF:Notify( NOTIFYINTENTTOMOVE )
			lSetDeleted := SetDeleted( FALSE )
			IF lSelectionActive
				uValue := uSelectionValue
				cbKey := cbSelectionIndexingExpression
				IF VODBSeek( uSelectionValue, FALSE )
					lRetCode := SELF:__DBServerEval( { || VODBRecall() },  ;
						NIL,  ;
						{ || Eval( cbKey ) = uValue },  ;
						NIL,  ;
						NIL,  ;
						TRUE,  ;
						DBCCON,  ;
						DBCCUPDATE )
					IF ! VODBGoBottom()
						BREAK ErrorBuild( _VODBErrInfoPtr() )
					ENDIF
					IF ! VODBSkip( 1 )
						BREAK ErrorBuild( _VODBErrInfoPtr() )
					ENDIF
					IF ! lRetCode
						BREAK ErrorBuild( _VODBErrInfoPtr() )
					ENDIF
				ENDIF
				siSelectionStatus := DBSELECTIONNULL
			ELSE
				//PP-040216 lRest requires a logic due to strong typing
				lRetCode := SELF:__DBServerEval( { || VODBRecall() },  ;
					NIL,  ;
					NIL,  ;
					NIL,  ;
					NIL,  ;
					FALSE,  ;
					DBCCON,  ;
					DBCCUPDATE )
			ENDIF
			SetDeleted( lSetDeleted )
			SELF:__ProcessConcurrency( TRUE )
			SELF:Notify( NOTIFYFILECHANGE )
		ELSE
			lRetCode := FALSE
			SELF:__SetStatusHL( #RecallAll, __CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE_CAPTION ),  ;
				__CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE ) )
		ENDIF
		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527

	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		SELF:__ProcessConcurrency( FALSE )
		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527
		lRetCode := FALSE
	END SEQUENCE


	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(lRetCode))
	#ENDIF
	RETURN lRetCode

METHOD RecordInfo( kRecInfoType, nRecordNumber, uRecVal )
	//SE-060601
   LOCAL dwCurrentWorkArea AS DWORD
	LOCAL oError AS USUAL

	#IFDEF __DEBUG__
		DBFDebug( "Entering "+__ENTITY__ )
	#ENDIF

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		IF IsNil( nRecordNumber )
			nRecordNumber := 0
		ENDIF
		IF ! VODBRecordInfo( kRecInfoType, nRecordNumber, @uRecVal )
			IF ! VODBEof() .OR. ! Used()
				BREAK DbError{ SELF, #RecordInfo, EG_ARG, "", kRecInfoType, "kRecInfoType" }
			ENDIF
			oErrorInfo := ErrorBuild( _VODBErrInfoPtr() )
			oHLStatus := SELF:__GenerateStatusHL( oErrorInfo )
		ENDIF
	   __DBSSetSelect( dwCurrentWorkArea )

	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oErrorInfo, #RecordInfo )
	END SEQUENCE


	#IFDEF __DEBUG__
		DBFDebug(__ENTITY__, AsString(uRecVal))
	#ENDIF
	RETURN uRecVal

METHOD Refresh() CLIPPER
	LOCAL lRet AS LOGIC
	LOCAL oError AS USUAL
	LOCAL n AS DWORD
	LOCAL uInfo AS USUAL
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL nRec AS LONGINT
	LOCAL lRelease AS LOGIC

	#IFDEF __DEBUG__
		DBFDebug( "Entering "+__ENTITY__ )
	#ENDIF

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		IF ! VODBInfo( DBI_ISFLOCK, @uInfo)
			BREAK ErrorBuild( _VODBErrInfoPtr( ) )
		ENDIF
	   IF uInfo //File is locked
	   	lRelease := FALSE
			lRet := TRUE
		ELSE
			uInfo := NIL
	   	IF ! VODBRecordInfo( DBRI_LOCKED, NIL, @uInfo )
	   		BREAK ErrorBuild( _VODBErrInfoPtr( ) )
	   	ENDIF
	      IF uInfo //Record is locked
	      	lRelease := FALSE
				lRet := TRUE
			ELSE
				nRec := VODBRecno()
				lRet := VODBRLock( nRec )
				lRelease := lRet
			ENDIF
		ENDIF

		IF lRet
			// We must write back changes to BLOB fields to the Server
			// Since they are not rolled back by VODBBuffRefresh()
			FOR n := 1 TO wFieldCount
				IF aOriginalBuffer[BUFFER_IS_BLOB, n]
					uInfo := aOriginalBuffer[BUFFER_VALUE, n]
					IF ! IsNil( uInfo ) .AND. ! IsArray( uInfo )
						SELF:FIELDPUT( n, uInfo )
					ENDIF
				ENDIF
			NEXT  // n

			IF lRelease
				VODBUnlock( nRec )
			ENDIF
		ENDIF

		IF nEffectiveCCMode == ccOptimistic
			lCCOptimisticRecChg := FALSE
		ENDIF

		IF ! (lRet := VODBBuffRefresh())
			BREAK ErrorBuild( _VODBErrInfoPtr() )
		ENDIF

		SELF:Notify( NOTIFYRECORDCHANGE )

	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo:= oError
		lRet := FALSE
	END SEQUENCE


	__DBSSetSelect( dwCurrentWorkArea )  //SE-060527

	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(lRet))
	#ENDIF
	RETURN lRet

METHOD Reindex()
	LOCAL lRetCode AS LOGIC
	LOCAL oError AS USUAL
	LOCAL dwCurrentWorkArea AS DWORD

	#IFDEF __DEBUG__
		DBFDebug("Entering "+ __ENTITY__ )
	#ENDIF

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		IF SELF:Notify( NOTIFYINTENTTOMOVE )
			lRetCode := VODBOrdListRebuild()
			IF ! lRetCode
				BREAK ErrorBuild( _VODBErrInfoPtr() )
			ENDIF
			lRetCode := SELF:__ProcessConcurrency( TRUE )
			SELF:Notify( NOTIFYCOMPLETION, #Reindex )
		ELSE
			lRetCode := FALSE
			SELF:__SetStatusHL( #Reindex, __CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE_CAPTION ),  ;
				__CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE ) )
		ENDIF
		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527

	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		SELF:__ProcessConcurrency( FALSE )
		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527
		lRetCode := FALSE
	END SEQUENCE


	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(lRetCode))
	#ENDIF
	RETURN lRetCode

METHOD Relation( nRelation )
	//SE-060601
   LOCAL dwCurrentWorkArea AS DWORD
#ifndef __VULCAN__
	LOCAL pszRelText AS PSZ
#endif
	LOCAL cRelation AS STRING
	LOCAL oError AS USUAL

	#IFDEF __DEBUG__
		DBFDebug("Entering "+ __ENTITY__, AsString(nRelation))
	#ENDIF

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
#ifdef __VULCAN__
		IF ! VODBRelation( nRelation,  @cRelation )
			BREAK ErrorBuild( _VODBErrInfoPtr() )
		ENDIF
#else
		IF ! VODBRelation( nRelation, @pszRelText )
			BREAK ErrorBuild( _VODBErrInfoPtr() )
		ENDIF
		cRelation := Psz2String( pszRelText )
#endif

	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		cRelation := ""
	END SEQUENCE

   __DBSSetSelect( dwCurrentWorkArea )


	#IFDEF __DEBUG__
		DBFDebug( "Leaving "+__ENTITY__, AsString(cRelation) )
	#ENDIF
	RETURN cRelation

METHOD Replace( acbExpression, aFieldList, cbForBlock, cbWhileBlock, uScope )
	LOCAL nNextCount AS LONGINT
	LOCAL lRestOfFile AS LOGIC
	LOCAL lRetCode AS LOGIC
	LOCAL aFieldNames AS ARRAY
	LOCAL w AS DWORD
	LOCAL acbExpr AS ARRAY
	//LOCAL iCount := 0 AS INT
	LOCAL wExprCount AS DWORD
	LOCAL wFieldCount AS DWORD
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL oError AS USUAL
	LOCAL oHLTemp AS OBJECT

	#IFDEF __DEBUG__
		DBFDebug( "Entering "+__ENTITY__ )
	#ENDIF

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		IF ! IsArray( acbExpression )
			acbExpression := { acbExpression }
		ENDIF

		wExprCount := ALen( acbExpression )
		acbExpr := ArrayNew( wExprCount )
		FOR w := 1 UPTO wExprCount
			IF __CanEval( acbExpression[w] )
				acbExpr[ w ] := acbExpression[w]
			ELSEIF IsString( acbExpression[w] )
             // RvdH 080212 This cannot happen..
             //IF IsInstanceOfUsual( acbExpression[w], #DataField )
             //   acbExpr[w] := &( "{ ||" + acbExpression[w] + " }" )
             //ELSE
             acbExpr[w] := acbExpression[w]
             //ENDIF
			ELSEIF IsSymbol( acbExpression[w] ) .OR. IsInstanceOfUsual( acbExpression[w], #DataField )
				acbExpr[w] := &( "{ || " + AsString( acbExpression[w] ) + " }" )
			ELSE
				acbExpr[w] := acbExpression[w]
			ENDIF
		NEXT

		IF ! IsArray( aFieldList )
			aFieldList := { aFieldList }
		ENDIF

		wFieldCount := ALen( aFieldList )
		aFieldNames := ArrayNew( wFieldCount )
		FOR w := 1 UPTO wFieldCount
			aFieldNames[w] := String2Symbol( AsString( aFieldList[w] ) )
		NEXT

		IF wExprCount > 1 .AND. wExprCount != wFieldCount
			BREAK DbError{ SELF, #Replace, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_MISMATCH ) }
		ENDIF
      // Per documentation:
      // If only two parameters are specified, DBServer:Replace() is subject
      // to the server scope set with the DBServer ForBlock, WhileBlock and
      // Scope access/assign methods.  If not set, this server scope defaults
      // to "no scope," and the method processes the current record.
		IF ! IsNil( cbForBlock ) .OR. ! IsNil( cbWhileBlock ) .OR. ! IsNil( uScope )
		   // More than one parameter: do all
			IF SELF:Notify( NOTIFYINTENTTOMOVE )
				IF IsString( cbForBlock )
					cbForBlock := &( "{ || " + cbForBlock + " }" )
				ENDIF
				IF IsString( cbWhileBlock )
					cbWhileBlock := &( "{ || " + cbWhileBlock + " }" )
				ENDIF
				IF ! IsNil( uScope )
					IF IsNumeric( uScope )
						nNextCount := uScope
					ELSE
						lRestOfFile := uScope
					ENDIF
				ENDIF
				lRetCode := SELF:__DBServerEval( { || __IterateForFieldAssign( acbExpr, aFieldNames ) },  ;
					cbForBlock,  ;
					cbWhileBlock,  ;
					nNextCount,  ;
					NIL,  ;
					lRestOfFile,  ;
					DBCCON,  ;
					DBCCUPDATE )
				lRetCode := SELF:__ProcessConcurrency( TRUE )
			ELSE
				lRetCode := FALSE
				SELF:__SetStatusHL( #Replace, __CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE_CAPTION ),  ;
					__CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE ) )
				oHLTemp := oHLStatus
			ENDIF
		ELSEIF lActiveScope
			IF SELF:Notify( NOTIFYINTENTTOMOVE )
				lRetCode := SELF:__DBServerEval( { || __IterateForFieldAssign( acbExpr, aFieldNames ) },  ;
					cbStoredForBlock,  ;
					cbStoredWhileBlock,  ;
					nStoredNextCount,  ;
					NIL,  ;
					lStoredRestOfFile,  ;
					DBCCON,  ;
					DBCCUPDATE )
				lRetCode := SELF:__ProcessConcurrency( TRUE )
			ELSE
				lRetCode := FALSE
				SELF:__SetStatusHL( #Replace, __CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE_CAPTION ),  ;
					__CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE ) )
				oHLTemp := oHLStatus
			ENDIF
		ELSE
         // RvdH 080212 acbExpr can be a codeblock , object or a 'normal' value
         // Therefore use the same method as above
         //  FOR w := 1 UPTO wExprCount
         //     SELF:FIELDPUT( aFieldNames[w], Eval( acbExpr[w] ) )
         //  NEXT
         //lRetCode := TRUE
         __IterateForFieldAssign( acbExpr, aFieldNames )
         lRetCode := SELF:__ProcessConcurrency( TRUE )
		ENDIF

		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527

	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oHLTemp := oHLStatus
		oErrorInfo := oError
		SELF:__ProcessConcurrency( FALSE )
		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527
		lRetCode := FALSE
	END SEQUENCE


	SELF:__Notify( NOTIFYFILECHANGE )

	IF ! lRetCode .AND. ! IsNil( oHLTemp )
		lErrorFlag := TRUE
		oHLStatus := oHLTemp
		IF ! IsNil( oError )
			oErrorInfo := oError
		ELSE
			oErrorInfo := NULL_OBJECT
		ENDIF
	ENDIF

	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(lRetCode))
	#ENDIF
	RETURN lRetCode

METHOD ResetNotification()

	IF siSuspendNotification > 0
		siSuspendNotification -= 1
	ENDIF

	RETURN SELF

METHOD RLOCK( nRecordNumber )
	LOCAL lRetCode AS LOGIC
	LOCAL oError AS USUAL
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL nTries AS DWORD

	#IFDEF __DEBUG__
		DBFDebug( "Entering "+__ENTITY__, AsString(nRecordNumber))
	#ENDIF

	lErrorFlag := FALSE
	nTries := SELF:nReTries

	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		//RvdH 030926 Strong typing of __DbsRLock() caused problems
		//with missing nRecordnumber (Bug # 12448)
		//PP-040416 Issue 12766 nRecordNumber must be allowed to be NIL, __DBSRLock changed to accept USUAL
// 		IF ! IsNumeric(nRecordNumber)
// 			nRecordNumber := VODBRecno()
// 		ENDIF

		lRetCode := __DBSRLock( nRecordNumber, nTries )
		SELF:__OptimisticFlushNoLock()
		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527

	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527
		lRetCode := FALSE
	END SEQUENCE


	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(lRetCode))
	#ENDIF
	RETURN lRetCode

METHOD RLockVerify()
	LOCAL lRetCode AS LOGIC
	LOCAL oError AS USUAL
	LOCAL dwCurrentWorkArea AS DWORD

	#IFDEF __DEBUG__
		DBFDebug( "Entering "+__ENTITY__ )
	#ENDIF

	//SE-060602
	IF nEffectiveCCMode != ccOptimistic
		RETURN FALSE
	ENDIF


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		lRetCode := SELF:__RLockVerify()
		IF lRetCode
			SELF:__OptimisticFlushNoLock()
		ENDIF
		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527

	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea ) //SE-060527
		lRetCode := FALSE
	END SEQUENCE


	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(lRetCode))
	#ENDIF
	RETURN lRetCode

METHOD Seek( uSearchExpr, lSoftSeek, lLast )
	LOCAL lRetCode AS LOGIC
	LOCAL oError AS USUAL
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL nTries AS DWORD

	#IFDEF __DEBUG__
		DBFDebug( "Entering "+__ENTITY__,AsString(uSearchExpr), AsString(lSoftSeek), AsString(lLast))
	#ENDIF

	lErrorFlag := FALSE
	nTries := SELF:nReTries
    DEFAULT(@lLast, FALSE)
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		IF SELF:Notify( NOTIFYINTENTTOMOVE )
			IF lSelectionActive
				IF uSearchExpr == uSelectionValue
					lRetCode := __DBSSeek( uSearchExpr, NIL, lLast, nTries )
					siSelectionStatus := DBSELECTIONFOUND
				ELSE
					SELF:__SetStatusHL( #Seek, EG_BOUND,  ;
						__CavoStr( __CAVOSTR_DBFCLASS_SELECTIVESEEK ) )
					lRetCode := FALSE
				ENDIF
				IF !lRetCode
					siSelectionStatus := DBSELECTIONEOF
					__DBSGoBottom( nTries )
					__DBSSkip( 1, nTries )
				ENDIF
			ELSE
				lRetCode := __DBSSeek( uSearchExpr, lSoftSeek, lLast, nTries )
			ENDIF
			IF lRetCode
				lRetCode := SELF:__ProcessConcurrency( TRUE )
			ENDIF
			SELF:Notify( NOTIFYRECORDCHANGE )


		ELSE
			lRetCode := FALSE
			SELF:__SetStatusHL( #Seek, __CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE_CAPTION ),  ;
				__CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE ) )
		ENDIF
		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527

	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		SELF:__ProcessConcurrency( FALSE )
		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527
		lRetCode := FALSE
	END SEQUENCE


	#IFDEF __DEBUG__
		DBFDebug( "Leaving "+__ENTITY__, AsString(lRetCode) )
	#ENDIF
	RETURN lRetCode

METHOD SELECT()
	// RvdH 060623 Added
	LOCAL dwCurrentWorkArea AS DWORD
	VODBSelect( wWorkArea, @dwCurrentWorkArea )
	RETURN dwCurrentWorkArea



METHOD SetDataField( nFieldPosition, oDataField )
	LOCAL wFieldPosition := nFieldPosition  AS DWORD
	LOCAL oField := oDataField AS DataField
	LOCAL lRetCode AS LOGIC
	LOCAL oError AS USUAL

	#IFDEF __DEBUG__
		DBFDebug( "Entering "+__ENTITY__, AsString(nFieldPosition), AsString(oDataField))
	#ENDIF

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		IF aDataFields == NULL_ARRAY
			BREAK DbError{ SELF, #SetDataField, EG_SEQUENCE,  ;
				__CavoStr( __CAVOSTR_DBFCLASS_NODATAFIELDSEXIST ) }

		ELSEIF IsNil( nFieldPosition ) .OR. ! IsNumeric( nFieldPosition ) .OR.  ;
			wFieldPosition < 1 .OR. wFieldPosition > ALen( aDataFields )
			BREAK DbError{ SELF, #SetDataField, EG_ARG,  ;
				__CavoStr( __CAVOSTR_DBFCLASS_BADFIELDPOSITION ), nFieldPosition, "nFieldPosition" }

		ELSEIF IsNil( oDataField ) .OR. ! IsInstanceOfUsual( oDataField, #DataField )
			BREAK DbError{ SELF, #SetDataField, EG_ARG,  ;
				__CavoStr( __CAVOSTR_DBFCLASS_BADFIELDPOSITION ), nFieldPosition, "nFieldPosition" }

		ELSE
			IF oField:Name == aStruct[wFieldPosition, DBS_NAME] .AND.  ;
				oField:__FieldSpec:ValType 	== aStruct[wFieldPosition, DBS_TYPE] .AND.  ;
				oField:__FieldSpec:Length 	== aStruct[wFieldPosition, DBS_LEN] .AND.  ;
				oField:__FieldSpec:Decimals 	== aStruct[wFieldPosition, DBS_DEC]
				aDataFields[wFieldPosition] := oField
				lRetCode := TRUE
			ELSE
				BREAK DbError{ SELF, #SetDataField, EG_ARG,  ;
					__CavoStr( __CAVOSTR_DBFCLASS_BADFIELDMATCH ), nFieldPosition, "nFieldPosition" }
			ENDIF
		ENDIF

	RECOVER USING oError
		oHLStatus 	:= SELF:__GenerateStatusHL( oError )
		oErrorInfo 	:= oError
		lRetCode 	:= FALSE
	END SEQUENCE


	#IFDEF __DEBUG__
		DBFDebug( "Leaving "+__ENTITY__, AsString(lRetCode) )
	#ENDIF
	RETURN lRetCode

METHOD SetFilter( cbFilterBlock, cFilterText )
	//SE-060601
   LOCAL dwCurrentWorkArea AS DWORD
	LOCAL lRetCode 			AS LOGIC
	LOCAL oError 				AS USUAL
	LOCAL lClearFilter		AS LOGIC
	LOCAL cFilter				AS STRING
	LOCAL oErr					AS Error

	#IFDEF __DEBUG__
		DBFDebug( "Entering "+__ENTITY__, AsString(cbFilterBlock), AsString(cFilterText))
	#ENDIF

	lErrorFlag := FALSE

	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		lRetCode := TRUE
		IF IsNil( cFilterText )  .AND. ! IsNil(cbFilterBlock)
			cFilterText := "UNKNOWN"
		ENDIF

		//RvdH 070717 Added 'clear filter' for empty strings and NO arguments

		IF PCount() == 0
			lClearFilter := TRUE
		ELSEIF __CanEval( cbFilterBlock )
			// Ok
		ELSEIF IsString( cbFilterBlock )
			cFilter := cbFilterBlock
			IF SLen(AllTrim(cFilter)) == 0
				lClearFilter := TRUE
			ELSE
				cFilterText 	:= cbFilterBlock
				cbFilterBlock 	:= &( "{ || " + cbFilterBlock + " }" )
			ENDIF
		ELSEIF IsNil( cbFilterBlock ) .AND. IsString(cFilterText)
			cFilter := cFilterText
			IF SLen(AllTrim(cFilter)) == 0
				lClearFilter := TRUE
			ELSE
				cbFilterBlock := &( "{ || " + cFilter   + " }" )
			ENDIF
		ELSE
			oErr := DbError{ SELF, #SetFilter, EG_ARG,  ;
				__CavoStr( __CAVOSTR_DBFCLASS_BADFILTERBLOCK ), cbFilterBlock, "cbFilterBlock" }
			oErr:ArgNum := 1
			BREAK oErr
		ENDIF
		//RvdH 070717 Added 'clear filter' for empty strings
		IF (lClearFilter)
			lRetCode := VODBClearFilter()
		ELSE
			lRetCode := VODBSetFilter( cbFilterBlock, cFilterText )
		ENDIF
		IF !lRetCode
         BREAK ErrorBuild( _VODBErrInfoPtr() )
      ENDIF
		SELF:GoTop()

	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		lRetCode := FALSE
	END SEQUENCE

   __DBSSetSelect( dwCurrentWorkArea )


	#IFDEF __DEBUG__
		DBFDebug( "Leaving "+__ENTITY__, AsString(lRetCode) )
	#ENDIF
	RETURN lRetCode
	///JSP

METHOD SetIndex( oFSIndexFile )
	// oFSIndexFile is a a FileSpec object for the index file,
	// or the filename of the index file as a string,
	// with or without file type.
	// File type defaults to the native type for the workarea RDD.
	//
	// If oFSIndexFile is omitted all currently opened indexes are closed.
	//
	LOCAL lRetCode          AS LOGIC
	LOCAL cIndexFileName    AS STRING
	LOCAL oError            AS USUAL
	LOCAL dwCurrentWorkArea  AS DWORD
	LOCAL nTries            AS DWORD


	#IFDEF __DEBUG__
		DBFDebug( "Entering "+__ENTITY__, AsString(oFSIndexFile))
	#ENDIF

	lErrorFlag := FALSE
	nTries := SELF:nReTries

	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		IF SELF:Notify( NOTIFYINTENTTOMOVE )
			IF IsNil(oFSIndexFile)
				lRetCode := __DBSOrdListClear("", NIL, nTries)
			ELSE
				 IF IsObject(oFSIndexFile) .and. __Usual.ToObject(oFSIndexFile) IS FileSpec VAR oFs
					cIndexFileName := oFS:FullPath
				ELSE
					cIndexFileName := oFSIndexFile
				ENDIF
				lRetCode := __DBSOrdListAdd(cIndexFileName, NIL, nTries)
			END

			SELF:Notify( NOTIFYFILECHANGE )
		ELSE
			lRetCode:=FALSE
			SELF:__SetStatusHL ( #SetIndex, __CavoStr(__CAVOSTR_DBFCLASS_INTENTTOMOVE_CAPTION), __CavoStr(__CAVOSTR_DBFCLASS_INTENTTOMOVE) )
		ENDIF
		__DBSSetSelect(dwCurrentWorkArea) //SE-060527

	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		__DBSSetSelect(dwCurrentWorkArea) //SE-060527
		lRetCode := FALSE
	END SEQUENCE


	RETURN lRetCode


METHOD SetOrder( uOrder, cIndexFileName )
	// Like function SetOrder
	// It sends a NotifyRecordChange
	// Does not change the current record
	//
	//SE-060601
   LOCAL dwCurrentWorkArea AS DWORD
	LOCAL lRetCode      AS LOGIC
	LOCAL oError        AS USUAL
#ifdef __VULCAN__
	LOCAL pszStuff      AS STRING
#else
	LOCAL pszStuff      AS PSZ
#endif

	#IFDEF __DEBUG__
		DBFDebug( "Entering "+__ENTITY__, AsString( uOrder ), AsString( cIndexFileName ))
	#ENDIF

	lErrorFlag := FALSE
	BEGIN SEQUENCE
      //RvdH 070925 Save pending changes
      SELF:__OptimisticFlush()

		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		IF IsObject(cIndexFileName) .and. __Usual.ToObject(cIndexFileName) IS FileSpec VAR oFs
			cIndexFileName := oFS:FullPath
		ELSEIF IsNil(cIndexFileName)
			cIndexFileName:=""
		ENDIF
		IF ! (lRetCode := VODBOrdSetFocus(cIndexFileName,uOrder, REF pszStuff))
			BREAK ErrorBuild(_VODBErrInfoPtr())
		ENDIF
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Notify( NOTIFYFILECHANGE )

	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		__DBSSetSelect( dwCurrentWorkArea )
		oErrorInfo := oError
		lRetCode := FALSE
	END SEQUENCE


	#IFDEF __DEBUG__
		DBFDebug( "Leaving "+__ENTITY__, AsString(lRetCode) )
	#ENDIF
	RETURN lRetCode

METHOD SetOrderCondition(   cFor,           ;
	cbForBlock,     ;
	lAll,           ;
	cbWhileBlock,   ;
	cbEvalBlock,    ;
	nStep,          ;
	nStart,         ;
	nNext,          ;
	nRecno,         ;
	lRest,          ;
	lDescending,    ;
	lAdditive,      ;
	lCurrent,       ;
	lCustom,        ;
	lNoOptimize)

   //SE-060601
   LOCAL dwCurrentWorkArea AS DWORD
	LOCAL lRetCode      AS LOGIC
	LOCAL oError        AS USUAL
	LOCAL cTemp         AS STRING

	#IFDEF __DEBUG__
		DBFDebug( "Entering "+__ENTITY__ )
	#ENDIF


	lErrorFlag := FALSE
	BEGIN SEQUENCE
      VODBSelect( wWorkArea, @dwCurrentWorkArea )
		IF !__CanEval(cbForBlock) .AND. ;
			!(IsObject(cbForBlock) .AND. IsMethod(cbForBlock,#Eval))

			IF IsString(cbForBlock)
				cTemp := cbForBlock
			ELSEIF IsString(cFor)
				cTemp := cFor
			ENDIF

			IF SLen(cTemp) > 0
				cbForBlock := MExec( MCompile("{||" + cTemp + "}") )  //&("{||" + cTemp + "}" )
			ENDIF
		ENDIF

		lRetCode := OrdCondSet(    cFor,           ;
			cbForBlock,     ;
			lAll,           ;
			cbWhileBlock,   ;
			cbEvalBlock,    ;
			nStep,          ;
			nStart,         ;
			nNext,          ;
			nRecno,         ;
			lRest,          ;
			lDescending,    ;
			lAdditive,      ;
			lCurrent,       ;
			lCustom,        ;
			lNoOptimize)

		__DBSSetSelect( dwCurrentWorkArea )

	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oErrorInfo, #SetOrderCondition )
	END SEQUENCE


	#IFDEF __DEBUG__
		DBFDebug( "Leaving "+__ENTITY__, AsString(lRetCode) )
	#ENDIF
	RETURN lRetCode



METHOD SetRelation(oDBChild,uRelation,cRelation,lSelective)
	// Sets a relation from this server to the child server.
	// The child server must be specified as a DbServer object ( not as an alias )
	// The relation may be specified in one of two ways:
	// - as one or several field names, in symbol or string form; they will be concatenated with plus signs
	// - as a codeblock, and an optional string version of the codeblock.
	// Thus, the API can take these forms:
	// oDBCustomer:SetRelation( oDBOrders, #CustNo )
	// oDBCustomer:SetRelation( oDBOrders, #LastName, #Initial, #FirstName )
	// oDBCustomer:SetRelation( oDBOrders, { ||CustNo }, "CustNo" )
	// As always, the child workarea should have a controlling index that matches the expression
	//
	//SE-060601
   LOCAL dwCurrentWorkArea AS DWORD
	LOCAL lRetCode              AS LOGIC
	LOCAL wFieldNo              AS DWORD
	LOCAL wFieldCount           AS DWORD
	LOCAL cChildAlias           AS STRING
	LOCAL cRelationExpression   AS STRING
	LOCAL cbRelationExpression  AS USUAL // AS CODEBLOCK
	LOCAL oError                AS USUAL

	#IFDEF __DEBUG__
		DBFDebug( "Entering "+__ENTITY__ )
	#ENDIF

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		IF oDBChild == NIL
			SELF:ClearRelation()
		ELSE
			IF IsObject(oDbChild) .and. __Usual.ToObject(oDBChild) IS DbServer VAR oDb
				cChildAlias := oDb:ALIAS
			ELSE
				BREAK DbError{ SELF, #SetRelation, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADCHILD), oDBChild, "oDBChild" }
			ENDIF
			IF __CanEval( uRelation )
				cbRelationExpression := uRelation
				IF !IsNil(cRelation)
					cRelationExpression := cRelation
				ENDIF
			ELSEIF IsSymbol( uRelation ) .OR. IsString( uRelation )
				cRelationExpression  := AsString( uRelation )
				cbRelationExpression := &( "{ ||" +cRelationExpression+" }")
			ELSEIF IsArray( uRelation )
				wFieldCount := ALen( uRelation )
				IF wFieldCount < 1
					BREAK DbError{ SELF, #SetRelation, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_NOFIELDS), uRelation, "uRelation" }
				ELSE
					cRelationExpression := AsString( uRelation[1] )
					FOR wFieldNo := 2 UPTO wFieldCount
						IF !IsNil(uRelation[wFieldNo])
							cRelationExpression += "+" + AsString( uRelation[wFieldNo] )
						ENDIF
					NEXT
					cbRelationExpression := &( "{ ||" +cRelationExpression+" }")
				ENDIF
			ELSE
				BREAK DbError{ SELF, #SetRelation, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_NOFIELDS), uRelation, "uRelation" }
			ENDIF

			Send(oDBChild,#__NotifyBufferFlush)

         VODBSelect( wWorkArea, @dwCurrentWorkArea )
         lRetCode := VODBSetRelation( cChildAlias, cbRelationExpression, cRelationExpression )
         __DBSSetSelect( dwCurrentWorkArea )
			IF ! lRetCode
				BREAK ErrorBuild(_VODBErrInfoPtr())
			ENDIF

			IF AScan( aRelationChildren, oDBChild ) == 0
				AAdd( aRelationChildren, oDBChild )
			ENDIF
			lRelationsActive := TRUE
			IF IsLogic(lSelective) .AND. lSelective
				Send(oDBChild,#__AcceptSelectiveRelation, SELF, wWorkArea, cbRelationExpression )
			ENDIF
			Send(oDBChild,#Notify, NOTIFYRELATIONCHANGE )
		ENDIF

	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		lRetCode := FALSE
	END SEQUENCE


	#IFDEF __DEBUG__
		DBFDebug( "Leaving "+__ENTITY__, AsString(lRetCode) )
	#ENDIF
	RETURN lRetCode



METHOD SetSelectiveRelation(oDBChild,uRelation,cRelation)
	#IFDEF __DEBUG__
		DBFDebug( "Entering "+__ENTITY__ )
	#ENDIF

	#IFDEF __DEBUG__
		// AltD()
	#ENDIF

	RETURN SELF:SetRelation( oDBChild, uRelation, cRelation, TRUE )


METHOD Skip( nRecordCount )
	LOCAL dwCurrentWorkArea  AS DWORD
	LOCAL iRecords          AS LONGINT
	LOCAL i                 AS LONGINT
	LOCAL nTries            AS DWORD
	LOCAL lRetCode          AS LOGIC
	LOCAL oError            AS USUAL

	#IFDEF __DEBUG__
		DBFDebug( "Entering "+__ENTITY__, AsString(nRecordCount))
	#ENDIF


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		nTries := SELF:nReTries
		VODBSelect(SELF:wWorkArea, @dwCurrentWorkArea)
		IF SELF:Notify( NOTIFYINTENTTOMOVE )
			IF IsNil(nRecordCount)
				iRecords := 1
			ELSE
				iRecords := nRecordCount
			ENDIF
			IF lSelectionActive
				lRetCode := TRUE

				IF iRecords > 0
					IF !(siSelectionStatus == DBSELECTIONEOF .OR. siSelectionStatus == DBSELECTIONEMPTY)
						siSelectionStatus := DBSELECTIONNULL
						FOR i :=1 UPTO iRecords
							__DBSSkip(1, nTries )
							IF VODBEof() .OR. ! ( Eval( cbSelectionIndexingExpression ) = uSelectionValue )
								siSelectionStatus := DBSELECTIONEOF
								IF ! VODBEof()
									wLastSelectionRec := VODBRecno() - 1
									__DBSGoBottom(nTries)
									__DBSSkip(1, nTries)
								ENDIF
								EXIT
							ENDIF
						NEXT
						SELF:Notify( NOTIFYRECORDCHANGE, iRecords )
					ENDIF
				ELSEIF iRecords < 0
					IF !(siSelectionStatus == DBSELECTIONBOF .OR. siSelectionStatus == DBSELECTIONEMPTY)
						IF siSelectionStatus == DBSELECTIONEOF
							IF wLastSelectionRec > 0
								__DBSGoTo(wLastSelectionRec, nTries)
							ELSE
								__DBSSeek(uSelectionValue, NIL, FALSE, nTries)
							ENDIF
							WHILE !VODBEof() .AND. Eval(cbSelectionIndexingExpression) = uSelectionValue
								__DBSSkip(1, nTries)
							ENDDO
						ENDIF
						siSelectionStatus := DBSELECTIONNULL
						FOR i :=1 UPTO -iRecords
							__DBSSkip(-1, nTries)
							IF VODBBof() .OR. ! ( Eval( cbSelectionIndexingExpression ) = uSelectionValue )
								siSelectionStatus := DBSELECTIONBOF
								IF !VODBBof()
									__DBSSkip(1, nTries)
								ENDIF
								SELF:__ProcessConcurrency(TRUE)
								EXIT
							ENDIF
						NEXT
						SELF:Notify( NOTIFYRECORDCHANGE, iRecords )
					ENDIF
				ENDIF
			ELSE
				__DBSSkip( iRecords, nTries )
				lRetCode := .T.
				SELF:Notify( NOTIFYRECORDCHANGE, iRecords )
			ENDIF

			IF lRetCode
				lRetCode := SELF:__ProcessConcurrency( TRUE )
			ENDIF
		ELSE
			lRetCode := FALSE
			SELF:__SetStatusHL ( #Skip, __CavoStr(__CAVOSTR_DBFCLASS_INTENTTOMOVE_CAPTION), __CavoStr(__CAVOSTR_DBFCLASS_INTENTTOMOVE) )
		ENDIF
		__DBSSetSelect(dwCurrentWorkArea) //SE-060527
		//dwCurrentWorkArea := 0  //SE-060527 ???
	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError

		SELF:__ProcessConcurrency(FALSE)

		__DBSSetSelect(dwCurrentWorkArea) //SE-060527
		lRetCode := FALSE
	END SEQUENCE
	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(lRetCode))
	#ENDIF
	RETURN lRetCode



METHOD Sort(oFSTarget,aFieldList,cbForBlock,cbWhileBlock,uScope)
	// Like Sort
	// Parameter differences:
	//      The target file may be specified as a FileSpec object
	//      in addition to the filename as a string
	//      The fieldlist may be specified as an array of symbols, strings or DataField objects
	// Sends NotifyRecordChange afterwards
	//
	LOCAL uValue                AS USUAL
	LOCAL cbKey                 AS USUAL
	LOCAL nNextCount            AS LONGINT
	LOCAL lRestOfFile           AS LOGIC
	LOCAL cTarget               AS STRING
	LOCAL w                     AS DWORD
	LOCAL lRetCode              AS LOGIC
	LOCAL aFieldNames := { }    AS ARRAY
	LOCAL dwCurrentWorkArea      AS DWORD
	LOCAL oHLTemp               AS OBJECT
	LOCAL wLen                  AS DWORD
	LOCAL lRestore					AS LOGIC
    LOCAL oError            AS USUAL
	#IFDEF __DEBUG__
		DBFDebug( "Entering "+__ENTITY__ )
	#ENDIF
   //RvdH 070711 Make sure we restore the workarea
   //				  The codeblocks may select another workarea
	lRestore	:= DbSetRestoreWorkarea(TRUE)
	lErrorFlag := FALSE
    BEGIN SEQUENCE
	VODBSelect( wWorkArea, @dwCurrentWorkArea )
	IF SELF:Notify( NOTIFYINTENTTOMOVE )
		IF IsObject(oFSTarget) .and. __Usual.ToObject(oFSTarget) IS FileSpec VAR oFs
			cTarget := oFS:FullPath
		ELSE
			cTarget := oFSTarget
		ENDIF

		aFieldNames := ArrayNew( ALen( aFieldList ) )
		wLen:=ALen(aFieldList)
		FOR w := 1 UPTO wLen
			aFieldNames[ w ] := AsString( aFieldList[ w ] )
		NEXT

		IF !IsNil(cbForBlock) .OR. !IsNil(cbWhileBlock) .OR. !IsNil(uScope)
			IF IsString( cbForBlock )
				cbForBlock := &( "{ ||" + cbForBlock + " }" )
			ENDIF
			IF IsString( cbWhileBlock )
				cbWhileBlock := &( "{ ||" + cbWhileBlock + " }" )
			ENDIF
			IF !IsNil(uScope)
				IF IsNumeric(uScope)
					nNextCount := uScope
				ELSE
					lRestOfFile := uScope
				ENDIF
			ENDIF
            lRetCode := __DBSDBSORT( cTarget,   aFieldNames,        ;
                cbForBlock,         ;                   // For
            cbWhileBlock,       ;                   // While
            nNextCount,         ;                   // Next
            NIL,                ;                   // Record #
            lRestOfFile,        ;                   // lRest
            aStruct, SELF:cRDDName)
		ELSEIF lActiveScope
			lRetCode := __DBSDBSORT( cTarget,   aFieldNames,    ;
				cbStoredForBlock,   ;
				cbStoredWhileBlock, ;
				nStoredNextCount,   ;
				,                   ;
				lStoredRestOfFile,  ;
				aStruct, SELF:cRDDName)
        ELSEIF lSelectionActive
            uValue := uSelectionValue
            cbKey := cbSelectionIndexingExpression
            IF VODBSeek( uSelectionValue, FALSE )
                lRetCode := __DBSDBSORT( cTarget,   aFieldNames,    ;
                    NIL,                ;               // For
                {| | Eval( cbKey ) = uValue },  ;   // While
                NIL,                ;               // Next
                NIL,                ;               // Record #
                TRUE,               ;               // lRest
                aStruct, SELF:cRDDName)
            ENDIF
        ELSE
            lRetCode := __DBSDBSORT( cTarget,   aFieldNames,    ;
                NIL,       ;                            // For
            NIL,       ;                            // While
            NIL,       ;                            // Next
            NIL,       ;                            // Record #
            NIL,       ;                            // lRest
            aStruct, SELF:cRDDName)
		ENDIF

		lRetCode := SELF:__ProcessConcurrency(TRUE)

	ELSE
		lRetCode:=FALSE
		SELF:__SetStatusHL ( #Sort, __CavoStr(__CAVOSTR_DBFCLASS_INTENTTOMOVE_CAPTION), __CavoStr(__CAVOSTR_DBFCLASS_INTENTTOMOVE) )
		oHLTemp := oHLStatus
	ENDIF
	__DBSSetSelect(dwCurrentWorkArea) //SE-060527

    RECOVER USING oError


        __DBSSetSelect(dwCurrentWorkArea) //SE-060527
        SELF:Error( oError, #Sort )
        oErrorInfo := oError
        oHLTemp := oHLStatus
    END SEQUENCE

	SELF:__Notify( NOTIFYRECORDCHANGE )

	IF !lRetCode .AND. !IsNil(oHLTemp)
		lErrorFlag := TRUE
		oHLStatus := oHLTemp
	ENDIF

	DbSetRestoreWorkarea(lRestore)
	#IFDEF __DEBUG__
		DBFDebug( "Leaving "+__ENTITY__, AsString(lRetCode) )
	#ENDIF
	RETURN lRetCode


METHOD Sum(acbExpression,cbForBlock,cbWhileBlock,uScope)
	// SUM totals a series of numeric expressions
	//
	// The expressions are specified in acbExpression is an array of codeblocks, or an array of fieldnames specified
	// as symbols or strings. A single expression may also be specified, as a codeblock, symbolic field name or
	// string field name.
	//
	// Differs from classical xBase in that it returns an array that
	// contains the sums for each field specified.
	// Sends NotifyRecordChange afterwards
	//
	LOCAL uValue                AS USUAL
	LOCAL cbKey                 AS USUAL // AS CODEBLOCK
	LOCAL nNextCount            AS LONGINT
	LOCAL lRestOfFile           AS LOGIC
	LOCAL acbExpr               AS ARRAY
	LOCAL aResults              AS ARRAY
	LOCAL w                     AS DWORD
	LOCAL iCount := 0           AS INT
	LOCAL wExprCount            AS DWORD
	LOCAL oError                AS USUAL
	LOCAL dwCurrentWorkArea      AS DWORD
	LOCAL oHLTemp               AS OBJECT

	#IFDEF __DEBUG__
		DBFDebug( "Entering "+__ENTITY__ )
	#ENDIF

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		IF !SELF:Notify( NOTIFYINTENTTOMOVE )
			BREAK DbError{ SELF, #Sum, 999, VO_Sprintf(__CAVOSTR_DBFCLASS_INTENTTOMOVE)}
		ENDIF
		IF !IsArray( acbExpression )
			acbExpression := { acbExpression }
		ENDIF

		wExprCount := ALen( acbExpression )
		acbExpr := ArrayNew( wExprCount )

		FOR w := 1 UPTO wExprCount
			IF __CanEval( acbExpression[ w ] )
				acbExpr[ w ] := acbExpression[ w ]
			ELSEIF IsString( acbExpression[ w ] )
				acbExpr[ w ] := &( "{ ||" + acbExpression[ w ] + " }" )
			ELSEIF IsSymbol( acbExpression[ w ] ) .OR. IsInstanceOfUsual( acbExpression[ w ], #DataField )
				acbExpr[ w ] := &( "{ ||" + AsString( acbExpression[ w ] ) + " }" )
			ELSE
				BREAK DbError{ SELF, #Sum, EG_ARG,  ;
					VO_Sprintf(__CAVOSTR_DBFCLASS_BADEXPRESSION,AllTrim( Str( w ) ) ), acbExpression[ w ], "acbExpression" }
			ENDIF
		NEXT

		aResults := ArrayNew( wExprCount )
		AFill( aResults, 0 )

		IF !IsNil(cbForBlock) .OR. !IsNil(cbWhileBlock) .OR. !IsNil(uScope)
			IF IsString( cbForBlock )
				cbForBlock := &( "{ ||" + cbForBlock + " }" )
			ENDIF
			IF IsString( cbWhileBlock )
				cbWhileBlock := &( "{ ||" + cbWhileBlock + " }" )
			ENDIF
			IF !IsNil(uScope)
				IF IsNumeric(uScope)
					nNextCount := uScope
				ELSE
					lRestOfFile := uScope
				ENDIF
			ENDIF
			SELF:__DBServerEval( { || iCount += 1, __IterateForSum( acbExpr, aResults ) }, ;
				cbForBlock,         ;
				cbWhileBlock,       ;
				nNextCount,         ;
				NIL,                   ;
				lRestOfFile,        ;
				DBCCON,             ;
				DBCCREADONLY )
		ELSEIF lActiveScope
			SELF:__DBServerEval( { || iCount += 1, __IterateForSum( acbExpr, aResults ) },  ;
				cbStoredForBlock,   ;
				cbStoredWhileBlock, ;
				nStoredNextCount,   ;
				NIL,                   ;
				lStoredRestOfFile,  ;
				DBCCON,             ;
				DBCCREADONLY )
		ELSEIF lSelectionActive
			uValue := uSelectionValue
			cbKey := cbSelectionIndexingExpression
			IF VODBSeek( uSelectionValue, FALSE )
				SELF:__DBServerEval( { || iCount += 1, __IterateForSum( acbExpr, aResults ) },  ;
					NIL,       ;
					{| | Eval( cbKey ) = uValue },     ;
					NIL,       ;
					NIL,       ;
					TRUE,   ;
					DBCCON, ;
					DBCCREADONLY )
			ENDIF
		ELSE
			//PP-040216 lRest requires a logic due to strong typing
			SELF:__DBServerEval( { || iCount += 1, __IterateForSum( acbExpr, aResults ) },  ;
				NIL,       ;
				NIL,       ;
				NIL,       ;
				NIL,       ;
				FALSE,       ;
				DBCCON, ;
				DBCCREADONLY )
		ENDIF

		SELF:__ProcessConcurrency(TRUE)

		__DBSSetSelect(dwCurrentWorkArea) //SE-060527

	RECOVER USING oError

		SELF:__ProcessConcurrency(FALSE)

		__DBSSetSelect(dwCurrentWorkArea) //SE-060527
		SELF:Error( oError, #Sum )
		oErrorInfo := oError
		oHLTemp := oHLStatus
		aResults := NULL_ARRAY
	END SEQUENCE


	SELF:__Notify( NOTIFYRECORDCHANGE )

	IF !IsNil(oHLTemp) //Restore oHLStatus and oErrorInfo
		lErrorFlag:=TRUE
		oHLStatus:=oHLTemp
		IF !IsNil(oError)
			oErrorInfo:=oError
		ELSE
			oErrorInfo:=NULL_OBJECT
		ENDIF
	ENDIF


	#IFDEF __DEBUG__
		DBFDebug( "Leaving "+__ENTITY__, AsString(aResults) )
	#ENDIF
	RETURN aResults



METHOD SuspendNotification()

	siSuspendNotification += 1
	RETURN SELF

METHOD Total(oFSTarget,cbKeyField,aFieldList,cbForBlock,cbWhileBlock,uScope)
	// Like DBTotal
	// Parameter differences:
	//      oFSTarget the target file may be specified as a FileSpec object, or as a string
	//      cbKeyField   the key field may be specified as a codeblock, or as a fieldname as a symbol or string
	//      aFieldList   the field list may be specified as an array of symbols or strings, or as an array of DataFields
	//
	// Sends NotifyRecordChange afterwards
	//
	LOCAL uValue                AS USUAL
	LOCAL cbKey                 AS USUAL // AS CODEBLOCK
	LOCAL nNextCount            AS LONGINT
	LOCAL lRestOfFile           AS LOGIC
	LOCAL cTarget               AS STRING
	LOCAL w                     AS DWORD
	LOCAL lRetCode              AS LOGIC
	LOCAL aFieldNames := { }    AS ARRAY
	LOCAL oError                AS USUAL
	LOCAL dwCurrentWorkArea      AS DWORD
	LOCAL oHLTemp               AS OBJECT
	LOCAL lRestore					AS LOGIC
	#IFDEF __DEBUG__
		DBFDebug( "Entering "+__ENTITY__ )
	#ENDIF
   //RvdH 070711 Make sure we restore the workarea
   //				  The codeblocks may select another workarea
	lRestore	:= DbSetRestoreWorkarea(TRUE)

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		IF SELF:Notify( NOTIFYINTENTTOMOVE )
			IF IsObject(oFSTarget) .and. __Usual.ToObject(oFSTarget) IS FileSpec VAR oFs
				cTarget := oFS:FullPath
			ELSE
				cTarget := oFSTarget
			ENDIF
			IF IsSymbol( cbKeyField )
				cbKeyField := &( "{ ||" + Symbol2String( cbKeyField ) + " }" )
			ELSEIF IsString( cbKeyField )
				cbKeyField := &( "{ ||" + cbKeyField + " }" )
			ENDIF
			aFieldNames := ArrayNew( ALen( aFieldList ) )
			FOR w := 1 UPTO ALen( aFieldList )
				aFieldNames[ w ] := AsString( aFieldList[ w ] )
			NEXT
			IF !IsNil(cbForBlock) .OR. !IsNil(cbWhileBlock) .OR. !IsNil(uScope)
				IF IsString( cbForBlock )
					cbForBlock := &( "{ ||" + cbForBlock + " }" )
				ELSEIF IsNil(cbForBlock)
					cbForBlock:={||TRUE}
				ENDIF

				IF IsNil(cbWhileBlock)
					cbWhileBlock:={||TRUE}
				ELSE
					lRestOfFile:=TRUE
					IF IsString( cbWhileBlock )
						cbWhileBlock := &( "{ ||" + cbWhileBlock + " }" )
					ENDIF
				ENDIF
				IF !IsNil(uScope)
					IF IsNumeric(uScope)
						nNextCount := uScope
						lRestOfFile := TRUE
					ELSE
						nNextCount:=-1
						lRestOfFile := uScope
					ENDIF
				//RvdH 030926 when no scope, nNextCount should NOT be zero. (Bug # 5)
				//Why was there no ELSE clause ?
				ELSE
					nNextCount:=-1
				ENDIF
				lRetCode := __DBSDBTOTAL( cTarget, cbKeyField, aFieldNames,         ;
					cbForBlock,         ;
					cbWhileBlock,       ;
					nNextCount,         ;
					NIL,                ;
					lRestOfFile,        ;
					aStruct, SELF:cRDDName)
			ELSEIF lActiveScope
				IF IsNil(cbStoredForBlock)
					cbForBlock:={||TRUE}
				ELSE
					cbForBlock:=cbStoredForBlock
				ENDIF

				lRestOfFile:=lStoredRestOfFile

				IF IsNil(cbStoredWhileBlock)
					cbWhileBlock:={||TRUE}
				ELSE
					cbWhileBlock:=cbStoredWhileBlock
					lRestOfFile:=TRUE
				ENDIF

				nNextCount:=nStoredNextCount
				IF IsNil(nNextCount)
					nNextCount:=-1
				ELSE
					lRestOfFile:=TRUE
				ENDIF
				lRetCode := __DBSDBTOTAL( cTarget, cbKeyField, aFieldNames,  ;
					cbForBlock,     ;
					cbWhileBlock,   ;
					nNextCount,     ;
					NIL,            ;
					lRestOfFile,    ;
					aStruct, SELF:cRDDName)
			ELSEIF lSelectionActive
				uValue := uSelectionValue
				cbKey := cbSelectionIndexingExpression
				IF VODBSeek( uSelectionValue, FALSE )
					lRetCode := __DBSDBTOTAL( cTarget, cbKeyField, aFieldNames,     ;
						{||TRUE},   ;
						{| | Eval( cbKey ) = uValue },     ;
						-1,         ;
						NIL,        ;
						TRUE,       ;
						aStruct, SELF:cRDDName)
				ELSE
					BREAK ErrorBuild(_VODBErrInfoPtr())
				ENDIF
			ELSE
				lRetCode := __DBSDBTOTAL( cTarget, cbKeyField, aFieldNames,         ;
					{||TRUE},   ;
					{||TRUE},   ;
					-1,         ;
					NIL,        ;
					FALSE,      ;
					aStruct, SELF:cRDDName)
			ENDIF

			lRetCode := SELF:__ProcessConcurrency(TRUE)

		ELSE
			lRetCode:=FALSE
			SELF:__SetStatusHL ( #Total, __CavoStr(__CAVOSTR_DBFCLASS_INTENTTOMOVE_CAPTION), __CavoStr(__CAVOSTR_DBFCLASS_INTENTTOMOVE) )
			oHLTemp := oHLStatus
		ENDIF
		__DBSSetSelect(dwCurrentWorkArea) //SE-060527

	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oHLTemp := oHLStatus
		oErrorInfo := oError

		SELF:__ProcessConcurrency(FALSE)

		__DBSSetSelect(dwCurrentWorkArea) //SE-060527
		lRetCode := FALSE
	END SEQUENCE


	SELF:__Notify( NOTIFYRECORDCHANGE )

	IF !lRetCode .AND. !IsNil(oHLTemp) //Restore oHLStatus and oErrorInfo
		lErrorFlag:=TRUE
		oHLStatus:=oHLTemp
		IF !IsNil(oError)
			oErrorInfo:=oError
		ELSE
			oErrorInfo:=NULL_OBJECT
		ENDIF
	ENDIF

	DbSetRestoreWorkarea(lRestore)
	#IFDEF __DEBUG__
		DBFDebug( "Leaving "+__ENTITY__, AsString(lRetCode) )
	#ENDIF
	RETURN lRetCode

METHOD UnLock( nRecordNumber )
	LOCAL lRetCode          AS LOGIC
	LOCAL oError            AS USUAL
	LOCAL dwCurrentWorkArea  AS DWORD

	#IFDEF __DEBUG__
		DBFDebug( "Entering "+__ENTITY__, AsString( nRecordNumber ))
	#ENDIF

	lErrorFlag := FALSE
	BEGIN SEQUENCE

		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		IF ! (lRetCode := VODBUnlock(nRecordNumber))
			BREAK ErrorBuild(_VODBErrInfoPtr())
		ENDIF
		IF lShared
			SELF:__InitRecordBuf()
		ENDIF
		__DBSSetSelect(dwCurrentWorkArea) //SE-060527

	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		__DBSSetSelect(dwCurrentWorkArea) //SE-060527
		lRetCode := FALSE
	END SEQUENCE


	#IFDEF __DEBUG__
		DBFDebug( "Leaving "+__ENTITY__, AsString(lRetCode) )
	#ENDIF
	RETURN lRetCode



METHOD Update(oDbServer,cbKey,lRandomFlag,cbReplace)
	// Like the DbUpdate function
	// Parameter differences:
	//      oDbServer the database to join with may be specified as a DbServer object, or as an alias in
	// symbol or string form
	//      cbKey        the key field may be specified as a codeblock, or as a fieldname as a symbol or string
	// Sends NotifyFileChange afterwards
	LOCAL cAlias               AS STRING
	LOCAL lRetCode             AS LOGIC
	LOCAL oError               AS USUAL
	LOCAL dwCurrentWorkArea    AS DWORD
	LOCAL lRestore					AS LOGIC
	#IFDEF __DEBUG__
		DBFDebug( "Entering "+__ENTITY__ )
	#ENDIF
   //RvdH 070711 Make sure we restore the workarea
   //				  The codeblocks may select another workarea
	lRestore	:= DbSetRestoreWorkarea(TRUE)

	lErrorFlag := FALSE
	BEGIN SEQUENCE
      //RvdH 070925 Save pending changes
      SELF:__OptimisticFlush()

		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		IF IsObject(oDbServer) .and. __Usual.ToObject(oDbServer) IS DbServer VAR oDb
			cAlias := oDb:ALIAS
		ELSE
			cAlias := AsString( oDbServer )
		ENDIF

		IF IsSymbol( cbKey )
			cbKey := &( "{ ||" + Symbol2String( cbKey ) + " }" )
		ELSEIF IsString( cbKey )
			cbKey := &( "{ ||" + cbKey + " }" )
		ENDIF

		IF IsNil(lRandomFlag)
			lRandomFlag:=FALSE
		ENDIF

		lRetCode := __DBSDBUPDATE( cAlias, cbKey, lRandomFlag, cbReplace )

		lRetCode := SELF:__ProcessConcurrency(TRUE)

		__DBSSetSelect(dwCurrentWorkArea) //SE-060527
		SELF:Notify( NOTIFYFILECHANGE )

	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError

		SELF:__ProcessConcurrency(FALSE)

		__DBSSetSelect(dwCurrentWorkArea) //SE-060527
		lRetCode := FALSE
	END SEQUENCE

	DbSetRestoreWorkarea(lRestore )

	#IFDEF __DEBUG__
		DBFDebug( "Leaving "+__ENTITY__, AsString(lRetCode) )
	#ENDIF

	RETURN lRetCode

METHOD Zap()
	// Like Zap
	// Sends a NotifyFileChange message
	//
	//SE-060601
   LOCAL dwCurrentWorkArea AS DWORD
	LOCAL lRetCode              AS LOGIC
	LOCAL oError                AS USUAL

	#IFDEF __DEBUG__
		DBFDebug( "Entering "+__ENTITY__ )
	#ENDIF

	lErrorFlag := FALSE
	BEGIN SEQUENCE
      VODBSelect( wWorkArea, @dwCurrentWorkArea )
		lRetCode := VODBZap()
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Notify( NOTIFYFILECHANGE )
		IF ! lRetCode
			BREAK ErrorBuild(_VODBErrInfoPtr())
		ENDIF

	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		lRetCode := FALSE
	END SEQUENCE

	#IFDEF __DEBUG__
		DBFDebug( "Leaving "+__ENTITY__, AsString(lRetCode) )
	#ENDIF

	RETURN lRetCode
END CLASS

