#translate DBFDebug(<c1> [, <cn>]) =>


PARTIAL CLASS DbServer


/// <include file="Rdd.xml" path="doc/DbServer.Append/*" />
METHOD Append( lReleaseLocks )
	LOCAL lRetCode AS LOGIC
	LOCAL oError AS USUAL
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL lLocks AS LOGIC
	LOCAL nTries AS DWORD


	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF


	lErrorFlag := FALSE
	nTries := SELF:nReTries


	BEGIN SEQUENCE
		VODBSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF SELF:Notify( NOTIFYINTENTTOMOVE )
			IF nEffectiveCCMode == ccRepeatable
				lRetCode := __DBSAPPEND( FALSE, nTries )
			ELSE
				IF IsNil( lReleaseLocks )
					lLocks :=TRUE
				ENDIF
				lRetCode := __DBSAPPEND( lLocks, nTries )
				IF nEffectiveCCMode == ccStable
					nLastLock := VODBRecno( )
				ENDIF
			ENDIF
			siSelectionStatus := DBSELECTIONNULL
			SELF:Notify( NOTIFYAPPEND )
		ELSE
			lRetCode := FALSE
			SELF:__SetStatusHL( #Append, __CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE_CAPTION ),  ;
				__CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE ) )
		ENDIF
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


/// <include file="Rdd.xml" path="doc/DbServer.AppendDB/*" />
METHOD AppendDB( oFSSource, aFieldList, cbForBlock, cbWhileBlock, uScope, cDriver, aRDD )
	LOCAL lRetCode 		AS LOGIC
	LOCAL nNextCount 		AS LONGINT
	LOCAL lRestOfFile 	AS LOGIC
	LOCAL cSource 			AS STRING
	LOCAL w 					AS DWORD
	LOCAL aFieldNames 	AS ARRAY
	LOCAL oError 			AS USUAL
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL wLen 				AS DWORD


	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF SELF:Notify( NOTIFYINTENTTOMOVE )
			IF  IsObject(oFSSource) .AND. __Usual.ToObject(oFSSource) IS FileSpec VAR oFsParam
				cSource := oFsParam:FullPath
			ELSEIF IsObject(oFSSource) .and. __Usual.ToObject(oFSSource) IS DbServer VAR oDb
				cSource := oDb:__FileSpec:FullPath
			ELSE
				cSource := oFSSource
			ENDIF


			IF ! IsNil( aFieldList )
				wLen := ALen( aFieldList )
				aFieldNames := ArrayNew( wLen )
				FOR w := 1 UPTO wLen
					aFieldNames[w] := AsString( aFieldList[w] )
				NEXT
			ENDIF


			IF IsNil( cDriver )
				cDriver := RDDSetDefault( )
			ENDIF


			IF ! IsNil( cbForBlock ) .OR. ! IsNil( cbWhileBlock ) .OR. ! IsNil( uScope )
				IF IsString( cbForBlock )
					cbForBlock := &( "{ | | " + cbForBlock + "  }" )
				ENDIF
				IF IsString( cbWhileBlock )
					cbWhileBlock := &( "{ | | " + cbWhileBlock + "  }" )
				ENDIF


				IF ! IsNil( uScope )
					IF IsNumeric( uScope )
						nNextCount := uScope
					ELSE
						lRestOfFile := uScope
					ENDIF
				ENDIF
    			//RvdH 061218 Added aRDD
				lRetCode := DBApp( cSource,  ;
					aFieldNames,  ;
					cbForBlock,  ;
					cbWhileBlock,  ;
					nNextCount,  ;
					,                     ;
					lRestOfFile,   ;
					cDriver, aRDD )


			ELSEIF lActiveScope
				//RvdH 061218 Added aRDD
				lRetCode := DBApp( cSource,  ;
					aFieldNames,  ;
					cbStoredForBlock,  ;
					cbStoredWhileBlock,  ;
					nStoredNextCount,  ;
					,                               ;
					lStoredRestOfFile,  ;
					cDriver, aRDD )


			ELSE
				//RvdH 061218 Added aRDD
				lRetCode := DBApp( cSource,  ;
					aFieldNames,  ;
					,       ;
					,       ;
					,       ;
					,       ;
					,       ;
					cDriver, aRDD )
			ENDIF


			siSelectionStatus := DBSELECTIONNULL
			IF lRetCode
				lRetCode := SELF:__ProcessConcurrency(  TRUE )
			ENDIF
			SELF:Notify( NOTIFYFILECHANGE )




		ELSE
			lRetCode := FALSE
			SELF:__SetStatusHL( #AppendDB, __CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE_CAPTION ),  ;
				__CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE ) )
		ENDIF
		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527


	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		SELF:__ProcessConcurrency(  FALSE )
		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527
		lRetCode := FALSE
	END SEQUENCE




	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(lRetCode))
	#ENDIF
	RETURN lRetCode


/// <include file="Rdd.xml" path="doc/DbServer.AppendDelimited/*" />
METHOD AppendDelimited( oFSSource, cDelimiter, aFieldList, cbForBlock, cbWhileBlock, uScope )
	LOCAL lRetCode AS LOGIC
	LOCAL nNextCount AS LONGINT
	LOCAL lRestOfFile AS LOGIC
	LOCAL cSource AS STRING
	LOCAL w AS DWORD
	LOCAL aFieldNames AS ARRAY
	LOCAL oError AS USUAL
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL wLen AS DWORD


	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF SELF:Notify( NOTIFYINTENTTOMOVE )
			IF IsObject(oFSSource) .and. __Usual.ToObject(oFSSource) IS FileSpec  VAR oFS
				cSource := oFS:FullPath
			ELSE
				cSource := oFSSource
			ENDIF


			wLen := ALen( aFieldList )
			aFieldNames := ArrayNew( wLen )
			FOR w := 1 UPTO wLen
				aFieldNames[w] := AsString( aFieldList[w] )
			NEXT


			IF IsNil( cDelimiter )
				cDelimiter:=""
			ENDIF


			IF ! IsNil( cbForBlock ) .OR. ! IsNil( cbWhileBlock ) .OR. ! IsNil( uScope )
				IF IsString( cbForBlock )
					cbForBlock := &( "{ || " + cbForBlock + "  }" )
				ENDIF
				IF IsString( cbWhileBlock )
					cbWhileBlock := &( "{ || " + cbWhileBlock + "  }" )
				ENDIF
				IF ! IsNil( uScope )
					IF IsNumeric( uScope )
						nNextCount := uScope
					ELSE
						lRestOfFile := uScope
					ENDIF
				ENDIF
				lRetCode := __DBSDBAPPDELIM( cSource, cDelimiter, aFieldNames,  ;
					cbForBlock,  ;
					cbWhileBlock,  ;
					nNextCount,  ;
					,                     ;
					lRestOfFile,   ;
					aStruct )
			ELSEIF lActiveScope
				lRetCode := __DBSDBAPPDELIM( cSource, cDelimiter, aFieldNames,  ;
					cbStoredForBlock,  ;
					cbStoredWhileBlock,  ;
					nStoredNextCount,  ;
					,                               ;
					lStoredRestOfFile,  ;
					aStruct )
			ELSE
				lRetCode := __DBSDBAPPDELIM( cSource, cDelimiter, aFieldNames,  ;
					,       ;
					,       ;
					,       ;
					,       ;
					,       ;
					aStruct )
			ENDIF
			IF lRetCode
				lRetCode := SELF:__ProcessConcurrency(  TRUE )
			ENDIF
			SELF:Notify( NOTIFYFILECHANGE )


			siSelectionStatus := DBSELECTIONNULL


		ELSE
			lRetCode := FALSE
			SELF:__SetStatusHL( #AppendDelimited,  ;
				__CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE_CAPTION ),  ;
				__CavoStr(__CAVOSTR_DBFCLASS_INTENTTOMOVE ) )
		ENDIF
		VODBSetSelect (LONGINT(dwCurrentWorkArea ))


	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError


		SELF:__ProcessConcurrency(  FALSE )


		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527
		lRetCode := FALSE
	END SEQUENCE




	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(lRetCode))
	#ENDIF
	RETURN lRetCode


/// <include file="Rdd.xml" path="doc/DbServer.AppendSDF/*" />
METHOD AppendSDF(oFSSource,aFieldList,cbForBlock,cbWhileBlock,uScope)
	LOCAL lRetCode AS LOGIC
	LOCAL nNextCount AS LONGINT
	LOCAL lRestOfFile AS LOGIC
	LOCAL cSource AS STRING
	LOCAL cPath AS STRING
	LOCAL w AS DWORD
	LOCAL aFieldNames AS ARRAY
	LOCAL oError AS USUAL
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL wLen AS DWORD


	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF


	lErrorFlag := FALSE


	BEGIN SEQUENCE
		VODBSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF SELF:Notify( NOTIFYINTENTTOMOVE )
			IF IsObject(oFSSource) .and. __Usual.ToObject(oFSSource) IS FileSpec
				cSource := ((FileSpec) oFSSource):FullPath


			ELSE
				cSource := oFSSource
				IF At2( "\", cSource ) == 0
					cPath := SELF:oFileSpec:FullPath
					cSource := SubStr3( cPath, 1, RAt2( "\", cPath ) ) + cSource
				ENDIF


			ENDIF


			wLen := ALen( aFieldList )
			aFieldNames := ArrayNew( wLen )


			FOR w := 1 UPTO wLen
				aFieldNames[w] := AsString( aFieldList[w] )
			NEXT


			IF ! IsNil( cbForBlock ) .OR. ! IsNil( cbWhileBlock ) .OR. ! IsNil( uScope )
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


				lRetCode := __DBSDBAPPSDF( cSource,  ;
					aFieldNames,  ;
					cbForBlock,  ;
					cbWhileBlock,  ;
					nNextCount,  ;
					NIL,               ;
					lRestOfFile,  ;
					aStruct )


			ELSEIF lActiveScope
				lRetCode := __DBSDBAPPSDF( cSource,  ;
					aFieldNames,  ;
					cbStoredForBlock,  ;
					cbStoredWhileBlock,  ;
					nStoredNextCount,  ;
					NIL,  ;
					lRestOfFile,  ;
					aStruct  )


			ELSE
				lRetCode := __DBSDBAPPSDF( cSource,  ;
					aFieldNames,  ;
					NIL,  ;
					NIL,  ;
					NIL,  ;
					NIL,  ;
					NIL,  ;
					aStruct )
			ENDIF


			IF lRetCode
				lRetCode := SELF:__ProcessConcurrency(  TRUE )
			ENDIF
			SELF:Notify( NOTIFYFILECHANGE )
			siSelectionStatus := DBSELECTIONNULL




		ELSE
			lRetCode := FALSE
			SELF:__SetStatusHL ( #AppendSDF, __CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE_CAPTION ),  ;
				__CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE ) )
		ENDIF
		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527


	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError


		SELF:__ProcessConcurrency(  FALSE )


		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527
		lRetCode := FALSE


	END SEQUENCE




	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(lRetCode))
	#ENDIF
	RETURN lRetCode


/// <include file="Rdd.xml" path="doc/DbServer.Average/*" />
METHOD Average( acbExpression, cbForBlock, cbWhileBlock, uScope )
	LOCAL uValue AS USUAL
	LOCAL cbKey AS USUAL
	LOCAL nNextCount AS LONGINT
	LOCAL lRestOfFile AS LOGIC
	LOCAL acbExpr AS ARRAY
	LOCAL aResults AS ARRAY
	LOCAL w AS DWORD
	LOCAL iCount := 0 AS INT
	LOCAL wExprCount AS DWORD
	LOCAL oError AS USUAL
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL oHLTemp AS OBJECT


	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF ! SELF:Notify( NOTIFYINTENTTOMOVE )
			BREAK DbError{ SELF, #Average, 999, VO_Sprintf( __CAVOSTR_DBFCLASS_INTENTTOMOVE ) }
		ENDIF
		IF ! IsArray( acbExpression )
			acbExpression := { acbExpression }
		ENDIF
		wExprCount := ALen( acbExpression )
		acbExpr := ArrayNew( wExprCount )
		FOR w := 1 UPTO wExprCount
			IF __CanEval( acbExpression[w] )
				acbExpr[w] := acbExpression[w]
			ELSEIF IsString( acbExpression[w] )
				acbExpr[w] := &( "{ || " + acbExpression[w] + " }" )
			ELSEIF IsSymbol( acbExpression[w] ) .OR. IsInstanceOfUsual( acbExpression[w], #DataField )
				acbExpr[w] := &( "{ || " + AsString( acbExpression[w] ) + " }" )
			ELSE
				BREAK DbError{ SELF, #Average, EG_ARG,  ;
					VO_Sprintf( __CAVOSTR_DBFCLASS_BADEXPRESSION,  ;
					AllTrim( Str( w ) ) ), acbExpression[w], "acbExpression" }
			ENDIF
		NEXT
		aResults := ArrayNew( wExprCount )
		AFill( aResults, 0 )
		IF ! IsNil( cbForBlock ) .OR. ! IsNil( cbWhileBlock ) .OR. ! IsNil( uScope )
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
			SELF:__DBServerEval( { || iCount += 1, __IterateForSum( acbExpr, aResults ) },  ;
				cbForBlock,  ;
				cbWhileBlock,  ;
				nNextCount,  ;
				NIL,           ;
				lRestOfFile,   ;
				DBCCON,      ;
				DBCCUPDATE )
		ELSEIF lActiveScope
			SELF:__DBServerEval( { || iCount += 1, __IterateForSum( acbExpr, aResults ) },  ;
				cbStoredForBlock,  ;
				cbStoredWhileBlock,  ;
				nStoredNextCount,  ;
				NIL,                 ;
				lStoredRestOfFile,   ;
				DBCCON,  ;
				DBCCUPDATE )
		ELSEIF lSelectionActive
			uValue := uSelectionValue
			cbKey := cbSelectionIndexingExpression
			IF !VODBSeek( uSelectionValue, FALSE )
				BREAK DbError{ SELF, #Average, EG_ARG, VO_Sprintf( __CAVOSTR_DBFCLASS_NOSEEK ) }
			ENDIF
			SELF:__DBServerEval( { || iCount += 1, __IterateForSum( acbExpr, aResults ) },  ;
				NIL,                   ;
				{| | Functions.Eval( cbKey ) = uValue },  ;
				NIL,                   ;
				NIL,                   ;
				TRUE,  ;
				DBCCON,  ;
				DBCCUPDATE )
			siSelectionStatus := DBSELECTIONNULL
		ELSE
			//PP-040216 lRest requires a logic due to strong typing
			SELF:__DBServerEval( { || iCount += 1, __IterateForSum( acbExpr, aResults ) },  ;
				NIL,       ;
				NIL,       ;
				NIL,       ;
				NIL,       ;
				FALSE,       ;
				DBCCON,  ;
				DBCCUPDATE )
		ENDIF
		IF iCount > 0
			FOR w := 1 UPTO wExprCount
				aResults[w] := aResults[w] / iCount
			NEXT
		ENDIF


		SELF:__ProcessConcurrency(  TRUE )


		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527


	RECOVER USING oError


		SELF:__ProcessConcurrency(  FALSE )


		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527
		SELF:Error( oError, #Average )
		oErrorInfo := oError
		oHLTemp := oHLStatus
		aResults := NULL_ARRAY
	END SEQUENCE




	SELF:__Notify( NOTIFYRECORDCHANGE )


	IF ! IsNil( oHLTemp )
		lErrorFlag := TRUE
		oHLStatus := oHLTemp
		IF ! IsNil( oError )
			oErrorInfo := oError
		ELSE
			oErrorInfo := NULL_OBJECT
		ENDIF
	ENDIF


	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(aResults))
	#ENDIF
	RETURN aResults


/// <exclude />
DESTRUCTOR( )
	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF
	IF SELF:wWorkArea != 0  .AND. SELF:oRDD != NULL_OBJECT        // These gets cleared when the file is closed
		TRY
			//RvdH 070508 Make sure the workarea is restored properly. This is the easiest
			//            way to do it.
			//(SELF:wWorkArea)->(VODBCloseArea( ))
            // The destructor runs on a separate thread. Therefore
            // we can't close it using the workarea number
            XSharp.RuntimeState.Workareas:CloseArea(SELF:oRDD)
		CATCH
			NOP     // We deliberately 'eat' the error because we don't want to see error messages at shutdown
		END TRY
	ENDIF
	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__)
	#ENDIF
	RETURN


/// <include file="Rdd.xml" path="doc/DbServer.BLOBDirectExport/*" />
METHOD BLOBDirectExport( nPointer, oFSTarget, kMode )
	//SE-060601
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL uRetCode AS USUAL
	LOCAL oError AS USUAL
	LOCAL cTarget AS STRING


	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF


	VODBSelect( wWorkArea, OUT dwCurrentWorkArea )


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		IF IsObject(oFSTarget) .and. __Usual.ToObject(oFSTarget) IS FileSpec VAR oFS
			cTarget := oFS:FullPath
		ELSE
			cTarget := oFSTarget
		ENDIF
		uRetCode := { nPointer, cTarget, kMode }
		IF ! VODBInfo( BLOB_DIRECT_EXPORT, REF uRetCode )
			BREAK ErrorBuild( _VODBErrInfoPtr( ) )
		ENDIF
	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		uRetCode := FALSE
	END SEQUENCE


   __DBSSetSelect( dwCurrentWorkArea )




	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(uRetCode))
	#ENDIF
	RETURN uRetCode


/// <include file="Rdd.xml" path="doc/DbServer.BLOBDirectGet/*" />
METHOD BLOBDirectGet( nPointer, nStart, nCount )
	//SE-060601
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL uRetVal AS USUAL
	LOCAL oError AS USUAL


	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, OUT dwCurrentWorkArea )
		uRetVal := { nPointer, nStart, nCount }
		IF ! VODBInfo( BLOB_DIRECT_GET, REF uRetVal )
			BREAK ErrorBuild( _VODBErrInfoPtr( ) )
		ENDIF
	   __DBSSetSelect( dwCurrentWorkArea )  //SE-060527
	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527
		SELF:Error( oErrorInfo, #BLOBDirectGet )
		uRetVal := NIL
	END SEQUENCE




	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(uRetVal))
	#ENDIF
	RETURN uRetVal


/// <include file="Rdd.xml" path="doc/DbServer.BLOBDirectImport/*" />
METHOD BLOBDirectImport( nPointer, oFSSource )
	//SE-060601
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL uRetVal AS USUAL
	LOCAL oError AS USUAL
	LOCAL cTarget AS STRING


	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF IsObject(oFSSource) .AND. __Usual.ToObject(oFSSource) IS FileSpec VAR oFsParam
			cTarget := oFsParam:FullPath
		ELSE
			cTarget := oFSSource
		ENDIF
		uRetVal := { nPointer, cTarget }
		IF ! VODBInfo( BLOB_DIRECT_IMPORT, REF uRetVal )
			BREAK ErrorBuild( _VODBErrInfoPtr( ) )
		ENDIF
	   __DBSSetSelect( dwCurrentWorkArea )  //SE-060527
	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527
		SELF:Error( oErrorInfo, #BLOBDirectImport )
		uRetVal := NIL
	END SEQUENCE




	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(uRetVal))
	#ENDIF
	RETURN uRetVal


/// <include file="Rdd.xml" path="doc/DbServer.BLOBDirectPut/*" />
METHOD BLOBDirectPut( nPointer, uBlob )
	//SE-060601
   LOCAL dwCurrentWorkArea AS DWORD
	LOCAL uRetVal AS USUAL
	LOCAL oError AS USUAL


	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, OUT dwCurrentWorkArea )
		uRetVal := { nPointer, uBlob }
		IF ! VODBInfo( BLOB_DIRECT_PUT, REF uRetVal )
			BREAK ErrorBuild( _VODBErrInfoPtr( ) )
		ENDIF
		__DBSSetSelect( dwCurrentWorkArea )
	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oErrorInfo, #BLOBDirectPut )
		uRetVal := NIL
	END SEQUENCE




	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(uRetVal))
	#ENDIF
	RETURN uRetVal


/// <include file="Rdd.xml" path="doc/DbServer.BLOBExport/*" />
METHOD BLOBExport( uField, oFSTarget, kMode )
	//SE-060527
	LOCAL lRetCode AS LOGIC
	LOCAL oError AS USUAL
	LOCAL cTarget AS STRING
	LOCAL wPos AS DWORD
	LOCAL dwCurrentWorkArea AS DWORD


	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, OUT dwCurrentWorkArea )
		wPos := __GetFldPos( uField, wFieldCount )
		IF wPos == 0
			BREAK DbError{ SELF, #BLOBExport, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_FIELDSPEC ),  ;
				uField, "uField" }
		ENDIF
        IF IsObject(oFSTarget) .and. __Usual.ToObject(oFSTarget) IS FileSpec VAR oFS
			cTarget := oFS:FullPath
		ELSE
			cTarget := oFSTarget
		ENDIF
		IF ! VODBInfo( BLOB_NMODE, REF kMode )
			BREAK ErrorBuild( _VODBErrInfoPtr( ) )
		ENDIF


		lRetCode := VODBFileGet( wPos, cTarget )


		IF !lRetCode
			BREAK ErrorBuild( _VODBErrInfoPtr( ) )
		ENDIF
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


/// <include file="Rdd.xml" path="doc/DbServer.BLOBGet/*" />
METHOD BLOBGet( uField, nStart, nCount )
	//SE-060527
	LOCAL uRetVal AS USUAL
	LOCAL oError AS USUAL
	LOCAL wPos AS DWORD
	LOCAL dwCurrentWorkArea AS DWORD


	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF (wPos := __GetFldPos( uField, wFieldCount )) == 0
			BREAK DbError{ SELF, #BLOBGet, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_FIELDSPEC ),  ;
				uField, "uField" }
		ENDIF
		uRetVal := { wPos,nStart,nCount }
		IF ! VODBInfo( BLOB_GET, REF uRetVal)
			BREAK ErrorBuild( _VODBErrInfoPtr( ) )
		ENDIF
	   __DBSSetSelect( dwCurrentWorkArea )  //SE-060527


	RECOVER USING oError
		oErrorInfo := oError
		SELF:Error( oErrorInfo, #BLOBGet )
		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527
		uRetVal := NIL
	END SEQUENCE




	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(uRetVal))
	#ENDIF
	RETURN uRetVal


/// <include file="Rdd.xml" path="doc/DbServer.BLOBImport/*" />
METHOD BLOBImport( uField, oFSSource )
	//SE-060527
	LOCAL lRetCode AS LOGIC
	LOCAL oError AS USUAL
	LOCAL cTarget AS STRING
	LOCAL wPos AS DWORD
	LOCAL symFieldName AS SYMBOL
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL nCurRec AS DWORD
	LOCAL xNewVal AS USUAL


	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF (wPos := __GetFldPos( uField, wFieldCount )) == 0
			BREAK DbError{ SELF, #BLOBImport, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_FIELDSPEC ),  ;
				uField, "uField" }
		ENDIF
		IF IsObject(oFSSource) .AND. __Usual.ToObject(oFSSource) IS FileSpec VAR oFsParam
			cTarget := oFsParam:FullPath
		ELSE
			cTarget := oFSSource
		ENDIF
		symFieldName:= Functions.FieldSym( wPos )
		IF nEffectiveCCMode == ccOptimistic .AND. ( nCurRec := VODBRecno( ) ) <= VODBLastRec( )
			IF SELF:__RLockVerify( )
				lRetCode := VODBFilePut( wPos, cTarget )
				IF ! lRetCode
					BREAK ErrorBuild( _VODBErrInfoPtr( ) )
				ENDIF
				VODBInfo( DBI_IsFLock, REF xNewVal )
				IF ! xNewVal
					VODBUnlock( nCurRec )
				ENDIF
			ELSE
				IF oErrorInfo == NULL_OBJECT
					BREAK DbError{ SELF, #BLOBImport, EG_LOCK,  ;
						__CavoStr( __CAVOSTR_DBFCLASS_RECORDCHANGED ) }
				ELSE
					BREAK oErrorInfo
				ENDIF
			ENDIF
		ELSE
			lRetCode := VODBFilePut( wPos, cTarget )
			IF ! lRetCode
				BREAK ErrorBuild( _VODBErrInfoPtr( ) )
			ENDIF
		ENDIF
		SELF:Notify( NotifyFieldChange, symFieldName )
		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527


	RECOVER USING oError
		__DBSSetSelect( dwCurrentWorkArea ) //SE-060527
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		lRetCode := FALSE
	END SEQUENCE




	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(lRetCode))
	#ENDIF
	RETURN lRetCode


/// <include file="Rdd.xml" path="doc/DbServer.BLOBRootGet/*" />
METHOD BLOBRootGet( )
	//SE-060601
   LOCAL dwCurrentWorkArea AS DWORD
	LOCAL uRetVal AS USUAL
	LOCAL oError AS USUAL


	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF ! VODBInfo( BLOB_ROOT_GET, REF uRetVal)
			BREAK ErrorBuild( _VODBErrInfoPtr( ) )
		ENDIF
      __DBSSetSelect( dwCurrentWorkArea )
	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oErrorInfo, #BLOBRootGet )
		uRetVal := NIL
	END SEQUENCE




	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(uRetVal))
	#ENDIF
	RETURN uRetVal


/// <include file="Rdd.xml" path="doc/DbServer.BLOBRootLock/*" />
METHOD BLOBRootLock( )
	//SE-060601
   LOCAL dwCurrentWorkArea AS DWORD
	LOCAL uRetCode AS USUAL
	LOCAL oError AS USUAL


	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF


	VODBSelect( wWorkArea, OUT dwCurrentWorkArea )


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		IF ! VODBInfo( BLOB_ROOT_LOCK, REF uRetCode )
			BREAK ErrorBuild( _VODBErrInfoPtr( ) )
		ENDIF


	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		uRetCode := FALSE
	END SEQUENCE


	__DBSSetSelect( dwCurrentWorkArea )




	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(uRetCode))
	#ENDIF
	RETURN uRetCode


/// <include file="Rdd.xml" path="doc/DbServer.BLOBRootPut/*" />
METHOD BLOBRootPut( uBlob )
	//SE-060601
   LOCAL dwCurrentWorkArea AS DWORD
	LOCAL oError AS USUAL


	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF


	VODBSelect( wWorkArea, OUT dwCurrentWorkArea )


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		IF ! VODBInfo( BLOB_ROOT_PUT, REF uBlob )
			BREAK ErrorBuild( _VODBErrInfoPtr( ) )
		ENDIF


	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		uBlob := FALSE
	END SEQUENCE


	__DBSSetSelect( dwCurrentWorkArea )




	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(uBlob))
	#ENDIF
	RETURN uBlob


/// <include file="Rdd.xml" path="doc/DbServer.BLOBRootUnlock/*" />
METHOD BLOBRootUnlock( )
	//SE-060601
   LOCAL dwCurrentWorkArea AS DWORD
	LOCAL uRetVal AS USUAL
	LOCAL oError AS USUAL


	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF ! VODBInfo( BLOB_ROOT_UNLOCK, REF uRetVal)
			BREAK ErrorBuild( _VODBErrInfoPtr( ) )
		ENDIF
      __DBSSetSelect( dwCurrentWorkArea )
	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oErrorInfo, #BLOBRootUnlock )
		uRetVal := NIL
	END SEQUENCE




	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(uRetVal))
	#ENDIF
	RETURN uRetVal


/// <include file="Rdd.xml" path="doc/DbServer.ClearFilter/*" />
METHOD ClearFilter( )
	//SE-060601
   LOCAL dwCurrentWorkArea AS DWORD
	LOCAL lRetCode AS LOGIC
	LOCAL oError AS USUAL


	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF


	VODBSelect( wWorkArea, OUT dwCurrentWorkArea )


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		IF ! (lRetCode := VODBClearFilter())
			BREAK ErrorBuild( _VODBErrInfoPtr() )
		ENDIF


	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		lRetCode := FALSE
	END SEQUENCE


   __DBSSetSelect( dwCurrentWorkArea )




	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(lRetCode))
	#ENDIF
	RETURN lRetCode


/// <include file="Rdd.xml" path="doc/DbServer.ClearIndex/*" />
METHOD ClearIndex( uOrder, cOrdBag )
	//SE-060601
   LOCAL dwCurrentWorkArea AS DWORD
	LOCAL lRetCode AS LOGIC
	LOCAL oError AS USUAL


	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__, AsString(uOrder), AsString(cOrdBag))
	#ENDIF


	VODBSelect( wWorkArea, OUT dwCurrentWorkArea )


	BEGIN SEQUENCE
        //RvdH 070925 Save pending changes
        SELF:__OptimisticFlush()
		IF IsNil( cOrdBag ) .OR. ! IsString( cOrdBag )
			cOrdBag := ""
		ENDIF
		IF ! (lRetCode := VODBOrdListClear( cOrdBag, uOrder ))
			BREAK ErrorBuild( _VODBErrInfoPtr( ) )
		ENDIF
		SELF:Notify( NOTIFYFILECHANGE )


	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		lRetCode := FALSE
	END SEQUENCE


	__DBSSetSelect( dwCurrentWorkArea )




	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(lRetCode))
	#ENDIF
	RETURN lRetCode


/// <include file="Rdd.xml" path="doc/DbServer.ClearLocate/*" />
METHOD ClearLocate( )
	//SE-060601
   LOCAL dwCurrentWorkArea AS DWORD
	LOCAL lRetCode AS LOGIC
	LOCAL oError AS USUAL


	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF


	VODBSelect( wWorkArea, OUT dwCurrentWorkArea )


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		IF ! (lRetCode := VODBClearScope( ))
			BREAK ErrorBuild( _VODBErrInfoPtr( ) )
		ENDIF


	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		lRetCode := FALSE
	END SEQUENCE


   __DBSSetSelect( dwCurrentWorkArea )




	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(lRetCode))
	#ENDIF
	RETURN lRetCode


/// <include file="Rdd.xml" path="doc/DbServer.ClearOrderScope/*" />
METHOD ClearOrderScope( )


	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF
	SELF:OrderScope( TOPSCOPE, NIL )
	SELF:OrderScope( BOTTOMSCOPE, NIL )


	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__)
	#ENDIF
	RETURN SELF


/// <include file="Rdd.xml" path="doc/DbServer.ClearRelation/*" />
METHOD ClearRelation( )
	//SE-060601
   LOCAL dwCurrentWorkArea AS DWORD
	LOCAL lRetCode AS LOGIC
	LOCAL oError AS USUAL


	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF


	VODBSelect( wWorkArea, OUT dwCurrentWorkArea )


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		IF ! (lRetCode := VODBClearRelation())
			BREAK ErrorBuild( _VODBErrInfoPtr( ) )
		ENDIF


		IF lRelationsActive
			ASend(aRelationChildren, #Notify, NOTIFYCLEARRELATION)
			lRelationsActive := FALSE
			aRelationChildren := { }
		ENDIF


	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		lRetCode := FALSE


	END SEQUENCE


   __DBSSetSelect( dwCurrentWorkArea )




	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(lRetCode))
	#ENDIF
	RETURN lRetCode


/// <include file="Rdd.xml" path="doc/DbServer.ClearScope/*" />
METHOD ClearScope( )


	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF


	cbStoredForBlock := NIL
	cbStoredWhileBlock := NIL
	lStoredRestOfFile := FALSE
	lStoredAllRecords := FALSE
	nStoredNextCount := 0
	uStoredScope := NIL
    lActiveScope := FALSE // RvdH 2019-02-16 needs to be reset
	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__)
	#ENDIF


	RETURN TRUE


/// <include file="Rdd.xml" path="doc/DbServer.Close/*" />
METHOD Close( )
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL lRetCode AS LOGIC
	LOCAL oError AS USUAL


	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		IF SELF:wWorkArea # 0
			SELF:Notify( NOTIFYCLOSE )
			VODBSelect( wWorkArea, OUT dwCurrentWorkArea )
			SELF:__OptimisticFlush( )
            if oDBSelectionParent is DbServer var dbS
                dbS:__ClearChildRelation(self)
				oDBSelectionParent := null_object
            endif
            if self:Used
			    self:ClearRelation( )
                VODBCloseArea( )
            endif
			__DBSSetSelect( dwCurrentWorkArea )  //SE-060527
			UnregisterAxit( SELF )
			SELF:wWorkArea := 0
			SELF:lSelectionActive := FALSE
			SELF:aClients := { }
			SELF:nClients := 0
			SELF:aDataFields := NULL_ARRAY
			SELF:wFieldCount := 0
			SELF:aOriginalBuffer := NULL_ARRAY
			SELF:aCurrentBuffer := NULL_ARRAY
			SELF:aStruct := NULL_ARRAY
            SELF:oRDD := null
			SELF:oHLStatus := HyperLabel{ #NoTable, __CavoStr( __CAVOSTR_DBFCLASS_NOTABLE_CAPTION ),  ;
				__CavoStr( __CAVOSTR_DBFCLASS_NOTABLE2 ), "DbServer_NoTable" }
		ENDIF
		lRetCode := TRUE


	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		lRetCode := FALSE


	END SEQUENCE




	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(lRetCode))
	#ENDIF
	RETURN lRetCode


/// <include file="Rdd.xml" path="doc/DbServer.Commit/*" />
METHOD Commit( )
	LOCAL lRetCode AS LOGIC
	LOCAL oError AS USUAL
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL oHLTemp AS OBJECT
	LOCAL nTries AS DWORD


	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF


	lErrorFlag := FALSE
	nTries := SELF:nReTries


	BEGIN SEQUENCE
		VODBSelect( wWorkArea, OUT dwCurrentWorkArea )
		SELF:__OptimisticFlush( )
        lRetCode := __DBSCommit( nTries )
		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527


	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oHLTemp := oHLStatus
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea ) //SE-060527
		lRetCode := FALSE


	END SEQUENCE




	SELF:__Notify( NotifyCompletion, #Commit )


    IF lRetCode
   	    IF ! SELF:lShared
	 	    SELF:__InitRecordBuf( )
	    ENDIF
	ELSE
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


/// <include file="Rdd.xml" path="doc/DbServer.ConstructUniqueAlias/*" />
METHOD ConstructUniqueAlias( cFileName )
    LOCAL sResult AS SYMBOL
	DEFAULT( REF cFileName, "" )
	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF


	sResult := __ConstructUniqueAlias( cFileName )
	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(sResult))
	#ENDIF
	RETURN sResult


/// <include file="Rdd.xml" path="doc/DbServer.Continue/*" />
METHOD Continue( )
	LOCAL lRetCode AS LOGIC
	LOCAL nValue AS DWORD
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL oError AS USUAL
	LOCAL oHLTemp AS OBJECT


	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( SELF:wWorkArea, OUT dwCurrentWorkArea )
		IF SELF:Notify( NOTIFYINTENTTOMOVE )
			nValue := VODBRecno( )
			lRetCode := VODBContinue( )
			IF ! lRetCode
				BREAK ErrorBuild( _VODBErrInfoPtr( ) )
			ENDIF


			IF lSelectionActive
				IF Functions.Eval( cbSelectionIndexingExpression ) = uSelectionValue
					siSelectionStatus := DBSELECTIONFOUND
				ELSE
					siSelectionStatus := DBSELECTIONEOF
					wLastSelectionRec := (LONG) nValue
					IF ! VODBGoBottom( )
						BREAK ErrorBuild( _VODBErrInfoPtr( ) )
					ENDIF
					IF ! VODBSkip( 1 )
						BREAK ErrorBuild( _VODBErrInfoPtr( ) )
					ENDIF
				ENDIF
			ENDIF


			SELF:__ProcessConcurrency(  TRUE )
		ELSE
			lRetCode := FALSE
			SELF:__SetStatusHL( #Continue, __CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE_CAPTION ),  ;
				__CavoStr(__CAVOSTR_DBFCLASS_INTENTTOMOVE) )
			oHLTemp := oHLStatus
		ENDIF
		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527


	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oHLTemp := oHLStatus
		oErrorInfo := oError
		lRetCode := FALSE
		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527


	END SEQUENCE


	SELF:__Notify( NOTIFYRECORDCHANGE )


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


END CLASS


