//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#pragma options ("enforceself", on)
#pragma warnings(165, off)

PARTIAL CLASS DbServer

    /// <include file="Rdd.xml" path="doc/DbServer.Append/*" />
METHOD Append( ) AS LOGIC
    RETURN SELF:Append(TRUE)


/// <include file="Rdd.xml" path="doc/DbServer.Append/*" />
METHOD Append( lReleaseLocks AS LOGIC) AS LOGIC
	LOCAL lRetCode AS LOGIC
	LOCAL oError AS USUAL
	LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL lLocks AS LOGIC
	LOCAL nTries AS DWORD


	lErrorFlag := FALSE
	nTries := SELF:nRetries


	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF SELF:Notify( NOTIFYINTENTTOMOVE )
			IF nEffectiveCCMode == ccRepeatable
				lRetCode := __DBSAPPEND( FALSE, nTries )
			ELSE
				IF IsNil( lReleaseLocks )
					lLocks :=TRUE
				ENDIF
				lRetCode := __DBSAPPEND( lLocks, nTries )
				IF nEffectiveCCMode == ccStable
					nLastLock := (LONG) VoDbRecno( )
				ENDIF
			ENDIF
			siSelectionStatus := DBSELECTIONNULL
			SELF:Notify( NOTIFYAPPEND )
		ELSE
			lRetCode := FALSE
			SELF:__SetStatusHL( #Append, __CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE_CAPTION ),  ;
				__CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE ) )
		ENDIF
		__DBSSetSelect( dwCurrentWorkArea )


	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		lRetCode := FALSE
	END SEQUENCE


	RETURN lRetCode


/// <include file="Rdd.xml" path="doc/DbServer.AppendDB/*" />
METHOD AppendDB( oFSSource, aFieldList, cbForBlock, cbWhileBlock, uScope, cDriver, aRDD )  AS LOGIC CLIPPER
	LOCAL lRetCode 		AS LOGIC
	LOCAL nNextCount 		AS LONGINT
	LOCAL lRestOfFile 	AS LOGIC
	LOCAL cSource 			AS STRING
	LOCAL w 					AS DWORD
	LOCAL aFieldNames 	AS ARRAY
	LOCAL oError 			AS USUAL
	LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL wLen 				AS DWORD


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF SELF:Notify( NOTIFYINTENTTOMOVE )
			IF  IsObject(oFSSource) .AND. __Usual.ToObject(oFSSource) IS FileSpec VAR oFsParam
				cSource := oFsParam:FullPath
			ELSEIF IsObject(oFSSource) .and. __Usual.ToObject(oFSSource) IS DbServer VAR oDb
				cSource := oDb:FileSpec:FullPath
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
				cDriver := RddSetDefault( )
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
				lRetCode := DbApp( cSource,  ;
					aFieldNames,  ;
					cbForBlock,  ;
					cbWhileBlock,  ;
					nNextCount,  ;
					,                     ;
					lRestOfFile,   ;
					cDriver, aRDD )


			ELSEIF lActiveScope
				lRetCode := DbApp( cSource,  ;
					aFieldNames,  ;
					cbStoredForBlock,  ;
					cbStoredWhileBlock,  ;
					nStoredNextCount,  ;
					,                               ;
					lStoredRestOfFile,  ;
					cDriver, aRDD )


			ELSE
				lRetCode := DbApp( cSource,  ;
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
		__DBSSetSelect( dwCurrentWorkArea )


	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		SELF:__ProcessConcurrency(  FALSE )
		__DBSSetSelect( dwCurrentWorkArea )
		lRetCode := FALSE
	END SEQUENCE


	RETURN lRetCode


/// <include file="Rdd.xml" path="doc/DbServer.AppendDelimited/*" />
METHOD AppendDelimited( oFSSource, cDelimiter, aFieldList, cbForBlock, cbWhileBlock, uScope )  AS LOGIC CLIPPER
	LOCAL lRetCode AS LOGIC
	LOCAL nNextCount AS LONGINT
	LOCAL lRestOfFile AS LOGIC
	LOCAL cSource AS STRING
	LOCAL w AS DWORD
	LOCAL aFieldNames AS ARRAY
	LOCAL oError AS USUAL
	LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL wLen AS DWORD


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
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
		VoDbSetSelect (LONGINT(dwCurrentWorkArea ))


	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError


		SELF:__ProcessConcurrency(  FALSE )


		__DBSSetSelect( dwCurrentWorkArea )
		lRetCode := FALSE
	END SEQUENCE


	RETURN lRetCode


/// <include file="Rdd.xml" path="doc/DbServer.AppendSDF/*" />
METHOD AppendSDF(oFSSource,aFieldList,cbForBlock,cbWhileBlock,uScope) AS LOGIC CLIPPER
	LOCAL lRetCode AS LOGIC
	LOCAL nNextCount AS LONGINT
	LOCAL lRestOfFile AS LOGIC
	LOCAL cSource AS STRING
	LOCAL cPath AS STRING
	LOCAL w AS DWORD
	LOCAL aFieldNames AS ARRAY
	LOCAL oError AS USUAL
	LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL wLen AS DWORD


	lErrorFlag := FALSE


	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF SELF:Notify( NOTIFYINTENTTOMOVE )
			IF IsObject(oFSSource) .AND. __Usual.ToObject(oFSSource) IS FileSpec
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
		__DBSSetSelect( dwCurrentWorkArea )


	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError


		SELF:__ProcessConcurrency(  FALSE )


		__DBSSetSelect( dwCurrentWorkArea )
		lRetCode := FALSE


	END SEQUENCE


	RETURN lRetCode


/// <include file="Rdd.xml" path="doc/DbServer.Average/*" />
METHOD Average( acbExpression AS USUAL, cbForBlock := NIL AS USUAL, cbWhileBlock:= NIL AS USUAL, uScope := NIL AS USUAL)  AS ARRAY
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
	LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL oHLTemp AS HyperLabel


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
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
			SELF:__DbServerEval( { || iCount += 1, __IterateForSum( acbExpr, aResults ) },  ;
				cbForBlock,  ;
				cbWhileBlock,  ;
				nNextCount,  ;
				NIL,           ;
				lRestOfFile,   ;
				DBCCON,      ;
				DBCCUPDATE )
		ELSEIF lActiveScope
			SELF:__DbServerEval( { || iCount += 1, __IterateForSum( acbExpr, aResults ) },  ;
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
			IF !VoDbSeek( uSelectionValue, FALSE )
				BREAK DbError{ SELF, #Average, EG_ARG, VO_Sprintf( __CAVOSTR_DBFCLASS_NOSEEK ) }
			ENDIF
			SELF:__DbServerEval( { || iCount += 1, __IterateForSum( acbExpr, aResults ) },  ;
				NIL,                   ;
				{| | Eval( cbKey ) = uValue },  ;
				NIL,                   ;
				NIL,                   ;
				TRUE,  ;
				DBCCON,  ;
				DBCCUPDATE )
			siSelectionStatus := DBSELECTIONNULL
		ELSE
			SELF:__DbServerEval( { || iCount += 1, __IterateForSum( acbExpr, aResults ) },  ;
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


		__DBSSetSelect( dwCurrentWorkArea )


	RECOVER USING oError


		SELF:__ProcessConcurrency(  FALSE )


		__DBSSetSelect( dwCurrentWorkArea )
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




	RETURN aResults


/// <exclude />
DESTRUCTOR( )


	IF SELF:wWorkArea != 0  .AND. SELF:oRDD != NULL_OBJECT        // These gets cleared when the file is closed
		TRY
            // The destructor runs on a separate thread. Therefore
            // we can't close it using the workarea number
            XSharp.RuntimeState.Workareas:CloseArea(SELF:oRDD)
		CATCH
			NOP     // We deliberately 'eat' the error because we don't want to see error messages at shutdown
		END TRY
	ENDIF


	RETURN


/// <include file="Rdd.xml" path="doc/DbServer.BLOBDirectExport/*" />
METHOD BLOBDirectExport( nPointer AS LONG, oFSTarget AS FileSpec, kMode := BLOB_EXPORT_OVERWRITE AS LONG) AS USUAL
    RETURN BLOBDirectExport(nPointer, oFSTarget:FullPath, kMode)


/// <include file="Rdd.xml" path="doc/DbServer.BLOBDirectExport/*" />
METHOD BLOBDirectExport( nPointer AS LONG, cTarget AS STRING, kMode := BLOB_EXPORT_OVERWRITE AS LONG) AS USUAL
	LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL uRetCode AS USUAL
	LOCAL oError AS USUAL


    VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		uRetCode := { nPointer, cTarget, kMode }
		IF ! VoDbInfo( BLOB_DIRECT_EXPORT, REF uRetCode )
			BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
		ENDIF
	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		uRetCode := FALSE
	END SEQUENCE


   __DBSSetSelect( dwCurrentWorkArea )


	RETURN uRetCode


/// <include file="Rdd.xml" path="doc/DbServer.BLOBDirectGet/*" />
METHOD BLOBDirectGet( nPointer AS LONG, nStart AS LONG, nCount AS LONG) AS USUAL


	LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL uRetVal AS USUAL
	LOCAL oError AS USUAL


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		uRetVal := { nPointer, nStart, nCount }
		IF ! VoDbInfo( BLOB_DIRECT_GET, REF uRetVal )
			BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
		ENDIF
	   __DBSSetSelect( dwCurrentWorkArea )
	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oErrorInfo, #BLOBDirectGet )
		uRetVal := NIL
	END SEQUENCE


	RETURN uRetVal


/// <include file="Rdd.xml" path="doc/DbServer.BLOBDirectImport/*" />
METHOD BLOBDirectImport( nPointer AS LONG, oFSSource AS FileSpec) AS USUAL
    RETURN BLOBDirectImport(nPointer, oFSSource:FullPath)


/// <include file="Rdd.xml" path="doc/DbServer.BLOBDirectImport/*" />
METHOD BLOBDirectImport( nPointer AS LONG, cSource AS STRING) AS USUAL


	LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL uRetVal AS USUAL
	LOCAL oError AS USUAL




	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		uRetVal := { nPointer, cSource }
		IF ! VoDbInfo( BLOB_DIRECT_IMPORT, REF uRetVal )
			BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
		ENDIF
	   __DBSSetSelect( dwCurrentWorkArea )
	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oErrorInfo, #BLOBDirectImport )
		uRetVal := NIL
	END SEQUENCE


	RETURN uRetVal


/// <include file="Rdd.xml" path="doc/DbServer.BLOBDirectPut/*" />
METHOD BLOBDirectPut( nPointer AS LONG, uBlob AS USUAL ) AS USUAL


    LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL uRetVal AS USUAL
	LOCAL oError AS USUAL


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		uRetVal := { nPointer, uBlob }
		IF ! VoDbInfo( BLOB_DIRECT_PUT, REF uRetVal )
			BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
		ENDIF
		__DBSSetSelect( dwCurrentWorkArea )
	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oErrorInfo, #BLOBDirectPut )
		uRetVal := NIL
	END SEQUENCE


	RETURN uRetVal


/// <include file="Rdd.xml" path="doc/DbServer.BLOBExport/*" />
METHOD BLOBExport( uField AS USUAL, oFSTarget AS FileSpec, kMode := BLOB_EXPORT_OVERWRITE AS LONG) AS LOGIC
        RETURN SELF:BLOBExport(uField, oFSTarget:FullPath, kMode)


/// <include file="Rdd.xml" path="doc/DbServer.BLOBExport/*" />
METHOD BLOBExport( uField AS USUAL, cTarget AS STRING, kMode := BLOB_EXPORT_OVERWRITE AS LONG) AS LOGIC


	LOCAL lRetCode AS LOGIC
	LOCAL oError AS USUAL
	LOCAL wPos AS DWORD
	LOCAL dwCurrentWorkArea := 0 AS DWORD


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		wPos := __GetFldPos( uField, wFieldCount )
		IF wPos == 0
			BREAK DbError{ SELF, #BLOBExport, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_FIELDSPEC ),  ;
				uField, "uField" }
        ENDIF
        LOCAL uMode := kMode AS USUAL
		IF ! VoDbInfo( BLOB_NMODE, REF uMode )
			BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
		ENDIF


		lRetCode := VoDbFileGet( wPos, cTarget )


		IF !lRetCode
			BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
		ENDIF
		__DBSSetSelect( dwCurrentWorkArea )


	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		lRetCode := FALSE
	END SEQUENCE


	RETURN lRetCode


/// <include file="Rdd.xml" path="doc/DbServer.BLOBGet/*" />
METHOD BLOBGet( uField AS USUAL, nStart AS LONG, nCount AS LONG) AS USUAL


	LOCAL uRetVal AS USUAL
	LOCAL oError AS USUAL
	LOCAL wPos AS DWORD
	LOCAL dwCurrentWorkArea := 0 AS DWORD


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF (wPos := __GetFldPos( uField, wFieldCount )) == 0
			BREAK DbError{ SELF, #BLOBGet, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_FIELDSPEC ),  ;
				uField, "uField" }
		ENDIF
		uRetVal := { wPos,nStart,nCount }
		IF ! VoDbInfo( BLOB_GET, REF uRetVal)
			BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
		ENDIF
	   __DBSSetSelect( dwCurrentWorkArea )


	RECOVER USING oError
		oErrorInfo := oError
		SELF:Error( oErrorInfo, #BLOBGet )
		__DBSSetSelect( dwCurrentWorkArea )
		uRetVal := NIL
	END SEQUENCE


	RETURN uRetVal


/// <include file="Rdd.xml" path="doc/DbServer.BLOBImport/*" />
METHOD BLOBImport( uField, oFSSource ) AS LOGIC CLIPPER


	LOCAL lRetCode AS LOGIC
	LOCAL oError AS USUAL
	LOCAL cTarget AS STRING
	LOCAL wPos AS DWORD
	LOCAL symFieldName AS SYMBOL
	LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL nCurRec AS DWORD
	LOCAL xNewVal AS USUAL


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF (wPos := __GetFldPos( uField, wFieldCount )) == 0
			BREAK DbError{ SELF, #BLOBImport, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_FIELDSPEC ),  ;
				uField, "uField" }
		ENDIF
		IF IsObject(oFSSource) .AND. __Usual.ToObject(oFSSource) IS FileSpec VAR oFsParam
			cTarget := oFsParam:FullPath
		ELSE
			cTarget := oFSSource
		ENDIF
		symFieldName:= FieldSym( wPos )
		IF nEffectiveCCMode == ccOptimistic .AND. ( nCurRec := VoDbRecno( ) ) <= VoDbLastRec( )
			IF SELF:__RLockVerify( )
				lRetCode := VoDbFilePut( wPos, cTarget )
				IF ! lRetCode
					BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
				ENDIF
				VoDbInfo( DBI_ISFLOCK, REF xNewVal )
				IF ! xNewVal
					VoDbUnlock( nCurRec )
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
			lRetCode := VoDbFilePut( wPos, cTarget )
			IF ! lRetCode
				BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
			ENDIF
		ENDIF
		SELF:Notify( Notify.FieldChange, symFieldName )
		__DBSSetSelect( dwCurrentWorkArea )


	RECOVER USING oError
		__DBSSetSelect( dwCurrentWorkArea )
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		lRetCode := FALSE
	END SEQUENCE


	RETURN lRetCode


/// <include file="Rdd.xml" path="doc/DbServer.BLOBRootGet/*" />
METHOD BLOBRootGet( ) AS USUAL STRICT


   LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL uRetVal AS USUAL
	LOCAL oError AS USUAL


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF ! VoDbInfo( BLOB_ROOT_GET, REF uRetVal)
			BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
		ENDIF
      __DBSSetSelect( dwCurrentWorkArea )
	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oErrorInfo, #BLOBRootGet )
		uRetVal := NIL
	END SEQUENCE


	RETURN uRetVal


/// <include file="Rdd.xml" path="doc/DbServer.BLOBRootLock/*" />
METHOD BLOBRootLock( ) AS USUAL STRICT


   LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL uRetCode AS USUAL
	LOCAL oError AS USUAL


	VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		IF ! VoDbInfo( BLOB_ROOT_LOCK, REF uRetCode )
			BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
		ENDIF


	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		uRetCode := FALSE
	END SEQUENCE


	__DBSSetSelect( dwCurrentWorkArea )


	RETURN uRetCode


/// <include file="Rdd.xml" path="doc/DbServer.BLOBRootPut/*" />
METHOD BLOBRootPut( uBlob AS USUAL) AS USUAL STRICT


   LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL oError AS USUAL


	VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		IF ! VoDbInfo( BLOB_ROOT_PUT, REF uBlob )
			BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
		ENDIF


	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		uBlob := FALSE
	END SEQUENCE


	__DBSSetSelect( dwCurrentWorkArea )


	RETURN uBlob


/// <include file="Rdd.xml" path="doc/DbServer.BLOBRootUnlock/*" />
METHOD BLOBRootUnlock( ) AS USUAL STRICT


    LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL uRetVal AS USUAL
	LOCAL oError AS USUAL


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF ! VoDbInfo( BLOB_ROOT_UNLOCK, REF uRetVal)
			BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
		ENDIF
      __DBSSetSelect( dwCurrentWorkArea )
	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oErrorInfo, #BLOBRootUnlock )
		uRetVal := NIL
	END SEQUENCE


	RETURN uRetVal


/// <include file="Rdd.xml" path="doc/DbServer.ClearFilter/*" />
METHOD ClearFilter( ) AS LOGIC STRICT
    LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL lRetCode AS LOGIC
	LOCAL oError AS USUAL


	VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		IF ! (lRetCode := VoDbClearFilter())
			BREAK ErrorBuild( _VoDbErrInfoPtr() )
		ENDIF


	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		lRetCode := FALSE
	END SEQUENCE


   __DBSSetSelect( dwCurrentWorkArea )


	RETURN lRetCode


/// <include file="Rdd.xml" path="doc/DbServer.ClearIndex/*" />
METHOD ClearIndex( uOrder AS USUAL, cOrdBag := "" AS STRING) AS LOGIC


   LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL lRetCode AS LOGIC
	LOCAL oError AS USUAL


	VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )


	BEGIN SEQUENCE
      SELF:__OptimisticFlush()
		cOrdBag := ""
		IF ! (lRetCode := VoDbOrdListClear( cOrdBag, uOrder ))
			BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
		ENDIF
		SELF:Notify( NOTIFYFILECHANGE )


	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		lRetCode := FALSE
	END SEQUENCE


	__DBSSetSelect( dwCurrentWorkArea )


	RETURN lRetCode


/// <include file="Rdd.xml" path="doc/DbServer.ClearLocate/*" />
METHOD ClearLocate( ) AS LOGIC STRICT


   LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL lRetCode AS LOGIC
	LOCAL oError AS USUAL


	VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		IF ! (lRetCode := VoDbClearScope( ))
			BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
		ENDIF


	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		lRetCode := FALSE
	END SEQUENCE


   __DBSSetSelect( dwCurrentWorkArea )


	RETURN lRetCode


/// <include file="Rdd.xml" path="doc/DbServer.ClearOrderScope/*" />
METHOD ClearOrderScope( ) AS LOGIC STRICT




	SELF:OrderScope( TOPSCOPE, NIL )
	SELF:OrderScope( BOTTOMSCOPE, NIL )




	RETURN TRUE


/// <include file="Rdd.xml" path="doc/DbServer.ClearRelation/*" />
METHOD ClearRelation( ) AS LOGIC STRICT


   LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL lRetCode AS LOGIC
	LOCAL oError AS USUAL


	VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		IF ! (lRetCode := VoDbClearRelation())
			BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
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


	RETURN lRetCode


/// <include file="Rdd.xml" path="doc/DbServer.ClearScope/*" />
METHOD ClearScope( ) AS LOGIC STRICT


	cbStoredForBlock := NIL
	cbStoredWhileBlock := NIL
	lStoredRestOfFile := FALSE
	lStoredAllRecords := FALSE
	nStoredNextCount := 0
	uStoredScope := NIL
    lActiveScope := FALSE




	RETURN TRUE


/// <include file="Rdd.xml" path="doc/DbServer.Close/*" />
METHOD Close( ) AS LOGIC STRICT
	LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL lRetCode AS LOGIC
	LOCAL oError AS USUAL


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		IF SELF:wWorkArea # 0
			SELF:Notify( NOTIFYCLOSE )
			VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
			SELF:__OptimisticFlush( )
			if oDBSelectionParent is DbServer var dbS
                dbS:__ClearChildRelation(self)
				oDBSelectionParent := NULL_OBJECT
            endif
            if self:Used
			    self:ClearRelation( )
                VoDbCloseArea( )
            endif
            __DBSSetSelect( dwCurrentWorkArea )
			UnRegisterAxit( self )
			SELF:wWorkArea := 0
			SELF:lSelectionActive := FALSE
			SELF:aClients := { }
			SELF:nClients := 0
			SELF:aDataFields := NULL_ARRAY
			SELF:wFieldCount := 0
			SELF:aOriginalBuffer := NULL_ARRAY
			SELF:aCurrentBuffer := NULL_ARRAY
			SELF:aStruct := NULL_ARRAY
            self:oRDD := null
			oHLStatus := HyperLabel{ #NoTable, __CavoStr( __CAVOSTR_DBFCLASS_NOTABLE_CAPTION ),  ;
				__CavoStr( __CAVOSTR_DBFCLASS_NOTABLE2 ), "DbServer_NoTable" }
		ENDIF
		lRetCode := TRUE


	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		lRetCode := FALSE


	END SEQUENCE


	RETURN lRetCode


/// <include file="Rdd.xml" path="doc/DbServer.Commit/*" />
METHOD Commit( ) AS LOGIC STRICT
	LOCAL lRetCode AS LOGIC
	LOCAL oError AS USUAL
	LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL oHLTemp AS HyperLabel
	LOCAL nTries AS DWORD


	lErrorFlag := FALSE
	nTries := SELF:nRetries


	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		SELF:__OptimisticFlush( )
		lRetCode := __DBSCommit( nTries )
		__DBSSetSelect( dwCurrentWorkArea )


	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oHLTemp := oHLStatus
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		lRetCode := FALSE


	END SEQUENCE




	SELF:__Notify( Notify.Completion, #Commit )


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




	RETURN lRetCode


/// <include file="Rdd.xml" path="doc/DbServer.ConstructUniqueAlias/*" />
METHOD ConstructUniqueAlias( cFileName := "" AS STRING ) AS STRING
    LOCAL sResult AS SYMBOL


	sResult := __ConstructUniqueAlias( cFileName )


	RETURN sResult


/// <include file="Rdd.xml" path="doc/DbServer.Continue/*" />
METHOD Continue( ) AS LOGIC STRICT
	LOCAL lRetCode AS LOGIC
	LOCAL nValue AS DWORD
	LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL oError AS USUAL
	LOCAL oHLTemp AS HyperLabel


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( SELF:wWorkArea, OUT dwCurrentWorkArea )
		IF SELF:Notify( NOTIFYINTENTTOMOVE )
			nValue := VoDbRecno( )
			lRetCode := VoDbContinue( )
			IF ! lRetCode
				BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
			ENDIF


			IF lSelectionActive
				IF Eval( cbSelectionIndexingExpression ) = uSelectionValue
					siSelectionStatus := DBSELECTIONFOUND
				ELSE
					siSelectionStatus := DBSELECTIONEOF
					wLastSelectionRec := nValue
					IF ! VoDbGoBottom( )
						BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
					ENDIF
					IF ! VoDbSkip( 1 )
						BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
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
		__DBSSetSelect( dwCurrentWorkArea )


	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oHLTemp := oHLStatus
		oErrorInfo := oError
		lRetCode := FALSE
		__DBSSetSelect( dwCurrentWorkArea )


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




	RETURN lRetCode


END CLASS


