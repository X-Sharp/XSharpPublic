//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#pragma options ("enforceself", on)
#pragma warnings(165, off)

PARTIAL CLASS DbServer


/// <include file="Rdd.xml" path="doc/DbServer.CopyDB/*" />
METHOD CopyDB( oFSTarget, aFieldList, cbForBlock, cbWhileBlock, uScope, cDriver, aRDD )   AS LOGIC CLIPPER
	LOCAL uValue AS USUAL
	LOCAL cbKey AS USUAL
	LOCAL nNextCount AS LONGINT
	LOCAL lRestOfFile AS LOGIC
	LOCAL cTarget AS STRING
	LOCAL w AS DWORD
	LOCAL aFieldNames AS ARRAY
	LOCAL lRetCode AS LOGIC
	LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL oError AS USUAL
	LOCAL oHLTemp AS HyperLabel
	LOCAL wLen AS DWORD
	LOCAL lRestore AS LOGIC


	lRestore	:= DbSetRestoreWorkarea(TRUE)


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF SELF:Notify( NOTIFYINTENTTOMOVE )
			IF IsObject(oFSTarget) .and. __Usual.ToObject(oFSTarget) IS FileSpec VAR oFS
				cTarget := oFS:FullPath


			ELSEIF IsObject(oFSTarget) .and. __Usual.ToObject(oFSTarget) IS DbServer VAR oDb
				cTarget := oDb:FileSpec:FullPath


			ELSE
				cTarget := oFSTarget


			ENDIF


			wLen := ALen( aFieldList )
			aFieldNames := ArrayNew( wLen )


			FOR w := 1 UPTO wLen
				aFieldNames[ w ] := AsString( aFieldList[ w ] )
			NEXT


			IF Empty( cDriver )
				cDriver := SELF:cRDDName
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
				lRetCode := __DBSDBCopy( cTarget,  ;
					aFieldNames,  ;
					cbForBlock,  ;
					cbWhileBlock,  ;
					nNextCount,  ;
					NIL,  ;
					lRestOfFile,  ;
					cDriver,  ;
					aRDD,  ;
					aStruct )


			ELSEIF lActiveScope
				lRetCode := __DBSDBCopy( cTarget,  ;
					aFieldNames,  ;
					cbStoredForBlock,  ;
					cbStoredWhileBlock,  ;
					nStoredNextCount,  ;
					NIL ,  ;
					lStoredRestOfFile,  ;
					cDriver,  ;
					aRDD,  ;
					aStruct )


			ELSEIF lSelectionActive
				uValue := uSelectionValue
				cbKey := cbSelectionIndexingExpression
				IF VoDbSeek( uSelectionValue, FALSE )
					lRetCode := __DBSDBCopy( cTarget,  ;
						aFieldNames,  ;
						NIL,  ;
						{ | | Eval( cbKey ) = uValue },  ;
						NIL,  ;
						NIL,  ;
						TRUE,  ;
						cDriver,  ;
						aRDD,  ;
						aStruct )
					siSelectionStatus := DBSELECTIONEOF
					IF ! VoDbGoBottom( )
						BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
					ENDIF
					IF ! VoDbSkip( 1 )
						BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
					ENDIF


				ELSE
					BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
				ENDIF


			ELSE
				lRetCode := __DBSDBCopy( cTarget,  ;
					aFieldNames,  ;
					NIL,  ;
					NIL,  ;
					NIL,  ;
					NIL,  ;
					NIL,  ;
					cDriver,  ;
					aRDD,  ;
					aStruct )
			ENDIF


			SELF:__ProcessConcurrency( TRUE )


		ELSE
			lRetCode := FALSE
			SELF:__SetStatusHL ( #CopyDB, __CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE_CAPTION ),  ;
				__CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE ) )
			oHLTemp := oHLStatus


		ENDIF


		__DBSSetSelect( dwCurrentWorkArea )


	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oHLTemp := oHLStatus
		oErrorInfo := oError
		SELF:__ProcessConcurrency( FALSE )
		__DBSSetSelect( dwCurrentWorkArea )
		lRetCode := FALSE


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
	DbSetRestoreWorkarea(lRestore)


	RETURN lRetCode




/// <include file="Rdd.xml" path="doc/DbServer.CopyDelimited/*" />
METHOD CopyDelimited( oFSTarget, cDelimiter, aFieldList, cbForBlock, cbWhileBlock, uScope )   AS LOGIC CLIPPER
	LOCAL uValue AS USUAL
	LOCAL cbKey AS USUAL
	LOCAL nNextCount AS LONGINT
	LOCAL lRestOfFile AS LOGIC
	LOCAL cTarget AS STRING
	LOCAL w AS DWORD
	LOCAL aFieldNames AS ARRAY
	LOCAL lRetCode AS LOGIC
	LOCAL oError AS USUAL
	LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL oHLTemp AS HyperLabel
	LOCAL wLen AS DWORD
	LOCAL lRestore AS LOGIC




	lRestore	:= DbSetRestoreWorkarea(TRUE)


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF SELF:Notify( NOTIFYINTENTTOMOVE )
			IF IsObject(oFSTarget) .and. __Usual.ToObject(oFSTarget) IS FileSpec VAR oFS
				cTarget := oFS:FullPath
			ELSE
				cTarget := oFSTarget
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
				lRetCode := __DBSDBCOPYDELIM( cTarget, cDelimiter,  aFieldNames,  ;
					cbForBlock,  ;
					cbWhileBlock,  ;
					nNextCount,  ;
					NIL,  ;
					lRestOfFile,  ;
					aStruct )


			ELSEIF lActiveScope
				lRetCode := __DBSDBCOPYDELIM( cTarget, cDelimiter,  aFieldNames,  ;
					cbStoredForBlock,  ;
					cbStoredWhileBlock,  ;
					nStoredNextCount,  ;
					NIL,  ;
					lStoredRestOfFile,  ;
					aStruct )


			ELSEIF lSelectionActive
				uValue := uSelectionValue
				cbKey := cbSelectionIndexingExpression
				IF VoDbSeek( uSelectionValue, FALSE )
					lRetCode := __DBSDBCOPYDELIM( cTarget, cDelimiter,  aFieldNames,  ;
						NIL,  ;
						{ || Eval( cbKey ) = uValue },  ;
						NIL,  ;
						NIL,  ;
						TRUE,  ;
						aStruct )
					siSelectionStatus := DBSELECTIONEOF
					IF ! VoDbGoBottom( )
						BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
					ENDIF
					IF ! VoDbSkip( 1 )
						BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
					ENDIF
				ELSE
					BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
				ENDIF
			ELSE
				lRetCode := __DBSDBCOPYDELIM( cTarget, cDelimiter,  aFieldNames,  ;
					NIL,  ;
					NIL,  ;
					NIL,  ;
					NIL,  ;
					NIL,  ;
					aStruct )
			ENDIF


			SELF:__ProcessConcurrency( TRUE )


		ELSE
			lRetCode := FALSE
			SELF:__SetStatusHL ( #CopyDelimited, __CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE_CAPTION),  ;
				__CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE ) )
			oHLTemp := oHLStatus
		ENDIF
		__DBSSetSelect( dwCurrentWorkArea )


	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oHLTemp := oHLStatus
		oErrorInfo := oError
		SELF:__ProcessConcurrency( FALSE )
		__DBSSetSelect( dwCurrentWorkArea )
		lRetCode := FALSE
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


	DbSetRestoreWorkarea(lRestore)




	RETURN lRetCode




/// <include file="Rdd.xml" path="doc/DbServer.CopySDF/*" />
METHOD CopySDF( oFSTarget, aFieldList, cbForBlock, cbWhileBlock, uScope )   AS LOGIC CLIPPER
	LOCAL uValue AS USUAL
	LOCAL cbKey AS USUAL
	LOCAL nNextCount AS LONGINT
	LOCAL lRestOfFile AS LOGIC
	LOCAL cTarget AS STRING
	LOCAL w AS DWORD
	LOCAL aFieldNames AS ARRAY
	LOCAL lRetCode AS LOGIC
	LOCAL oError AS USUAL
	LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL oHLTemp AS HyperLabel
	LOCAL wLen AS DWORD
	LOCAL lRestore AS LOGIC


	lRestore	:= DbSetRestoreWorkarea(TRUE)


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF SELF:Notify( NOTIFYINTENTTOMOVE )
			IF IsObject(oFSTarget) .and. __Usual.ToObject(oFSTarget) IS FileSpec VAR oFS
				cTarget := oFS:FullPath
			ELSE
				cTarget := oFSTarget
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
				lRetCode := __DBSDBCOPYSDF( cTarget, aFieldNames,  ;
					cbForBlock,  ;
					cbWhileBlock,  ;
					nNextCount,  ;
					NIL,  ;
					lRestOfFile,  ;
					aStruct )


			ELSEIF lActiveScope
				lRetCode := __DBSDBCOPYSDF( cTarget, aFieldNames,  ;
					cbStoredForBlock,  ;
					cbStoredWhileBlock,  ;
					nStoredNextCount,  ;
					NIL,  ;
					lStoredRestOfFile,  ;
					aStruct  )


			ELSEIF lSelectionActive
				uValue := uSelectionValue
				cbKey := cbSelectionIndexingExpression
				IF VoDbSeek( uSelectionValue, FALSE )
					lRetCode := __DBSDBCOPYSDF( cTarget, aFieldNames,  ;
						NIL,  ;
						{ || Eval( cbKey ) = uValue },  ;
						NIL,  ;
						NIL,  ;
						TRUE,  ;
						aStruct )
					siSelectionStatus := DBSELECTIONEOF
					IF ! VoDbGoBottom( )
						BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
					ENDIF
					IF ! VoDbSkip( 1 )
						BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
					ENDIF
				ELSE
					BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
				ENDIF
			ELSE
				lRetCode := __DBSDBCOPYSDF( cTarget, aFieldNames,  ;
					NIL,  ;
					NIL,  ;
					NIL,  ;
					NIL,  ;
					NIL,  ;
					aStruct )
			ENDIF


			SELF:__ProcessConcurrency( TRUE )


		ELSE
			lRetCode := FALSE
			SELF:__SetStatusHL ( #CopySDF, __CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE_CAPTION ),  ;
				__CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE ) )
			oHLTemp := oHLStatus
		ENDIF
		__DBSSetSelect( dwCurrentWorkArea )


	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oHLTemp := oHLStatus
		oErrorInfo := oError
		SELF:__ProcessConcurrency( FALSE )
		__DBSSetSelect( dwCurrentWorkArea )
		lRetCode := FALSE
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
	DbSetRestoreWorkarea(lRestore)




	RETURN lRetCode




/// <include file="Rdd.xml" path="doc/DbServer.CopyStructure/*" />
METHOD CopyStructure( oFSTarget AS FileSpec, aFieldList := NULL AS ARRAY) AS LOGIC
    RETURN SELF:CopyStructure(oFSTarget:FullPath, aFieldList)


/// <include file="Rdd.xml" path="doc/DbServer.CopyStructure/*" />
METHOD CopyStructure( cFileName AS STRING, aFieldList := NULL AS ARRAY )   AS LOGIC
	LOCAL cTarget AS STRING
	LOCAL w AS DWORD
	LOCAL j AS DWORD
	LOCAL aNew AS ARRAY
	LOCAL cName AS STRING
	LOCAL wFieldCountUsed AS DWORD
	LOCAL lRetCode AS LOGIC
	LOCAL oError AS USUAL
	LOCAL oHLTemp AS HyperLabel
	LOCAL cAlias AS STRING
	LOCAL cPath AS STRING
	LOCAL aFullPath AS ARRAY
	LOCAL aRdds AS ARRAY
	LOCAL rddList AS _RddList


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		cTarget := cFileName
		aFullPath := ArrayNew( 4 )
		__SplitPath( NULL, cTarget, aFullPath )
		IF SubStr2( aFullPath[2], SLen( aFullPath[2] ) ) == "\"
			cPath := aFullPath[1] + aFullPath[2]
		ELSE
			cPath := aFullPath[1] + aFullPath[2] + "\"
		ENDIF


		cFileName := aFullPath[3] + aFullPath[4]


		IF aFullPath[4] == NULL_STRING
			cFileName += oFileSpec:Extension
		ENDIF


		IF aFullPath[3] == SELF:oFileSpec:FileName
			cAlias := Symbol2String( __ConstructUniqueAlias( SELF:oFileSpec:FileName ) )
		ENDIF
		cTarget := cPath + cFileName


		IF aFieldList == NULL
			aNew := aStruct
		ELSE
			aNew := { }
			wFieldCountUsed := ALen( aFieldList )
			FOR w := 1 UPTO wFieldCountUsed
				cName := Upper( Trim( SubStr( aFieldList[w], At( ">", aFieldList[w] ) + 1 ) ) )
				j := AScan( aStruct, { | aFld | aFld[DBS_NAME] == cName } )
				IF j > 0
					AAdd( aNew, aStruct[j] )
				ENDIF
			NEXT
		ENDIF


		lRetCode := TRUE


		aRdds := __RDDList( SELF:cRDDName )
		rddList := __AllocRddList( aRdds )


		lRetCode := VoDbCreate( cTarget, aNew, rddList, TRUE, cAlias, "", FALSE, FALSE )


		IF ! lRetCode
			BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
		ENDIF


	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oHLTemp := oHLStatus
		oErrorInfo := oError
		lRetCode := FALSE


	END SEQUENCE




	SELF:__Notify( Notify.Completion, #CopyStructure )


	IF ! lRetCode .AND. ! IsNil( oHLTemp )
		lErrorFlag := TRUE
		IF ! IsNil( oError )
			oErrorInfo := oError
		ELSE
			oErrorInfo := NULL_OBJECT
		ENDIF
	ENDIF




	RETURN lRetCode




/// <include file="Rdd.xml" path="doc/DbServer.Count/*" />
METHOD Count( cbForBlock := NIL AS USUAL, cbWhileBlock := NIL AS USUAL, uScope := NIL AS USUAL)   AS LONG
	LOCAL uValue AS USUAL
	LOCAL cbKey AS USUAL
	LOCAL nNextCount AS LONGINT
	LOCAL lRestOfFile AS LOGIC
	LOCAL iTally := 0 AS INT
	LOCAL oError AS USUAL
	LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL oHLTemp AS HyperLabel


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF ! SELF:Notify( NOTIFYINTENTTOMOVE )
			BREAK DbError{ SELF, #Count, 999, VO_Sprintf( __CAVOSTR_DBFCLASS_INTENTTOMOVE ) }
		ENDIF
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
			SELF:__DbServerEval( { || iTally++ },  ;
				cbForBlock,  ;
				cbWhileBlock,  ;
				nNextCount,  ;
				NIL,  ;
				lRestOfFile,  ;
				DBCCON,  ;
				DBCCREADONLY )


		ELSEIF lActiveScope
			SELF:__DbServerEval( { || iTally++ },  ;
				cbStoredForBlock,  ;
				cbStoredWhileBlock,  ;
				nStoredNextCount,  ;
				NIL,  ;
				lStoredRestOfFile,  ;
				DBCCON,  ;
				DBCCREADONLY )


		ELSEIF lSelectionActive
			uValue := uSelectionValue
			cbKey := cbSelectionIndexingExpression
			IF VoDbSeek( uSelectionValue, FALSE )
				SELF:__DbServerEval( { || iTally++ },  ;
					NIL,  ;
					{ || Eval( cbKey ) = uValue },  ;
					NIL,  ;
					NIL,  ;
					TRUE,  ;
					DBCCON,  ;
					DBCCREADONLY )
				siSelectionStatus := DBSELECTIONEOF
				IF ! VoDbGoBottom( )
					BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
				ENDIF
				IF ! VoDbSkip( 1 )
					BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
				ENDIF
			ELSE
				BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
			ENDIF
		ELSE
			SELF:__DbServerEval( { || iTally++ },  ;
				NIL,  ;
				NIL,  ;
				NIL,  ;
				NIL,  ;
				FALSE,  ;
				DBCCON,  ;
				DBCCREADONLY )
		ENDIF


		SELF:__ProcessConcurrency( TRUE )


		__DBSSetSelect( dwCurrentWorkArea )


	RECOVER USING oError
		SELF:__ProcessConcurrency( FALSE )
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oError, #Count )
		oErrorInfo := oError
		oHLTemp := oHLStatus
		iTally := 0
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




	RETURN iTally




/// <include file="Rdd.xml" path="doc/DbServer.CreateIndex/*" />
METHOD CreateIndex( oFSIndex, cExpr, cbExpr, lUnique )  AS LOGIC CLIPPER
	LOCAL cIndexFileName AS STRING
	LOCAL lRetCode AS LOGIC
	LOCAL oError AS USUAL
	LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL oHLTemp AS HyperLabel
	LOCAL lRestore AS LOGIC


	lRestore	:= DbSetRestoreWorkarea(TRUE)


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF SELF:Notify( NOTIFYINTENTTOMOVE )
			IF IsObject(oFSIndex) .and. __Usual.ToObject(oFSIndex) IS FileSpec VAR oFs
				cIndexFileName := oFs:FullPath
			ELSEIF ! IsNil( oFSIndex )
				cIndexFileName := oFSIndex
			ELSE
				BREAK DbError{ SELF, #CreateIndex, EG_ARG,  ;
					VO_Sprintf( __CAVOSTR_DBFCLASS_INVALIDINDEX ) }
			ENDIF


			IF IsSymbol( cExpr )
				cExpr := Symbol2String( cExpr )
			ELSEIF IsNil( cExpr )
				cExpr := ""
				IF IsNil( cbExpr )
					BREAK DbError{ SELF, #CreateIndex, EG_ARG,  ;
						VO_Sprintf( __CAVOSTR_DBFCLASS_KEYVALUE ) }
				ENDIF
			ENDIF


			IF IsNil( cbExpr )
				cbExpr := &( "{ || " + cExpr + " }" )
			ENDIF


			IF IsNil( lUnique )
				lUnique := SetUnique( )
			ENDIF


			IF ! VoDbOrdCreate( cIndexFileName, NIL, cExpr, cbExpr, lUnique, NULL )
				BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
			ENDIF
			lRetCode := TRUE


			siSelectionStatus := DBSELECTIONNULL


		ELSE
			lRetCode := FALSE
			SELF:__SetStatusHL( #CreateIndex, __CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE_CAPTION ),  ;
				__CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE ) )
			oHLTemp := oHLStatus
		ENDIF
		__DBSSetSelect( dwCurrentWorkArea )


	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oHLTemp := oHLStatus
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
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
	DbSetRestoreWorkarea(lRestore)




	RETURN lRetCode




/// <include file="Rdd.xml" path="doc/DbServer.CreateOrder/*" />
METHOD CreateOrder( cOrderName, cIndexFileName, cExpr, cbExpr, lUnique )  AS LOGIC CLIPPER
	LOCAL lRetCode AS LOGIC
	LOCAL oError AS USUAL
	LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL oHLTemp AS HyperLabel
	LOCAL lRestore AS LOGIC


	lRestore	:= DbSetRestoreWorkarea(TRUE)


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF SELF:Notify( NOTIFYINTENTTOMOVE )
			IF IsObject(cIndexFileName) .and. __Usual.ToObject(cIndexFileName) IS FileSpec VAR oFs
				cIndexFileName := oFs:FullPath
			ELSEIF IsNil( cIndexFileName )
				IF IsNil( cOrderName )
					BREAK DbError{ SELF, #CreateOrder, EG_ARG,  ;
						VO_Sprintf( __CAVOSTR_DBFCLASS_INVALIDORDER ) }
				ENDIF
				cIndexFileName := ""
			ENDIF


			IF IsSymbol( cExpr )
				cExpr := Symbol2String( cExpr )
			ELSEIF IsNil( cExpr )
				cExpr := ""
				IF IsNil( cbExpr )
					BREAK DbError{ SELF, #CreateOrder, EG_ARG,  ;
						VO_Sprintf( __CAVOSTR_DBFCLASS_KEYVALUE ) }
				ENDIF
			ENDIF
			IF IsNil( cbExpr )
				cbExpr := &( "{ || " + cExpr + " }" )
			ENDIF


			IF IsNil( lUnique )
				lUnique := SetUnique( )
			ENDIF


			IF ! VoDbOrdCreate( cIndexFileName, cOrderName, cExpr, cbExpr, lUnique, NULL )
				BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
			ENDIF
			lRetCode := TRUE
			siSelectionStatus := DBSELECTIONEOF
		ELSE
			lRetCode := FALSE
			SELF:__SetStatusHL( #CreateOrder, __CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE_CAPTION ),  ;
				__CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE ) )
			oHLTemp := oHLStatus
		ENDIF
		__DBSSetSelect( dwCurrentWorkArea )


	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oHLTemp := oHLStatus
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
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
	DbSetRestoreWorkarea(lRestore)


	RETURN lRetCode




/// <include file="Rdd.xml" path="doc/DbServer.DataField/*" />
METHOD DataField( uField AS USUAL) AS DataField


	LOCAL oResult AS DataField
	LOCAL dwPos   AS DWORD
	LOCAL dwCurrentWorkArea := 0 AS DWORD




	IF SELF:wWorkArea == 0
		SELF:__SetStatusHL( #DataField, EG_NOTABLE, __CavoStr( __CAVOSTR_DBFCLASS_NOTABLE2 ) )
		oResult := NULL_OBJECT
	ELSE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		dwPos := __GetFldPos( uField, wFieldCount )
		__DBSSetSelect( dwCurrentWorkArea )
		IF dwPos > 0
			IF IsNil( aDataFields[dwPos] )
				aDataFields[dwPos] := SELF:__BuildDataField( aStruct[dwPos] )
			ENDIF
			oResult := aDataFields[dwPos]
		ELSE
			SELF:__SetStatusHL( #DataField, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_FIELDSPEC ) )
			oResult := NULL_OBJECT
		ENDIF
	ENDIF


	RETURN oResult




/// <include file="Rdd.xml" path="doc/DbServer.Delete/*" />
METHOD Delete( cbForBlock, cbWhileBlock, uScope ) AS LOGIC CLIPPER
	LOCAL nNextCount AS LONGINT
	LOCAL lRestOfFile AS LOGIC
	LOCAL lRetCode AS LOGIC
	LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL oError AS USUAL
	LOCAL nCurrRec AS DWORD
	LOCAL uFlock AS USUAL


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF ! IsNil( cbForBlock ) .OR. ! IsNil( cbWhileBlock ) .OR. ! IsNil( uScope )
			IF SELF:Notify( NOTIFYINTENTTOMOVE )
				IF IsString( cbForBlock )
					cbForBlock := &( "{ ||" + cbForBlock + " }" )
				ENDIF


				IF IsString( cbWhileBlock )
					cbWhileBlock := &( "{ ||" + cbWhileBlock + " }" )
				ENDIF


				IF ! IsNil( uScope )
					IF IsNumeric( uScope )
						nNextCount := uScope
					ELSE
						lRestOfFile := uScope
					ENDIF
				ENDIF


				lRetCode := SELF:__DbServerEval( { || VoDbDelete( ) },  ;
					cbForBlock,  ;
					cbWhileBlock,  ;
					nNextCount,  ;
					NIL,  ;
					lRestOfFile,  ;
					DBCCON,  ;
					DBCCUPDATE )
				SELF:Notify( NOTIFYFILECHANGE )
				IF ! lRetCode
					BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
				ENDIF


			ELSE
				lRetCode := FALSE
				SELF:__SetStatusHL( #Delete, __CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE_CAPTION ),  ;
					__CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE ) )
			ENDIF


		ELSEIF lActiveScope
			IF SELF:Notify( NOTIFYINTENTTOMOVE )
				lRetCode := SELF:__DbServerEval( { || VoDbDelete( ) },  ;
					cbStoredForBlock,  ;
					cbStoredWhileBlock,  ;
					nStoredNextCount,  ;
					NIL,  ;
					lStoredRestOfFile,  ;
					DBCCON,  ;
					DBCCUPDATE )
				SELF:Notify( NOTIFYFILECHANGE )
				IF ! lRetCode
					BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
				ENDIF
			ELSE
				lRetCode := FALSE
				SELF:__SetStatusHL( #Delete, __CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE_CAPTION ),  ;
					__CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE ) )
			ENDIF
		ELSE
			IF ! VoDbInfo( DBI_ISFLOCK, REF uFlock )
				BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
			ENDIF
			IF nEffectiveCCMode == ccOptimistic .AND.  ;
					( nCurrRec := VoDbRecno( ) ) <= VoDbLastRec( ) .AND. ! uFlock
				IF ! VoDbRlock( nCurrRec )
					BREAK DbError{ NIL, #Delete, EG_LOCK, __CavoStr( __CAVOSTR_DBFCLASS_LOCKFAILED ) }
				ENDIF
				IF ! VoDbDelete( )
					BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
				ENDIF
				lRetCode := TRUE
				IF ! uFlock
					VoDbUnlock( nCurrRec )
				ENDIF
			ELSE
				IF ! VoDbDelete( )
					BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
				ENDIF
				lRetCode := TRUE
			ENDIF
			SELF:Notify( NOTIFYDELETE )
		ENDIF


		IF lRetCode
			lRetCode := SELF:__ProcessConcurrency(  TRUE )
		ENDIF


		__DBSSetSelect( dwCurrentWorkArea )


	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError


		SELF:__ProcessConcurrency( FALSE )


		__DBSSetSelect( dwCurrentWorkArea )
		lRetCode := FALSE
	END SEQUENCE
	RETURN lRetCode




/// <include file="Rdd.xml" path="doc/DbServer.DeleteAll/*" />
METHOD DeleteAll( )   AS LOGIC STRICT
	LOCAL uValue AS USUAL
	LOCAL cbKey AS USUAL
	LOCAL lRetCode AS LOGIC
	LOCAL oError AS USUAL
	LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL oHLTemp AS HyperLabel


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF SELF:Notify( NOTIFYINTENTTOMOVE )
			IF lSelectionActive
				uValue := uSelectionValue
				cbKey := cbSelectionIndexingExpression
				IF VoDbSeek( uSelectionValue, FALSE )
					lRetCode := SELF:__DbServerEval( { || VoDbDelete( ) },  ;
						NIL,  ;
						{ || Eval( cbKey ) = uValue },  ;
						NIL,  ;
						NIL,  ;
						TRUE,  ;
						DBCCON,  ;
						DBCCUPDATE )
					IF ! VoDbGoBottom( )
						BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
					ENDIF
					IF ! VoDbSkip( 1 )
						BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
					ENDIF
				ENDIF


			ELSE
				lRetCode := SELF:__DbServerEval( { || VoDbDelete( ) },  ;
					NIL,  ;
					NIL,  ;
					NIL,  ;
					NIL,  ;
					FALSE,  ;
					DBCCON,  ;
					DBCCUPDATE )
			ENDIF


			siSelectionStatus := DBSELECTIONEOF


			IF ! lRetCode
				BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
			ENDIF


			SELF:__ProcessConcurrency( TRUE )


		ELSE
			lRetCode := FALSE
			SELF:__SetStatusHL( #DeleteAll, __CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE_CAPTION ),  ;
				__CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE ) )
			oHLTemp := oHLStatus
		ENDIF
		__DBSSetSelect( dwCurrentWorkArea )


	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oHLTemp := oHLStatus
		oErrorInfo := oError
		SELF:__ProcessConcurrency( FALSE )
		__DBSSetSelect( dwCurrentWorkArea )
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




	RETURN lRetCode




/// <include file="Rdd.xml" path="doc/DbServer.DeleteOrder/*" />
METHOD DeleteOrder( uOrder, cIndexFileName )  AS LOGIC CLIPPER
	LOCAL lRetCode AS LOGIC
	LOCAL oError AS USUAL
	LOCAL cOrder AS USUAL
	LOCAL cOrdBag AS STRING
	LOCAL dwCurrentWorkArea := 0 AS DWORD


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF IsObject(cIndexFileName) .and. __Usual.ToObject(cIndexFileName) IS FileSpec VAR oFs
			cOrdBag := oFs:FullPath
		ELSE
			cOrdBag := cIndexFileName
		ENDIF


		IF IsNumeric( uOrder )
			cOrder := NIL
			VoDbOrderInfo( DBOI_NAME, "", uOrder, REF cOrder )
			uOrder := cOrder
		ELSEIF ! IsString( uOrder )
			BREAK DbError{ SELF, #DeleteOrder, EG_ARG, VO_Sprintf( __CAVOSTR_DBFCLASS_INVALIDORDER ) }
		ENDIF


		lRetCode := TRUE
		IF ! VoDbOrdDestroy( cOrdBag, uOrder )
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




/// <include file="Rdd.xml" path="doc/DbServer.Error/*" />
METHOD Error( oError AS Error, symMethod AS SYMBOL) AS USUAL
	STATIC LOCAL lErrorProcessingSemaphor AS LOGIC
	LOCAL cErrorValType AS STRING
    LOCAL oErr  as Error




	IF lErrorProcessingSemaphor
		Eval( ErrorBlock( ), oError )
	ELSE
		lErrorProcessingSemaphor := TRUE


		IF ! IsInstanceOfUsual( oError, #Error )
			cErrorValType := ValType( oError )
			oErr := oError := Error{ }
			oErr:Gencode := EG_ERRORBUILD
			oErr:Description := VO_Sprintf( __CAVOSTR_DBFCLASS_BADERROROBJECT, cErrorValType )
        ELSE
            oErr := oError
		ENDIF
		IF IsNil( symMethod ) .OR. ! IsSymbol( symMethod )
			oErr:FuncSym := #Unknown
		ELSE
			oErr:FuncSym := symMethod
		ENDIF


		oErr:MethodSelf := SELF


		oHLStatus := SELF:__GenerateStatusHL( oErr )


		IF IsArray( aClients ) .AND. nClients != 0 .AND. IsMethod( aClients[1], #Error )
			Send(aClients[1],#Error, oErr )
		ELSE
			lErrorProcessingSemaphor := FALSE
			Eval( ErrorBlock( ), oErr )
		ENDIF


		lErrorProcessingSemaphor := FALSE


	ENDIF




	RETURN SELF




/// <include file="Rdd.xml" path="doc/DbServer.Eval/*" />
METHOD Eval( cbBlock, cbForBlock, cbWhileBlock, uScope )   AS LOGIC CLIPPER
	LOCAL uValue AS USUAL
	LOCAL cbKey AS USUAL
	LOCAL nNextCount AS LONGINT
	LOCAL lRestOfFile AS LOGIC
	LOCAL lRetCode AS LOGIC
	LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL oError AS USUAL
	LOCAL oHLTemp AS HyperLabel


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF SELF:Notify( NOTIFYINTENTTOMOVE )
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
				lRetCode := SELF:__DbServerEval( cbBlock, cbForBlock,  ;
					cbWhileBlock,  ;
					nNextCount,  ;
					NIL,  ;
					lRestOfFile,  ;
					DBCCON,  ;
					DBCCUPDATE )


			ELSEIF lActiveScope
				lRetCode := SELF:__DbServerEval( cbBlock, cbStoredForBlock,  ;
					cbStoredWhileBlock,  ;
					nStoredNextCount,  ;
					NIL,  ;
					lStoredRestOfFile,  ;
					DBCCON,  ;
					DBCCUPDATE )


			ELSEIF lSelectionActive
				uValue := uSelectionValue
				cbKey := cbSelectionIndexingExpression
				IF VoDbSeek( uSelectionValue, FALSE )
					lRetCode := SELF:__DbServerEval( cbBlock,  ;
						NIL,  ;
						{ || Eval( cbKey ) = uValue },  ;
						NIL,  ;
						NIL,  ;
						TRUE,  ;
						DBCCON,  ;
						DBCCUPDATE )
					siSelectionStatus := DBSELECTIONEOF
					IF ! VoDbGoBottom( )
						BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
					ENDIF
					IF ! VoDbSkip( 1 )
						BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
					ENDIF
				ENDIF


			ELSE
				lRetCode := SELF:__DbServerEval( cbBlock,  ;
					NIL,  ;
					NIL,  ;
					NIL,  ;
					NIL,  ;
					FALSE,  ;
					DBCCON,  ;
					DBCCUPDATE )
			ENDIF


			IF ! lRetCode
				BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
			ENDIF


			SELF:__ProcessConcurrency( TRUE )


		ELSE
			lRetCode := FALSE
			SELF:__SetStatusHL ( #Eval, __CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE_CAPTION ),  ;
				__CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE ) )
			oHLTemp := oHLStatus
		ENDIF
		__DBSSetSelect( dwCurrentWorkArea )


	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oHLTemp := oHLStatus
		oErrorInfo := oError
		lRetCode := FALSE
		SELF:__ProcessConcurrency( FALSE )
		__DBSSetSelect( dwCurrentWorkArea )
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




	RETURN lRetCode




/// <include file="Rdd.xml" path="doc/DbServer.FieldGet/*" />
METHOD FieldGet( uField AS USUAL) AS USUAL


	LOCAL uRetVal AS USUAL
	LOCAL oError AS USUAL
	LOCAL wPos AS DWORD
	LOCAL dwCurrentWorkArea := 0 AS DWORD


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )


        IF (wPos := __GetFldPos( uField, wFieldCount )) = 0
         BREAK DbError{ SELF, #FIELDGET, EG_ARG, ;
            __CavoStr( __CAVOSTR_DBFCLASS_FIELDSPEC ), uField, "uField" }
        ENDIF


		IF wPos > 0  .AND. nEffectiveCCMode == ccOptimistic .AND. lCCOptimisticRecChg .AND. ;
				aCurrentBuffer[BUFFER_IS_CHANGED, wPos] .AND. ! aOriginalBuffer[BUFFER_IS_BLOB, wPos]


			uRetVal := aCurrentBuffer[BUFFER_VALUE, wPos]
		ELSE
			IF ! VoDbFieldGet( wPos, REF uRetVal )
				BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
			ENDIF
		ENDIF


		__DBSSetSelect( dwCurrentWorkArea )


	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oErrorInfo, #FIELDGET )
	END SEQUENCE


	RETURN uRetVal


/// <include file="Rdd.xml" path="doc/DbServer.FieldGetBytes/*" />
METHOD FieldGetBytes( uField AS USUAL ) AS BYTE[]
	LOCAL bRetVal AS BYTE[]
	LOCAL oError AS USUAL
	LOCAL wPos AS DWORD
	LOCAL dwCurrentWorkArea := 0 AS DWORD


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
        IF (wPos := __GetFldPos( uField, wFieldCount )) = 0
             BREAK DbError{ SELF, #FIELDGET, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_FIELDSPEC ), uField, "uField" }
        ENDIF
		bRetVal := NULL
		IF ! VoDbFieldGetBytes( wPos, REF bRetVal )
			BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
		ENDIF


		__DBSSetSelect( dwCurrentWorkArea )


	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oErrorInfo, #FieldGetBytes )
	END SEQUENCE


	RETURN bRetVal


/// <include file="Rdd.xml" path="doc/DbServer.FieldGetFormatted/*" />
METHOD FieldGetFormatted( uField AS USUAL) AS USUAL


	LOCAL uRetVal AS USUAL
	LOCAL wPos AS DWORD
	LOCAL oError AS USUAL
	LOCAL dwCurrentWorkArea := 0 AS DWORD


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF (wPos := __GetFldPos( uField, wFieldCount )) == 0
			BREAK DbError{ SELF, #FieldGetFormatted, EG_ARG,  ;
				__CavoStr( __CAVOSTR_DBFCLASS_FIELDSPEC ), uField, "uField" }
		ELSE
			IF nEffectiveCCMode == ccOptimistic .AND. lCCOptimisticRecChg .AND.  ;
					aCurrentBuffer[BUFFER_IS_CHANGED, wPos] .AND. ! aOriginalBuffer[BUFFER_IS_BLOB, wPos]


				uRetVal := aCurrentBuffer[BUFFER_VALUE, wPos]
			ELSE
				IF ! VoDbFieldGet( wPos, REF uRetVal )
					BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
				ENDIF
			ENDIF
		ENDIF
		__DBSSetSelect( dwCurrentWorkArea )
		IF IsNil( aDataFields[wPos] )
			aDataFields[wPos] := SELF:__BuildDataField( aStruct[wPos] )
		ENDIF
        uRetVal := SELF:DataField(wPos):FieldSpec:Transform( uRetVal )


	RECOVER USING oError
		oErrorInfo := oError
		SELF:Error( oErrorInfo, #FieldGetFormatted )
		__DBSSetSelect( dwCurrentWorkArea )
		RETURN NULL_STRING
	END SEQUENCE


	RETURN uRetVal




/// <include file="Rdd.xml" path="doc/DbServer.FieldHyperLabel/*" />
METHOD FieldHyperLabel( uField AS USUAL) AS HyperLabel


	LOCAL wPos AS DWORD
	LOCAL uRetVal AS USUAL
	LOCAL oError AS USUAL
	LOCAL dwCurrentWorkArea := 0 AS DWORD


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		wPos := __GetFldPos( uField, wFieldCount )
		__DBSSetSelect( dwCurrentWorkArea )
		IF wPos > 0
			IF IsNil( aDataFields[wPos] )
				aDataFields[wPos] := SELF:__BuildDataField( aStruct[wPos] )
			ENDIF
			uRetVal := SELF:DataField(wPos):HyperLabel
		ELSE
			BREAK DbError{ SELF, #FieldHyperlabel, EG_ARG,  ;
				__CavoStr( __CAVOSTR_DBFCLASS_FIELDSPEC ), uField, "uField" }
		ENDIF


	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		uRetVal := NULL_OBJECT
	END SEQUENCE


	RETURN uRetVal




/// <include file="Rdd.xml" path="doc/DbServer.FieldInfo/*" />
METHOD FieldInfo( kFieldInfoType, uField, uFieldVal ) AS USUAL CLIPPER


	LOCAL nPos AS DWORD
	LOCAL oError AS USUAL
	LOCAL dwCurrentWorkArea := 0 AS DWORD


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF kFieldInfoType == DBS_BLOB_DIRECT_LEN .OR.  ;
				kFieldInfoType == DBS_BLOB_DIRECT_TYPE
			nPos := uField
		ELSE
			nPos := __GetFldPos( uField, wFieldCount )
		ENDIF


		IF ! VoDbFieldInfo( kFieldInfoType, nPos, REF uFieldVal )
			BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
		ENDIF
		__DBSSetSelect( dwCurrentWorkArea )


	RECOVER USING oError
		oErrorInfo := oError
		SELF:Error( oErrorInfo, #FieldInfo )
		__DBSSetSelect( dwCurrentWorkArea )
		uFieldVal := NIL
	END SEQUENCE


	RETURN uFieldVal




/// <include file="Rdd.xml" path="doc/DbServer.FieldName/*" />
METHOD FieldName( nFieldPosition AS USUAL ) AS STRING


	LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL uRetVal AS USUAL
	LOCAL oError AS USUAL


	VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		IF nFieldPosition > 0 .AND. nFieldPosition <= wFieldCount
			uRetVal := FieldName( nFieldPosition )
		ELSE
			SELF:__SetStatusHL ( #FieldName, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_FIELDSPEC ) )
			uRetVal := NULL_STRING
		ENDIF


	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		uRetVal := NULL_STRING
	END SEQUENCE


	__DBSSetSelect( dwCurrentWorkArea )


	RETURN uRetVal




/// <include file="Rdd.xml" path="doc/DbServer.FieldPos/*" />
METHOD FieldPos( cFieldName AS USUAL) AS DWORD
	LOCAL uRetVal AS USUAL
	LOCAL oError AS USUAL
	LOCAL dwCurrentWorkArea := 0 AS DWORD


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		uRetVal := __GetFldPos( cFieldName, wFieldCount )
		__DBSSetSelect( dwCurrentWorkArea )


	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		uRetVal := 0
	END SEQUENCE


	RETURN uRetVal




/// <include file="Rdd.xml" path="doc/DbServer.FieldPut/*" />
METHOD FieldPut( uField AS USUAL, uValue AS USUAL)  AS USUAL
	LOCAL wPos AS DWORD
	LOCAL symFieldName AS SYMBOL
	LOCAL uError AS USUAL
	LOCAL dwCurrentWorkArea := 0 AS DWORD
	//LOCAL nCurRec AS LONGINT
	// 	LOCAL uVORetVal AS USUAL
	LOCAL uRLock AS USUAL


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
        IF (wPos := __GetFldPos( uField, wFieldCount )) = 0
            BREAK DbError{ SELF, #FIELDGET, EG_ARG, ;
                 __CavoStr( __CAVOSTR_DBFCLASS_FIELDSPEC ), uField, "uField" }
        ENDIF
        IF nEffectiveCCMode == ccOptimistic .AND. VoDbRecno() <= VoDbLastRec()
            //type checking for optimistic locking
            IF ! __CheckFieldType(REF uValue, aStruct[wPos], REF uError)
                ASize(uError, 3)
                BREAK DbError{SELF, #FIELDPUT, uError[1], VO_Sprintf(uError[2], "Field " + aStruct[wPos,DBS_NAME], uError[3]), uValue, "uValue"}
            ENDIF


			IF ! VoDbRecordInfo( DBRI_LOCKED, 0, REF uRLock )
				BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
			ENDIF
			IF ! uRLock
				aCurrentBuffer[BUFFER_VALUE, wPos]		:= uValue
				aCurrentBuffer[BUFFER_IS_CHANGED, wPos]	:= TRUE
				lCCOptimisticRecChg := TRUE
			ELSE
				IF ! VoDbFieldPut( wPos, uValue )
					BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
				ENDIF
			ENDIF
		ELSE
			IF ! VoDbFieldPut( wPos, uValue )
				BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
			ENDIF
		ENDIF
      symFieldName := FieldSym(wPos)
		SELF:Notify( Notify.FieldChange, symFieldName )
		__DBSSetSelect( dwCurrentWorkArea )


	RECOVER USING uError
		oErrorInfo := uError
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oErrorInfo, #FIELDPUT )
		uValue := NIL
	END SEQUENCE


	RETURN uValue




/// <include file="Rdd.xml" path="doc/DbServer.FieldPutBytes/*" />
METHOD FieldPutBytes( uField AS USUAL, bValue AS BYTE[])  AS BYTE[]
	LOCAL wPos AS DWORD
	LOCAL symFieldName AS SYMBOL
	LOCAL uError AS USUAL
	LOCAL dwCurrentWorkArea := 0 AS DWORD


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
        IF (wPos := __GetFldPos( uField, wFieldCount )) = 0
            BREAK DbError{ SELF, #FIELDGET, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_FIELDSPEC ), uField, "uField" }
        ENDIF


		IF ! VoDbFieldPutBytes( wPos, bValue )
			BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
		ENDIF
      symFieldName := FieldSym(wPos)
		SELF:Notify( Notify.FieldChange, symFieldName )
		__DBSSetSelect( dwCurrentWorkArea )


	RECOVER USING uError
		oErrorInfo := uError
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oErrorInfo, #FieldPutBytes )
		bValue := NULL
	END SEQUENCE


	RETURN bValue


/// <include file="Rdd.xml" path="doc/DbServer.FieldSpec/*" />
METHOD FieldSpec( uField AS USUAL)  AS FieldSpec


	LOCAL wPos AS DWORD
	LOCAL uRetVal AS USUAL
	LOCAL oError AS USUAL
	LOCAL dwCurrentWorkArea := 0 AS DWORD


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		wPos := __GetFldPos( uField, wFieldCount )
		__DBSSetSelect( dwCurrentWorkArea )
		IF wPos > 0
			IF IsNil( aDataFields[wPos] )
				aDataFields[wPos] := SELF:__BuildDataField( aStruct[wPos] )
			ENDIF
			uRetVal := SELF:DataField(wPos):FieldSpec
		ELSE
			SELF:__SetStatusHL( #FieldSpec, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_FIELDSPEC ) )
			uRetVal := NULL_OBJECT
		ENDIF


	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		uRetVal := NULL_OBJECT
	END SEQUENCE


	RETURN uRetVal




/// <include file="Rdd.xml" path="doc/DbServer.FieldStatus/*" />
METHOD FieldStatus( uField AS USUAL) AS HyperLabel


	LOCAL wPos AS DWORD
	LOCAL uRetVal AS USUAL
	LOCAL oError AS USUAL
	LOCAL dwCurrentWorkArea := 0 AS DWORD


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		wPos := __GetFldPos( uField, wFieldCount )
		__DBSSetSelect( dwCurrentWorkArea )
		IF wPos > 0
			IF IsNil( aDataFields[wPos] )
				aDataFields[wPos] := SELF:__BuildDataField( aStruct[wPos] )
			ENDIF
			uRetVal := SELF:DataField(wPos):FieldSpec:Status
		ELSE
			SELF:__SetStatusHL( #FieldStatus, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_FIELDSPEC ) )
			uRetVal := NULL_OBJECT
		ENDIF


	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		uRetVal := NULL_OBJECT
	END SEQUENCE


	RETURN uRetVal




/// <include file="Rdd.xml" path="doc/DbServer.FieldSym/*" />
METHOD FieldSym( uField AS USUAL) AS SYMBOL


	LOCAL wPos AS DWORD
	LOCAL uRetVal AS USUAL
	LOCAL oError AS USUAL
	LOCAL dwCurrentWorkArea := 0 AS DWORD


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF (wPos := __GetFldPos( uField, wFieldCount )) > 0
			uRetVal := FieldSym( wPos )
		ELSE
			SELF:__SetStatusHL( #FieldSym, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_FIELDSPEC ) )
			uRetVal := NULL_SYMBOL
		ENDIF
		__DBSSetSelect( dwCurrentWorkArea )


	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		uRetVal := NULL_SYMBOL
	END SEQUENCE


	RETURN uRetVal




/// <include file="Rdd.xml" path="doc/DbServer.FieldValidate/*" />
METHOD FieldValidate( uField AS USUAL, uValue AS USUAL) AS LOGIC


	LOCAL wPos AS DWORD
	LOCAL uRetVal AS USUAL
	LOCAL oError AS USUAL
	LOCAL dwCurrentWorkArea := 0 AS DWORD


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		wPos := __GetFldPos( uField, wFieldCount )
		__DBSSetSelect( dwCurrentWorkArea )
		IF wPos > 0
			IF IsNil( aDataFields[wPos] )
				aDataFields[wPos] := SELF:__BuildDataField( aStruct[wPos] )
			ENDIF
			uRetVal := SELF:DataField(wPos):FieldSpec:PerformValidations( uValue )
		ELSE
			BREAK DbError{ SELF, #FieldValidate, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_FIELDSPEC ),  ;
				uField, "uField" }
		ENDIF


	RECOVER USING oError
		oErrorInfo := oError
		SELF:Error( oErrorInfo, #FieldValidate )
		__DBSSetSelect( dwCurrentWorkArea )
		uRetVal := FALSE
	END SEQUENCE


	RETURN uRetVal




/// <include file="Rdd.xml" path="doc/DbServer.FLock/*" />
METHOD FLock( )  AS LOGIC STRICT
	LOCAL lRetCode AS LOGIC
	LOCAL oError AS USUAL
	LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL nTries AS DWORD


	lErrorFlag := FALSE
	nTries := SELF:nRetries


	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		lRetCode := __DBSFLock( nTries )
		SELF:__OptimisticFlushNoLock( )
		__DBSSetSelect( dwCurrentWorkArea )


	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		lRetCode := FALSE
	END SEQUENCE


	RETURN lRetCode


END CLASS


