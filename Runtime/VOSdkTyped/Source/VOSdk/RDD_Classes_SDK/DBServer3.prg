
PARTIAL CLASS DbServer

METHOD GetArray( nMaxRows, uField1, uSearchValue )  
	LOCAL uValue AS USUAL
	LOCAL cbKey AS USUAL
	LOCAL aResult := { } AS ARRAY
	LOCAL wRows := 32767 AS DWORD
	LOCAL oError AS USUAL
	LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL oHLTemp AS HyperLabel
	LOCAL wPos AS DWORD


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF ! SELF:Notify( NOTIFYINTENTTOMOVE )
			BREAK DbError{ SELF, #GetArray, 999, VO_Sprintf( __CAVOSTR_DBFCLASS_INTENTTOMOVE ) }
		ENDIF

		IF IsNil( nMaxRows )
			wRows := 100
		ELSEIF nMaxRows < wRows
			wRows := nMaxRows
		ENDIF

		IF IsNil( uField1 )
			wPos := 1
		ELSEIF IsSymbol( uField1 )
			wPos := FieldPosSym( uField1 )
		ELSEIF IsString( uField1 )
			wPos := FieldPos( uField1 )
		ELSE
			wPos := uField1
		ENDIF

		IF lSelectionActive
			uValue := uSelectionValue
			cbKey := cbSelectionIndexingExpression
			IF ! VoDbSeek( uValue, FALSE )
				SELF:__SetStatusHL( #GetArray, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_NOSEEK ) )
				oHLTemp := oHLStatus
				aResult := NULL_ARRAY
			ELSE
				SELF:__DbServerEval( { | | AAdd( aResult, __DBSFieldGet( wPos ) ) },  ;
					NIL,  ;
					{ || Eval( cbKey ) = uValue },  ;
					wRows,  ;
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
			ENDIF

		ELSEIF ! IsNil( uSearchValue )
			IF ! VoDbSeek( uSearchValue, FALSE )
				SELF:__SetStatusHL( #GetArray, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_NOSEEK ) )
				oHLTemp := oHLStatus
				aResult := NULL_ARRAY
			ELSE
				cbKey := &( "{ ||"+__DBSDbOrderInfo( DBOI_EXPRESSION, "", 0 ) + " }" )
				SELF:__DbServerEval( { || AAdd( aResult, __DBSFieldGet( wPos ) ) },  ;
					NIL,  ;
					NIL,  ;
					wRows,  ;
					NIL,  ;
					TRUE,  ;
					DBCCON,  ;
					DBCCREADONLY )
			ENDIF

		ELSE
			SELF:__DbServerEval( { || AAdd( aResult, __DBSFieldGet( wPos ) ) },  ;
				NIL,  ;
				NIL,  ;
				wRows,  ;
				NIL,  ;
				TRUE,  ;
				DBCCON,  ;
				DBCCREADONLY )
		ENDIF

		SELF:__ProcessConcurrency( TRUE )

		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527

	RECOVER USING oError
		SELF:__ProcessConcurrency(FALSE)
		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527
		SELF:Error( oError, #GetArray )
		oErrorInfo := oError
		oHLTemp := oHLStatus
		aResult := NULL_ARRAY
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

	RETURN aResult

METHOD GetLocate ( ) 
	//SE-060601
   LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL oError AS USUAL
	LOCAL uInfo AS USUAL

	

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF ! VoDbInfo( DBI_GETSCOPE, REF uInfo )
			BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
		ENDIF
      __DBSSetSelect( dwCurrentWorkArea )
	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oErrorInfo, #Info )
	END SEQUENCE

	

	RETURN uInfo

METHOD GetLookupTable( nMaxRows, uField1, uField2, uSearchValue )  
	LOCAL uValue AS USUAL
	LOCAL cbKey AS USUAL
	LOCAL aResult := { } AS ARRAY
	LOCAL wRows := 32767 AS DWORD
	LOCAL oError AS USUAL
	LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL oHLTemp AS HyperLabel

	

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF ! SELF:Notify( NOTIFYINTENTTOMOVE )
			BREAK DbError{ SELF, #GetLookupTable, 999, VO_Sprintf( __CAVOSTR_DBFCLASS_INTENTTOMOVE ) }
		ENDIF

		IF IsNil( nMaxRows )
			wRows := 100
		ELSEIF nMaxRows < wRows
			wRows := nMaxRows
		ENDIF

		IF IsNil( uField1 )
			uField1 := 1
		ELSEIF IsSymbol( uField1 )
			uField1 := FieldPosSym( uField1 )
		ELSEIF IsString( uField1 )
			uField1 := FieldPos( uField1 )
		ENDIF

		IF IsNil( uField2 )
			uField2 := 2
		ELSEIF IsSymbol( uField2 )
			uField2 := FieldPosSym( uField2 )
		ELSEIF IsString( uField2 )
			uField2 := FieldPos( uField2 )
		ENDIF

		IF lSelectionActive
			uValue := uSelectionValue
			cbKey := cbSelectionIndexingExpression
			IF ! VoDbSeek( uValue, FALSE )
				SELF:__SetStatusHL( #GetLookupTable, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_NOSEEK ) )
				oHLTemp := oHLStatus
				aResult := NULL_ARRAY
			ELSE
				SELF:__DbServerEval( { || AAdd( aResult, { __DBSFieldGet( uField1 ), __DBSFieldGet( uField2 ) } ) },  ;
					NIL,  ;
					{ || Eval( cbKey ) == uValue },  ;
					wRows,  ;
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
			ENDIF

		ELSEIF ! IsNil( uSearchValue )
			IF ! VoDbSeek( uSearchValue, FALSE )
				SELF:__SetStatusHL ( #GetLookupTable, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_NOSEEK ) )
				oHLTemp := oHLStatus
				aResult := NULL_ARRAY
			ELSE
				SELF:__DbServerEval( { || AAdd( aResult, { __DBSFieldGet( uField1 ), __DBSFieldGet( uField2 ) } ) },  ;
					NIL,  ;
					NIL,  ;
					wRows,  ;
					NIL,  ;
					TRUE,  ;
					DBCCON,  ;
					DBCCREADONLY )
			ENDIF

		ELSE
			SELF:__DbServerEval( { || AAdd( aResult, { __DBSFieldGet( uField1 ), __DBSFieldGet( uField2 ) } ) },  ;
				NIL,  ;
				NIL,  ;
				wRows,  ;
				NIL,  ;
				TRUE,  ;
				DBCCON,  ;
				DBCCREADONLY )
		ENDIF

		SELF:__ProcessConcurrency( TRUE )
		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527

	RECOVER USING oError
		oErrorInfo := oError
		SELF:__ProcessConcurrency( FALSE )
		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527
		SELF:Error( oError, #GetLookupTable )
		oErrorInfo := oError
		oHLTemp := oHLStatus
		aResult := NULL_ARRAY
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

	
	RETURN aResult

METHOD GoBottom( )   AS LOGIC
	LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL uValue AS USUAL
	LOCAL cbKey AS USUAL
	LOCAL lRetCode := FALSE AS LOGIC
	LOCAL oError AS USUAL
	LOCAL oHLTemp AS HyperLabel
	LOCAL nTries AS DWORD

	

	lErrorFlag := FALSE
	nTries := SELF:nRetries

	BEGIN SEQUENCE
		VoDbSelect( SELF:wWorkArea, OUT dwCurrentWorkArea )
		IF SELF:Notify( NOTIFYINTENTTOMOVE )
			IF lSelectionActive
				IF siSelectionStatus == DBSELECTIONEMPTY
					lRetCode := TRUE
				ELSEIF siSelectionStatus == DBSELECTIONEOF
					lRetCode := SELF:Skip( -1 )
					siSelectionStatus := DBSELECTIONNULL
				ELSE
					uValue := uSelectionValue
					cbKey := cbSelectionIndexingExpression
					__DBSSeek( uSelectionValue, FALSE, FALSE , nTries )
					IF Eval( cbKey ) = uValue .OR. VoDbFound( )
						lRetCode := SELF:__DbServerEval( { || },  ;
							NIL,  ;
							{ || Eval( cbKey ) = uValue },  ;
							NIL,  NIL,  TRUE , FALSE, FALSE)
						lRetCode := __DBSSkip( -1, nTries )
						siSelectionStatus := DBSELECTIONNULL
					ELSE
						siSelectionStatus := DBSELECTIONEMPTY
						SELF:__SetStatusHL( #GoBottom, EG_BOUND, __CavoStr( __CAVOSTR_DBFCLASS_SELECTIVEVALUE ) )
						oHLTemp := oHLStatus
						lRetCode := FALSE
					ENDIF
				ENDIF
			ELSE
				lRetCode := __DBSGoBottom( nTries )
			ENDIF

			SELF:Notify( NOTIFYGOBOTTOM )
			IF ! lRetCode .AND. ! IsNil( oHLTemp )
				lErrorFlag := TRUE
				oHLStatus := oHLTemp
			ENDIF
			IF lRetCode
				lRetCode := SELF:__ProcessConcurrency( TRUE )
			ENDIF
		ELSE
			lRetCode := FALSE
			SELF:__SetStatusHL( #GoBottom, __CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE_CAPTION ),  ;
				__CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE ) )
		ENDIF

		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527

	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527
		lRetCode := FALSE
	END SEQUENCE


	RETURN lRetCode
 
METHOD GoTo( nRecordNumber ) AS LOGIC
	LOCAL nCurrentRecord AS LONGINT
	LOCAL lRetCode := FALSE AS LOGIC
	LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL oError AS USUAL
	LOCAL nTries AS DWORD


	lErrorFlag := FALSE

	nTries := SELF:nRetries

	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF SELF:Notify( NOTIFYINTENTTOMOVE )
			nRecordNumber := INT( nRecordNumber )
			IF lSelectionActive

				IF siSelectionStatus == DBSELECTIONEMPTY
					lRetCode := TRUE
				ELSE
					nCurrentRecord := VoDbRecno( )
					lRetCode := __DBSGoTo( nRecordNumber, nTries )


					IF Eval( cbSelectionIndexingExpression ) = uSelectionValue
						siSelectionStatus := DBSELECTIONNULL

					ELSE

						IF ! siSelectionStatus == DBSELECTIONEOF
							__DBSGoBottom( nTries )
							__DBSSkip( 1, nTries )
						ELSE
							__DBSGoTo( nCurrentRecord, nTries )
						ENDIF
					ENDIF
				ENDIF
			ELSE
				lRetCode := __DBSGoTo( nRecordNumber, nTries )
			ENDIF
			IF lRetCode
				lRetCode := SELF:__ProcessConcurrency( TRUE )
			ENDIF
			SELF:Notify( NOTIFYRECORDCHANGE )

		ELSE
			lRetCode := FALSE
			SELF:__SetStatusHL( #GoTo, __CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE_CAPTION ),  ;
				__CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE ) )
		ENDIF

		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527

	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea ) //SE-060527
		lRetCode := FALSE
	END SEQUENCE


	RETURN lRetCode

METHOD GoTop( ) AS LOGIC
	LOCAL lRetCode := FALSE AS LOGIC
	LOCAL oError AS USUAL
	LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL nTries AS DWORD

	

	lErrorFlag := FALSE
	nTries     := SELF:nRetries

	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF SELF:Notify( NOTIFYINTENTTOMOVE )
			IF lSelectionActive
				IF siSelectionStatus == DBSELECTIONEMPTY
					lRetCode := TRUE
				ELSE
					lRetCode := __DBSSeek( uSelectionValue, FALSE, FALSE, nTries )
					IF lRetCode
						siSelectionStatus := DBSELECTIONNULL
					ELSE
						siSelectionStatus := DBSELECTIONEMPTY
					ENDIF
				ENDIF
			ELSE
				lRetCode := __DBSGoTop( nTries )
			ENDIF

			IF lRetCode
				lRetCode := SELF:__ProcessConcurrency( TRUE )
			ENDIF
			SELF:Notify( NOTIFYGOTOP )
		ELSE
			lRetCode := FALSE
			SELF:__SetStatusHL( #GoTop, __CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE_CAPTION ),  ;
				__CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE ) )
		ENDIF
		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527

	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527
		lRetCode := FALSE
	END SEQUENCE


	RETURN lRetCode

METHOD INDEXKEY( uOrder ) 
	//SE-060601
   LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL oError AS USUAL
	LOCAL uOrdVal AS USUAL


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF ! VoDbOrderInfo( DBOI_EXPRESSION, "", uOrder, REF uOrdVal )
			BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
		ENDIF
	   __DBSSetSelect( dwCurrentWorkArea )
	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oErrorInfo, #OrderInfo )
	END SEQUENCE


	RETURN uOrdVal

METHOD INDEXORD( ) 
	//SE-060601
   LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL oError AS USUAL
	LOCAL uOrdVal AS USUAL

	

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF ! VoDbOrderInfo( DBOI_NUMBER, "", NIL, REF uOrdVal )
			BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
		ENDIF
	   __DBSSetSelect( dwCurrentWorkArea )
	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oErrorInfo, #OrderInfo )
	END SEQUENCE


	RETURN uOrdVal

METHOD Info( kInfoType, uInfo ) 
	//SE-060601
   LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL oError AS USUAL


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF ! VoDbInfo( kInfoType, REF uInfo)
			BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
		ENDIF
      __DBSSetSelect( dwCurrentWorkArea )
	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oErrorInfo, #Info )
	END SEQUENCE


	
	RETURN uInfo

METHOD Join( oDBSource, oFSTarget, aFieldList, cbForBlock ) 
	LOCAL cSource AS STRING
	LOCAL cTarget AS STRING
	LOCAL aFieldNames AS ARRAY
	LOCAL w AS DWORD
	LOCAL lRetCode := FALSE AS LOGIC
	LOCAL oError AS USUAL
	LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL wLen AS DWORD
	LOCAL lRestore	AS LOGIC
	
   //RvdH 070711 Make sure we restore the workarea
   //				  The codeblocks may select another workarea
	lRestore	:= DbSetRestoreWorkarea(TRUE)      

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF SELF:Notify( NOTIFYINTENTTOMOVE )
			IF IsObject(oDBSource) .and. __Usual.ToObject(oDBSource) IS DbServer VAR oDb
				cSource := oDb:Alias
			ELSE
				cSource := AsString( oDBSource )
			ENDIF

			IF IsObject(oFSTarget) .and. __Usual.ToObject(oFSTarget) IS FileSpec VAR oFs
				cTarget := oFs:FullPath
			ELSE
				cTarget := AsString( oFSTarget )
			ENDIF

			aFieldNames := ArrayNew( ALen( aFieldList ) )
			wLen := ALen( aFieldList )
			FOR w := 1 UPTO wLen
				aFieldNames[w] := AsString( aFieldList[w] )
			NEXT
			IF IsNil( cbForBlock )
				cbForBlock := cbStoredForBlock
				IF IsNil( cbStoredForBlock )
					cbStoredForBlock := { || TRUE }
				ENDIF
			ENDIF
			lRetCode := __DBSDBJOIN( cSource, cTarget, aFieldNames, cbForBlock, SELF:cRDDName )
			lRetCode := SELF:__ProcessConcurrency( TRUE )
			siSelectionStatus := DBSELECTIONNULL
			SELF:Notify( NOTIFYRECORDCHANGE )

		ELSE
			lRetCode := FALSE
			SELF:__SetStatusHL ( #Join, __CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE_CAPTION ),  ;
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

	DbSetRestoreWorkarea(lRestore)
	RETURN lRetCode

METHOD Locate( cbForBlock, cbWhileBlock, uScope )  
	LOCAL uValue AS USUAL
	LOCAL cbKey AS USUAL
	LOCAL nNextCount AS LONGINT
	LOCAL lRestOfFile AS LOGIC
	LOCAL lRetCode := FALSE AS LOGIC
	LOCAL oError AS USUAL
	LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL oHLTemp AS HyperLabel
	LOCAL lRestore	AS LOGIC
	
   //RvdH 070711 Make sure we restore the workarea
   //				  The codeblocks may select another workarea
	lRestore	:= DbSetRestoreWorkarea(TRUE)      

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF SELF:Notify( NOTIFYINTENTTOMOVE )
			IF ! IsNil( cbForBlock ) .OR. ! IsNil( cbWhileBlock ) .OR. ! IsNil( uScope )
				IF Empty( cbForBlock )
					cbForBlock := { || TRUE }
				ELSEIF IsString( cbForBlock )
					cbForBlock := &( "{ || " + cbForBlock + " } " )
				ENDIF

				IF Empty( cbWhileBlock )
					cbWhileBlock := { || TRUE }
				ELSE
					lRestOfFile := TRUE
					IF IsString( cbWhileBlock )
						cbWhileBlock := &( "{ || " + cbWhileBlock + " }" )
					ENDIF
				ENDIF

				IF ! IsNil( uScope )
					IF IsNumeric( uScope )
						nNextCount := uScope
					ELSE
						lRestOfFile := uScope
					ENDIF
				ENDIF
				IF ! VoDbLocate( cbForBlock,  ;
					cbWhileBlock,  ;
					nNextCount,  ;
					NIL,  ;
					lRestOfFile )
					BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
				ENDIF
				lRetCode := VoDbFound( )

			ELSEIF lActiveScope
				lRestOfFile := lStoredRestOfFile
				IF IsNil( cbStoredWhileBlock )
					cbWhileBlock := { || TRUE }
				ELSE
					cbWhileBlock := cbStoredWhileBlock
					lRestOfFile := TRUE
				ENDIF

				IF ! VoDbLocate( cbStoredForBlock,  ;
					cbWhileBlock,  ;
					nStoredNextCount,  ;
					NIL,  ;
					lRestOfFile  )
					BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
				ENDIF
				lRetCode := VoDbFound( )

			ELSEIF lSelectionActive
				uValue := uSelectionValue
				cbKey := cbSelectionIndexingExpression
				IF ! VoDbLocate( { || Eval( cbKey ) = uValue },  ;
					{ || TRUE },  ;
					0,  ;
					NIL,  ;
					TRUE  )
					BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
				ENDIF
				lRetCode := VoDbFound( )
				IF lRetCode
					siSelectionStatus := DBSELECTIONFOUND
				ELSE
					siSelectionStatus := DBSELECTIONEOF
					IF ! VoDbGoBottom( )
						BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
					ENDIF
					IF ! VoDbSkip( 1 )
						BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
					ENDIF
				ENDIF

			ELSE
				IF ! VoDbLocate( { || TRUE },  ;
					{ || TRUE },  ;
					0,  ;
					NIL,  ;
					FALSE  )
					BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
				ENDIF
				lRetCode := VoDbFound( )
			ENDIF
			SELF:__ProcessConcurrency( TRUE )

		ELSE
			lRetCode := FALSE
			SELF:__SetStatusHL( #Locate, __CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE_CAPTION ),  ;
				__CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE ) )
			oHLTemp := oHLStatus
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

METHOD LockCurrentRecord( ) 
	LOCAL lRetCode := FALSE AS LOGIC
	LOCAL oError AS USUAL
	LOCAL dwCurrentWorkArea := 0 AS DWORD

	

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		lRetCode := VoDbRlock( VoDbRecno( ) )
		IF !lRetCode
			BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
		ENDIF
		SELF:__OptimisticFlushNoLock( )
		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527

	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527
		lRetCode := FALSE
	END SEQUENCE


	RETURN lRetCode

METHOD LockSelection( )  
	LOCAL uCurrentRecord AS USUAL
	LOCAL uValue AS USUAL
	LOCAL cbKey AS USUAL
	LOCAL lRetCode := FALSE AS LOGIC
	LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL oError AS USUAL

	

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF lSelectionActive
			IF SELF:Notify( NOTIFYINTENTTOMOVE )
				uCurrentRecord := VoDbRecno( )
				uValue := uSelectionValue
				cbKey := cbSelectionIndexingExpression
				IF VoDbSeek( uSelectionValue, FALSE )
					lRetCode := SELF:__DbServerEval( { || VoDbRlock( VoDbRecno( ) ) },  ;
						NIL,  ;
						{ || Eval( cbKey ) = uValue },  ;
						NIL,  NIL,  TRUE , FALSE, FALSE)
					IF ! lRetCode .OR. ! VoDbGoto( uCurrentRecord )
						BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
					ENDIF
				ELSE
					SELF:__SetStatusHL( #LockSelection, EG_BOUND,  ;
						__CavoStr( __CAVOSTR_DBFCLASS_SELECTIVENOTFOUND ) )
					lRetCode := FALSE
				ENDIF
			ELSE
				lRetCode := FALSE
				SELF:__SetStatusHL( #LockSelection,  ;
					__CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE_CAPTION ),  ;
					__CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE ) )
			ENDIF
		ELSE
			lRetCode := VoDbFlock( )
			IF ! lRetCode
				BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
			ENDIF
		ENDIF
		SELF:__OptimisticFlushNoLock( )
		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527

	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527
		lRetCode := FALSE
	END SEQUENCE


	RETURN lRetCode

METHOD NoIVarGet( symFieldName ) 
	//SE-060601
   LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL uRetVal := NIL AS USUAL
	LOCAL oError AS USUAL
	LOCAL wPos AS DWORD


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		wPos:= FieldPosSym( symFieldName )
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
		SELF:Error( oErrorInfo, #symFieldName )
	END SEQUENCE


	RETURN uRetVal

METHOD NoIVarPut( symFieldName, uValue ) 
    //SE-080608 Updated error handling
	LOCAL uRetVal := NIL AS USUAL
	LOCAL uError AS USUAL
	LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL wPos AS DWORD
	LOCAL uIsRlock AS USUAL

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF nEffectiveCCMode == ccOptimistic .AND. VoDbRecno() <= VoDbLastRec()
			IF ! VoDbRecordInfo( DBRI_LOCKED, 0, REF uIsRlock )
				BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
			ENDIF

			IF uIsRlock
				uRetVal := FieldPutSym( symFieldName, uValue )
			ELSE
				wPos := FieldPosSym( symFieldName )
                //type checking for optimistic locking 
                IF ! __CheckFieldType(REF uValue, aStruct[wPos], REF uError)
                    ASize(uError, 3)
                    BREAK DbError{SELF, #NoIVarPut, uError[1], VO_Sprintf(uError[2], "Field " + aStruct[wPos,DBS_NAME], uError[3]), uValue, "uValue"}
                ENDIF 
                aCurrentBuffer[BUFFER_VALUE, wPos]   := uRetVal := uValue
                aCurrentBuffer[BUFFER_IS_CHANGED, wPos] := TRUE
                lCCOptimisticRecChg := TRUE
			ENDIF
		ELSE
			uRetVal := FieldPutSym( symFieldName, uValue )
		ENDIF
		SELF:Notify( Notify.FieldChange, symFieldName )
		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527

	RECOVER USING uError
		oErrorInfo := uError
		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527
		SELF:Error( oErrorInfo, #symFieldName )
	END SEQUENCE


	RETURN uRetVal

METHOD Notify(	 kNotification,	 uDescription )	 
	//SE-060527
	LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL uVOVal, uVoVal2 AS USUAL
	LOCAL uRetValue AS USUAL
	//	STATIC lNITMStart := FALSE AS LOGIC
	
	
	uRetValue := TRUE     
	DO CASE
	CASE kNotification <= NOTIFYCOMPLETION
		IF siSuspendNotification == 0 .AND. nClients > 0
			VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
            FOREACH oClient AS USUAL IN aClients
                Send(oClient,#Notify, kNotification, uDescription ) 
            NEXT
			VoDbSetSelect( LONGINT(dwCurrentWorkArea ) )
		ENDIF
		
	CASE kNotification == NOTIFYINTENTTOMOVE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		uRetValue := TRUE
		IF siSuspendNotification == 0
			IF nClients > 0
                FOREACH oClient AS USUAL IN aClients
                    uRetValue := Send(oClient,#Notify, kNotification, uDescription ) 
                    IF ! uRetValue
                        EXIT
                    ENDIF
                NEXT
			ENDIF
			
			IF uRetValue .AND. lRelationsActive
				FOREACH oChild AS USUAL IN aRelationChildren
                    uRetValue := Send(oChild, #Notify, kNotification , uDescription)
                    IF ! uRetValue
						EXIT
					ENDIF
				NEXT  // nChild
			ENDIF
		ELSE
			FOREACH oChild AS USUAL IN aRelationChildren
                Send(oChild, #__NotifyBufferFlush)
			NEXT  // nChild
		ENDIF
		
		IF uRetValue
			VoDbSetSelect( LONGINT( wWorkArea ) )
			SELF:__OptimisticFlush()
		ENDIF
		VoDbSetSelect( LONGINT(dwCurrentWorkArea ) )
		
	CASE kNotification <= NOTIFYFILECHANGE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		
		SELF:__InitRecordBuf()
		
		IF siSuspendNotification == 0
			IF lRelationsActive
				FOREACH oChild AS USUAL IN aRelationChildren
                    Send(oChild, #Notify, NOTIFYRELATIONCHANGE)
				NEXT  
			ENDIF
			IF nClients > 0
				FOREACH oClient AS USUAL IN aClients
                    Send(oClient, #Notify, kNotification, uDescription )
				NEXT  
			ENDIF
		ENDIF
		VoDbSetSelect( LONGINT(dwCurrentWorkArea ))
		
	CASE kNotification == NOTIFYRELATIONCHANGE
		IF siSuspendNotification == 0
			IF lSelectionActive
				IF uDescription == NIL .OR. uDescription == DBSELECTIONNULL
					VoDbSelect( wSelectionWorkArea, OUT dwCurrentWorkArea )
					uSelectionValue := Eval( cbSelectionParentExpression )
					VoDbSetSelect( LONGINT( wWorkArea ) )
					IF VoDbEof( ) .OR. ! ( Eval( cbSelectionIndexingExpression ) = uSelectionValue )
						siSelectionStatus := DBSELECTIONEMPTY
					ELSE
						siSelectionStatus := DBSELECTIONNULL
					ENDIF
				ELSE
					VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
					siSelectionStatus := DBSELECTIONEMPTY
				ENDIF
				
			ELSEIF lCDXSelectionActive
				VoDbSelect( wSelectionWorkArea, OUT dwCurrentWorkArea )
				uVOVal := uVoVal2 := Eval( cbSelectionParentExpression )
				VoDbSetSelect( LONGINT(wWorkArea ) )
				VoDbOrderInfo( DBOI_SCOPETOP	 , "", NIL, REF uVOVal )
				VoDbOrderInfo( DBOI_SCOPEBOTTOM, "", NIL, REF uVoVal2 )
				IF ! VoDbGoTop()
					VoDbSetSelect( LONGINT(dwCurrentWorkArea ))
					BREAK ErrorBuild(_VoDbErrInfoPtr())
				ENDIF
			ELSE
				VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
			ENDIF
			
			SELF:__InitRecordBuf()
			
			IF nClients > 0
                FOREACH oClient AS USUAL IN aClients
                    Send(oClient, #Notify, NOTIFYFILECHANGE)
				NEXT 
			ENDIF
			IF lRelationsActive
                FOREACH oChild AS USUAL IN aRelationChildren
                    Send(oChild, #Notify, NOTIFYRELATIONCHANGE, siSelectionStatus)
				NEXT 
			ENDIF
			
			VoDbSetSelect( LONGINT(dwCurrentWorkArea ) )
		ENDIF
		
	CASE kNotification == NOTIFYCLEARRELATION
		lSelectionActive 	:= FALSE
		oDBSelectionParent := NULL_OBJECT
		wSelectionWorkArea := 0
		cbSelectionParentExpression := NIL
		cbSelectionIndexingExpression := NIL
		IF lCDXSelectionActive
			lCDXSelectionActive := FALSE       
			//RvdH 070711 Select the correct workarea
			//VoDbSelect( wSelectionWorkArea, OUT dwCurrentWorkArea )			
			VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
			uVOVal := NIL
			VoDbOrderInfo( DBOI_SCOPETOPCLEAR, "", NIL, REF uVOVal )
			uVOVal := NIL
			VoDbOrderInfo( DBOI_SCOPEBOTTOMCLEAR, "", NIL, REF uVOVal )
			VoDbSetSelect( LONGINT(dwCurrentWorkArea ) )
		ENDIF
		
	OTHERWISE        
		SELF:__InitRecordBuf()
		IF siSuspendNotification == 0
			VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
			IF nClients > 0
                FOREACH oClient AS USUAL IN aClients
                    Send(oClient, #Notify, kNotification )
                NEXT
			ENDIF
			IF lRelationsActive
                FOREACH oChild AS USUAL IN aRelationChildren
                    Send(oChild, #Notify, kNotification )
                NEXT
			ENDIF
			VoDbSetSelect( LONGINT(dwCurrentWorkArea ))
		ENDIF
	ENDCASE
	
	RETURN uRetValue
	

METHOD OrderDescend( uOrder, oFSIndex, lNew ) 
	//SE-060601
   LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL oError AS USUAL
	LOCAL cTarget AS STRING

	

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF IsObject(oFSIndex) .and. __Usual.ToObject(oFSIndex) IS FileSpec VAR oFS
			cTarget := oFS:FullPath
		ELSE
			IF IsString( oFSIndex )
				cTarget := oFSIndex
			ENDIF
		ENDIF
		IF ! IsLogic( lNew )
			lNew := NIL
		ENDIF
		VoDbOrderInfo( DBOI_ISDESC, cTarget, uOrder, REF lNew )
		__DBSSetSelect( dwCurrentWorkArea )
	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oErrorInfo, #OrderDescend )
		lNew := NIL
	END SEQUENCE


	RETURN lNew

METHOD OrderInfo( kOrderInfoType, oFSIndex, uOrder, uOrdVal ) 
	//SE-060601
	LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL oError AS USUAL
	LOCAL cTarget AS STRING
	LOCAL lKeyVal AS LOGIC

	

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF IsObject(oFSIndex) .and. __Usual.ToObject(oFSIndex) IS FileSpec VAR oFS
			cTarget := oFS:FullPath
		ELSE
			IF IsString( oFSIndex )
				cTarget := oFSIndex
			ENDIF
		ENDIF

		/* UH 04/12/2002
		        IF ! VoDbOrderInfo(kOrderInfoType, cTarget, uOrder, REF uOrdVal)
		            //BREAK DbError{SELF, #OrderInfo, EG_ARG, "", kOrderInfoType, "kOrderInfoType" }
		            BREAK ErrorBuild(_VoDbErrInfoPtr()) 
		        ENDIF
		*/
		//RvdH 030926 uOrderVal parameter was missing ! (Bug # 170)
		//uOrdVal := DBORDERINFO( kOrderInfoType, cTarget, uOrder )
		//SE-060602 error handling was missing
		//uOrdVal := DBORDERINFO( kOrderInfoType, cTarget, uOrder, uOrdVal )

		IF IsString(uOrder)
			IF Len(uOrder) == 0
				uOrder := NIL
			ENDIF
		ENDIF

		IF kOrderInfoType == DBOI_KEYVAL
			lKeyVal := .T. 
			kOrderInfoType := DBOI_EXPRESSION
		ENDIF

		IF ! VoDbOrderInfo(kOrderInfoType, cTarget, uOrder, REF uOrdVal)
		   BREAK ErrorBuild(_VoDbErrInfoPtr()) 
		ENDIF

	   IF lKeyVal
			IF IsString(uOrdVal)
				IF Len(uOrdVal) == 0
					uOrdVal := NIL
				ELSE
					uOrdVal := &(uOrdVal)
				ENDIF
			ENDIF
		ENDIF

      __DBSSetSelect( dwCurrentWorkArea )

	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oErrorInfo, #OrderInfo )
	END SEQUENCE


	RETURN uOrdVal

METHOD OrderIsUnique( uOrder, oFSIndex ) 
	//SE-060601
   LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL lRetVal AS USUAL
	LOCAL oError AS USUAL
	LOCAL cTarget AS STRING

	

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF IsObject(oFSIndex) .and. __Usual.ToObject(oFSIndex) IS FileSpec VAR oFS
			cTarget := oFS:FullPath
		ELSE
			IF IsString( oFSIndex )
				cTarget := oFSIndex
			ENDIF
		ENDIF
		IF ! VoDbOrderInfo( DBOI_UNIQUE, cTarget, uOrder, REF lRetVal )
			BREAK ErrorBuild(_VoDbErrInfoPtr())
		ENDIF
		__DBSSetSelect( dwCurrentWorkArea )

	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oErrorInfo, #OrderIsUnique )
	END SEQUENCE


	RETURN lRetVal

METHOD OrderKeyAdd( uOrder, oFSIndex, uKeyValue ) 
	//SE-060601
	LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL oError AS USUAL
	LOCAL cTarget AS STRING

	

	VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		IF IsObject(oFSIndex) .and. __Usual.ToObject(oFSIndex) IS FileSpec VAR oFS
			cTarget := oFS:FullPath
		ELSE
			IF IsString( oFSIndex )
				cTarget := oFSIndex
			ENDIF
		ENDIF
		IF ! VoDbOrderInfo( DBOI_KEYADD, cTarget, uOrder, REF uKeyValue )
			BREAK ErrorBuild(_VoDbErrInfoPtr())
		ENDIF

	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		uKeyValue := FALSE
	END SEQUENCE

   __DBSSetSelect( dwCurrentWorkArea )


	RETURN uKeyValue

METHOD OrderKeyCount( uOrder, oFSIndex ) 
	//SE-060601
   LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL uRetVal := NIL AS USUAL
	LOCAL oError AS USUAL
	LOCAL cTarget AS STRING


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF IsObject(oFSIndex) .and. __Usual.ToObject(oFSIndex) IS FileSpec VAR oFS
			cTarget := oFS:FullPath
		ELSE
			IF IsString( oFSIndex )
				cTarget := oFSIndex
			ENDIF
		ENDIF
		IF ! VoDbOrderInfo( DBOI_KEYCOUNT, cTarget, uOrder, REF uRetVal )
			BREAK ErrorBuild(_VoDbErrInfoPtr())
		ENDIF
		__DBSSetSelect( dwCurrentWorkArea )

	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oErrorInfo, #OrderKeyCount )
		uRetVal := NIL
	END SEQUENCE


	RETURN uRetVal

METHOD OrderKeyDel( uOrder, oFSIndex ) 
	//SE-060601
   LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL lRetCode AS USUAL
	LOCAL oError AS USUAL
	LOCAL cTarget AS STRING


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF IsObject(oFSIndex) .and. __Usual.ToObject(oFSIndex) IS FileSpec VAR oFS
			cTarget := oFS:FullPath
		ELSE
			IF IsString( oFSIndex )
				cTarget := oFSIndex
			ENDIF
		ENDIF
		IF ! VoDbOrderInfo( DBOI_KEYDELETE, cTarget, uOrder, REF lRetCode )
			BREAK ErrorBuild(_VoDbErrInfoPtr())
		ENDIF

	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		lRetCode := FALSE
	END SEQUENCE

	__DBSSetSelect( dwCurrentWorkArea )


	RETURN lRetCode

METHOD OrderKeyGoTo( nKeyNo ) 
	LOCAL lRetCode := FALSE AS LOGIC
	LOCAL oError AS USUAL
	LOCAL dwCurrentWorkArea := 0 AS DWORD


	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF SELF:Notify( NOTIFYINTENTTOMOVE )
			IF IsNil( nKeyNo )
				nKeyNo := 1
			ENDIF
			IF IsNumeric(nKeyNo)
			   IF ! VoDbGoTop()
			   	BREAK ErrorBuild(_VoDbErrInfoPtr())
			   ENDIF
			   IF ! VoDbSkip(nKeyNo-1L)
			   	BREAK ErrorBuild(_VoDbErrInfoPtr())
			   ENDIF
			   lRetCode := TRUE
	      ENDIF
			IF lRetCode
				lRetCode := SELF:__ProcessConcurrency( TRUE )
			ENDIF
			SELF:Notify( NOTIFYRECORDCHANGE )
		ELSE
			lRetCode := FALSE
			SELF:__SetStatusHL( #OrderKeyGoTo,  ;
				__CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE_CAPTION ),  ;
				__CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE ) )
		ENDIF
		__DBSSetSelect( dwCurrentWorkArea )

	RECOVER USING oError
		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		lRetCode := FALSE
	END SEQUENCE


	RETURN lRetCode

METHOD OrderKeyNo( uOrder, oFSIndex ) 
	////SE-060601
   LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL uRetVal := NIL AS USUAL
	LOCAL oError AS USUAL
	LOCAL cTarget AS STRING

	

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF IsObject(oFSIndex) .and. __Usual.ToObject(oFSIndex) IS FileSpec VAR oFS
			cTarget := oFS:FullPath
		ELSE
			IF IsString( oFSIndex )
				cTarget := oFSIndex
			ENDIF
		ENDIF
		IF ! VoDbOrderInfo( DBOI_POSITION, cTarget, uOrder, REF uRetVal )
			BREAK ErrorBuild(_VoDbErrInfoPtr())
		ENDIF
		__DBSSetSelect( dwCurrentWorkArea )

	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oErrorInfo, #OrderKeyNo )
		uRetVal := 0
	END SEQUENCE


	RETURN uRetVal

METHOD OrderScope( nScope, uValue ) 
	//SE-060601
   LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL oError AS USUAL
	LOCAL n AS DWORD

	

	lErrorFlag := FALSE
	BEGIN SEQUENCE
      //RvdH 070925 Save pending changes
      SELF:__OptimisticFlush()

		//RvdH 050705 Changed to explicitely use TOPSCOPE
		DEFAULT(REF nScope, TOPSCOPE)
		IF nScope == TOPSCOPE
			n := DBOI_SCOPETOP
			IF IsNil( uValue )
				n := DBOI_SCOPETOPCLEAR
			ENDIF
		ELSE
			n := DBOI_SCOPEBOTTOM
			IF IsNil( uValue )
				n := DBOI_SCOPEBOTTOMCLEAR
			ENDIF
		ENDIF
		//IF IsNumeric( nScope )
		//	nScope := INT( nScope )
		//	IF nScope > 0
		//		n += 1
		//	ENDIF
		//ENDIF
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF ! VoDbOrderInfo( n, "", NIL, REF uValue )
			BREAK ErrorBuild(_VoDbErrInfoPtr())
		ENDIF
		IF ! __DBSGoTop(SELF:nRetries)
			BREAK ErrorBuild(_VoDbErrInfoPtr())
		ENDIF
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Notify( NOTIFYFILECHANGE )

	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oErrorInfo, #OrderScope )
		uValue := NIL
	END SEQUENCE


	RETURN uValue

METHOD OrderSkipUnique( nDirection ) 
	LOCAL lRetCode := FALSE AS LOGIC
	LOCAL oError AS USUAL
	LOCAL dwCurrentWorkArea := 0 AS DWORD

	

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF SELF:Notify( NOTIFYINTENTTOMOVE )
			lRetCode := VoDbOrderInfo( DBOI_SKIPUNIQUE, "", NIL, REF nDirection )
			// RvdH 060629 __ProcessConcurrency needs a Logic and not a numeric !
			//lRetCode := SELF:__ProcessConcurrency( nDirection, TRUE )
			//lRetCode := SELF:__ProcessConcurrency( lRetCode, TRUE )
			IF lRetCode
				lRetCode := SELF:__ProcessConcurrency( TRUE )
			ENDIF

			SELF:Notify( NOTIFYRECORDCHANGE )
		ELSE
			lRetCode := FALSE
			SELF:__SetStatusHL( #OrderSkipUnique,  ;
				__CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE_CAPTION ),  ;
				__CavoStr( __CAVOSTR_DBFCLASS_INTENTTOMOVE ) )
		ENDIF
		__DBSSetSelect( dwCurrentWorkArea )

	RECOVER USING oError
		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527
		oErrorInfo := oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		lRetCode := FALSE
	END SEQUENCE


	RETURN lRetCode

METHOD Pack( ) 
	//SE-060601
   LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL lRetCode := FALSE AS LOGIC
	LOCAL oError AS USUAL

	

	lErrorFlag := FALSE
	BEGIN SEQUENCE
      //RvdH 070925 Save pending changes
      SELF:__OptimisticFlush()
		VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
		IF (lRetCode := VoDbPack( ))
			__DBSSetSelect( dwCurrentWorkArea )
			SELF:Notify( NOTIFYFILECHANGE )
		ELSE
			BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
		ENDIF
		wLastSelectionRec := 0

	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		__DBSSetSelect( dwCurrentWorkArea )
		oErrorInfo := oError
		lRetCode := FALSE
	END SEQUENCE

	RETURN lRetCode
END CLASS

