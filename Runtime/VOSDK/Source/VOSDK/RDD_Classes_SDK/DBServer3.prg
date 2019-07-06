PARTIAL CLASS DbServer

METHOD GetArray( nMaxRows, uField1, uSearchValue )  
	LOCAL uValue AS USUAL
	LOCAL cbKey AS USUAL
	LOCAL aResult := { } AS ARRAY
	LOCAL wRows := 32767 AS DWORD
	LOCAL oError AS USUAL
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL oHLTemp AS OBJECT
	LOCAL wPos AS DWORD

	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
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
			IF ! VODBSeek( uValue, FALSE )
				SELF:__SetStatusHL( #GetArray, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_NOSEEK ) )
				oHLTemp := oHLStatus
				aResult := NULL_ARRAY
			ELSE
				SELF:__DBServerEval( { | | AAdd( aResult, __DBSFieldGet( wPos ) ) },  ;
					NIL,  ;
					{ || Eval( cbKey ) = uValue },  ;
					wRows,  ;
					NIL,  ;
					TRUE,  ;
					DBCCON,  ;
					DBCCREADONLY )
				siSelectionStatus := DBSELECTIONEOF
				IF ! VODBGoBottom( )
					BREAK ErrorBuild( _VODBErrInfoPtr( ) )
				ENDIF
				IF ! VODBSkip( 1 )
					BREAK ErrorBuild( _VODBErrInfoPtr( ) )
				ENDIF
			ENDIF

		ELSEIF ! IsNil( uSearchValue )
			IF ! VODBSeek( uSearchValue, FALSE )
				SELF:__SetStatusHL( #GetArray, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_NOSEEK ) )
				oHLTemp := oHLStatus
				aResult := NULL_ARRAY
			ELSE
				cbKey := &( "{ ||"+__DBSDBOrderInfo( DBOI_EXPRESSION, "", 0 ) + " }" )
				SELF:__DBServerEval( { || AAdd( aResult, __DBSFieldGet( wPos ) ) },  ;
					NIL,  ;
					NIL,  ;
					wRows,  ;
					NIL,  ;
					TRUE,  ;
					DBCCON,  ;
					DBCCREADONLY )
			ENDIF

		ELSE
			SELF:__DBServerEval( { || AAdd( aResult, __DBSFieldGet( wPos ) ) },  ;
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
	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(aResult))
	#ENDIF

	RETURN aResult

METHOD GetLocate ( ) 
	//SE-060601
   LOCAL dwCurrentWorkArea AS DWORD
	LOCAL oError AS USUAL
	LOCAL uInfo AS USUAL

	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		IF ! VODBInfo( DBI_GETSCOPE, @uInfo )
			BREAK ErrorBuild( _VODBErrInfoPtr( ) )
		ENDIF
      __DBSSetSelect( dwCurrentWorkArea )
	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oErrorInfo, #Info )
	END SEQUENCE

	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(uInfo))
	#ENDIF

	RETURN uInfo

METHOD GetLookupTable( nMaxRows, uField1, uField2, uSearchValue )  
	LOCAL uValue AS USUAL
	LOCAL cbKey AS USUAL
	LOCAL aResult := { } AS ARRAY
	LOCAL wRows := 32767 AS DWORD
	LOCAL oError AS USUAL
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL oHLTemp AS OBJECT

	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
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
			IF ! VODBSeek( uValue, FALSE )
				SELF:__SetStatusHL( #GetLookupTable, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_NOSEEK ) )
				oHLTemp := oHLStatus
				aResult := NULL_ARRAY
			ELSE
				SELF:__DBServerEval( { || AAdd( aResult, { __DBSFieldGet( uField1 ), __DBSFieldGet( uField2 ) } ) },  ;
					NIL,  ;
					{ || Eval( cbKey ) == uValue },  ;
					wRows,  ;
					NIL,  ;
					TRUE,  ;
					DBCCON,  ;
					DBCCREADONLY )
				siSelectionStatus := DBSELECTIONEOF
				IF ! VODBGoBottom( )
					BREAK ErrorBuild( _VODBErrInfoPtr( ) )
				ENDIF
				IF ! VODBSkip( 1 )
					BREAK ErrorBuild( _VODBErrInfoPtr( ) )
				ENDIF
			ENDIF

		ELSEIF ! IsNil( uSearchValue )
			IF ! VODBSeek( uSearchValue, FALSE )
				SELF:__SetStatusHL ( #GetLookupTable, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_NOSEEK ) )
				oHLTemp := oHLStatus
				aResult := NULL_ARRAY
			ELSE
				SELF:__DBServerEval( { || AAdd( aResult, { __DBSFieldGet( uField1 ), __DBSFieldGet( uField2 ) } ) },  ;
					NIL,  ;
					NIL,  ;
					wRows,  ;
					NIL,  ;
					TRUE,  ;
					DBCCON,  ;
					DBCCREADONLY )
			ENDIF

		ELSE
			SELF:__DBServerEval( { || AAdd( aResult, { __DBSFieldGet( uField1 ), __DBSFieldGet( uField2 ) } ) },  ;
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

	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(aResult))
	#ENDIF
	RETURN aResult

METHOD GoBottom( )  
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL uValue AS USUAL
	LOCAL cbKey AS USUAL
	LOCAL lRetCode AS LOGIC
	LOCAL oError AS USUAL
	LOCAL oHLTemp AS OBJECT
	LOCAL nTries AS DWORD

	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF

	lErrorFlag := FALSE
	nTries := SELF:nRetries

	BEGIN SEQUENCE
		VODBSelect( SELF:wWorkArea, @dwCurrentWorkArea )
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
					IF Eval( cbKey ) = uValue .OR. VODBFound( )
						lRetCode := SELF:__DBServerEval( { || },  ;
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


	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(lRetCode))
	#ENDIF
	RETURN lRetCode

METHOD GoTo( nRecordNumber ) 
	LOCAL nCurrentRecord AS LONGINT
	LOCAL lRetCode AS LOGIC
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL oError AS USUAL
	LOCAL nTries AS DWORD

	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__, AsString(nRecordNumber))
	#ENDIF

	lErrorFlag := FALSE

	nTries := SELF:nRetries

	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		IF SELF:Notify( NOTIFYINTENTTOMOVE )
			nRecordNumber := INT( nRecordNumber )
			IF lSelectionActive
				#IFDEF __DEBUG__
					DBFDebug( "SERVER:GoTo(" + NTrim( nRecordNumber ) + " )" )
				#ENDIF

				IF siSelectionStatus == DBSELECTIONEMPTY
					lRetCode := TRUE
				ELSE
					nCurrentRecord := VODBRecno( )
					lRetCode := __DBSGoTo( nRecordNumber, nTries )

					#IFDEF __DEBUG__
						DBFDebug( "SelectionValue: " + AsString( uSelectionValue ) )
					#ENDIF

					IF Eval( cbSelectionIndexingExpression ) = uSelectionValue
						siSelectionStatus := DBSELECTIONNULL

						#IFDEF __DEBUG
							DBFDebug( " ... found " )
						#ENDIF
					ELSE
						#IFDEF __DEBUG
							DBFDebug( " ... not found -> GO to EOF" )
						#ENDIF

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


	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(lRetCode))
	#ENDIF
	RETURN lRetCode

METHOD GoTop( ) 
	LOCAL lRetCode AS LOGIC
	LOCAL oError AS USUAL
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL nTries AS DWORD

	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF

	lErrorFlag := FALSE
	nTries     := SELF:nRetries

	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
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


	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(lRetCode))
	#ENDIF
	RETURN lRetCode

METHOD INDEXKEY( uOrder ) 
	//SE-060601
   LOCAL dwCurrentWorkArea AS DWORD
	LOCAL oError AS USUAL
	LOCAL uOrdVal AS USUAL

	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__, AsString(uOrder))
	#ENDIF

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		IF ! VODBOrderInfo( DBOI_EXPRESSION, "", uOrder, @uOrdVal )
			BREAK ErrorBuild( _VODBErrInfoPtr( ) )
		ENDIF
	   __DBSSetSelect( dwCurrentWorkArea )
	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oErrorInfo, #OrderInfo )
	END SEQUENCE


	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(uOrdVal))
	#ENDIF
	RETURN uOrdVal

METHOD INDEXORD( ) 
	//SE-060601
   LOCAL dwCurrentWorkArea AS DWORD
	LOCAL oError AS USUAL
	LOCAL uOrdVal AS USUAL

	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		IF ! VODBOrderInfo( DBOI_NUMBER, "", NIL, @uOrdVal )
			BREAK ErrorBuild( _VODBErrInfoPtr( ) )
		ENDIF
	   __DBSSetSelect( dwCurrentWorkArea )
	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oErrorInfo, #OrderInfo )
	END SEQUENCE


	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(uOrdVal))
	#ENDIF
	RETURN uOrdVal

METHOD Info( kInfoType, uInfo ) 
	//SE-060601
   LOCAL dwCurrentWorkArea AS DWORD
	LOCAL oError AS USUAL

	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__, AsString(kInfoType), AsString(uInfo))
	#ENDIF

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		IF ! VODBInfo( kInfoType, @uInfo)
			BREAK ErrorBuild( _VODBErrInfoPtr( ) )
		ENDIF
      __DBSSetSelect( dwCurrentWorkArea )
	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oErrorInfo, #Info )
	END SEQUENCE


	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(uInfo))
	#ENDIF
	RETURN uInfo

METHOD Join( oDBSource, oFSTarget, aFieldList, cbForBlock ) 
	LOCAL cSource AS STRING
	LOCAL cTarget AS STRING
	LOCAL aFieldNames AS ARRAY
	LOCAL w AS DWORD
	LOCAL lRetCode AS LOGIC
	LOCAL oError AS USUAL
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL wLen AS DWORD
	LOCAL lRestore	AS LOGIC
	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF
   //RvdH 070711 Make sure we restore the workarea
   //				  The codeblocks may select another workarea
	lRestore	:= DbSetRestoreWorkarea(TRUE)      

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		IF SELF:Notify( NOTIFYINTENTTOMOVE )
			IF IsObject(oDBSource) .and. __Usual.ToObject(oDBSource) IS DbServer VAR oDb
				cSource := oDb:Alias
			ELSE
				cSource := AsString( oDBSource )
			ENDIF

			IF IsObject(oFSTarget) .and. __Usual.ToObject(oFSTarget) IS FileSpec VAR oFs
				cTarget := oFS:FullPath
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
	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(lRetCode))
	#ENDIF
	RETURN lRetCode

METHOD Locate( cbForBlock, cbWhileBlock, uScope )  
	LOCAL uValue AS USUAL
	LOCAL cbKey AS USUAL
	LOCAL nNextCount AS LONGINT
	LOCAL lRestOfFile AS LOGIC
	LOCAL lRetCode AS LOGIC
	LOCAL oError AS USUAL
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL oHLTemp AS OBJECT
	LOCAL lRestore	AS LOGIC
	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF
   //RvdH 070711 Make sure we restore the workarea
   //				  The codeblocks may select another workarea
	lRestore	:= DbSetRestoreWorkarea(TRUE)      

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
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
				IF ! VODBLocate( cbForBlock,  ;
					cbWhileBlock,  ;
					nNextCount,  ;
					NIL,  ;
					lRestOfFile )
					BREAK ErrorBuild( _VODBErrInfoPtr( ) )
				ENDIF
				lRetCode := VODBFound( )

			ELSEIF lActiveScope
				lRestOfFile := lStoredRestOfFile
				IF IsNil( cbStoredWhileBlock )
					cbWhileBlock := { || TRUE }
				ELSE
					cbWhileBlock := cbStoredWhileBlock
					lRestOfFile := TRUE
				ENDIF

				IF ! VODBLocate( cbStoredForBlock,  ;
					cbWhileBlock,  ;
					nStoredNextCount,  ;
					NIL,  ;
					lRestOfFile  )
					BREAK ErrorBuild( _VODBErrInfoPtr( ) )
				ENDIF
				lRetCode := VODBFound( )

			ELSEIF lSelectionActive
				uValue := uSelectionValue
				cbKey := cbSelectionIndexingExpression
				IF ! VODBLocate( { || Eval( cbKey ) = uValue },  ;
					{ || TRUE },  ;
					0,  ;
					NIL,  ;
					TRUE  )
					BREAK ErrorBuild( _VODBErrInfoPtr( ) )
				ENDIF
				lRetCode := VODBFound( )
				IF lRetCode
					siSelectionStatus := DBSELECTIONFOUND
				ELSE
					siSelectionStatus := DBSELECTIONEOF
					IF ! VODBGoBottom( )
						BREAK ErrorBuild( _VODBErrInfoPtr( ) )
					ENDIF
					IF ! VODBSkip( 1 )
						BREAK ErrorBuild( _VODBErrInfoPtr( ) )
					ENDIF
				ENDIF

			ELSE
				IF ! VODBLocate( { || TRUE },  ;
					{ || TRUE },  ;
					0,  ;
					NIL,  ;
					FALSE  )
					BREAK ErrorBuild( _VODBErrInfoPtr( ) )
				ENDIF
				lRetCode := VODBFound( )
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
	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(lRetCode))
	#ENDIF
	RETURN lRetCode

METHOD LockCurrentRecord( ) 
	LOCAL lRetCode AS LOGIC
	LOCAL oError AS USUAL
	LOCAL dwCurrentWorkArea AS DWORD

	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		lRetCode := VODBRLock( VODBRecno( ) )
		IF !lRetCode
			BREAK ErrorBuild( _VODBErrInfoPtr( ) )
		ENDIF
		SELF:__OptimisticFlushNoLock( )
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

METHOD LockSelection( )  
	LOCAL uCurrentRecord AS USUAL
	LOCAL uValue AS USUAL
	LOCAL cbKey AS USUAL
	LOCAL lRetCode AS LOGIC
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL oError AS USUAL

	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		IF lSelectionActive
			IF SELF:Notify( NOTIFYINTENTTOMOVE )
				uCurrentRecord := VODBRecno( )
				uValue := uSelectionValue
				cbKey := cbSelectionIndexingExpression
				IF VODBSeek( uSelectionValue, FALSE )
					lRetCode := SELF:__DBServerEval( { || VODBRLock( VODBRecno( ) ) },  ;
						NIL,  ;
						{ || Eval( cbKey ) = uValue },  ;
						NIL,  NIL,  TRUE , FALSE, FALSE)
					IF ! lRetCode .OR. ! VODBGoTo( uCurrentRecord )
						BREAK ErrorBuild( _VODBErrInfoPtr( ) )
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
			lRetCode := VODBFLock( )
			IF ! lRetCode
				BREAK ErrorBuild( _VODBErrInfoPtr( ) )
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


	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(lRetCode))
	#ENDIF
	RETURN lRetCode

METHOD NoIVarGet( symFieldName ) 
	//SE-060601
   LOCAL dwCurrentWorkArea AS DWORD
	LOCAL uRetVal AS USUAL
	LOCAL oError AS USUAL
	LOCAL wPos AS DWORD

	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__, AsString(symFieldName))
	#ENDIF

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		wPos:= FieldPosSym( symFieldName )
		IF wPos > 0  .AND. nEffectiveCCMode == ccOptimistic .AND. lCCOptimisticRecChg .AND. ;
			aCurrentBuffer[BUFFER_IS_CHANGED, wPos] .AND. ! aOriginalBuffer[BUFFER_IS_BLOB, wPos]

			uRetVal := aCurrentBuffer[BUFFER_VALUE, wPos]
		ELSE
			IF ! VODBFieldGet( wPos, @uRetVal )
				BREAK ErrorBuild( _VODBErrInfoPtr( ) )
			ENDIF
		ENDIF
	   __DBSSetSelect( dwCurrentWorkArea )
	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oErrorInfo, #symFieldName )
	END SEQUENCE


	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(uRetVal))
	#ENDIF
	RETURN uRetVal

METHOD NoIVarPut( symFieldName, uValue ) 
    //SE-080608 Updated error handling
	LOCAL uRetVal AS USUAL
	LOCAL uError AS USUAL
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL wPos AS DWORD
	LOCAL uIsRlock AS USUAL
	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__, AsString(symFieldName), AsString(uValue))
	#ENDIF

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		IF nEffectiveCCMode == ccOptimistic .AND. VODBRecno() <= VODBLastRec()
			IF ! VODBRecordInfo( DBRI_LOCKED, 0, @uIsRLock )
				BREAK ErrorBuild( _VODBErrInfoPtr( ) )
			ENDIF

			IF uIsRLock
				uRetVal := FieldPutSym( symFieldName, uValue )
			ELSE
				wPos := FieldPosSym( symFieldName )
                //type checking for optimistic locking 
                IF ! __CheckFieldType(@uValue, aStruct[wPos], @uError)
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
		SELF:Notify( NotifyFieldChange, symFieldName )
		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527

	RECOVER USING uError
		oErrorInfo := uError
		__DBSSetSelect( dwCurrentWorkArea )  //SE-060527
		SELF:Error( oErrorInfo, #symFieldName )
	END SEQUENCE


	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(uRetVal))
	#ENDIF
	RETURN uRetVal

METHOD Notify(	 kNotification,	 uDescription )	 
	//SE-060527
	LOCAL w AS DWORD
	LOCAL nChild AS DWORD
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL uVOVal, uVoVal2 AS USUAL
	LOCAL uRetValue AS USUAL
	//	STATIC lNITMStart := FALSE AS LOGIC
	
	#IFDEF __DEBUG__
		DBFDebug ("Entering "+__ENTITY__, AsString(kNotification), AsString(uDescription))
	#ENDIF
	
	uRetValue := TRUE     
	DO CASE
	CASE kNotification <= NOTIFYCOMPLETION
		IF siSuspendNotification == 0 .AND. nClients > 0
			VODBSelect( wWorkArea, @dwCurrentWorkArea )
			FOR w := 1 UPTO nClients
				Send(aClients[w], #Notify, kNotification, uDescription )
			NEXT  // w
			VODBSetSelect( LONGINT(dwCurrentWorkArea ) )
		ENDIF
		
	CASE kNotification == NOTIFYINTENTTOMOVE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		//		lNITMStart := TRUE
		uRetValue := TRUE
		IF siSuspendNotification == 0
			IF nClients > 0
				FOR w := 1 UPTO nClients
					IF ! ( uRetValue := Send(aClients[w],#Notify, kNotification ) )
						EXIT
					ENDIF
				NEXT  // w
			ENDIF
			
			IF uRetValue .AND. lRelationsActive
				IF (w := ALen( aRelationChildren )) > 0
					FOR nChild := 1 UPTO w
						IF ! ( uRetValue := Send(aRelationChildren[nChild],#Notify, kNotification ) )
							EXIT
						ENDIF
					NEXT  // nChild
				ENDIF
			ENDIF
		ELSE
			IF (w := ALen( aRelationChildren )) > 0
				FOR nChild := 1 UPTO w
					Send(aRelationChildren[nChild],#__NotifyBufferFlush)
				NEXT  // nChild
			ENDIF
		ENDIF
		
		IF uRetValue
			VODBSetSelect( LONGINT( wWorkArea ) )
			SELF:__OptimisticFlush()
		ENDIF
		VODBSetSelect( LONGINT(dwCurrentWorkArea ) )
		
	CASE kNotification <= NOTIFYFILECHANGE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		
		SELF:__InitRecordBuf()
		
		IF siSuspendNotification == 0
			IF lRelationsActive
				ASend( aRelationChildren, #Notify, NOTIFYRELATIONCHANGE )
			ENDIF
			IF nClients > 0
				ASend( aClients, #Notify, kNotification, uDescription )
			ENDIF
		ENDIF
		VODBSetSelect( LONGINT(dwCurrentWorkArea ))
		
	CASE kNotification == NOTIFYRELATIONCHANGE
		IF siSuspendNotification == 0
			IF lSelectionActive
				IF uDescription == NIL .OR. uDescription == DBSELECTIONNULL
					VODBSelect( wSelectionWorkArea, @dwCurrentWorkArea )
					uSelectionValue := Eval( cbSelectionParentExpression )
					VODBSetSelect( LONGINT( wWorkArea ) )
					IF VODBEof( ) .OR. ! ( Eval( cbSelectionIndexingExpression ) = uSelectionValue )
						siSelectionStatus := DBSELECTIONEMPTY
					ELSE
						siSelectionStatus := DBSELECTIONNULL
					ENDIF
				ELSE
					VODBSelect( wWorkArea, @dwCurrentWorkArea )
					siSelectionStatus := DBSELECTIONEMPTY
				ENDIF
				
			ELSEIF lCDXSelectionActive
				VODBSelect( wSelectionWorkArea, @dwCurrentWorkArea )
				uVOVal := uVOVal2 := Eval( cbSelectionParentExpression )
				VODBSetSelect( LONGINT(wWorkArea ) )
				VODBOrderInfo( DBOI_SCOPETOP	 , "", NIL, @uVOVal )
				VODBOrderInfo( DBOI_SCOPEBottom, "", NIL, @uVOVal2 )
				IF ! VODBGoTop()
					VODBSetSelect( LONGINT(dwCurrentWorkArea ))
					BREAK ErrorBuild(_VODBErrInfoPtr())
				ENDIF
			ELSE
				VODBSelect( wWorkArea, @dwCurrentWorkArea )
			ENDIF
			
			SELF:__InitRecordBuf()
			
			IF nClients > 0
				ASend( aClients, #Notify, NOTIFYFILECHANGE )
			ENDIF
			IF lRelationsActive
				ASend( aRelationChildren, #Notify, NOTIFYRELATIONCHANGE, siSelectionStatus )
			ENDIF
			
			VODBSetSelect( LONGINT(dwCurrentWorkArea ) )
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
			//VODBSelect( wSelectionWorkArea, @dwCurrentWorkArea )			
			VODBSelect( wWorkArea, @dwCurrentWorkArea )
			uVOVal := NIL
			VODBOrderInfo( DBOI_SCOPETOPCLEAR, "", NIL, @uVOVal )
			uVOVal := NIL
			VODBOrderInfo( DBOI_SCOPEBOTTOMCLEAR, "", NIL, @uVOVal )
			VODBSetSelect( LONGINT(dwCurrentWorkArea ) )
		ENDIF
		
	OTHERWISE        
		SELF:__InitRecordBuf()
		IF siSuspendNotification == 0
			VODBSelect( wWorkArea, @dwCurrentWorkArea )
			IF nClients > 0
				ASend( aClients, #Notify, kNotification )
			ENDIF
			IF lRelationsActive
				ASend( aRelationChildren, #Notify, kNotification )
			ENDIF
			VODBSetSelect( LONGINT(dwCurrentWorkArea ))
		ENDIF
	ENDCASE
	
	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(uRetValue))
	#ENDIF
	RETURN uRetValue
	

METHOD OrderDescend( uOrder, oFSIndex, lNew ) 
	//SE-060601
   LOCAL dwCurrentWorkArea AS DWORD
	LOCAL oError AS USUAL
	LOCAL cTarget AS STRING

	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
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
		VODBOrderInfo( DBOI_ISDESC, cTarget, uOrder, @lNew )
		__DBSSetSelect( dwCurrentWorkArea )
	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oErrorInfo, #OrderDescend )
		lNew := NIL
	END SEQUENCE

	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(lNew))
	#ENDIF

	RETURN lNew

METHOD OrderInfo( kOrderInfoType, oFSIndex, uOrder, uOrdVal ) 
	//SE-060601
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL oError AS USUAL
	LOCAL cTarget AS STRING
	LOCAL lKeyVal AS LOGIC

	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		IF IsObject(oFSIndex) .and. __Usual.ToObject(oFSIndex) IS FileSpec VAR oFS
			cTarget := oFS:FullPath
		ELSE
			IF IsString( oFSIndex )
				cTarget := oFSIndex
			ENDIF
		ENDIF

		/* UH 04/12/2002
		        IF ! VODBOrderInfo(kOrderInfoType, cTarget, uOrder, @uOrdVal)
		            //BREAK DbError{SELF, #OrderInfo, EG_ARG, "", kOrderInfoType, "kOrderInfoType" }
		            BREAK ErrorBuild(_VODBErrInfoPtr()) //375@070
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

		IF ! VODBOrderInfo(kOrderInfoType, cTarget, uOrder, @uOrdVal)
		   BREAK ErrorBuild(_VODBErrInfoPtr()) //375@070
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


	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(uOrdVal))
	#ENDIF
	RETURN uOrdVal

METHOD OrderIsUnique( uOrder, oFSIndex ) 
	//SE-060601
   LOCAL dwCurrentWorkArea AS DWORD
	LOCAL lRetVal AS USUAL
	LOCAL oError AS USUAL
	LOCAL cTarget AS STRING

	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		IF IsObject(oFSIndex) .and. __Usual.ToObject(oFSIndex) IS FileSpec VAR oFS
			cTarget := oFS:FullPath
		ELSE
			IF IsString( oFSIndex )
				cTarget := oFSIndex
			ENDIF
		ENDIF
		IF ! VODBOrderInfo( DBOI_UNIQUE, cTarget, uOrder, @lRetVal )
			BREAK ErrorBuild(_VODBErrInfoPtr())
		ENDIF
		__DBSSetSelect( dwCurrentWorkArea )

	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oErrorInfo, #OrderIsUnique )
	END SEQUENCE


	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(lRetVal))
	#ENDIF
	RETURN lRetVal

METHOD OrderKeyAdd( uOrder, oFSIndex, uKeyValue ) 
	//SE-060601
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL oError AS USUAL
	LOCAL cTarget AS STRING

	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF

	VODBSelect( wWorkArea, @dwCurrentWorkArea )

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		IF IsObject(oFSIndex) .and. __Usual.ToObject(oFSIndex) IS FileSpec VAR oFS
			cTarget := oFS:FullPath
		ELSE
			IF IsString( oFSIndex )
				cTarget := oFSIndex
			ENDIF
		ENDIF
		IF ! VODBOrderInfo( DBOI_KEYADD, cTarget, uOrder, @uKeyValue )
			BREAK ErrorBuild(_VODBErrInfoPtr())
		ENDIF

	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		oErrorInfo := oError
		uKeyValue := FALSE
	END SEQUENCE

   __DBSSetSelect( dwCurrentWorkArea )


	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(uKeyValue))
	#ENDIF
	RETURN uKeyValue

METHOD OrderKeyCount( uOrder, oFSIndex ) 
	//SE-060601
   LOCAL dwCurrentWorkArea AS DWORD
	LOCAL uRetVal AS USUAL
	LOCAL oError AS USUAL
	LOCAL cTarget AS STRING

	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__, AsString(uOrder), AsString(OFsIndex))
	#ENDIF

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		IF IsObject(oFSIndex) .and. __Usual.ToObject(oFSIndex) IS FileSpec VAR oFS
			cTarget := oFS:FullPath
		ELSE
			IF IsString( oFSIndex )
				cTarget := oFSIndex
			ENDIF
		ENDIF
		IF ! VODBOrderInfo( DBOI_KEYCOUNT, cTarget, uOrder, @uRetVal )
			BREAK ErrorBuild(_VODBErrInfoPtr())
		ENDIF
		__DBSSetSelect( dwCurrentWorkArea )

	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oErrorInfo, #OrderKeyCount )
		uRetVal := NIL
	END SEQUENCE


	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(uRetVal))
	#ENDIF
	RETURN uRetVal

METHOD OrderKeyDel( uOrder, oFSIndex ) 
	//SE-060601
   LOCAL dwCurrentWorkArea AS DWORD
	LOCAL lRetCode AS USUAL
	LOCAL oError AS USUAL
	LOCAL cTarget AS STRING

	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__, AsString(uOrder), AsString(OFsIndex))
	#ENDIF

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		IF IsObject(oFSIndex) .and. __Usual.ToObject(oFSIndex) IS FileSpec VAR oFS
			cTarget := oFS:FullPath
		ELSE
			IF IsString( oFSIndex )
				cTarget := oFSIndex
			ENDIF
		ENDIF
		IF ! VODBOrderInfo( DBOI_KEYDELETE, cTarget, uOrder, @lRetCode )
			BREAK ErrorBuild(_VODBErrInfoPtr())
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

METHOD OrderKeyGoTo( nKeyNo ) 
	LOCAL lRetCode AS LOGIC
	LOCAL oError AS USUAL
	LOCAL dwCurrentWorkArea AS DWORD

	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		IF SELF:Notify( NOTIFYINTENTTOMOVE )
			IF IsNil( nKeyno )
				nKeyno := 1
			ENDIF
			IF IsNumeric(nKeyno)
			   IF ! VODBGoTop()
			   	BREAK ErrorBuild(_VODBErrInfoPtr())
			   ENDIF
			   IF ! VODBSkip(nKeyNo-1L)
			   	BREAK ErrorBuild(_VODBErrInfoPtr())
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


	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(lRetCode))
	#ENDIF
	RETURN lRetCode

METHOD OrderKeyNo( uOrder, oFSIndex ) 
	////SE-060601
   LOCAL dwCurrentWorkArea AS DWORD
	LOCAL uRetVal AS USUAL
	LOCAL oError AS USUAL
	LOCAL cTarget AS STRING

	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		IF IsObject(oFSIndex) .and. __Usual.ToObject(oFSIndex) IS FileSpec VAR oFS
			cTarget := oFS:FullPath
		ELSE
			IF IsString( oFSIndex )
				cTarget := oFSIndex
			ENDIF
		ENDIF
		IF ! VODBOrderInfo( DBOI_POSITION, cTarget, uOrder, @uRetVal )
			BREAK ErrorBuild(_VODBErrInfoPtr())
		ENDIF
		__DBSSetSelect( dwCurrentWorkArea )

	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oErrorInfo, #OrderKeyNo )
		uRetVal := 0
	END SEQUENCE


	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(uRetVal))
	#ENDIF
	RETURN uRetVal

METHOD OrderScope( nScope, uValue ) 
	//SE-060601
   LOCAL dwCurrentWorkArea AS DWORD
	LOCAL oError AS USUAL
	LOCAL n AS DWORD

	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF

	lErrorFlag := FALSE
	BEGIN SEQUENCE
      //RvdH 070925 Save pending changes
      SELF:__OptimisticFlush()

		//RvdH 050705 Changed to explicitely use TOPSCOPE
		Default(@nScope, TOPSCOPE)
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
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		IF ! VODBOrderInfo( n, "", NIL, @uValue )
			BREAK ErrorBuild(_VODBErrInfoPtr())
		ENDIF
		IF ! __DBSGoTop(SELF:nRetries)
			BREAK ErrorBuild(_VODBErrInfoPtr())
		ENDIF
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Notify( NOTIFYFILECHANGE )

	RECOVER USING oError
		oErrorInfo := oError
		__DBSSetSelect( dwCurrentWorkArea )
		SELF:Error( oErrorInfo, #OrderScope )
		uValue := NIL
	END SEQUENCE


	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(uValue))
	#ENDIF
	RETURN uValue

METHOD OrderSkipUnique( nDirection ) 
	LOCAL lRetCode AS LOGIC
	LOCAL oError AS USUAL
	LOCAL dwCurrentWorkArea AS DWORD

	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		IF SELF:Notify( NOTIFYINTENTTOMOVE )
			lRetCode := VODBOrderInfo( DBOI_SKIPUNIQUE, "", NIL, @nDirection )
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


	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(lRetCode))
	#ENDIF
	RETURN lRetCode

METHOD Pack( ) 
	//SE-060601
   LOCAL dwCurrentWorkArea AS DWORD
	LOCAL lRetCode AS LOGIC
	LOCAL oError AS USUAL

	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF

	lErrorFlag := FALSE
	BEGIN SEQUENCE
      //RvdH 070925 Save pending changes
      SELF:__OptimisticFlush()
		VODBSelect( wWorkArea, @dwCurrentWorkArea )
		IF (lRetCode := VODBPack( ))
			__DBSSetSelect( dwCurrentWorkArea )
			SELF:Notify( NOTIFYFILECHANGE )
		ELSE
			BREAK ErrorBuild( _VODBErrInfoPtr( ) )
		ENDIF
		wLastSelectionRec := 0

	RECOVER USING oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
		__DBSSetSelect( dwCurrentWorkArea )
		oErrorInfo := oError
		lRetCode := FALSE
	END SEQUENCE

	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(lRetCode))
	#ENDIF
	RETURN lRetCode
END CLASS

