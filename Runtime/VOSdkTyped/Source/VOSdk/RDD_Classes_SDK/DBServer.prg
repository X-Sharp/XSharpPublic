#pragma warnings(165, off)

PARTIAL CLASS DbServer INHERIT DataServer
	PROTECT lShared AS LOGIC
	PROTECT lReadOnly AS LOGIC
	PROTECT symAlias AS SYMBOL
	PROTECT wWorkArea AS DWORD
	PROTECT cRDDName AS STRING
	PROTECT oFileSpec AS FileSpec
	PROTECT aOriginalBuffer AS ARRAY
	//PROTECT aRLockVerifyBuffer AS ARRAY
	PROTECT siSuspendNotification AS SHORTINT
	PROTECT lRelationsActive AS LOGIC
	PROTECT lCDXSelectionActive AS LOGIC
	PROTECT aRelationChildren AS ARRAY
	PROTECT lSelectionActive AS LOGIC
	PROTECT oDBSelectionParent AS OBJECT
	PROTECT wSelectionWorkArea AS DWORD
	PROTECT cbSelectionParentExpression AS USUAL
	PROTECT uSelectionValue AS USUAL
	PROTECT cbSelectionIndexingExpression AS USUAL
	PROTECT siSelectionStatus AS SHORTINT
	PROTECT lActiveScope AS LOGIC
	PROTECT cbStoredForBlock AS USUAL
	PROTECT cbStoredWhileBlock AS USUAL
	PROTECT uStoredScope AS USUAL
	PROTECT nStoredNextCount AS LONGINT
	PROTECT lStoredAllRecords AS USUAL
	PROTECT lStoredRestOfFile AS LOGIC
	PROTECT wLastSelectionRec AS LONGINT
	PROTECT oErrorInfo AS Error
	PROTECT lErrorFlag AS LOGIC
	PROTECT nEffectiveCCMode AS DWORD
	PROTECT aCurrentBuffer AS ARRAY
	PROTECT lCCOptimisticRecChg AS LOGIC
	PROTECT aStruct AS ARRAY
	PROTECT aRdds AS ARRAY
	PROTECT nRetries AS DWORD
	PROTECT oRDD AS XSharp.RDD.IRdd


METHOD __AcceptSelectiveRelation( oDBParent AS DbServer, wParentWorkArea AS DWORD, cbSelection AS USUAL) AS VOID STRICT 
	
	LOCAL cIndexExt AS STRING
	LOCAL dwCurrentWorkArea AS DWORD

	
	oDBSelectionParent := oDBParent
	wSelectionWorkArea := wParentWorkArea

	cbSelectionParentExpression := cbSelection

	VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )

	cIndexExt := IndexExt( )

	IF Instr( "MDX", cIndexExt )
		lSelectionActive := TRUE
		siSelectionStatus := DBSELECTIONNULL
		cbSelectionIndexingExpression := DBSelectionIndex{ SELF, __DBSDbOrderInfo( DBOI_EXPRESSION, "", 0 ), wWorkArea }
	ELSE
		lCDXSelectionActive := TRUE
	ENDIF

   __DBSSetSelect( dwCurrentWorkArea  ) 

	RETURN

METHOD __BuildDataField( a AS ARRAY ) AS DataField STRICT 
	LOCAL oRet := NULL AS DataField
	
	IF IsArray( a )
		oRet := DataField{ a[ DBS_NAME ], FieldSpec{ a[ DBS_NAME ], a[ DBS_TYPE ],  ;
			a[ DBS_LEN ], a[ DBS_DEC ] } }
	ENDIF
	RETURN oRet

METHOD __ClearChildRelation( oChild AS DbServer ) AS VOID STRICT 
	LOCAL w AS DWORD
	LOCAL wLen AS DWORD

	
	IF ! ( ( w := AScan( aRelationChildren, oChild ) ) == 0 )
		IF ALen( aRelationChildren ) == 1
			lRelationsActive := FALSE
			aRelationChildren := { }
		ELSE
			wLen := ALen( aRelationChildren )
			aRelationChildren[w] := aRelationChildren[wLen]
			ASize( aRelationChildren, wLen - 1 )
		ENDIF
	ENDIF

	RETURN

METHOD __ClearLocks( )  AS VOID STRICT 
	LOCAL uVOVal AS USUAL

	DO CASE
	CASE nEffectiveCCMode == ccOptimistic
		SELF:__OptimisticFlush( )

	CASE nEffectiveCCMode == ccStable
		IF ! VoDbInfo( DBI_ISFLOCK, REF uVOVal )
			BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
		ENDIF
		IF ! uVOVal
			SELF:UnLock( nLastLock )
		ENDIF

	CASE nEffectiveCCMode == ccRepeatable
		IF ! VoDbInfo( DBI_ISFLOCK, REF uVOVal )
			BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
		ENDIF
		IF ! uVOVal
			SELF:UnLock( )
		ENDIF

	CASE nEffectiveCCMode == ccFile
		SELF:UnLock( )
	ENDCASE

	
	RETURN

METHOD __DbServerEval( uBlock AS USUAL, uCobFor AS USUAL, uCobWhile AS USUAL, ;
	nNext AS USUAL, nRecno AS USUAL, lRest AS LOGIC, lCC AS LOGIC, lCCUpdate AS LOGIC ) AS LOGIC STRICT 
	LOCAL lRetCode := TRUE AS LOGIC
	LOCAL lLimit AS LOGIC
	LOCAL lBlock AS LOGIC
	LOCAL lFor AS LOGIC
	LOCAL lWhile AS LOGIC
	LOCAL lInternalError AS LOGIC
	LOCAL iRecno AS INT
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL uRC AS USUAL
	LOCAL oError AS USUAL
	LOCAL nCurrRec AS DWORD
	LOCAL uFLock AS USUAL
   LOCAL lRestore	AS LOGIC
	
   // Make sure we restore the workarea
   // The codeblocks may select another workarea
	lRestore	:= DbSetRestoreWorkarea(TRUE)      

	VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )

	lErrorFlag := FALSE
	BEGIN SEQUENCE

		IF ! IsNil( nNext ) .AND. nNext != 0
			lLimit := TRUE
			iRecno := nNext
			IF iRecno < 1
				oHLStatus := HyperLabel{ #BadNext, __CavoStr( __CAVOSTR_DBFCLASS_BADNEXT_CAPTION ),  ;
					__CavoStr( __CAVOSTR_DBFCLASS_BADNEXT ) }
				lInternalError := TRUE
				BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
			ENDIF
		ELSE
			lLimit := FALSE
			IF IsNil( uCobWhile ) .AND. ! lRest .AND. ! SELF:GoTop( )
				oHLStatus := HyperLabel{ #NoGoTop, __CavoStr( __CAVOSTR_DBFCLASS_NOGOTOP_CAPTION ),  ;
					__CavoStr( __CAVOSTR_DBFCLASS_NOGOTOP ) }
				lInternalError := TRUE
				BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
			ENDIF
		ENDIF

		lBlock 	:= ! IsNil( uBlock )
		lFor 		:= ! IsNil( uCobFor )
		lWhile 	:= ! IsNil( uCobWhile )


		IF ! VoDbInfo( DBI_ISFLOCK, REF uFLock )
			BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
		ENDIF
		DO WHILE ! VoDbEof( )
			IF lWhile .AND. ! Eval( uCobWhile )
				EXIT
			ENDIF
			IF ( ! lFor .OR. Eval( uCobFor ) ) .AND. lBlock
				IF lCC
					IF nEffectiveCCMode = ccOptimistic
						IF lCCUpdate .AND. ! uFLock
							nCurrRec := VoDbRecno( )
							VoDbRlock( nCurrRec )
						ENDIF
					ELSE
						IF nEffectiveCCMode = ccStable .AND. lCCUpdate .AND. ! uFLock
							IF nLastLock != 0
								VoDbUnlock( nLastLock )
							ENDIF
							nLastLock := VoDbRecno( )
							IF ! VoDbEof( )
								lRetCode := VoDbRlock( nLastLock )
							ELSE
								nLastLock := 0
							ENDIF
						ELSEIF nEffectiveCCMode = ccRepeatable
							IF ! VoDbEof( )
								lRetCode := VoDbRlock( VoDbRecno( ) )
							ENDIF
						ENDIF
					ENDIF
				ENDIF

				uRC := Eval( uBlock )
				IF lCC .AND. nEffectiveCCMode = ccOptimistic .AND. lCCUpdate .AND. ! uFLock
					VoDbUnlock( nCurrRec )
				ENDIF

				IF IsLogic( uRC )
					lRetCode := lRetCode .AND. uRC
					IF ! lRetCode
						EXIT
					ENDIF
				ENDIF
			ENDIF

			IF lLimit .AND. ( --iRecno = 0 )
				EXIT
			ENDIF

			IF ! VoDbSkip( 1 )
				BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
			ENDIF
		ENDDO
	RECOVER USING oError
		oErrorInfo := oError
		IF ! lInternalError
			__DBSSetSelect(dwCurrentWorkArea  ) 
			SELF:Error( oErrorInfo, #__DbServerEval )
		ENDIF
		lRetCode := FALSE
	END SEQUENCE

	__DBSSetSelect( dwCurrentWorkArea )  

	
   DbSetRestoreWorkarea(lRestore)
	RETURN lRetCode

METHOD __GenerateStatusHL( oError AS  Error) AS HyperLabel STRICT 
	LOCAL oRet AS HyperLabel 
	LOCAL cDesc AS STRING
    IF oError == NULL_OBJECT
        oError := Error{}
    ENDIF
	
	lErrorFlag := TRUE

	cDesc := Symbol2String( ClassName(SELF) ) + ": "
	IF Len( oError:Description ) > 0
		cDesc += oError:Description
	ELSE
		cDesc += ErrString( oError:Gencode ) 
	ENDIF                                   
	IF  SLen(oError:SubCodeText) > 0
		cDesc += " ("+oError:SubCodeText+")"
	ENDIF

	IF oError:OSCode != 0
		cDesc += ":" + DosErrString( oError:OSCode )
	ENDIF

	oRet := HyperLabel{ oError:FuncSym, AsString( oError:Gencode ), cDesc }

	
	RETURN oRet

METHOD __InitRecordBuf( ) AS VOID STRICT 
	LOCAL i AS DWORD
	LOCAL x AS USUAL

	

	FOR i := 1 TO SELF:wFieldCount
		IF VoDbFieldGet( i, REF x )
			//Mark as BLOB when fieldtype = 'M' and not string
			aOriginalBuffer[BUFFER_VALUE, i] 			:= x                                                       
			IF ! IsString(x) .AND. SELF:aStruct[i,DBS_TYPE] == "M"  
				aOriginalBuffer[BUFFER_IS_BLOB, i] 		:= TRUE    
			ELSE                                           
				aOriginalBuffer[BUFFER_IS_BLOB, i] 	:= FALSE
			ENDIF
		ELSE
			// If VoDbFieldGet() fails this may be a BLOB field > 64 Kb
			aOriginalBuffer[BUFFER_VALUE, i] 			:= NIL
			aOriginalBuffer[BUFFER_IS_BLOB, i] 		:= TRUE
		ENDIF
		aCurrentBuffer[BUFFER_IS_CHANGED, i] := FALSE
	NEXT  // i


	RETURN


METHOD __Notify( kNotification, uDescription ) 
   LOCAL uRetValue AS USUAL
   LOCAL oError AS USUAL

	BEGIN SEQUENCE
		uRetValue := SELF:Notify( kNotification, uDescription )
	RECOVER USING oError
		oErrorInfo := oError
		oHLStatus := SELF:__GenerateStatusHL( oError )
	END SEQUENCE

   RETURN uRetValue

METHOD __NotifyBufferFlush( ) AS VOID STRICT  
	LOCAL dwCurrentWorkArea  AS DWORD
	

	ASend( aRelationChildren, #__NotifyBufferFlush )
	VoDbSelect( wWorkArea, OUT dwCurrentWorkArea )
	SELF:__OptimisticFlush( )
	__DBSSetSelect( dwCurrentWorkArea  ) 

	
	RETURN

METHOD __OptimisticFlush() AS VOID STRICT 
	LOCAL w AS DWORD
	LOCAL uFLock AS USUAL
	LOCAL uValue AS USUAL
	LOCAL nCurRec AS DWORD
	LOCAL uIsRLock AS USUAL
	LOCAL nOrgBuffLen AS DWORD
	LOCAL cFieldType	AS STRING
	

	IF nEffectiveCCMode == ccOptimistic .AND. lCCOptimisticRecChg
		nCurRec := VoDbRecno( )

		IF ! VoDbRecordInfo( DBRI_LOCKED, 0, REF uIsRLock )
			BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
		ENDIF

		IF ! uIsRLock
			IF SELF:__RLockVerify( )
				IF ! VoDbInfo( DBI_ISFLOCK, REF uFLock )
					BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
				ENDIF
				FOR w := 1 UPTO wFieldCount
					cFieldType := SELF:aStruct[w, DBS_TYPE]
					IF aCurrentBuffer[BUFFER_IS_CHANGED, w] .AND. ! aOriginalBuffer[BUFFER_IS_BLOB, w]
						uValue := aCurrentBuffer[BUFFER_VALUE, w]
						IF ! VoDbFieldPut( w, uValue )
							BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
						ENDIF
						IF ! uFLock
							// Memo Fields must NOT be padded !
							IF cFieldType != "M" .AND. IsString( aOriginalBuffer[BUFFER_VALUE, w] )
								nOrgBuffLen := SLen( aOriginalBuffer[BUFFER_VALUE, w] )
								aOriginalBuffer[BUFFER_VALUE, w] := PadR( uValue, nOrgBuffLen )
							ELSE
								aOriginalBuffer[BUFFER_VALUE, w] := uValue
							ENDIF
						ENDIF
						aCurrentBuffer[BUFFER_VALUE, w]   := NIL
						aCurrentBuffer[BUFFER_IS_CHANGED, w] := FALSE
					ENDIF
				NEXT

				lCCOptimisticRecChg := FALSE

				IF ! uFLock
					VoDbUnlock( nCurRec )
				ENDIF
			ELSE
				IF oErrorInfo = NULL_OBJECT
					BREAK DbError{ SELF, #Optimistic_Buffer_flush, EG_LOCK,  ;
						         __CavoStr( __CAVOSTR_DBFCLASS_RECORDCHANGED ) }
				ELSE
					BREAK oErrorInfo
				ENDIF
			ENDIF
		ENDIF
	ENDIF

	
	RETURN

METHOD __OptimisticFlushNoLock( ) AS VOID STRICT 
	LOCAL w AS DWORD
	LOCAL uValue AS USUAL
	IF nEffectiveCCMode == ccOptimistic .AND. lCCOptimisticRecChg
		FOR w := 1 UPTO wFieldCount
			IF aCurrentBuffer[BUFFER_IS_CHANGED, w] .AND. ! aOriginalBuffer[BUFFER_IS_BLOB, w]
				uValue := aCurrentBuffer[BUFFER_VALUE, w]
				IF ! VoDbFieldPut( w, uValue )
					BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
				ENDIF
				aCurrentBuffer[BUFFER_VALUE, w]		:= NIL
				aCurrentBuffer[BUFFER_IS_CHANGED, w]	:= FALSE
			ENDIF
		NEXT
		lCCOptimisticRecChg := FALSE
	ENDIF

	
	RETURN

METHOD __ProcessConcurrency( lBreak AS LOGIC) AS LOGIC STRICT 
	LOCAL uVOVal AS USUAL
	LOCAL lError AS LOGIC
	LOCAL lRetCode AS LOGIC
	

	lError := FALSE
	IF SELF:nEffectiveCCMode = ccStable
		IF VoDbInfo( DBI_ISFLOCK, REF uVOVal )
			IF ! uVOVal
				IF nLastLock != 0
					VoDbUnlock( SELF:nLastLock )
				ENDIF
				nLastLock := VoDbRecno( )
				IF ! VoDbEof( )
					lRetCode := VoDbRlock( SELF:nLastLock )
				ELSE
					SELF:nLastLock := 0
				ENDIF
			ELSE
				SELF:nLastLock := 0
			ENDIF
		ELSE
			lError := TRUE
		ENDIF

	ELSEIF SELF:nEffectiveCCMode = ccRepeatable
		IF ! VoDbEof( )
			IF VoDbInfo( DBI_ISFLOCK, REF uVOVal )
				IF ! uVOVal
					lRetCode := VoDbRlock( VoDbRecno( ) )
				ENDIF
			ELSE
				lError := TRUE
			ENDIF
		ENDIF 
	ELSE
		lRetCode := TRUE
	ENDIF

	IF lBreak .AND. (lError .OR. ! lRetCode) .AND. CanBreak()
		BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
	ENDIF

	
	
	RETURN lRetCode

METHOD __RLockVerify( ) AS LOGIC STRICT 
	LOCAL w AS DWORD
	LOCAL siCurrentRec AS LONGINT
	LOCAL uWasLocked AS USUAL
	LOCAL uVOVal AS USUAL
	LOCAL lRetCode AS LOGIC
   LOCAL nDiff	AS FLOAT
	LOCAL uValue	AS USUAL
	LOCAL aRLockVerifyBuffer AS ARRAY

	
	lRetCode := TRUE
	siCurrentRec := VoDbRecno( )
	IF ! VoDbInfo( DBI_ISFLOCK, REF uVOVal )
		BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
	ENDIF
	IF ! VoDbRecordInfo( DBRI_LOCKED, 0, REF uWasLocked )
		BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
	ENDIF
	uWasLocked := uWasLocked .OR. uVOVal
	IF ! uWasLocked
		lRetCode := VoDbRlock( siCurrentRec )
	ENDIF
	IF ! lRetCode
		BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
	ELSE    
		// Store our 'current record' 
 		aRLockVerifyBuffer := ArrayNew( wFieldCount )
 		FOR w := 1 UPTO wFieldCount
 			aRLockVerifyBuffer[w] := __DBSFieldGet(w)
 		NEXT                            
		// Get the current record buffer from disk and compare the fields
		// in the current buffer with our values.
		VoDbBuffRefresh( )
		FOR w := 1 UPTO wFieldCount
			uValue := __DBSFieldGet( w )           
			//                                                        
			IF aOriginalBuffer[BUFFER_IS_BLOB, w]
				// Field was a blob. Compare field types
				IF UsualType(aOriginalBuffer[BUFFER_VALUE, w]) != UsualType(uValue)
					lRetCode := FALSE
				ENDIF
			ELSEIF ! ( aOriginalBuffer[BUFFER_VALUE, w] == uValue )
				// Field has changed. 
				// Test for float fields with non-relevant differences
				IF IsFloat(uValue)
				   nDiff := 10 ^ -(aStruct[w, DBS_DEC])
				   IF Abs(aOriginalBuffer[BUFFER_VALUE, w] - uValue) > nDiff
					   lRetCode := FALSE
				   ENDIF
				ELSE
				   lRetCode := FALSE
				ENDIF
			//ELSE
				// The field has not changed 
			ENDIF
			IF ! lRetCode
				oHLStatus := HyperLabel{ #RECORDCHANGED,  ;
					__CavoStr( __CAVOSTR_DBFCLASS_RECORDCHANGED_CAPTION ),  ;
					__CavoStr( __CAVOSTR_DBFCLASS_RECORDCHANGED ), NIL }
				oErrorInfo := NULL_OBJECT
				lErrorFlag := TRUE
				EXIT
			ENDIF
		NEXT
      // restore the 'current record' in the buffer
 		FOR w := 1 UPTO wFieldCount
 			IF ! VoDbFieldPut( w, aRLockVerifyBuffer[w] )
 				BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
 			ENDIF
 		NEXT

		IF ! lRetCode .AND. ! uWasLocked
			VoDbUnlock( siCurrentRec )
		ENDIF
	ENDIF

	
	RETURN lRetCode

METHOD __SetAlias( cName AS STRING, aField AS ARRAY, nField AS DWORD) AS VOID STRICT 
	LOCAL cAlias AS STRING
	LOCAL oFSpec AS FieldSpec

	
	cAlias := "_" + cName
	oFSpec := FieldSpec{ cAlias, aField[DBS_TYPE], aField[DBS_LEN], aField[DBS_DEC] }
	SELF:aDataFields[nField] := DataField{ cName, oFSpec }

	
	RETURN

METHOD __SetStatusHL( uFuncSym AS USUAL, uGenCode AS USUAL, uMessage AS USUAL ) AS VOID STRICT 
	
	lErrorFlag := TRUE
	oErrorInfo := NULL_OBJECT
	IF ! IsString( uGenCode )
		uGenCode := AsString( uGenCode )
	ENDIF
	oHLStatus := HyperLabel{ uFuncSym, uGenCode, uMessage, NIL }
	
	RETURN

METHOD __SetupLocks( )  AS VOID STRICT 
	LOCAL w AS DWORD
	LOCAL uFlock AS USUAL

	
	nLastLock := 0
	DO CASE
	CASE nEffectiveCCMode == ccNone
		//nothing to do

	CASE nEffectiveCCMode == ccOptimistic
		FOR w := 1 UPTO wFieldCount
			aCurrentBuffer[BUFFER_VALUE, w]   := NIL
			aCurrentBuffer[BUFFER_IS_CHANGED, w] := FALSE
		NEXT
		lCCOptimisticRecChg := FALSE
		SELF:__InitRecordBuf()

	CASE nEffectiveCCMode == ccStable .OR. nEffectiveCCMode == ccRepeatable
		IF ! VoDbInfo( DBI_ISFLOCK, REF uFlock )
			BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
		ENDIF
		IF ! uFlock
			nLastLock := SELF:RecNo
			IF ! SELF:RLock( nLastLock )
				nLastLock := 0
				oHLStatus := SELF:Status
			ENDIF
		ENDIF

	CASE nEffectiveCCMode == ccFile
		IF ! SELF:FLock( )
			oHLStatus := SELF:Status
		ENDIF

	OTHERWISE
		oErrorInfo := DbError{ SELF, #ConcurrencyControl, EG_ARG,  ;
			__CavoStr( __CAVOSTR_DBFCLASS_BADCONCURRENCYASSIGN ), nCCMode, "nCCMode" }
		SELF:Error( oErrorInfo, #ConcurrencyControl )
	ENDCASE

	
	RETURN


CONSTRUCTOR( cFile AS STRING, lShareMode := FALSE AS OBJECT, lReadOnlyMode := FALSE AS OBJECT, xDriver:= "" AS STRING, aRdd := NULL_ARRAY AS ARRAY) 
    SELF(FileSpec{cFile}, lShareMode, lReadOnlyMode , xDriver, aRdd )
    
CONSTRUCTOR( oFS AS FileSpec, lShareMode := FALSE AS OBJECT, lReadOnlyMode := FALSE AS OBJECT, xDriver:= "" AS STRING, aRdd := NULL_ARRAY AS ARRAY) 
	LOCAL dwCurrentWorkArea := 0 AS DWORD
	LOCAL cFileName AS STRING
	LOCAL w AS DWORD
	LOCAL n AS DWORD
	LOCAL oError AS USUAL
	LOCAL symTemp AS SYMBOL
	LOCAL cTemp AS STRING
	LOCAL rddList AS _RddList
	LOCAL uTemp AS USUAL
	LOCAL aField AS ARRAY
	LOCAL uProps AS USUAL
	LOCAL wProps AS DWORD
	LOCAL lRetCode AS LOGIC
	
	SUPER( )

	aRelationChildren := { }

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		siSuspendNotification := 0
		dwCurrentWorkArea := VoDbGetSelect( )

		IF Empty( oFS:Extension )
			oFS:Extension := ".DBF"
		ENDIF
		oFileSpec := oFS

		cFileName := oFileSpec:FileName

		symAlias := SELF:ConstructUniqueAlias( cFileName )

		IF lShareMode  == NULL
			SELF:lShared := ! SetExclusive( )
		ELSEIF lShareMode  IS LOGIC
			SELF:lShared := (LOGIC) lShareMode
		ELSEIF lShareMode IS SYMBOL VAR symShareMode
			IF symShareMode = #Shared
				SELF:lShared := TRUE
			ELSEIF symShareMode = #Exclusive
				SELF:lShared := FALSE
			ELSE
				BREAK DbError{ SELF, #Init, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_BADSHAREMODE ),  ;
					lShareMode, "lShareMode" }
			ENDIF

		ELSEIF lShareMode IS STRING VAR strShareMode
			symTemp := String2Symbol( strShareMode )
			IF symTemp = #Shared
				SELF:lShared := TRUE
			ELSEIF symTemp = #Exclusive
				SELF:lShared := FALSE
			ELSE
				BREAK DbError{ SELF, #Init, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_BADSHAREMODE ),  ;
					lShareMode, "lShareMode" }
			ENDIF
		ELSE
			BREAK DbError{ SELF, #Init, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_BADSHAREMODE ),  ;
				lShareMode, "lShareMode" }
		ENDIF

		IF lReadOnlyMode == NULL
			lReadOnly := FALSE
		ELSEIF lReadOnlyMode IS LOGIC
			lReadOnly := (LOGIC) lReadOnlyMode
		ELSEIF lReadOnlyMode  IS SYMBOL VAR symReadOnlyMode
			IF symReadOnlyMode = #ReadOnly
				lReadOnly := TRUE
			ELSEIF symReadOnlyMode = #ReadWrite
				lReadOnly := FALSE
			ELSE
				BREAK DbError{ SELF, #Init, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_BADREADONLYMODE ),  ;
					lReadOnlyMode, "lReadOnlyMode" }
			ENDIF
		ELSEIF lReadOnlyMode IS STRING VAR strReadOnlyMode
			symTemp := String2Symbol( strReadOnlyMode)
			IF symTemp = #ReadOnly
				lReadOnly := TRUE
			ELSEIF symTemp = #ReadWrite
				lReadOnly := FALSE
			ELSE
				BREAK DbError{ SELF, #Init, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_BADREADONLYMODE ),  ;
					lReadOnlyMode, "lReadOnlyMode" }
			ENDIF
		ELSE
			BREAK DbError{ SELF, #Init, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_BADREADONLYMODE ),  ;
				lReadOnlyMode, "lReadOnlyMode" }
		ENDIF

		cTemp := Symbol2String( ClassName( SELF ) )
		oHyperLabel := HyperLabel{ cFileName, cFileName,  ;
			cTemp + ": " + cFileName + " " +  ;
			VO_Sprintf( __CAVOSTR_DBFCLASS_ALIAS, Symbol2String( symAlias ) ),  ;
			cTemp + "_" + cFileName }

		oHLStatus := NIL

		SELF:wWorkArea := VoDbSetSelect( -1 )

		IF ! ( IsArray( xDriver ) .OR. IsString( xDriver ) )
			xDriver := RddName( )
		ENDIF

		SELF:aRdds := __RDDList( xDriver, aRdd )
		rddList := __AllocRddList( aRdds )

		lRetCode := VoDbUseArea( FALSE, rddList, oFileSpec:FullPath, Symbol2String( symAlias ),  ;
			lShared, lReadOnly )

		IF ! lRetCode
			BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
		ENDIF

		SELF:cRDDName := RddName( )
        SELF:oRDD     := (XSharp.RDD.IRdd) DbInfo(DBI_RDD_OBJECT)

        SELF:nCCMode := SELF:nEffectiveCCMode := DbGetDefaultLockMode()
		IF lReadOnly .OR. ! lShared
			SELF:nEffectiveCCMode := ccNone
		ENDIF

		wFieldCount := FCount( )

		aStruct := ArrayCreate( wFieldCount )

		FOR w := 1 UPTO wFieldCount
			uProps := NIL
			IF ! VoDbFieldInfo( DBS_PROPERTIES, w, REF uProps )
				BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
			ENDIF
			wProps := uProps
			aField := ArrayCreate( wProps )
			FOR n := 1 UPTO wProps
				VoDbFieldInfo( n, w, REF uTemp )
				aField[n] := uTemp
			NEXT
			aStruct[w] := aField
		NEXT

		aDataFields := ArrayNew( wFieldCount )

		aOriginalBuffer := ArrayNew( 2, wFieldCount )
		aCurrentBuffer  := ArrayNew( 2, wFieldCount )
		SELF:__InitRecordBuf( )
		

		__DBSSetSelect(dwCurrentWorkArea) 

	RECOVER USING oError
		oErrorInfo := oError
		IF Used( )
			VoDbCloseArea( )
		ENDIF
		wWorkArea := 0
		__DBSSetSelect(dwCurrentWorkArea) 
		oHLStatus := HyperLabel{ #NoTable, __CavoStr( __CAVOSTR_DBFCLASS_NOTABLE_CAPTION ),  ;
			__CavoStr( __CAVOSTR_DBFCLASS_NOTABLE ) }
		SELF:Error( oErrorInfo, #Init )
	END SEQUENCE

	SELF:nRetries := LockTries( )
	
	RETURN 


END CLASS

