#translate DBFDebug(<c1> [, <cn>]) =>

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
	PROTECT nReTries AS DWORD
	PROTECT oRDD AS XSharp.RDD.IRdd


METHOD __AcceptSelectiveRelation( oDBParent AS DbServer, wParentWorkArea AS DWORD, ;
	cbSelection AS USUAL) AS VOID STRICT 
	//SE-060601
	LOCAL cIndexExt AS STRING
	LOCAL dwCurrentWorkArea AS DWORD

	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF
	oDBSelectionParent := oDBParent
	wSelectionWorkArea := wParentWorkArea

	cbSelectionParentExpression := cbSelection

	VODBSelect( wWorkArea, @dwCurrentWorkArea )

	cIndexExt := IndexExt( )

	IF InStr( "MDX", cIndexExt )
		lSelectionActive := TRUE
		siSelectionStatus := DBSELECTIONNULL
		cbSelectionIndexingExpression := DBSelectionIndex{ SELF, __DBSDBOrderInfo( DBOI_EXPRESSION, "", 0 ), wWorkArea }
	ELSE
		lCDXSelectionActive := TRUE
	ENDIF

   __DBSSetSelect( dwCurrentWorkArea  ) //SE-060527

	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__)
	#ENDIF
	RETURN

METHOD __BuildDataField( a AS ARRAY ) AS DataField STRICT 
	LOCAL oRet AS OBJECT

	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF
	IF IsArray( a )
		oRet := DataField{ a[ DBS_NAME ], FieldSpec{ a[ DBS_NAME ], a[ DBS_TYPE ],  ;
			a[ DBS_LEN ], a[ DBS_DEC ] } }
	ENDIF

	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(oRet))
	#ENDIF
	RETURN oRet

METHOD __ClearChildRelation( oChild AS DbServer ) AS VOID STRICT 
	LOCAL w AS DWORD
	LOCAL wLen AS DWORD

	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF
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

	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__)
	#ENDIF
	RETURN

METHOD __ClearLocks( )  AS VOID STRICT 
	LOCAL uVOVal AS USUAL

	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF
	DO CASE
	CASE nEffectiveCCMode == ccOptimistic
		SELF:__OptimisticFlush( )

	CASE nEffectiveCCMode == ccStable
		IF ! VODBInfo( DBI_IsFLock, @uVOVal )
			BREAK ErrorBuild( _VODBErrInfoPtr( ) )
		ENDIF
		IF ! uVOVal
			SELF:Unlock( nLastLock )
		ENDIF

	CASE nEffectiveCCMode == ccRepeatable
		IF ! VODBInfo( DBI_IsFLock, @uVOVal )
			BREAK ErrorBuild( _VODBErrInfoPtr( ) )
		ENDIF
		IF ! uVOVal
			SELF:Unlock( )
		ENDIF

	CASE nEffectiveCCMode == ccFile
		SELF:Unlock( )
	ENDCASE

	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__)
	#ENDIF
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
	//LOCAL cobOldErrFunc AS USUAL
	LOCAL oError AS USUAL
	LOCAL nCurrRec AS DWORD
	LOCAL uFLock AS USUAL
   LOCAL lRestore	AS LOGIC
	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF
	//cobOldErrFunc := ErrorBlock( { | oErr | _Break( oErr ) } )
   //RvdH 070711 Make sure we restore the workarea
   //				  The codeblocks may select another workarea
	lRestore	:= DbSetRestoreWorkarea(TRUE)      

	VODBSelect( wWorkArea, @dwCurrentWorkArea )

	lErrorFlag := FALSE
	BEGIN SEQUENCE
      //PP-031124 Don't Default lRest since it is now strongly typed as LOGIC
      //    Default( @lRest, FALSE )

		IF ! IsNil( nNext ) .AND. nNext != 0
			lLimit := TRUE
			iRecno := nNext
			IF iRecno < 1
				oHLStatus := HyperLabel{ #BadNext, __CavoStr( __CAVOSTR_DBFCLASS_BADNEXT_CAPTION ),  ;
					__CavoStr( __CAVOSTR_DBFCLASS_BADNEXT ) }
				lInternalError := TRUE
				BREAK ErrorBuild( _VODBErrInfoPtr( ) )
			ENDIF
		ELSE
			lLimit := FALSE
			IF IsNil( uCobWhile ) .AND. ! lRest .AND. ! SELF:GoTop( )
				oHLStatus := HyperLabel{ #NoGoTop, __CavoStr( __CAVOSTR_DBFCLASS_NOGOTOP_CAPTION ),  ;
					__CavoStr( __CAVOSTR_DBFCLASS_NOGOTOP ) }
				lInternalError := TRUE
				BREAK ErrorBuild( _VODBErrInfoPtr( ) )
			ENDIF
		ENDIF

		lBlock 	:= ! IsNil( uBlock )
		lFor 		:= ! IsNil( uCobFor )
		lWhile 	:= ! IsNil( uCobWhile )


		IF ! VODBInfo( DBI_IsFLock, @uFLock )
			BREAK ErrorBuild( _VODBErrInfoPtr( ) )
		ENDIF
		DO WHILE ! VODBEof( )
			IF lWhile .AND. ! Eval( uCobWhile )
				EXIT
			ENDIF
			IF ( ! lFor .OR. Eval( uCobFor ) ) .AND. lBlock
				IF lCC
					IF nEffectiveCCMode = ccOptimistic
						IF lCCUpdate .AND. ! uFLock
							nCurrRec := VODBRecno( )
							VODBRLock( nCurrRec )
						ENDIF
					ELSE
						IF nEffectiveCCMode = ccStable .AND. lCCUpdate .AND. ! uFLock
							IF nLastLock != 0
								VODBUnlock( nLastLock )
							ENDIF
							nLastLock := VODBRecno( )
							IF ! VODBEof( )
								lRetCode := VODBRLock( nLastLock )
							ELSE
								nLastLock := 0
							ENDIF
						ELSEIF nEffectiveCCMode = ccRepeatable
							IF ! VODBEof( )
								lRetCode := VODBRLock( VODBRecno( ) )
							ENDIF
						ENDIF
					ENDIF
				ENDIF

				uRC := Eval( uBlock )
				IF lCC .AND. nEffectiveCCMode = ccOptimistic .AND. lCCUpdate .AND. ! uFLock
					VODBUnlock( nCurrRec )
				ENDIF

				IF IsLogic( uRc )
					lRetCode := lRetCode .AND. uRC
					IF ! lRetCode
						EXIT
					ENDIF
				ENDIF
			ENDIF

			IF lLimit .AND. ( --iRecno = 0 )
				EXIT
			ENDIF

			IF ! VODBSkip( 1 )
				BREAK ErrorBuild( _VODBErrInfoPtr( ) )
			ENDIF
		ENDDO
	RECOVER USING oError
		//ErrorBlock( cobOldErrFunc )
		oErrorInfo := oError
		IF ! lInternalError
			__DBSSetSelect(dwCurrentWorkArea  ) //SE-060527
			SELF:Error( oErrorInfo, #__DbServerEval )
		ENDIF
		lRetCode := FALSE
	END SEQUENCE

	__DBSSetSelect( dwCurrentWorkArea )  //SE-060527
	//ErrorBlock( cobOldErrFunc )

	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(lRetCode))
	#ENDIF
   DbSetRestoreWorkarea(lRestore)
	RETURN lRetCode

METHOD __GenerateStatusHL( oError AS  Error) AS HyperLabel STRICT 
	LOCAL oRet AS HyperLabel // OBJECT  dcaton 070428 changed from OBJECT
	LOCAL cDesc AS STRING
    IF oError == NULL_OBJECT
        oError := Error{}
    ENDIF

	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF
	lErrorFlag := TRUE

	cDesc := Symbol2String( ClassName(SELF) ) + ": "
	IF Len( oError:Description ) > 0
		cDesc += oError:Description
	ELSE
		cDesc += ErrString( oError:gencode ) 
	ENDIF                                   
	IF  SLen(oError:SubCodeText) > 0
		cDesc += " ("+oError:SubCodeText+")"
	ENDIF

	IF oError:OSCode != 0
		cDesc += ":" + DosErrString( oError:OSCode )
	ENDIF

	oRet := HyperLabel{ oError:FuncSym, AsString( oError:GenCode ), cDesc }

	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(oRet))
	#ENDIF
	RETURN oRet

METHOD __InitRecordBuf( ) AS VOID STRICT 
	LOCAL i AS DWORD
	LOCAL x AS USUAL
	#IFDEF __DEBUG__
	  LOCAL lRet AS LOGIC
	#ENDIF

	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF

	FOR i := 1 TO SELF:wFieldCount
		IF VODBFieldGet( i, @x )
			#IFDEF __DEBUG__
			  lRet := TRUE
			#ENDIF                             
			//RvdH 060928 Mark as BLOB when fieldtype = 'M' and not string
			aOriginalBuffer[BUFFER_VALUE, i] 			:= x                                                       
			IF ! IsString(x) .AND. SELF:aStruct[i,DBS_TYPE] == "M"  
				aOriginalBuffer[BUFFER_IS_BLOB, i] 		:= TRUE    
			ELSE                                           
				aOriginalBuffer[BUFFER_IS_BLOB, i] 	:= FALSE
			ENDIF
		ELSE
			// If VODBFieldGet() fails this may be a BLOB field > 64 Kb
			aOriginalBuffer[BUFFER_VALUE, i] 			:= NIL
			aOriginalBuffer[BUFFER_IS_BLOB, i] 		:= TRUE
		ENDIF
		aCurrentBuffer[BUFFER_IS_CHANGED, i] := FALSE
	NEXT  // i

	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(lRet))
	#ENDIF

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
	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF

	ASend( aRelationChildren, #__NotifyBufferFlush )
	VODBSelect( wWorkArea, @dwCurrentWorkArea )
	SELF:__OptimisticFlush( )
	__DBSSetSelect( dwCurrentWorkArea  ) //SE-060527

	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__)
	#ENDIF
	RETURN

METHOD __OptimisticFlush() AS VOID STRICT 
	LOCAL w AS DWORD
	LOCAL uFLock AS USUAL
	LOCAL uValue AS USUAL
	LOCAL nCurRec AS DWORD
	LOCAL uIsRLock AS USUAL
	LOCAL nOrgBuffLen AS DWORD
	LOCAL cFieldType	AS STRING
	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF

	IF nEffectiveCCMode == ccOptimistic .AND. lCCOptimisticRecChg
		nCurRec := VODBRecno( )

		IF ! VODBRecordInfo( DBRI_LOCKED, 0, @uIsRLock )
			BREAK ErrorBuild( _VODBErrInfoPtr( ) )
		ENDIF

		IF ! uIsRLock
			IF SELF:__RLockVerify( )
				IF ! VODBInfo( DBI_IsFLock, @uFLock )
					BREAK ErrorBuild( _VODBErrInfoPtr( ) )
				ENDIF
				FOR w := 1 UPTO wFieldCount
					cFieldType := SELF:aStruct[w, DBS_TYPE]
					IF aCurrentBuffer[BUFFER_IS_CHANGED, w] .AND. ! aOriginalBuffer[BUFFER_IS_BLOB, w]
						uValue := aCurrentBuffer[BUFFER_VALUE, w]
						IF ! VODBFieldPut( w, uValue )
							BREAK ErrorBuild( _VODBErrInfoPtr( ) )
						ENDIF
						IF ! uFLock
							//RvdH 051216 Memo Fields must NOT be padded !
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
					VODBUnlock( nCurRec )
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

	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__)
	#ENDIF
	RETURN

METHOD __OptimisticFlushNoLock( ) AS VOID STRICT 
	LOCAL w AS DWORD
	LOCAL uValue AS USUAL
	// LOCAL nCurRec AS DWORD         dcaton 070430 never used

	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF
	// nCurRec := VODBRecno( )      // dcaton 070430 never used
	IF nEffectiveCCMode == ccOptimistic .AND. lCCOptimisticRecChg
		FOR w := 1 UPTO wFieldCount
			IF aCurrentBuffer[BUFFER_IS_CHANGED, w] .AND. ! aOriginalBuffer[BUFFER_IS_BLOB, w]
				uValue := aCurrentBuffer[BUFFER_VALUE, w]
				IF ! VODBFieldPut( w, uValue )
					BREAK ErrorBuild( _VODBErrInfoPtr( ) )
				ENDIF
				aCurrentBuffer[BUFFER_VALUE, w]		:= NIL
				aCurrentBuffer[BUFFER_IS_CHANGED, w]	:= FALSE
			ENDIF
		NEXT
		lCCOptimisticRecChg := FALSE
	ENDIF

	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__)
	#ENDIF
	RETURN

METHOD __ProcessConcurrency( lBreak AS LOGIC) AS LOGIC STRICT 
	LOCAL uVOVal AS USUAL
	LOCAL lError AS LOGIC
	LOCAL lRetCode AS LOGIC
	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF

	lError := FALSE
	IF SELF:nEffectiveCCMode = ccStable
		IF VODBInfo( DBI_IsFLock, @uVOVal )
			IF ! uVOVal
				IF nLastLock != 0
					VODBUnlock( SELF:nLastLock )
				ENDIF
				nLastLock := VODBRecno( )
				IF ! VODBEof( )
					lRetCode := VODBRLock( SELF:nLastLock )
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
		IF ! VODBEof( )
			IF VODBInfo( DBI_IsFLock, @uVOVal )
				IF ! uVOVal
					lRetCode := VODBRLock( VODBRecno( ) )
				ENDIF
			ELSE
				lError := TRUE
			ENDIF
		ENDIF 
	ELSE
		lRetCode := TRUE
	ENDIF

	IF lBreak .AND. (lError .OR. ! lRetCode) .AND. CanBreak()
		BREAK ErrorBuild( _VODBErrInfoPtr( ) )
	ENDIF

	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(lRetCode))
	#ENDIF
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

	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF
	lRetCode := TRUE
	siCurrentRec := VODBRecno( )
	IF ! VODBInfo( DBI_IsFLock, @uVOVal )
		BREAK ErrorBuild( _VODBErrInfoPtr( ) )
	ENDIF
	IF ! VODBRecordInfo( DBRI_LOCKED, 0, @uWasLocked )
		BREAK ErrorBuild( _VODBErrInfoPtr( ) )
	ENDIF
	uWasLocked := uWasLocked .OR. uVOVal
	IF ! uWasLocked
		lRetCode := VODBRLock( siCurrentRec )
	ENDIF
	IF ! lRetCode
		BREAK ErrorBuild( _VODBErrInfoPtr( ) )
	ELSE    
		// Store our 'current record' 
 		aRLockVerifyBuffer := ArrayNew( wFieldCount )
 		FOR w := 1 UPTO wFieldCount
 			aRLockVerifyBuffer[w] := __DBSFieldGet(w)
 		NEXT                            
		// Get the current record buffer from disk and compare the fields
		// in the current buffer with our values.
		VODBBuffRefresh( )
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
 			IF ! VODBFieldPut( w, aRLockVerifyBuffer[w] )
 				BREAK ErrorBuild( _VODBErrInfoPtr( ) )
 			ENDIF
 		NEXT

		IF ! lRetCode .AND. ! uWasLocked
			VODBUnlock( siCurrentRec )
		ENDIF
	ENDIF

	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__, AsString(lRetCode))
	#ENDIF
	RETURN lRetCode

METHOD __SetAlias( cName AS STRING, aField AS ARRAY, nField AS DWORD) AS VOID STRICT 
	LOCAL cAlias AS STRING
	LOCAL oFSpec AS FieldSpec

	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF
	cAlias := "_" + cName
	oFSpec := FieldSpec{ cAlias, aField[DBS_TYPE], aField[DBS_LEN], aField[DBS_DEC] }
	SELF:aDataFields[nField] := DataField{ cName, oFSpec }

	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__)
	#ENDIF
	RETURN

METHOD __SetStatusHL( uFuncSym AS USUAL, uGenCode AS USUAL, uMessage AS USUAL ) AS VOID STRICT 
	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF
	lErrorFlag := TRUE
	oErrorInfo := NULL_OBJECT
	IF ! IsString( uGenCode )
		uGenCode := AsString( uGenCode )
	ENDIF
	oHLStatus := HyperLabel{ uFuncSym, uGenCode, uMessage, NIL }
	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__)
	#ENDIF
	RETURN

METHOD __SetupLocks( )  AS VOID STRICT 
	LOCAL w AS DWORD
	LOCAL uFlock AS USUAL

	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF
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
		IF ! VODBInfo( DBI_IsFLock, @uFLock )
			BREAK ErrorBuild( _VODBErrInfoPtr( ) )
		ENDIF
		IF ! uFLock
			nLastLock := SELF:Recno
			IF ! SELF:RLOCK( nLastLock )
				nLastLock := 0
				oHLStatus := SELF:Status
			ENDIF
		ENDIF

	CASE nEffectiveCCMode == ccFile
		IF ! SELF:FLOCK( )
			oHLStatus := SELF:Status
		ENDIF

	OTHERWISE
		oErrorInfo := DbError{ SELF, #ConcurrencyControl, EG_ARG,  ;
			__CavoStr( __CAVOSTR_DBFCLASS_BADCONCURRENCYASSIGN ), nCCMode, "nCCMode" }
		SELF:Error( oErrorInfo, #ConcurrencyControl )
	ENDCASE

	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__)
	#ENDIF
	RETURN


CONSTRUCTOR( oFile, lShareMode, lReadOnlyMode, xDriver, aRdd ) 
	//LOCAL cobOldErrFunc AS USUAL
	LOCAL dwCurrentWorkArea AS DWORD
	LOCAL cFileName AS STRING
	LOCAL w AS DWORD
	LOCAL n AS DWORD
	LOCAL oError AS USUAL
	LOCAL symTemp AS SYMBOL
	LOCAL cTemp AS STRING
	LOCAL rddList AS _RDDLIST
	LOCAL uTemp AS USUAL
	LOCAL aField AS ARRAY
	LOCAL uProps AS USUAL
	LOCAL wProps AS DWORD
	LOCAL lRetCode AS LOGIC
	#IFDEF __DEBUG__
		DBFDebug("Entering "+__ENTITY__)
	#ENDIF
	SUPER( )

	//cobOldErrFunc := ErrorBlock( { | oErr | _Break( oErr ) } )

	aRelationChildren := { }

	lErrorFlag := FALSE
	BEGIN SEQUENCE
		siSuspendNotification := 0
		dwCurrentWorkArea := VODBGetSelect( )

		IF  IsObject(oFile) .AND. __Usual.ToObject(oFile) IS FileSpec VAR oFS
			IF Empty( oFS:Extension )
				oFS:Extension := ".DBF"
			ENDIF
			oFileSpec := oFS

		ELSEIF IsString( oFile ) .OR. IsSymbol( oFile )
			oFileSpec := FileSpec{ oFile }
			IF oFileSpec:Extension == NULL_STRING
				oFileSpec:Extension := ".DBF"
			ENDIF

		ELSEIF IsNil( oFile )
			BREAK DbError{ SELF, #Init, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_NOFILENAME ),  ;
				NIL, "oFile" }
		ELSE
			BREAK DbError{ SELF, #Init, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_BADFILENAME ),  ;
				oFile, "oFile" }
		ENDIF

		cFileName := oFileSpec:FileName

		symAlias := SELF:ConstructUniqueAlias( cFileName )

		IF IsNil( lShareMode )
			SELF:lShared := ! SetExclusive( )
		ELSEIF IsLogic( lShareMode )
			SELF:lShared := lShareMode
		ELSEIF IsSymbol( lShareMode )
			IF lShareMode = #Shared
				SELF:lShared := TRUE
			ELSEIF lShareMode = #Exclusive
				SELF:lShared := FALSE
			ELSE
				BREAK DbError{ SELF, #Init, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_BADSHAREMODE ),  ;
					lShareMode, "lShareMode" }
			ENDIF

		ELSEIF IsString( lShareMode )
			symTemp := String2Symbol( lShareMode )
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

		IF IsNil( lReadOnlyMode )
			lReadOnly := FALSE
		ELSEIF IsLogic( lReadOnlyMode )
			lReadOnly := lReadOnlyMode
		ELSEIF IsSymbol( lReadOnlyMode )
			IF lReadOnlyMode = #ReadOnly
				lReadOnly := TRUE
			ELSEIF lReadOnlyMode = #ReadWrite
				lReadOnly := FALSE
			ELSE
				BREAK DbError{ SELF, #Init, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_BADREADONLYMODE ),  ;
					lReadOnlyMode, "lReadOnlyMode" }
			ENDIF
		ELSEIF IsString( lReadOnlyMode )
			symTemp := String2Symbol( lReadOnlyMode )
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

		SELF:wWorkArea := VODBSetSelect( -1 )

		IF ! ( IsArray( xDriver ) .OR. IsString( xDriver ) )
			xDriver := RDDName( )
		ENDIF

		SELF:aRdds := __RddList( xDriver, aRdd )
		rddList := __AllocRddList( aRdds )

		lRetCode := VODBUseArea( FALSE, rddList, oFileSpec:FullPath, Symbol2String( symAlias ),  ;
			lShared, lReadOnly )

#ifndef __VULCAN__
		MemFree( RDDLIST )
#endif

		IF ! lRetCode
			BREAK ErrorBuild( _VODBErrInfoPtr( ) )
		ENDIF

		SELF:cRDDName := RDDName( )
        SELF:oRDD     := DbInfo(DBI_RDD_OBJECT)

        SELF:nCCMode := SELF:nEffectiveCCMode := DbGetDefaultLockMode()
		IF lReadOnly .OR. ! lShared
			SELF:nEffectiveCCMode := ccNone
		ENDIF

		wFieldCount := FCount( )

		aStruct := ArrayCreate( wFieldCount )

		FOR w := 1 UPTO wFieldCount
			uProps := NIL
			IF ! VODBFieldInfo( DBS_PROPERTIES, w, @uProps )
				BREAK ErrorBuild( _VODBErrInfoPtr( ) )
			ENDIF
			wProps := uProps
			aField := ArrayCreate( wProps )
			FOR n := 1 UPTO wProps
				VODBFieldInfo( n, w, @uTemp )
				aField[n] := uTemp
			NEXT
			aStruct[w] := aField
		NEXT

		aDataFields := ArrayNew( wFieldCount )

		aOriginalBuffer := ArrayNew( 2, wFieldCount )
		aCurrentBuffer  := ArrayNew( 2, wFieldCount )
		SELF:__InitRecordBuf( )
		

		__DBSSetSelect(dwCurrentWorkArea) //SE-060527

	RECOVER USING oError
		oErrorInfo := oError
		//ErrorBlock( cobOldErrFunc )
		IF Used( )
			VODBCloseArea( )
		ENDIF
		wWorkArea := 0
		__DBSSetSelect(dwCurrentWorkArea) //SE-060527
		oHLStatus := HyperLabel{ #NoTable, __CavoStr( __CAVOSTR_DBFCLASS_NOTABLE_CAPTION ),  ;
			__CavoStr( __CAVOSTR_DBFCLASS_NOTABLE ) }
		SELF:Error( oErrorInfo, #Init )
	END SEQUENCE

	//ErrorBlock( cobOldErrFunc )
	SELF:nReTries := LockTries( )
	#IFDEF __DEBUG__
		DBFDebug("Leaving "+__ENTITY__)
	#ENDIF
	RETURN 


END CLASS

