FUNCTION __GetFldPos( uField AS USUAL, wFieldCount AS DWORD ) AS DWORD STRICT
	LOCAL dwPos AS DWORD
   //SE-060527
	IF IsNumeric( uField )
		IF uField > 0 .AND. uField <= wFieldCount
			dwPos := uField
		ENDIF

	ELSEIF IsSymbol( uField )
		dwPos := FieldPosSym( uField )

	ELSEIF IsString( uField )
		dwPos := FieldPos( uField )

	ENDIF

   RETURN dwPos

FUNCTION __DBSAPPEND( lRelease AS LOGIC, nTries := 1 AS DWORD ) AS LOGIC STRICT
	LOCAL lOk := FALSE AS LOGIC
   //SE-060527
   DO WHILE nTries > 0
		NetErr( FALSE )
		IF VODBAppend(lRelease)
			lOk := TRUE
			EXIT
		ENDIF
		// Append always retries. Does not check for NetErr()!
		nTries --
   ENDDO     
   IF (! lOk)
		NetErr(TRUE)
   ENDIF
	RETURN lOk

FUNCTION __DBSCommit( nTries := 1 AS DWORD) AS LOGIC STRICT
	LOCAL lOk := FALSE AS LOGIC
   //SE-060527
   DO WHILE nTries > 0
		NetErr( FALSE )
		IF VODBCommit()
			lOk := TRUE
			EXIT
		ENDIF
		IF NetErr( )
			nTries --
		ELSE
			EXIT
		ENDIF
	ENDDO
	RETURN lOk

FUNCTION __DBSDBAPP( cFile, aFields, uCobFor, uCobWhile,  ;
		nNext, nRec, lRest, cDriver, aRDD, aStruct ) AS LOGIC  CLIPPER
	//SE-060601
	LOCAL cobOldErrFunc AS USUAL
	LOCAL oError AS USUAL
	LOCAL lBreak AS LOGIC
	LOCAL dwFrom AS DWORD
	LOCAL dwTo AS DWORD
	LOCAL i AS DWORD
	LOCAL n AS DWORD
	LOCAL lRetCode AS LOGIC
	LOCAL lAnsi AS LOGIC
	LOCAL aRdds AS ARRAY
	LOCAL aMatch AS ARRAY
	LOCAL rddList AS _RDDLIST

	lAnsi  := SetAnsi( )
	dwTo := VODBGetSelect( )

	IF Empty( aStruct := __DBFLEDIT( aStruct, aFields, NULL_ARRAY ) )
		BREAK DbError{ NIL, #AppendDB, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_NOFIELDMATCH ),  ;
			aFields, "aFields" }
	ENDIF

	aRdds := __RddList( cDriver, aRDD )
	rddList := __AllocRddList( aRdds )
	lRetCode := VODBUseArea( TRUE, rddList, cFile, __UniqueAlias( cFile ), TRUE, TRUE )
	#ifndef __VULCAN__  // RDDLIST is a class in Vulcan, not a structure
	   MemFree( RDDLIST )
	#endif   

	IF !lRetCode
		BREAK ErrorBuild( _VODBErrInfoPtr( ) )
	ENDIF

	dwFrom := VODBGetSelect( )
	aFields := { }
	n := FCount( )
	aMatch := DBStruct( )

	FOR i := 1 UPTO n
		AAdd( aFields, FieldName( i ) )
	NEXT

   IF ( ! lAnsi ) .AND. ( __DBSDBINFO( DBI_ISANSI) )
		SetAnsi( TRUE)
	ENDIF

   cobOldErrFunc := ErrorBlock( { | oErr | _Break( oErr ) } )
   BEGIN SEQUENCE
		IF ! Empty( aStruct := __DBFLEDIT( aStruct, aFields, aMatch ) )
			lRetCode := DBTrans( dwTo, aStruct, uCobFor, uCobWhile, nNext, nRec, lRest )
		ENDIF
	RECOVER USING oError
      ErrorBlock(cobOldErrFunc)
      lBreak := TRUE
	END SEQUENCE

	IF ( dwFrom > 0 )
		VODBCloseArea( )
	ENDIF

	VODBSetSelect(LONGINT(dwTo ) )
	SetAnsi( lAnsi )

	IF lBreak
		BREAK oError
	ENDIF

	RETURN lRetCode

FUNCTION __DBSDBAPPDELIM( cFile, cDelim, aFields,	uCobFor, uCobWhile,   ;
		nNext, nRec, lRest, aStruct ) AS LOGIC  CLIPPER
	//SE-060601
	LOCAL cobOldErrFunc 	AS USUAL
	LOCAL oError 			AS USUAL
	LOCAL lBreak 			AS LOGIC
	// LOCAL dwFrom 			AS DWORD   // dcaton 070430 never used
	LOCAL dwTo 				AS DWORD
	// LOCAL siPos 			AS DWORD   // dcaton 070430 never used
	LOCAL lRetCode 		AS LOGIC
	LOCAL lAnsi 			AS LOGIC
	LOCAL lDbfAnsi 		AS LOGIC
	LOCAL rddList 			AS _RDDLIST

	lAnsi := SetAnsi( )
	dwTo := VODBGetSelect( )

	IF Empty( aStruct := __DBFLEDIT( aStruct, aFields, NULL_ARRAY ) )
		BREAK DbError{ NIL, #AppendDelimited, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_NOFIELDMATCH ),  ;
			aFields, "aFields" }
	ENDIF

	IF Empty( cFile )
		BREAK DbError{ NIL, #AppendDelimited, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_NOFILENAME ),  ;
			cFile, "cFile" }
	ENDIF

//	IF Empty( siPos := At(".", cFile ) )   // dcaton 070430 siPos never used
	IF At(".", cFile ) == 0
		cFile := cFile + ".TXT"
	ENDIF

	rddList := __AllocRddList( { "DELIM" } )

	lRetCode := VODBCreate( cFile, aStruct, rddList, TRUE, __UniqueAlias( cFile ), cDelim, TRUE, TRUE )

#ifndef __VULCAN__
	MemFree( RDDLIST )
#endif	

	IF ! lRetCode
		BREAK ErrorBuild( _VODBErrInfoPtr( ) )
	ENDIF

	// dwFrom := VODBGetSelect( )   // dcaton 070430 never used

	lDbfAnsi := __DBSDBINFO( DBI_ISANSI )

	IF ( ! lAnsi .AND. lDbfAnsi )
		SetAnsi( TRUE )
	ENDIF

   cobOldErrFunc := ErrorBlock( { | oErr | _Break( oErr ) } )
	BEGIN SEQUENCE

		lRetCode := DBTrans( dwTo, aStruct, uCobFor, uCobWhile, nNext, nRec, lRest )

	RECOVER USING oError
		ErrorBlock(cobOldErrFunc)
		lBreak := TRUE
	END SEQUENCE
   ErrorBlock(cobOldErrFunc)

	VODBCloseArea( )
	VODBSetSelect(LONGINT(dwTo ) )
	SetAnsi( lAnsi )

	IF lBreak
		BREAK oError
	ENDIF

	RETURN lRetCode

FUNCTION __DBSDBAPPSDF( cFile, aFields, uCobFor, uCobWhile,  ;
		nNext, nRec, lRest, aStruct ) AS LOGIC  CLIPPER
	//SE-060601
	LOCAL cobOldErrFunc AS USUAL
	LOCAL oError AS USUAL
	LOCAL lBreak AS LOGIC
	// LOCAL dwFrom AS DWORD  dcaton 070430 never used
	LOCAL dwTo AS DWORD
	// LOCAL siPos AS DWORD   dcaton 070430 never used
	LOCAL lRetCode AS LOGIC
	LOCAL lAnsi AS LOGIC
	LOCAL lDbfAnsi AS LOGIC
	LOCAL rddList AS _RDDLIST

	lAnsi := SetAnsi( )
	dwTo := VODBGetSelect( )

	IF Empty( aStruct := __DBFLEDIT( DBStruct( ), aFields, NULL_ARRAY ) )
		BREAK DbError{ NIL, #AppendSDF, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_NOFIELDMATCH ),  ;
			aFields, "aFields" }
	ENDIF

	IF Empty( cFile )
		BREAK DbError{ NIL, #AppendSDF, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_NOFILENAME ),  ;
			cFile, "cFile" }
	ENDIF

//	IF Empty( siPos := At( ".", cFile ) )
	IF At( ".", cFile ) == 0
		cFile := cFile + ".TXT"
	ENDIF

	IF ! File( cFile )
		BREAK DbError{ NIL, #AppendSDF, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_BADFILENAME ),  ;
			cFile, "cFile" }
	ENDIF

	lDbfAnsi := __DBSDBINFO( DBI_ISANSI )
	rddList := __AllocRddList( { "SDF" } )
	lRetCode := VODBCreate( cFile, aStruct, rddList, TRUE, __UniqueAlias( cFile ), "", TRUE, TRUE )

#ifndef __VULCAN__
	MemFree( RDDLIST )
#endif	

	IF ! lRetCode
		BREAK ErrorBuild( _VODBErrInfoPtr( ) )
	ENDIF

	// dwFrom := VODBGetSelect( )    // dcaton 070430 never used

	IF ( ! lAnsi .AND. lDbfAnsi )
		SetAnsi( TRUE )
	ENDIF

   cobOldErrFunc := ErrorBlock( { | oErr | _Break( oErr ) } )
	BEGIN SEQUENCE

		lRetCode := DBTrans( dwTo, aStruct, uCobFor, uCobWhile, nNext, nRec, lRest )

	RECOVER USING oError
		ErrorBlock(cobOldErrFunc)
		lBreak := TRUE
	END SEQUENCE
   ErrorBlock(cobOldErrFunc)

	VODBCloseArea( )
	VODBSetSelect(LONGINT(dwTo ) )
	SetAnsi( lAnsi )

	IF lBreak
		BREAK oError
	ENDIF

	RETURN lRetCode

FUNCTION __DBSDBCopy( cFile, aFields, uCobFor,	uCobWhile, nNext, nRec,	lRest,  ;
		cDriver, aRDD, aStruct ) AS LOGIC  CLIPPER
	//SE-060601
	LOCAL cobOldErrFunc AS USUAL
	LOCAL oError AS USUAL
	LOCAL lBreak AS LOGIC
	LOCAL dwFrom AS DWORD
	LOCAL dwTo AS DWORD
	LOCAL lRetCode AS LOGIC
	LOCAL lAnsi AS LOGIC
	LOCAL aRdds AS ARRAY
	LOCAL rddList AS _RDDLIST

	dwFrom := VODBGetSelect( )

	IF  Empty( AFIELDS ) .AND. IsNil( uCobFor ) .AND. IsNil( uCobWhile ) .AND.  ;
		IsNil( nNext ) .AND. IsNil( nRec ) .AND. 	Empty( lRest ) .AND. IsNil( cDriver ) .AND.  ;
		( __DBSDBINFO( DBI_ISANSI ) == SetAnsi( ) ) .AND.  ;
		( __DBSDBINFO( DBI_MEMOHANDLE ) == 0 ) .AND.  ;
		( __DBSDBOrderInfo( DBOI_ORDERCOUNT ) = 0 )

		lRetCode := DBFileCopy( __DBSDBINFO( DBI_FILEHANDLE ), cFile, __DBSDBINFO( DBI_FULLPATH ) )
	ELSE

		lAnsi := SetAnsi( )
		IF Empty( aStruct := __DBFLEDIT( aStruct, aFields, NULL_ARRAY ) )
			BREAK DbError{ NIL, #CopyDB, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_NOFIELDMATCH ),  ;
				aFields, "aFields" }
		ENDIF

		IF IsNil( cDriver )
			cDriver := ""
		ENDIF

		aRdds := __RddList( cDriver, aRDD )
		rddList := __AllocRddList( aRdds )

		lRetCode := VODBCreate( cFile, aStruct, rddList, TRUE, __UniqueAlias( cFile ), "", FALSE, FALSE )

		IF ! lRetCode
		#ifndef __VULCAN__
			MemFree( RDDLIST )
		#endif	
			BREAK ErrorBuild( _VODBErrInfoPtr( ) )
		ENDIF

		IF ( ! lAnsi ) .AND. ( __DBSDBINFO( DBI_ISANSI ) )
			SetAnsi( TRUE )
		ENDIF

		lRetCode := VODBUseArea( TRUE, rddList, cFile, __UniqueAlias( cFile ), ! SetExclusive( ), FALSE )

#ifndef __VULCAN__
		MemFree( RDDLIST )
#endif		

		IF ! lRetCode
			BREAK ErrorBuild( _VODBErrInfoPtr( ) )
		ENDIF

		VODBSelect( dwFrom, @dwTo )

      cobOldErrFunc := ErrorBlock( { | oErr | _Break( oErr ) } )
		BEGIN SEQUENCE

			lRetCode := DBTrans( dwTo, aStruct, uCobFor, uCobWhile, nNext, nRec, lRest )

		RECOVER USING oError
			ErrorBlock(cobOldErrFunc)
			lBreak := TRUE
		END SEQUENCE
	   ErrorBlock(cobOldErrFunc)

		IF ( dwTo > 0 )
			VODBSetSelect(LONGINT(dwTo ) )
			VODBCloseArea( )
		ENDIF

		VODBSetSelect(LONGINT(dwFrom ) )
		SetAnsi( lAnsi )

		IF lBreak
			BREAK oError
		ENDIF
	ENDIF

	RETURN lRetCode

FUNCTION __DBSDBCOPYDELIM( cFile, cDelim, aFields, uCobFor, uCobWhile, nNext,  ;
		nRec, lRest, aStruct ) AS LOGIC  CLIPPER
	//SE-060601
	LOCAL cobOldErrFunc AS USUAL
	LOCAL oError AS USUAL
	LOCAL lBreak AS LOGIC
	LOCAL dwFrom AS DWORD
	LOCAL dwTo AS DWORD
	// LOCAL siPos AS DWORD   dcaton 070430 never used
	LOCAL lRetCode AS LOGIC
	LOCAL lAnsi AS LOGIC
	LOCAL lDbfAnsi AS LOGIC
	LOCAL rddList AS _RDDLIST

	lAnsi  := SetAnsi( )
	dwFrom := VODBGetSelect( )

	IF Empty( aStruct := __DBFLEDIT( aStruct, aFields, NULL_ARRAY ) )
		BREAK DbError{ NIL, #CopyDelimited, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_NOFIELDMATCH ),  ;
			aFields, "aFields" }
	ENDIF

	IF Empty( cFile )
		BREAK DbError{ NIL, #CopyDelimited, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_BADFILENAME ),  ;
			cFile, "cFile" }
	ENDIF
//	IF Empty( siPos := At( ".", cFile ) )  // dcaton 070430 siPos never used
	IF At( ".", cFile ) == 0 
		cFile := cFile + ".TXT"
	ENDIF

	IF IsNil( cDelim )
		cDelim := ""
	ENDIF

	lDbfAnsi := __DBSDBINFO( DBI_ISANSI )
	rddList := __AllocRddList( { "DELIM" } )

	lRetCode := VODBCreate( cFile, aStruct, rddList, TRUE, __UniqueAlias( cFile ), cDelim, TRUE, FALSE )
#ifndef __VULCAN__
	MemFree( RDDLIST )
#endif	

	IF ! lRetCode
		BREAK ErrorBuild( _VODBErrInfoPtr( ) )
	ENDIF

	IF ( ! lAnsi .AND. lDbfAnsi )
		SetAnsi( TRUE )
	ENDIF

	VODBSelect( dwFrom, @dwTo )

	cobOldErrFunc := ErrorBlock( { | oErr | _Break( oErr ) } )
	BEGIN SEQUENCE

		lRetCode := DBTrans( dwTo, aStruct, uCobFor, uCobWhile, nNext, nRec, lRest )

	RECOVER USING oError
		ErrorBlock(cobOldErrFunc)
		lBreak := TRUE
	END SEQUENCE
   ErrorBlock(cobOldErrFunc)

	VODBSetSelect(LONGINT(dwTo ) )
	VODBCloseArea( )
	VODBSetSelect(LONGINT(dwFrom ) )

	SetAnsi( lAnsi )

	IF lBreak
		BREAK oError
	ENDIF

	RETURN lRetCode

FUNCTION __DBSDBCOPYSDF( cFile, aFields, uCobFor, uCobWhile, nNext,  ;
		nRec, lRest, aStruct ) AS LOGIC  CLIPPER
	//SE-060601
	LOCAL cobOldErrFunc AS USUAL
	LOCAL oError AS USUAL
	LOCAL lBreak AS LOGIC
	LOCAL dwFrom AS DWORD
	LOCAL dwTo AS DWORD
	// LOCAL siPos AS DWORD     dcaton 070430 never used
	LOCAL lRetCode AS LOGIC
	LOCAL cAlias AS STRING
	LOCAL lAnsi AS LOGIC
	LOCAL lDbfAnsi AS LOGIC
	LOCAL rddList AS _RDDLIST

	lAnsi := SetAnsi( )
	dwFrom := VODBGetSelect( )

	IF Empty( aStruct := __DBFLEDIT( aStruct, aFields, NULL_ARRAY ) )
		BREAK DbError{ NIL, #CopySDF, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_NOFIELDMATCH ),  ;
			aFields, "aFields" }
	ENDIF

	IF Empty( cFile )
		BREAK DbError{ NIL, #CopySDF, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_BADFILENAME ),  ;
			cFile, "cFile" }
	ENDIF
//	IF Empty( siPos := At( ".", cFile ) )  // dcaton 070430 siPos never used
	IF At( ".", cFile ) == 0 
		cFile := cFile + ".TXT"
	ENDIF

	cAlias := __UniqueAlias( cFile )
	lDbfAnsi := __DBSDBINFO( DBI_ISANSI )

	rddList := __AllocRddList( { "SDF" } )
	lRetCode := VODBCreate( cFile, aStruct, rddList, TRUE, cAlias, "", TRUE, FALSE )

#ifndef __VULCAN__
	MemFree( RDDLIST )
#endif

	IF ! lRetCode
		BREAK ErrorBuild( _VODBErrInfoPtr( ) )
	ENDIF

	IF ( ! lAnsi .AND. lDbfAnsi )
		SetAnsi( TRUE )
	ENDIF

	VODBSelect( dwFrom, @dwTo )

	cobOldErrFunc := ErrorBlock( { | oErr | _Break( oErr ) } )
	BEGIN SEQUENCE

		lRetCode := DBTrans( dwTo, aStruct, uCobFor, uCobWhile, nNext, nRec, lRest )

	RECOVER USING oError
		ErrorBlock(cobOldErrFunc)
		lBreak := TRUE
	END SEQUENCE
   ErrorBlock(cobOldErrFunc)

	VODBSetSelect(LONGINT(dwTo ) )
	VODBCloseArea( )
	VODBSetSelect(LONGINT(dwFrom ) )

	SetAnsi( lAnsi )

	IF lBreak
		BREAK oError
	ENDIF

	RETURN lRetCode
	
FUNCTION __DBSDBINFO( nOrdinal AS DWORD , xNewVal := NIL AS USUAL, nTries := 1 AS DWORD) AS USUAL STRICT
   //SE-060601 
   LOCAL lOk := FALSE AS LOGIC
   DO WHILE nTries > 0
		IF VODBInfo(nOrdinal, @xNewVal)
			lOk := TRUE
			EXIT
		ENDIF
	   nTries--
   ENDDO     
   IF ! lOk
   	BREAK ErrorBuild(_VODBErrInfoPtr())
   ENDIF
	RETURN xNewVal

FUNCTION __DBSDBJOIN( cAlias, cFile, aFields, uCobFor, cRDD ) AS LOGIC  CLIPPER
	LOCAL dwFrom1 AS DWORD
	LOCAL dwFrom2 AS DWORD
	LOCAL dwTo AS DWORD
	LOCAL aStruct AS ARRAY
	LOCAL lRetCode AS LOGIC
	LOCAL rddList AS _RDDLIST
	LOCAL aRdds AS ARRAY
	LOCAL pJoinList AS _JOINLIST

	IF uCobFor == NIL
		RETURN FALSE
	ENDIF

	dwFrom1 := VODBGetSelect( )

	dwFrom2 := SELECT( cAlias )

	IF dwFrom2 = 0
		BREAK DbError{ NIL, #JOIN, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_BADALIAS ), cAlias, "cAlias" }
	ENDIF

	VODBSetSelect(LONGINT(dwFrom1 ) )

	IF Empty( aStruct := __TargetFields( cAlias, aFields, @pJoinList ) )
		BREAK DbError{ NIL, #JOIN, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_NOFIELDMATCH ), aFields, "aFields" }
	ENDIF

	aRdds := __RddList( cRdd )
	rddList := __AllocRddList( aRdds )

	lRetCode := VODBCreate( cFile, aStruct, rddList, TRUE, NULL_STRING, NULL_STRING, TRUE, FALSE )

#ifndef __VULCAN__
	MemFree( RDDLIST )
#endif

	IF ! lRetCode
		BREAK ErrorBuild( _VODBErrInfoPtr( ) )
	ENDIF

	VODBSelect( dwFrom1, @dwTo )

#ifdef __VULCAN__
	pJoinList:uiDestSel := dwTo
#else
	pJoinList.uiDestSel := dwTo
#endif	

	lRetCode := VODBGoTop( )

	DO WHILE ! VODBEof( )
		VODBSetSelect(LONGINT(dwFrom2 ) )
		lRetCode := VODBGoTop( )
		DO WHILE ! VODBEof( )
			VODBSetSelect(LONGINT(dwFrom1 ) )
			IF ( Eval( uCobFor ) )
				VODBJoinAppend( dwTo, pJoinList )
			ENDIF
			VODBSetSelect(LONGINT(dwFrom2 ) )
			VODBSkip( 1 )
		ENDDO
		VODBSetSelect(LONGINT(dwFrom1 ) )
		VODBSkip( 1 )
	ENDDO

#ifndef __VULCAN__
	MemFree( pJoinList )
#endif	

	IF dwTo > 0
		VODBSetSelect(LONGINT(dwTo ) )
		VODBCloseArea( )
	ENDIF

	VODBSetSelect(LONGINT(dwFrom1 ) )

	RETURN lRetCode

FUNCTION __DBSDBOrderInfo( nOrdinal AS DWORD, cBagName := NULL_STRING AS STRING, uOrder:= NIL AS USUAL, ;
	xNewVal := NIL AS USUAL, nTries := 1 AS DWORD) AS USUAL STRICT
	//SE-060527
	LOCAL lKeyVal   AS LOGIC

	IF IsString(uOrder)
		IF Len(uOrder) == 0
			uOrder := NIL
		ENDIF
	ENDIF

	IF nOrdinal == DBOI_KEYVAL
		lKeyVal  := .T. 
		nOrdinal := DBOI_EXPRESSION
	ENDIF

   DO WHILE nTries > 0
		IF VODBOrderInfo(nOrdinal, cBagName, uOrder, @xNewVal)
			EXIT
		ENDIF
	   nTries--
	ENDDO

   IF nTries = 0
   	BREAK ErrorBuild(_VODBErrInfoPtr())
   ENDIF

	IF lKeyVal
		IF IsString(xNewVal)
			IF Len(xNewval) == 0
				xNewVal := NIL
			ELSE
				xNewVal := &(xNewVal)
			ENDIF
		ENDIF
	ENDIF

	RETURN xNewVal
FUNCTION __DBSDBSORT( cFile, aFields, uCobFor, uCobWhile, nNext, nRec, lRest,  ;
		aStruct, cRDD ) AS LOGIC  CLIPPER
	//SE-060601
	LOCAL dwFrom 			AS DWORD
	LOCAL dwTo 				AS DWORD
	LOCAL fnFieldNames 	AS _FIELDNAMES
	LOCAL fnSortNames 	AS _FIELDNAMES
	LOCAL aRdds 			AS ARRAY
	LOCAL rddList 			AS _RDDLIST
	LOCAL lRetCode 		AS LOGIC
	LOCAL oError 			AS USUAL

	IF ! IsLogic( lRest )
		lRest := FALSE
	ENDIF

	dwFrom := VODBGetSelect( )

	fnFieldNames := _allocFieldNames( aStruct )

	IF Empty( AFields )
		BREAK DbError{ NIL, #SORT, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_NOFIELDS ),  ;
			aFields, "aFields" }
	ENDIF

	fnSortNames := __allocNames( AFields )

	aRdds := __RddList( cRDD )
	rddList := __AllocRddList( aRdds )

	lRetCode := VODBCreate( cFile, aStruct, rddList, TRUE, NULL_STRING, NULL_STRING, TRUE, FALSE )

#ifndef __VULCAN__
	MemFree( RDDLIST )
#endif

	IF ! lRetCode
		BREAK ErrorBuild( _VODBErrInfoPtr( ) )
	ENDIF

	VODBSelect( dwFrom, @dwTo )

	lRetCode := VODBSort( dwTo, fnFieldNames, uCobFor, uCobWhile, nNext, nRec, lRest, fnSortNames )
	IF ! lRetCode
		oError := ErrorBuild( _VODBErrInfoPtr( ) )
	ENDIF

	_freeFieldNames( fnFieldNames )
	_freeFieldNames( fnSortNames )

	IF ( dwTo > 0 )
		VODBSetSelect(LONGINT(dwTo ) )
		VODBCloseArea( )
	ENDIF

	VODBSetSelect(LONGINT(dwFrom ) )

	IF ! lRetCode
		BREAK oError
	ENDIF

	RETURN TRUE

FUNCTION __DBSDBTOTAL( cFile, bKey, aFields, uCobFor, uCobWhile, nNext, nRec,  ;
		lRest, aStruct, cRDD ) AS LOGIC  CLIPPER
	//SE-060601
	LOCAL oError AS USUAL
   LOCAL lBreak AS LOGIC
	LOCAL dwFrom AS DWORD
	LOCAL dwTo AS DWORD
	LOCAL i AS DWORD
	LOCAL n AS DWORD
	LOCAL aFldNum AS ARRAY
	LOCAL aNum AS ARRAY
	LOCAL lSomething AS LOGIC
	LOCAL kEval AS USUAL
	LOCAL lRetCode AS LOGIC
	LOCAL fldNames AS _FIELDNAMES
	LOCAL aRdds AS ARRAY
	LOCAL rddList AS _RDDLIST
	LOCAL nCountMemos AS DWORD  //PP-040416 Issue 12643, from PDB

	IF ! lRest
		IF ! VODBGoTop( )
			BREAK ErrorBuild( _VODBErrInfoPtr( ) )
		ENDIF
	ENDIF

	aFldNum := { }

	n := Len( AFields )

	FOR i := 1 UPTO n
		AAdd( aFldNum, FieldPos( AllTrim( AFields[i] ) ) )
	NEXT

	aNum  := ArrayNew( n )

	dwFrom := VODBGetSelect( )

	n := ALen( aStruct )
	FOR i := n DOWNTO 1
		IF aStruct[i, DBS_TYPE] = "M"
			nCountMemos += 1    //PP-040416 Issue 12643, from PDB
			ADel( aStruct, i )
		ENDIF
	NEXT
	//PP-040416 Issue 12643 PDB: resize array with the number of deleted Memo entries
	ASize( aStruct, ALen( aStruct ) - nCountMemos)

	IF ( Empty( aStruct ) )
		BREAK DbError{ NIL, #TOTAL, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_NOFIELDS ),  ;
			aFields, "aFields" }
	ENDIF

	fldNames := _allocFieldNames( aStruct )

	aRdds := __RddList( cRDD )
	rddList := __AllocRddList( aRdds )

	lRetCode := VODBCreate( cFile, aStruct, rddList, TRUE, NULL_STRING, NULL_STRING, TRUE, FALSE )

#ifndef __VULCAN__
	MemFree( RDDLIST )
#endif

	IF ! lRetCode
		BREAK ErrorBuild( _VODBErrInfoPtr( ) )
	ENDIF

	VODBSelect( dwFrom, @dwTo )

	n := Len( aFldNum )

	BEGIN SEQUENCE

		DO WHILE ( ( ! VODBEof( ) ) .AND. nNext != 0 .AND. Eval( uCobWhile ) )
			lSomething := FALSE
			AFill( aNum, 0 )
			kEval := Eval( bKey )
			DO WHILE ( nNext-- != 0 .AND. Eval( uCobWhile ) )
				IF kEval != Eval( bKey )
					EXIT
				ENDIF
				IF ( Eval( uCobFor ) )
					IF ( ! lSomething )
						IF ! (lRetCode := VODBTransRec( dwTo, fldNames ))
							BREAK ErrorBuild( _VODBErrInfoPtr( ) )
						ENDIF
						lSomething := TRUE
					ENDIF
					FOR i := 1 UPTO n
						aNum[i] := aNum[i] + __DBSFieldGet( aFldNum[i] )
					NEXT  // i
				ENDIF
				VODBSkip( 1 )

				IF lSomething
					VODBSetSelect(LONGINT(dwTo ) )
					FOR i := 1 UPTO n
						//PP-040416 Issue 12643 commented next line, put in the one after
						//VODBFieldPut( aFldNum[i], aNum[i] )
						IF ! VODBFieldPut( FieldPos(AFields[i]), aNum[i] ) //SE-060601
							BREAK ErrorBuild( _VODBErrInfoPtr( ) )
						ENDIF
						//FieldPutSym(String2Symbol(aFields[i]), aNum[i])  //  added by PDB
					NEXT  // i
					VODBSetSelect(LONGINT(dwFrom ) )
				ENDIF
			ENDDO

   	ENDDO

	RECOVER USING oError
      lBreak := TRUE
	END SEQUENCE

	_freeFieldNames( fldNames )

	IF ( dwTo > 0 )
		VODBSetSelect(LONGINT(dwTo ) )
		VODBCloseArea( )
	ENDIF

	VODBSetSelect( LONGINT(dwFrom ) )

	IF lBreak
		BREAK oError
	ENDIF

	RETURN lRetCode

FUNCTION __DBSDBUPDATE( cAlias, uCobKey, lRand, bReplace ) AS LOGIC  CLIPPER
	LOCAL dwTo AS DWORD
	LOCAL dwFrom AS DWORD
	LOCAL kEval AS USUAL
	LOCAL lRetCode AS LOGIC

	lRetCode := TRUE

	VODBGoTop( )
	dwTo := VODBGetSelect( )

	dwFrom := SELECT( cAlias )
	VODBGoTop( )

	DO WHILE ! VODBEof( )
		kEval := Eval( uCobKey )
		VODBSetSelect(LONGINT(dwTo ) )
		IF lRand
			VODBSeek( kEval, SetSoftSeek( ), FALSE )
			IF VODBFound( )
				Eval( bReplace )
			ENDIF
      ELSE
			DO WHILE ( Eval( uCobKey ) < kEval .AND. ! VODBEof( ) )
				VODBSkip( 1 )
			ENDDO
			IF ( Eval( uCobKey ) == kEval .AND. ! VODBEof( ) )
				Eval( bReplace )
			ENDIF
		ENDIF

		VODBSetSelect(LONGINT(dwFrom ) )
		VODBSkip( 1 )
	ENDDO

	VODBSetSelect(LONGINT(dwTo ) )

	RETURN lRetCode


FUNCTION __DBSFLock( nTries := 1 AS DWORD) AS LOGIC STRICT
   LOCAL lOk := FALSE AS LOGIC
   //SE-060527
   DO WHILE nTries > 0
		NetErr( FALSE )
		IF VODBFLock()
			lOk := TRUE
			EXIT
		ENDIF
		IF NetErr( )
			nTries --
		ELSE
			EXIT
		ENDIF
	ENDDO

	RETURN lOk
FUNCTION __DBSGoTop( nTries := 1 AS DWORD) AS LOGIC STRICT
   LOCAL lOk := FALSE AS LOGIC
   //SE-060527
   DO WHILE nTries > 0
		NetErr( FALSE )
		IF VODBGoTop()
			lOk := TRUE
			EXIT
		ENDIF
		IF NetErr( )
			nTries --
		ELSE
			EXIT
		ENDIF
	ENDDO
   IF ! lOk
		BREAK ErrorBuild(_VODBErrInfoPtr())
   ENDIF
	RETURN lOk

FUNCTION __DBSGoBottom( nTries := 1 AS DWORD) AS LOGIC STRICT
	LOCAL lOk := FALSE AS LOGIC
   //SE-060527
   DO WHILE nTries > 0
		NetErr( FALSE )
		IF VODBGoBottom()
			lOk := TRUE
			EXIT
		ENDIF
		IF NetErr( )
			nTries --
		ELSE
			EXIT
		ENDIF
	ENDDO

   IF ! lOk
		BREAK ErrorBuild(_VODBErrInfoPtr())
   ENDIF
	RETURN lOk

FUNCTION __DBSOrdListAdd( cBag AS STRING, xOrder AS USUAL, nTries  := 1 AS DWORD) AS LOGIC STRICT
	LOCAL lOk := FALSE AS LOGIC
   //SE-060527
   DO WHILE nTries > 0
		NetErr( FALSE )
		IF VODBOrdListAdd(cBag, xOrder)
			lOk := TRUE
			EXIT
		ENDIF
		IF NetErr( )
			nTries --
		ELSE
			EXIT
		ENDIF
	ENDDO
   IF ! lOk
		BREAK ErrorBuild(_VODBErrInfoPtr())
   ENDIF
	RETURN lOk

FUNCTION __DBSOrdListClear( cBag AS STRING, xOrder AS USUAL, nTries := 1 AS DWORD ) AS LOGIC STRICT
	LOCAL lOk := FALSE AS LOGIC
	//SE-060527
   DO WHILE nTries > 0
		NetErr( FALSE )
		IF VODBOrdListClear(cBag, xOrder)
			lOk := TRUE
			EXIT
		ENDIF
		IF NetErr( )
			nTries --
		ELSE
			EXIT
		ENDIF
	ENDDO
   IF ! lOk
		BREAK ErrorBuild(_VODBErrInfoPtr())
   ENDIF
	RETURN lOk

FUNCTION __DBSRLock( n AS USUAL, nTries := 1 AS DWORD) AS LOGIC STRICT
	 LOCAL lOk := FALSE AS LOGIC
	//PP-040416 Issue 12766 First parameter must be usual
	//SE-060527
   DO WHILE nTries > 0
		NetErr( FALSE )
		IF VODBRLock(n)
			lOk := TRUE
			EXIT
		ENDIF
		IF NetErr( )
			nTries --
		ELSE
			EXIT
		ENDIF
	ENDDO

	RETURN lOk

FUNCTION __DBSSeek( xValue AS USUAL, lSoft AS USUAL, lLast AS USUAL, nTries  := 1 AS DWORD) AS LOGIC STRICT
   //SE-060527
   //RvdH 080611 Changed lLast from USUAL to LOGIC and changed the way 
   //            in which lLast is passed to the RDD.
	LOCAL lRet  AS LOGIC

	DEFAULT(@lSoft, SetSoftSeek())
	DEFAULT(@lLast, FALSE)
	DEFAULT(@xValue, "")

	DO WHILE nTries > 0
		NetErr( FALSE )
		IF VODBSeek(xValue, lSoft, lLast)
			EXIT
		ENDIF
		IF NetErr( )
			nTries --
		ELSE
			EXIT
		ENDIF
	ENDDO

	IF nTries = 0
		BREAK ErrorBuild(_VODBErrInfoPtr())
	ENDIF

	lRet := VODBFound()

	RETURN lRet

FUNCTION __DBSSetSelect(dwNew AS DWORD) AS DWORD STRICT
   //SE-060527
   IF __glRestoreWorkarea
      RETURN VODBSetSelect(LONGINT(dwNew))
   ENDIF
   RETURN dwNew

STATIC GLOBAL __glRestoreWorkarea := FALSE AS LOGIC //SE-060527

FUNCTION __DBSFieldGet( wPos AS DWORD ) AS USUAL
	LOCAL xRetVal AS USUAL

	IF ! VODBFieldGet( wPos, @xRetVal )
		BREAK ErrorBuild( _VODBErrInfoPtr( ) )
	ENDIF

	RETURN xRetVal

FUNCTION __IsBlob( nField AS INT ) AS LOGIC
	LOCAL lRetCode AS LOGIC
	LOCAL uVal AS USUAL

		IF VODBFieldInfo( DBS_BLOB_TYPE, DWORD(nField), @uVal )
			lRetCode := ! IsNil( uVal )
		ENDIF

	RETURN lRetCode

FUNCTION __IterateForFieldAssign( acbExpression AS ARRAY, aFields AS ARRAY ) AS VOID
	LOCAL n AS DWORD
	LOCAL nLen AS DWORD
	LOCAL xValue AS USUAL
	LOCAL nPos AS DWORD

	nLen := ALen( acbExpression )
	FOR n := 1 UPTO nLen
		nPos := FieldPosSym( AFIELDS[n] )

		IF __CanEval( acbExpression[n] )
			xValue := Eval( acbExpression[n] )
		ELSE
			xValue := acbExpression[n]
		ENDIF

		IF ! VODBFieldPut( nPos, xValue )
			BREAK ErrorBuild( _VODBErrInfoPtr( ))
		ENDIF
	NEXT
	RETURN
FUNCTION __IterateForSum( acbExpression AS ARRAY, aResults AS ARRAY ) AS VOID
	LOCAL wLen AS DWORD
	LOCAL w AS DWORD

	wLen := ALen( acbExpression )
	FOR w := 1 UPTO wLen
		aResults[w] += Eval( acbExpression[w] )
	NEXT
	RETURN

FUNCTION __MakeErrObj( nTries ) AS USUAL  CLIPPER
	LOCAL oError AS OBJECT

	oError := ErrorBuild( _VODBErrInfoPtr( ) )

	IF ! IsNil( nTries )
		((Error)oError):Tries := nTries
	ENDIF

	RETURN oError     
	
FUNCTION DBFDebug( p1 := NULL_STRING AS STRING, p2 := NULL_STRING AS STRING, ;
	p3 := NULL_STRING AS STRING ,p4 := NULL_STRING AS STRING ) AS LOGIC
/*		
	LOCAL cMsg  AS STRING

	IF SLen( p1 ) > 0
		cMsg := p1
	ENDIF
	IF SLen( p2 ) > 0
		cMsg += ", " + p2
	ENDIF
	IF SLen( p3 ) > 0
		cMsg += ", " + p3
	ENDIF
	IF SLen( p4 ) > 0
		cMsg += ", " + p4
	ENDIF
	IF SLen(cMsg) > 250
		cMsg := Left(cMsg, 250)+"..."
	ENDIF
	cMsg += _CHR( 13 )
	_DebOut32( String2Psz(cMsg )) 
*/
	RETURN TRUE

FUNCTION DbSetRestoreWorkarea(lEnable := NIL AS USUAL) AS LOGIC STRICT
	//SE-060527
   LOCAL lOldValue AS LOGIC

   lOldValue := __glRestoreWorkarea

   IF IsLogic(lEnable)
      __glRestoreWorkarea := lEnable
   ENDIF

   RETURN lOldValue

FUNCTION __DBSErrorBlock() AS USUAL CLIPPER
   RETURN ErrorBlock( { | oErr | _Break( oErr ) } )

//  UH 11/12/2000
FUNCTION __ConstructUniqueAlias ( cFileName AS STRING ) AS SYMBOL STRICT
	LOCAL cTryNewAlias AS STRING
	LOCAL w AS DWORD
	IF SLen(cFileName) == 0
		cFileName := "NOFILENAME"
	ELSEIF ( w := At( ".", cFileName ) )>0
		cFileName := SubStr( cFileName, 1, w-1 )
	ENDIF
	w := 1
	cTryNewAlias := Upper(cFileName)
	DO WHILE VODBSymSelect( String2Symbol( cTryNewAlias ) )>0
		cTryNewAlias := Upper(cFileName)+"_"+AllTrim( Str( w++ ) )
	ENDDO

	RETURN String2Symbol( cTryNewAlias )
#ifndef __VULCAN__
_DLL FUNCTION VODBGetSelect ()              AS DWORD PASCAL:VO28RUN.VODBGetSelect 
_DLL FUNCTION VODBSetSelect (siNew AS INT)  AS DWORD PASCAL:VO28RUN.VODBSetSelect
#endif
FUNCTION __DBSGoTo( n AS LONGINT, nTries := 1 AS DWORD) AS LOGIC STRICT
   LOCAL lOk := FALSE AS LOGIC
   //SE-060527
   DO WHILE nTries > 0
		NetErr( FALSE )
		IF VODBGoTo(n)
			lOk := TRUE
			EXIT
		ENDIF
		IF NetErr( )
			nTries --
		ELSE
			EXIT
		ENDIF
	ENDDO

   IF ! lOk
		BREAK ErrorBuild(_VODBErrInfoPtr())
   ENDIF
	RETURN lOk

FUNCTION __DBSSkip( n AS LONGINT, nTries := 1 AS DWORD) AS LOGIC STRICT
	LOCAL lOk := FALSE AS LOGIC
   //SE-060527
   DO WHILE nTries > 0
		NetErr( FALSE )
		IF VODBSkip(n)
			lOk := TRUE
			EXIT
		ENDIF
		IF NetErr( )
			nTries --
		ELSE
			EXIT
		ENDIF
	ENDDO

   IF ! lOk
		BREAK ErrorBuild(_VODBErrInfoPtr())
   ENDIF
	RETURN lOk


FUNCTION __CheckFieldType(uValue REF USUAL, aField AS ARRAY, uError REF USUAL) AS LOGIC  PASCAL
    //SE-080609 Type checking for DBServer:Fieldput() and NoIvarGet() 
    LOCAL dwType AS DWORD
    LOCAL cType  AS STRING
    LOCAL lOK    AS LOGIC
    
    dwType := Asc(aField[DBS_TYPE]) 
    SWITCH dwType
    CASE 67 // "C"
        lOK := IsString(uValue)
    CASE 68 // "D"
        lOK := IsDate(uValue)
    CASE 78 // "N"
        lOK := IsNumeric(uValue)
    CASE 76 // "L"
        lOK := IsLogic(uValue) 
    CASE 77 // "M"
        lOK := TRUE
    END SWITCH
    IF lOK
        IF dwType = 67 // "C"
            IF SLen(uValue) != aField[DBS_LEN]
                uValue := PadR(uValue, aField[DBS_LEN]) 
            ENDIF
        ELSEIF dwType = 78 // "N"
            IF InStr("*", Str3(uValue, aField[DBS_LEN], aField[DBS_DEC]))
                lOk     := FALSE   
                uError  := { EG_DATAWIDTH, __CAVOSTR_DBFCLASS_INVALIDLENGTH, NTrim(aField[DBS_LEN]) + "(decimals "+NTrim(aField[DBS_DEC]) + ")"}
                
            ENDIF
        ENDIF 
    ELSE             
        SWITCH dwType
        CASE 67 // "C"
            cType := "STRING"
        CASE 68 // "D"
            cType := "DATE"
        CASE 78 // "N"
            cType := "NUMERIC"
        CASE 76 // "L"
            cType := "LOGIC"
        END SWITCH
        uError := {EG_DATATYPE, __CAVOSTR_DBFCLASS_INVALIDTYPE, cType}
    ENDIF
    
    RETURN lOk
//RvdH 080613 Allow control over default Setting for Lockmode  
STATIC GLOBAL sgLockMode := ccOptimistic AS DWORD
FUNCTION DbSetDefaultLockMode(dwLockMode AS DWORD) AS DWORD PASCAL
    LOCAL dwOld AS DWORD
    dwOld := sgLockMode
    sgLockMode := dwLockMode
    RETURN dwOld
FUNCTION DbGetDefaultLockMode() AS DWORD PASCAL
    RETURN sgLockMode

