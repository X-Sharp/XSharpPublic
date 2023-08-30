//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


#pragma warnings(165, off)
/// <exclude/>
FUNCTION __GetFldPos( uField AS USUAL, wFieldCount AS DWORD ) AS DWORD STRICT
	LOCAL dwPos := 0 AS DWORD
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


/// <exclude/>
FUNCTION __DBSAPPEND( lRelease AS LOGIC, nTries := 1 AS DWORD ) AS LOGIC STRICT
	LOCAL lOk := FALSE AS LOGIC
   DO WHILE nTries > 0
		NetErr( FALSE )
		IF VoDbAppend(lRelease)
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


/// <exclude/>
FUNCTION __DBSCommit( nTries := 1 AS DWORD) AS LOGIC STRICT
	LOCAL lOk := FALSE AS LOGIC
   DO WHILE nTries > 0
		NetErr( FALSE )
		IF VoDbCommit()
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


/// <exclude/>
FUNCTION __DBSDBAPP( cFile, aFields, uCobFor, uCobWhile,  nNext, nRec, lRest, cDriver, aRDD, aStruct ) AS LOGIC  CLIPPER
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
	LOCAL rddList AS _RddList


	lAnsi  := SetAnsi( )
	dwTo := VoDbGetSelect( )


	IF Empty( aStruct := __DBFLEDIT( aStruct, aFields, NULL_ARRAY ) )
		BREAK DbError{ NIL, #AppendDB, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_NOFIELDMATCH ),  ;
			aFields, "aFields" }
	ENDIF


	aRdds := __RDDList( cDriver, aRDD )
	rddList := __AllocRddList( aRdds )
	lRetCode := VoDbUseArea( TRUE, rddList, cFile, __UniqueAlias( cFile ), TRUE, TRUE )




	IF !lRetCode
		BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
	ENDIF


	dwFrom := VoDbGetSelect( )
	aFields := { }
	n := FCount( )
	aMatch := DbStruct( )


	FOR i := 1 UPTO n
		AAdd( aFields, FieldName( i ) )
	NEXT


   IF ( ! lAnsi ) .AND. ( __DBSDBINFO( DBI_ISANSI) )
		SetAnsi( TRUE)
	ENDIF


   cobOldErrFunc := ErrorBlock( { | oErr | _Break( oErr ) } )
   BEGIN SEQUENCE
		IF ! Empty( aStruct := __DBFLEDIT( aStruct, aFields, aMatch ) )
			lRetCode := DbTrans( dwTo, aStruct, uCobFor, uCobWhile, nNext, nRec, lRest )
		ENDIF
	RECOVER USING oError
      ErrorBlock(cobOldErrFunc)
      lBreak := TRUE
	END SEQUENCE


	IF ( dwFrom > 0 )
		VoDbCloseArea( )
	ENDIF


	VoDbSetSelect(LONGINT(dwTo ) )
	SetAnsi( lAnsi )


	IF lBreak
		BREAK oError
	ENDIF


	RETURN lRetCode


/// <exclude/>
FUNCTION __DBSDBAPPDELIM( cFile, cDelim, aFields,	uCobFor, uCobWhile,   ;
		nNext, nRec, lRest, aStruct ) AS LOGIC  CLIPPER
	LOCAL cobOldErrFunc 	AS USUAL
	LOCAL oError 			AS USUAL
	LOCAL lBreak 			AS LOGIC
	LOCAL dwTo 				AS DWORD
	LOCAL lRetCode 		AS LOGIC
	LOCAL lAnsi 			AS LOGIC
	LOCAL lDbfAnsi 		AS LOGIC
	LOCAL rddList 			AS _RddList


	lAnsi := SetAnsi( )
	dwTo := VoDbGetSelect( )


	IF Empty( aStruct := __DBFLEDIT( aStruct, aFields, NULL_ARRAY ) )
		BREAK DbError{ NIL, #AppendDelimited, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_NOFIELDMATCH ),  ;
			aFields, "aFields" }
	ENDIF


	IF Empty( cFile )
		BREAK DbError{ NIL, #AppendDelimited, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_NOFILENAME ),  ;
			cFile, "cFile" }
	ENDIF


	IF At(".", cFile ) == 0
		cFile := cFile + ".TXT"
	ENDIF


	rddList := __AllocRddList( { "DELIM" } )


	lRetCode := VoDbCreate( cFile, aStruct, rddList, TRUE, __UniqueAlias( cFile ), cDelim, TRUE, TRUE )




	IF ! lRetCode
		BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
	ENDIF


	lDbfAnsi := __DBSDBINFO( DBI_ISANSI )


	IF ( ! lAnsi .AND. lDbfAnsi )
		SetAnsi( TRUE )
	ENDIF


   cobOldErrFunc := ErrorBlock( { | oErr | _Break( oErr ) } )
	BEGIN SEQUENCE


		lRetCode := DbTrans( dwTo, aStruct, uCobFor, uCobWhile, nNext, nRec, lRest )


	RECOVER USING oError
		ErrorBlock(cobOldErrFunc)
		lBreak := TRUE
	END SEQUENCE
   ErrorBlock(cobOldErrFunc)


	VoDbCloseArea( )
	VoDbSetSelect(LONGINT(dwTo ) )
	SetAnsi( lAnsi )


	IF lBreak
		BREAK oError
	ENDIF


	RETURN lRetCode


/// <exclude/>
FUNCTION __DBSDBAPPSDF( cFile, aFields, uCobFor, uCobWhile,  nNext, nRec, lRest, aStruct ) AS LOGIC  CLIPPER
	LOCAL cobOldErrFunc AS USUAL
	LOCAL oError AS USUAL
	LOCAL lBreak := FALSE  AS LOGIC
	LOCAL dwTo AS DWORD
	LOCAL lRetCode AS LOGIC
	LOCAL lAnsi AS LOGIC
	LOCAL lDbfAnsi AS LOGIC
	LOCAL rddList AS _RddList


	lAnsi := SetAnsi( )
	dwTo := VoDbGetSelect( )


	IF Empty( aStruct := __DBFLEDIT( DbStruct( ), aFields, NULL_ARRAY ) )
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
	lRetCode := VoDbCreate( cFile, aStruct, rddList, TRUE, __UniqueAlias( cFile ), "", TRUE, TRUE )


	IF ! lRetCode
		BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
	ENDIF


	IF ( ! lAnsi .AND. lDbfAnsi )
		SetAnsi( TRUE )
	ENDIF


    cobOldErrFunc := ErrorBlock( { | oErr | _Break( oErr ) } )
	BEGIN SEQUENCE


		lRetCode := DbTrans( dwTo, aStruct, uCobFor, uCobWhile, nNext, nRec, lRest )


	RECOVER USING oError
		ErrorBlock(cobOldErrFunc)
		lBreak := TRUE
	END SEQUENCE
   ErrorBlock(cobOldErrFunc)


	VoDbCloseArea( )
	VoDbSetSelect(LONGINT(dwTo ) )
	SetAnsi( lAnsi )


	IF lBreak
		BREAK oError
	ENDIF


	RETURN lRetCode


/// <exclude/>
FUNCTION __DBSDBCopy( cFile, aFields, uCobFor,	uCobWhile, nNext, nRec,	lRest,  cDriver, aRDD, aStruct ) AS LOGIC  CLIPPER
	LOCAL cobOldErrFunc AS USUAL
	LOCAL oError AS USUAL
	LOCAL lBreak  := FALSE AS LOGIC
	LOCAL dwFrom AS DWORD
	LOCAL dwTo AS DWORD
	LOCAL lRetCode AS LOGIC
	LOCAL lAnsi AS LOGIC
	LOCAL aRdds AS ARRAY
	LOCAL rddList AS _RddList


	dwFrom := VoDbGetSelect( )


	IF  Empty( aFields ) .AND. IsNil( uCobFor ) .AND. IsNil( uCobWhile ) .AND.  ;
		IsNil( nNext ) .AND. IsNil( nRec ) .AND. 	Empty( lRest ) .AND. IsNil( cDriver ) .AND.  ;
		( __DBSDBINFO( DBI_ISANSI ) == SetAnsi( ) ) .AND.  ;
		( __DBSDBINFO( DBI_MEMOHANDLE ) == 0 ) .AND.  ;
		( __DBSDbOrderInfo( DBOI_ORDERCOUNT ) = 0 )


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


		aRdds := __RDDList( cDriver, aRDD )
		rddList := __AllocRddList( aRdds )


		lRetCode := VoDbCreate( cFile, aStruct, rddList, TRUE, __UniqueAlias( cFile ), "", FALSE, FALSE )


		IF ! lRetCode
			BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
		ENDIF


		IF ( ! lAnsi ) .AND. ( __DBSDBINFO( DBI_ISANSI ) )
			SetAnsi( TRUE )
		ENDIF


		lRetCode := VoDbUseArea( TRUE, rddList, cFile, __UniqueAlias( cFile ), ! SetExclusive( ), FALSE )


		IF ! lRetCode
			BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
		ENDIF


		VoDbSelect( dwFrom, OUT dwTo )


      cobOldErrFunc := ErrorBlock( { | oErr | _Break( oErr ) } )
		BEGIN SEQUENCE


			lRetCode := DbTrans( dwTo, aStruct, uCobFor, uCobWhile, nNext, nRec, lRest )


		RECOVER USING oError
			ErrorBlock(cobOldErrFunc)
			lBreak := TRUE
		END SEQUENCE
	   ErrorBlock(cobOldErrFunc)


		IF ( dwTo > 0 )
			VoDbSetSelect(LONGINT(dwTo ) )
			VoDbCloseArea( )
		ENDIF


		VoDbSetSelect(LONGINT(dwFrom ) )
		SetAnsi( lAnsi )


		IF lBreak
			BREAK oError
		ENDIF
	ENDIF


	RETURN lRetCode


/// <exclude/>
FUNCTION __DBSDBCOPYDELIM( cFile, cDelim, aFields, uCobFor, uCobWhile, nNext,  nRec, lRest, aStruct ) AS LOGIC  CLIPPER
	LOCAL cobOldErrFunc AS USUAL
	LOCAL oError AS USUAL
	LOCAL lBreak := FALSE AS LOGIC
	LOCAL dwFrom AS DWORD
	LOCAL dwTo AS DWORD
	LOCAL lRetCode AS LOGIC
	LOCAL lAnsi AS LOGIC
	LOCAL lDbfAnsi AS LOGIC
	LOCAL rddList AS _RddList


	lAnsi  := SetAnsi( )
	dwFrom := VoDbGetSelect( )


	IF Empty( aStruct := __DBFLEDIT( aStruct, aFields, NULL_ARRAY ) )
		BREAK DbError{ NIL, #CopyDelimited, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_NOFIELDMATCH ),  ;
			aFields, "aFields" }
	ENDIF


	IF Empty( cFile )
		BREAK DbError{ NIL, #CopyDelimited, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_BADFILENAME ),  ;
			cFile, "cFile" }
	ENDIF
	IF At( ".", cFile ) == 0
		cFile := cFile + ".TXT"
	ENDIF


	IF IsNil( cDelim )
		cDelim := ""
	ENDIF


	lDbfAnsi := __DBSDBINFO( DBI_ISANSI )
	rddList := __AllocRddList( { "DELIM" } )


	lRetCode := VoDbCreate( cFile, aStruct, rddList, TRUE, __UniqueAlias( cFile ), cDelim, TRUE, FALSE )


	IF ! lRetCode
		BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
	ENDIF


	IF ( ! lAnsi .AND. lDbfAnsi )
		SetAnsi( TRUE )
	ENDIF


	VoDbSelect( dwFrom, OUT dwTo )


	cobOldErrFunc := ErrorBlock( { | oErr | _Break( oErr ) } )
	BEGIN SEQUENCE


		lRetCode := DbTrans( dwTo, aStruct, uCobFor, uCobWhile, nNext, nRec, lRest )


	RECOVER USING oError
		ErrorBlock(cobOldErrFunc)
		lBreak := TRUE
	END SEQUENCE
   ErrorBlock(cobOldErrFunc)


	VoDbSetSelect(LONGINT(dwTo ) )
	VoDbCloseArea( )
	VoDbSetSelect(LONGINT(dwFrom ) )


	SetAnsi( lAnsi )


	IF lBreak
		BREAK oError
	ENDIF


	RETURN lRetCode


/// <exclude/>
FUNCTION __DBSDBCOPYSDF( cFile, aFields, uCobFor, uCobWhile, nNext, nRec, lRest, aStruct ) AS LOGIC  CLIPPER
	LOCAL cobOldErrFunc AS USUAL
	LOCAL oError AS USUAL
	LOCAL lBreak  := FALSE AS LOGIC
	LOCAL dwFrom AS DWORD
	LOCAL dwTo AS DWORD
	LOCAL lRetCode AS LOGIC
	LOCAL cAlias AS STRING
	LOCAL lAnsi AS LOGIC
	LOCAL lDbfAnsi AS LOGIC
	LOCAL rddList AS _RddList


	lAnsi := SetAnsi( )
	dwFrom := VoDbGetSelect( )


	IF Empty( aStruct := __DBFLEDIT( aStruct, aFields, NULL_ARRAY ) )
		BREAK DbError{ NIL, #CopySDF, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_NOFIELDMATCH ),  ;
			aFields, "aFields" }
	ENDIF


	IF Empty( cFile )
		BREAK DbError{ NIL, #CopySDF, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_BADFILENAME ),  ;
			cFile, "cFile" }
	ENDIF
	IF At( ".", cFile ) == 0
		cFile := cFile + ".TXT"
	ENDIF


	cAlias := __UniqueAlias( cFile )
	lDbfAnsi := __DBSDBINFO( DBI_ISANSI )


	rddList := __AllocRddList( { "SDF" } )
	lRetCode := VoDbCreate( cFile, aStruct, rddList, TRUE, cAlias, "", TRUE, FALSE )


	IF ! lRetCode
		BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
	ENDIF


	IF ( ! lAnsi .AND. lDbfAnsi )
		SetAnsi( TRUE )
	ENDIF


	VoDbSelect( dwFrom, OUT dwTo )


	cobOldErrFunc := ErrorBlock( { | oErr | _Break( oErr ) } )
	BEGIN SEQUENCE


		lRetCode := DbTrans( dwTo, aStruct, uCobFor, uCobWhile, nNext, nRec, lRest )


	RECOVER USING oError
		ErrorBlock(cobOldErrFunc)
		lBreak := TRUE
	END SEQUENCE
   ErrorBlock(cobOldErrFunc)


	VoDbSetSelect(LONGINT(dwTo ) )
	VoDbCloseArea( )
	VoDbSetSelect(LONGINT(dwFrom ) )


	SetAnsi( lAnsi )


	IF lBreak
		BREAK oError
	ENDIF


	RETURN lRetCode


/// <exclude/>
FUNCTION __DBSDBINFO( nOrdinal AS DWORD , xNewVal := NIL AS USUAL, nTries := 1 AS DWORD) AS USUAL STRICT
   LOCAL lOk := FALSE AS LOGIC
   DO WHILE nTries > 0
		IF VoDbInfo(nOrdinal, REF xNewVal)
			lOk := TRUE
			EXIT
		ENDIF
	   nTries--
   ENDDO
   IF ! lOk
   	BREAK ErrorBuild(_VoDbErrInfoPtr())
   ENDIF
	RETURN xNewVal


/// <exclude/>
FUNCTION __DBSDBJOIN( cAlias, cFile, aFields, uCobFor, cRDD ) AS LOGIC  CLIPPER
	LOCAL dwFrom1 AS DWORD
	LOCAL dwFrom2 AS DWORD
	LOCAL dwTo AS DWORD
	LOCAL aStruct AS ARRAY
	LOCAL lRetCode AS LOGIC
	LOCAL rddList AS _RddList
	LOCAL aRdds AS ARRAY


	IF uCobFor == NIL
		RETURN FALSE
	ENDIF


	dwFrom1 := VoDbGetSelect( )


	dwFrom2 := @@Select( cAlias )


	IF dwFrom2 = 0
		BREAK DbError{ NIL, #JOIN, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_BADALIAS ), cAlias, "cAlias" }
	ENDIF


	VoDbSetSelect(LONGINT(dwFrom1 ) )

    LOCAL pJoinList AS _JoinList
	IF Empty( aStruct := __TargetFields( cAlias, aFields, OUT pJoinList ) )
		BREAK DbError{ NIL, #JOIN, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_NOFIELDMATCH ), aFields, "aFields" }
	ENDIF


	aRdds := __RDDList( cRDD )
	rddList := __AllocRddList( aRdds )


	lRetCode := VoDbCreate( cFile, aStruct, rddList, TRUE, NULL_STRING, NULL_STRING, TRUE, FALSE )


	IF ! lRetCode
		BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
	ENDIF


	VoDbSelect( dwFrom1, OUT dwTo )


	pJoinList:uiDestSel := dwTo


	lRetCode := VoDbGoTop( )


	DO WHILE ! VoDbEof( )
		VoDbSetSelect(LONGINT(dwFrom2 ) )
		lRetCode := VoDbGoTop( )
		DO WHILE ! VoDbEof( )
			VoDbSetSelect(LONGINT(dwFrom1 ) )
			IF ( Eval( uCobFor ) )
				VoDbJoinAppend( dwTo, pJoinList )
			ENDIF
			VoDbSetSelect(LONGINT(dwFrom2 ) )
			VoDbSkip( 1 )
		ENDDO
		VoDbSetSelect(LONGINT(dwFrom1 ) )
		VoDbSkip( 1 )
	ENDDO


	IF dwTo > 0
		VoDbSetSelect(LONGINT(dwTo ) )
		VoDbCloseArea( )
	ENDIF


	VoDbSetSelect(LONGINT(dwFrom1 ) )


	RETURN lRetCode


/// <exclude/>
FUNCTION __DBSDbOrderInfo( nOrdinal AS DWORD, cBagName := NULL_STRING AS STRING, uOrder:= NIL AS USUAL, xNewVal := NIL AS USUAL, nTries := 1 AS DWORD) AS USUAL STRICT
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
		IF VoDbOrderInfo(nOrdinal, cBagName, uOrder, REF xNewVal)
			EXIT
		ENDIF
	   nTries--
	ENDDO


   IF nTries = 0
   	BREAK ErrorBuild(_VoDbErrInfoPtr())
   ENDIF


	IF lKeyVal
		IF IsString(xNewVal)
			IF Len(xNewVal) == 0
				xNewVal := NIL
			ELSE
				xNewVal := &(xNewVal)
			ENDIF
		ENDIF
	ENDIF


	RETURN xNewVal


/// <exclude/>
FUNCTION __DBSDBSORT( cFile, aFields, uCobFor, uCobWhile, nNext, nRec, lRest, aStruct, cRDD ) AS LOGIC  CLIPPER
	LOCAL dwFrom 			AS DWORD
	LOCAL dwTo 				AS DWORD
	LOCAL fnFieldNames 	AS _FieldNames
	LOCAL fnSortNames 	AS _FieldNames
	LOCAL aRdds 			AS ARRAY
	LOCAL rddList 			AS _RddList
	LOCAL lRetCode 		AS LOGIC
	LOCAL oError 			AS USUAL


	IF ! IsLogic( lRest )
		lRest := FALSE
	ENDIF


	dwFrom := VoDbGetSelect( )


	fnFieldNames := _AllocFieldNames( aStruct )


	IF Empty( aFields )
		BREAK DbError{ NIL, #SORT, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_NOFIELDS ),  ;
			aFields, "aFields" }
	ENDIF


	fnSortNames := __allocNames( aFields )


	aRdds := __RDDList( cRDD )
	rddList := __AllocRddList( aRdds )


	lRetCode := VoDbCreate( cFile, aStruct, rddList, TRUE, NULL_STRING, NULL_STRING, TRUE, FALSE )


	IF ! lRetCode
		BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
	ENDIF


	VoDbSelect( dwFrom, OUT dwTo )


	lRetCode := VoDbSort( dwTo, fnFieldNames, uCobFor, uCobWhile, nNext, nRec, lRest, fnSortNames )
	IF ! lRetCode
		oError := ErrorBuild( _VoDbErrInfoPtr( ) )
	ENDIF


	_FreeFieldNames( fnFieldNames )
	_FreeFieldNames( fnSortNames )


	IF ( dwTo > 0 )
		VoDbSetSelect(LONGINT(dwTo ) )
		VoDbCloseArea( )
	ENDIF


	VoDbSetSelect(LONGINT(dwFrom ) )


	IF ! lRetCode
		BREAK oError
	ENDIF


	RETURN TRUE


/// <exclude/>
FUNCTION __DBSDBTOTAL( cFile, bKey, aFields, uCobFor, uCobWhile, nNext, nRec, lRest, aStruct, cRDD ) AS LOGIC  CLIPPER
	LOCAL oError AS USUAL
   LOCAL lBreak  := FALSE AS LOGIC
	LOCAL dwFrom AS DWORD
	LOCAL dwTo AS DWORD
	LOCAL i AS DWORD
	LOCAL n AS DWORD
	LOCAL aFldNum AS ARRAY
	LOCAL aNum AS ARRAY
	LOCAL lSomething AS LOGIC
	LOCAL kEval AS USUAL
	LOCAL lRetCode AS LOGIC
	LOCAL fldNames AS _FieldNames
	LOCAL aRdds AS ARRAY
	LOCAL rddList AS _RddList
	LOCAL nCountMemos AS DWORD


	IF ! lRest
		IF ! VoDbGoTop( )
			BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
		ENDIF
	ENDIF


	aFldNum := { }


	n := Len( aFields )


	FOR i := 1 UPTO n
		AAdd( aFldNum, FieldPos( AllTrim( aFields[i] ) ) )
	NEXT


	aNum  := ArrayNew( n )


	dwFrom := VoDbGetSelect( )


	n := ALen( aStruct )
	FOR i := n DOWNTO 1
		IF aStruct[i, DBS_TYPE] = "M"
			nCountMemos += 1
			ADel( aStruct, i )
		ENDIF
	NEXT
	ASize( aStruct, ALen( aStruct ) - nCountMemos)


	IF ( Empty( aStruct ) )
		BREAK DbError{ NIL, #TOTAL, EG_ARG, __CavoStr( __CAVOSTR_DBFCLASS_NOFIELDS ),  ;
			aFields, "aFields" }
	ENDIF


	fldNames := _AllocFieldNames( aStruct )


	aRdds := __RDDList( cRDD )
	rddList := __AllocRddList( aRdds )


	lRetCode := VoDbCreate( cFile, aStruct, rddList, TRUE, NULL_STRING, NULL_STRING, TRUE, FALSE )


	IF ! lRetCode
		BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
	ENDIF


	VoDbSelect( dwFrom, OUT dwTo )


	n := Len( aFldNum )


	BEGIN SEQUENCE


		DO WHILE ( ( ! VoDbEof( ) ) .AND. nNext != 0 .AND. Eval( uCobWhile ) )
			lSomething := FALSE
			AFill( aNum, 0 )
			kEval := Eval( bKey )
			DO WHILE ( nNext-- != 0 .AND. Eval( uCobWhile ) )
				IF kEval != Eval( bKey )
					EXIT
				ENDIF
				IF ( Eval( uCobFor ) )
					IF ( ! lSomething )
						IF ! (lRetCode := VoDbTransRec( dwTo, fldNames ))
							BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
						ENDIF
						lSomething := TRUE
					ENDIF
					FOR i := 1 UPTO n
						aNum[i] := aNum[i] + __DBSFieldGet( aFldNum[i] )
					NEXT  // i
				ENDIF
				VoDbSkip( 1 )


				IF lSomething
					VoDbSetSelect(LONGINT(dwTo ) )
					FOR i := 1 UPTO n
						IF ! VoDbFieldPut( FieldPos(aFields[i]), aNum[i] )
							BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
						ENDIF
					NEXT  // i
					VoDbSetSelect(LONGINT(dwFrom ) )
				ENDIF
			ENDDO


   	ENDDO


	RECOVER USING oError
      lBreak := TRUE
	END SEQUENCE


	_FreeFieldNames( fldNames )


	IF ( dwTo > 0 )
		VoDbSetSelect(LONGINT(dwTo ) )
		VoDbCloseArea( )
	ENDIF


	VoDbSetSelect( LONGINT(dwFrom ) )


	IF lBreak
		BREAK oError
	ENDIF


	RETURN lRetCode


/// <exclude/>
FUNCTION __DBSDBUPDATE( cAlias, uCobKey, lRand, bReplace ) AS LOGIC  CLIPPER
	LOCAL dwTo AS DWORD
	LOCAL dwFrom AS DWORD
	LOCAL kEval AS USUAL
	LOCAL lRetCode AS LOGIC


	lRetCode := TRUE


	VoDbGoTop( )
	dwTo := VoDbGetSelect( )


	dwFrom := @@Select( cAlias )
	VoDbGoTop( )


	DO WHILE ! VoDbEof( )
		kEval := Eval( uCobKey )
		VoDbSetSelect(LONGINT(dwTo ) )
		IF lRand
			VoDbSeek( kEval, SetSoftSeek( ), FALSE )
			IF VoDbFound( )
				Eval( bReplace )
			ENDIF
      ELSE
			DO WHILE ( Eval( uCobKey ) < kEval .AND. ! VoDbEof( ) )
				VoDbSkip( 1 )
			ENDDO
			IF ( Eval( uCobKey ) == kEval .AND. ! VoDbEof( ) )
				Eval( bReplace )
			ENDIF
		ENDIF


		VoDbSetSelect(LONGINT(dwFrom ) )
		VoDbSkip( 1 )
	ENDDO


	VoDbSetSelect(LONGINT(dwTo ) )


	RETURN lRetCode




/// <exclude/>
FUNCTION __DBSFLock( nTries := 1 AS DWORD) AS LOGIC STRICT
   LOCAL lOk := FALSE AS LOGIC
   DO WHILE nTries > 0
		NetErr( FALSE )
		IF VoDbFlock()
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
/// <exclude/>
FUNCTION __DBSGoTop( nTries := 1 AS DWORD) AS LOGIC STRICT
   LOCAL lOk := FALSE AS LOGIC
   DO WHILE nTries > 0
		NetErr( FALSE )
		IF VoDbGoTop()
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
		BREAK ErrorBuild(_VoDbErrInfoPtr())
   ENDIF
	RETURN lOk


/// <exclude/>
FUNCTION __DBSGoBottom( nTries := 1 AS DWORD) AS LOGIC STRICT
	LOCAL lOk := FALSE AS LOGIC
   DO WHILE nTries > 0
		NetErr( FALSE )
		IF VoDbGoBottom()
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
		BREAK ErrorBuild(_VoDbErrInfoPtr())
   ENDIF
	RETURN lOk


/// <exclude/>
FUNCTION __DBSOrdListAdd( cBag AS STRING, xOrder AS USUAL, nTries  := 1 AS DWORD) AS LOGIC STRICT
	LOCAL lOk := FALSE AS LOGIC
   DO WHILE nTries > 0
		NetErr( FALSE )
		IF VoDbOrdListAdd(cBag, xOrder)
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
		BREAK ErrorBuild(_VoDbErrInfoPtr())
   ENDIF
	RETURN lOk


/// <exclude/>
FUNCTION __DBSOrdListClear( cBag AS STRING, xOrder AS USUAL, nTries := 1 AS DWORD ) AS LOGIC STRICT
	LOCAL lOk := FALSE AS LOGIC
	DO WHILE nTries > 0
		NetErr( FALSE )
		IF VoDbOrdListClear(cBag, xOrder)
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
		BREAK ErrorBuild(_VoDbErrInfoPtr())
   ENDIF
	RETURN lOk


/// <exclude/>
function __DBSRLock( n as long, nTries := 1 as dword) as logic strict
	 LOCAL lOk := FALSE AS LOGIC
	DO WHILE nTries > 0
		NetErr( FALSE )
		IF VoDbRlock(n)
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


/// <exclude/>
FUNCTION __DBSSeek( xValue AS USUAL, lSoft AS USUAL, lLast AS USUAL, nTries  := 1 AS DWORD) AS LOGIC STRICT
   LOCAL lRet  AS LOGIC


	Default(ref lSoft, SetSoftSeek())
	Default(ref lLast, false)
	Default(ref xValue, "")


	DO WHILE nTries > 0
		NetErr( FALSE )
		IF VoDbSeek(xValue, lSoft, lLast)
			EXIT
		ENDIF
		IF NetErr( )
			nTries --
		ELSE
			EXIT
		ENDIF
	ENDDO


	IF nTries = 0
		BREAK ErrorBuild(_VoDbErrInfoPtr())
	ENDIF


	lRet := VoDbFound()


	RETURN lRet


/// <exclude/>
FUNCTION __DBSSetSelect(dwNew AS DWORD) AS DWORD STRICT
   IF __glRestoreWorkarea
      RETURN VoDbSetSelect(LONGINT(dwNew))
   ENDIF
   RETURN dwNew


STATIC GLOBAL __glRestoreWorkarea := FALSE AS LOGIC


/// <exclude/>
FUNCTION __DBSFieldGet( wPos AS DWORD ) AS USUAL
	LOCAL xRetVal AS USUAL


	IF ! VoDbFieldGet( wPos, REF xRetVal )
		BREAK ErrorBuild( _VoDbErrInfoPtr( ) )
	ENDIF


	RETURN xRetVal


/// <exclude/>
FUNCTION __IsBlob( nField AS INT ) AS LOGIC
	LOCAL lRetCode AS LOGIC
	LOCAL uVal AS USUAL


	IF VoDbFieldInfo( DBS_BLOB_TYPE, DWORD(nField), REF uVal )
		lRetCode := ! IsNil( uVal )
	ENDIF


	RETURN lRetCode


/// <exclude/>
FUNCTION __IterateForFieldAssign( acbExpression AS ARRAY, aFields AS ARRAY ) AS VOID
	LOCAL n AS DWORD
	LOCAL nLen AS DWORD
	LOCAL xValue AS USUAL
	LOCAL nPos AS DWORD


	nLen := ALen( acbExpression )
	FOR n := 1 UPTO nLen
		nPos := FieldPosSym( aFields[n] )


		IF __CanEval( acbExpression[n] )
			xValue := Eval( acbExpression[n] )
		ELSE
			xValue := acbExpression[n]
		ENDIF


		IF ! VoDbFieldPut( nPos, xValue )
			BREAK ErrorBuild( _VoDbErrInfoPtr( ))
		ENDIF
	NEXT
	RETURN
/// <exclude/>
FUNCTION __IterateForSum( acbExpression AS ARRAY, aResults AS ARRAY ) AS VOID
	LOCAL wLen AS DWORD
	LOCAL w AS DWORD


	wLen := ALen( acbExpression )
	FOR w := 1 UPTO wLen
		aResults[w] += Eval( acbExpression[w] )
	NEXT
	RETURN


/// <exclude/>
FUNCTION __MakeErrObj( nTries ) AS USUAL  CLIPPER
	LOCAL oError AS OBJECT


	oError := ErrorBuild( _VoDbErrInfoPtr( ) )


	IF ! IsNil( nTries )
		((Error)oError):Tries := nTries
	ENDIF


	RETURN oError


/// <summary>Get/Set the flag that determines if DbServer operations restore the current workarea </summary>


/// <include file="Rdd.xml" path="doc/DbSetRestoreWorkarea/*" />
FUNCTION DbSetRestoreWorkarea(lEnable := NIL AS USUAL) AS LOGIC STRICT
   LOCAL lOldValue AS LOGIC


   lOldValue := __glRestoreWorkarea


   IF IsLogic(lEnable)
      __glRestoreWorkarea := lEnable
   ENDIF


   RETURN lOldValue


/// <exclude/>
FUNCTION __DBSErrorBlock() AS USUAL CLIPPER
   RETURN ErrorBlock( { | oErr | _Break( oErr ) } )


//  UH 11/12/2000
/// <exclude/>
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
	DO WHILE VoDbSymSelect( String2Symbol( cTryNewAlias ) )>0
		cTryNewAlias := Upper(cFileName)+"_"+AllTrim( Str( w++ ) )
	ENDDO


	RETURN String2Symbol( cTryNewAlias )


/// <exclude/>
FUNCTION __DBSGoTo( n AS LONGINT, nTries := 1 AS DWORD) AS LOGIC STRICT
   LOCAL lOk := FALSE AS LOGIC
   DO WHILE nTries > 0
		NetErr( FALSE )
		IF VoDbGoto(n)
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
		BREAK ErrorBuild(_VoDbErrInfoPtr())
   ENDIF
	RETURN lOk


/// <exclude/>
FUNCTION __DBSSkip( n AS LONGINT, nTries := 1 AS DWORD) AS LOGIC STRICT
	LOCAL lOk := FALSE AS LOGIC
   DO WHILE nTries > 0
		NetErr( FALSE )
		IF VoDbSkip(n)
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
		BREAK ErrorBuild(_VoDbErrInfoPtr())
   ENDIF
	RETURN lOk




/// <exclude/>
FUNCTION __CheckFieldType(uValue REF USUAL, aField AS ARRAY, uError REF USUAL) AS LOGIC  PASCAL
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
            IF Instr("*", Str3(uValue, aField[DBS_LEN], aField[DBS_DEC]))
                lOK     := FALSE
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


    RETURN lOK
STATIC GLOBAL sgLockMode := ccOptimistic AS DWORD


/// <summary>Get/Set the default locking mode for the DbServer class</summary>
/// <param name="dwLockMode">The new locking mode. The default = ccOptimistic</param>
/// <returns>The previous locking mode</returns>
FUNCTION DbSetDefaultLockMode(dwLockMode AS DWORD) AS DWORD PASCAL
    LOCAL dwOld AS DWORD
    dwOld := sgLockMode
    sgLockMode := dwLockMode
    RETURN dwOld


/// <summary>Get the default locking mode for the DbServer class</summary>
/// <returns>The current locking mode</returns>
FUNCTION DbGetDefaultLockMode() AS DWORD PASCAL
    RETURN sgLockMode


