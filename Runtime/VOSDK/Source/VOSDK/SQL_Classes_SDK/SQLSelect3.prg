PARTIAL CLASS SQLSelect

METHOD FieldHyperLabel  ( uFieldPos ) 
	LOCAL nIndex    AS DWORD
	LOCAL oRet      AS OBJECT

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:FieldHyperLabel( "+AsString( uFieldPos )+" )" )
	#ENDIF

	nIndex := SELF:__GetColIndex( uFieldPos, TRUE )
	IF ( nIndex = 0 .OR. nIndex > nNumCols )
		oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADFLD ), #FieldHyperLabel )
	ELSE
		oStmt:__ErrInfo:ErrorFlag := FALSE
		oRet := SELF:__GetColumn(nIndex):HyperLabel
	ENDIF
	RETURN oRet

METHOD FieldInfo( kFieldInfoType, uFieldPos, uFieldVal ) 
	//
	//  Retrieves information about fields
	//  uFieldPos is numeric, symbol or string
	//  kFieldInfoType is one of a set of manifest constants:
	//  DBS_NAME    := 1
	//  DBS_TYPE    := 2
	//  DBS_LEN     := 3
	//  DBS_DEC     := 4
	//  DBS_STRUCT  := 5
	//  DBS_ALIAS   := 6
	//
	LOCAL nIndex    AS DWORD
	LOCAL xRet      AS USUAL  
	LOCAL oColumn	AS SQLColumn

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:FieldInfo( "+AsString( kFieldInfoType )+","+    ;
			AsString( uFieldPos )+","+AsString( uFieldVal )+" )" )
	#ENDIF

	nIndex := SELF:__GetColIndex( uFieldPos, TRUE )

	IF nIndex = 0 .OR. nIndex > nNumCols
		oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADFLD ), #FieldInfo )
	ELSE
		oStmt:__ErrInfo:ErrorFlag := FALSE
		oColumn := aSQLColumns[nIndex]
		DO CASE
		CASE kFieldInfoType = DBS_NAME
			xRet := oColumn:ColName

		CASE kFieldInfoType = DBS_TYPE
			xRet := oColumn:__FieldSpec:ValType

		CASE kFieldInfoType = DBS_LEN
			xRet := oColumn:__FieldSpec:Length

		CASE kFieldInfoType = DBS_DEC
			xRet := oColumn:__FieldSpec:Decimals

		CASE kFieldInfoType = DBS_ALIAS
			IF IsSymbol( uFieldVal )
				oColumn:AliasName := AsString( uFieldVal )
			ELSEIF IsString( uFieldVal )
				oColumn:AliasName := uFieldVal
			ELSE
				IF !IsNil( uFieldVal )
					oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADPAR ), #FieldInfo )
				ENDIF
			ENDIF
			xRet := oColumn:AliasName

		OTHERWISE
			oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADPAR ), #FieldInfo )
		ENDCASE
	ENDIF

	RETURN xRet


METHOD FieldName( siFieldPosition ) 
	LOCAL nIndex    AS DWORD
	LOCAL cRet      AS STRING

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:FieldName( "+AsString( siFieldPosition )+" )" )
	#ENDIF

	nIndex := SELF:__GetColIndex( siFieldPosition, TRUE )

	IF ( nIndex = 0 .OR. nIndex > nNumCols )
		oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADFLD ), #FieldName )
	ELSE
		cRet := SELF:__GetColumn(nIndex):ColName
		oStmt:__ErrInfo:ErrorFlag := FALSE
	ENDIF

	RETURN cRet


METHOD FieldPos( cFieldName ) 
	LOCAL nIndex    AS DWORD
	LOCAL nRet      AS DWORD

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:FieldPos( "+AsString( cFieldName )+" )" )
	#ENDIF
	nIndex := SELF:__GetColIndex( cFieldName, TRUE )
	IF nIndex = 0 .OR. nIndex > nNumCols
		nRet := 0
	ELSE
		nRet := nIndex
		oStmt:__ErrInfo:ErrorFlag := FALSE
	ENDIF
	RETURN nRet

METHOD FIELDPUT( uFieldPos, uValue ) 
	LOCAL nIndex    AS DWORD
	LOCAL nODBCType AS SHORTINT
	LOCAL nDataType AS DWORD
	LOCAL nPos      AS DWORD
	LOCAL nVal      AS INT
	LOCAL liVal     AS LONGINT
	LOCAL fVal      AS REAL4
	LOCAL dVal      AS REAL8
	LOCAL cVal      AS STRING
	LOCAL lVal      AS LOGIC
	LOCAL dDate     AS DATE
	LOCAL pTemp     AS PTR
	LOCAL pBigInt   AS PTR
	LOCAL nLen      AS LONGINT
	LOCAL nDec      AS LONGINT
	LOCAL bVal      AS BYTE
	LOCAL nMax      AS DWORD
	LOCAL nLow      AS DWORD
	LOCAL nHigh     AS DWORD
	LOCAL fBigVal   AS FLOAT
	LOCAL aData     AS ARRAY
	LOCAL lNull     AS LOGIC
	LOCAL oColumn	 AS SQLColumn
	LOCAL oData		 AS SQLData
	LOCAL oFs		 AS FieldSpec
	#IFDEF __DEBUG__
		LOCAL cOut  AS STRING
		IF IsNil( uValue )
			cOut := "NIL ( NULL )"
		ELSE
			cOut := AsString( uValue )
		ENDIF
		__SQLOutputDebug( "** SQLSelect:FieldPut( "+AsString( uFieldPos )+","+ cOut +" )" )
	#ENDIF

	nIndex := SELF:__GetColIndex( uFieldPos, TRUE )

	IF ( nIndex = 0 .OR. nIndex > nNumCols )
		oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADFLD ), #FieldPut )
		SELF:Error( oStmt:ErrInfo )
		RETURN NIL
	ENDIF

	IF ! ((SqlColumnAttributes)SELF:ColumnAttributes( nIndex )):Updatable
		oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__UPDATE_COL ), #FieldPut )
		SELF:Error( oStmt:ErrInfo )
		RETURN NIL
	ENDIF

	lNull := FALSE
	IF IsNil( uValue )
		lNull := TRUE
	ELSE
		IF IsString( uValue ) .AND. NULL_STRING = uValue
			lNull := TRUE

		ELSEIF IsDate( uValue ) .AND. NULL_DATE = uValue
			lNull := TRUE
		ENDIF
	ENDIF

	// 	lNull 	  := __CheckNULLDataValue( uValue, nODBCType )
	oColumn		:= aSQLColumns[nIndex]
	nODBCType 	:= oColumn:ODBCType
	oFs		   := oColumn:FieldSpec

	IF lAppendFlag
		aData := aAppendData
	ELSE
		aData := aSQLData
	ENDIF
	IF  oColumn:Nullable .AND. lNull      
		//RvdH 070430 changed aAppendData to aData
		SELF:__SetNullData(nODBCType, nIndex, aData)
		// SELF:__SetNullData(nODBCType, nIndex, aAppendData)
		//RvdH 061002 Don't return here. Otherwise no FieldChange and no correct return value
		//RETURN NIL
	ELSE
		//  UH 04/20/2000 -> Later due to garbage collection
		//  pTemp := PTR( _CAST, aData[nIndex]:Value )
		// RvdH 050413 get pTemp before case
		oData := aData[nIndex]
		IF oData:Null
			oData:Null := FALSE
			oData:ValueChanged := TRUE
			lRowModified := TRUE
		ENDIF
	
		//RvdH 050429 Clear buffer first
		//pTemp := PTR( _CAST, oData:Value)
		oData:Clear()
		pTemp := oData:ptrValue
	
		DO CASE
		CASE nODBCType = SQL_SMALLINT
			nVal   := uValue
			SHORTINT( pTemp ) := SHORTINT( _CAST,nVal )
	
		CASE nODBCType = SQL_INTEGER
			liVal := uValue
			LONGINT( pTemp ) := liVal
	
		CASE nODBCType = SQL_REAL
			fVal := uValue
			REAL4( pTemp ) := fVal
	
		CASE nODBCType = SQL_FLOAT .OR. nODBCType = SQL_DOUBLE
			dVal   := uValue
			REAL8( pTemp ) := dVal
	
		CASE nODBCType = SQL_DECIMAL .OR. nODBCType = SQL_NUMERIC
			IF oFs <> NULL_OBJECT
				nLen := oFs:Length
	   	   nDec := oFs:Decimals
	   	ELSE
	   		nLen := nDec := -1
	   	ENDIF
	
			cVal := __Str( uValue, nLen , nDec)
	
			//RvdH 030925 Changed to MemCopyString: Safer
			//MemCopy( pTemp, PTR( _CAST, cVal ), nLen )
			MemCopyString( pTemp, cVal , DWORD(nLen) )
	
		CASE nODBCType = SQL_BIT
			lVal := uValue
	
			IF lVal
				bVal := SQL_LOGICAL_TRUE
			ELSE
				bVal := SQL_LOGICAL_FALSE
			ENDIF
	
			BYTE( pTemp ) := bVal
	
		CASE nODBCType = SQL_TINYINT
			bVal  := uValue
			BYTE( pTemp ) := bVal
	
		CASE nODBCType = SQL_DATE
			IF IsDate( uValue )
				dDate := uValue
			ELSEIF IsString( uValue )
				dDate := CToDAnsi( uValue )
			ENDIF
	
			IF dDate = NULL_DATE
				oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADPAR ), #FIELDPUT )
				SELF:Error( oStmt:ERRINFO )
				RETURN NIL
			ENDIF
			cVal := DToCSQL( dDate )
			//RvdH 030925 Changed to MemCopyString: Safer
			//MemCopy( pTemp, PTR( _CAST, cVal ), 10 )
			MemSet ( pTemp, 0, SQL_DATE_LEN+1 )
			MemCopyString( pTemp, cVal , SQL_DATE_LEN )
	
	
		CASE nODBCType = SQL_TIMESTAMP
			nDataType := UsualType( uValue )
	
			IF nDataType == STRING        
				//RvdH 070430 When date = empty, assign NULL when allowed
				IF (Left(uValue, 10) == "    -  -  ") .AND. oColumn:Nullable
					SELF:__SetNullData(nODBCType, nIndex, aData)
				ELSE					
					nPos := At2( " ", uValue )
					IF nPos > 0
						//
						//  We assume right timestamp format
						//
						cVal := uValue
					ELSE
						//
						//  Create time stamp
						//
						dDate := CToDAnsi( uValue )
						cVal  := MakeTimeStamp( dDate, 0 )
					ENDIF
				ENDIF	
			ELSEIF nDataType == DATE
				cVal  := MakeTimeStamp( uValue, 0 )
			ENDIF
			nLow := SLen( cVal )
			nMax := oData:Length
			nMax := IIF(nMax > nLow, nLow, nMax)
	
			//RvdH 030925 Changed to MemCopyString: Safer
			//MemCopy( pTemp, PTR( _CAST, cVal ), nLow )
			MemCopyString( pTemp, cVal , nMax )
	
	
		CASE nODBCType = SQL_LONGVARCHAR .OR. nODBCType = SQL_LONGVARBINARY .OR. nODBCType = SQL_WLONGVARCHAR
			IF IsNil( uValue )
				cVal := NULL_STRING // Space( 10 )
			ELSE
				cVal := AsString( uValue )
				IF SLen( cVal ) = 0
					cVal := NULL_STRING // Space( 10 )
				ENDIF
			ENDIF
			oData:LongValue:= cVal
			uValue := cVal
	
	
		CASE nODBCType = SQL_BIGINT
			fBigVal := uValue
			nLow    := Integer( fBigVal % ( 2^32 ) )
			nHigh   := Integer( fBigVal / ( 2^32 ) )
			// RvdH 030703 Bug report Dirk H
			//liVal   := uValue
	
			pBigInt := oData:ptrValue
			MemCopy( pBigInt, @nLow, 4 )
			pBigInt := PTR( _CAST, DWORD( _CAST, pBigInt ) + 4 )
			MemCopy( pBigInt, @nHigh, 4 )
	
		OTHERWISE
			cVal := AsString( uValue )
			nLow := SLen( cVal )
			nMax := oData:Length
			nMax := IIF(nMax > nLow, nLow, nMax)
			MemCopyString( pTemp, cVal , nMax )
		ENDCASE
	
		oData:ValueChanged := TRUE
		lRowModified       := TRUE
	ENDIF
	SELF:Notify( NOTIFYFIELDCHANGE, oColumn:NameSym )
	oStmt:__ErrInfo:ErrorFlag := FALSE
	RETURN uValue


METHOD FieldSpec( uFieldPos ) 
	LOCAL nIndex AS DWORD

	nIndex := SELF:__GetColIndex( uFieldPos, TRUE )
	IF ( nIndex = 0 .OR. nIndex > SELF:FCount )
		oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADFLD ), #FieldSpec )
		RETURN NULL_OBJECT
	ENDIF
	oStmt:__ErrInfo:ErrorFlag := FALSE
	RETURN ((DataField)aDataFields[nIndex]):FieldSpec

METHOD FieldStatus( uFieldPos ) 
	LOCAL nIndex AS DWORD
	LOCAL oRet   AS OBJECT

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:FieldStatus( "+AsString( uFieldPos )+" )" )
	#ENDIF
	nIndex := SELF:__GetColIndex( uFieldPos, TRUE )
	IF nIndex = 0 .OR. nIndex > nNumCols
		oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADFLD ), #FieldStatus )
	ELSE
		oStmt:__ErrInfo:ErrorFlag := FALSE

		oRet := SELF:__GetColumn(nIndex):HyperLabel
	ENDIF
	RETURN oRet

METHOD FieldSym( uFieldPos ) 
	LOCAL nIndex        AS DWORD
	LOCAL symRet        AS SYMBOL

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:FieldSym( "+AsString( uFieldPos )+" )" )
	#ENDIF
	nIndex := SELF:__GetColIndex( uFieldPos, TRUE )
	IF nIndex = 0 .OR. nIndex > nNumCols
		oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADFLD ), #FieldSym )
	ELSE
		oStmt:__ErrInfo:ErrorFlag := FALSE
		symRet := SELF:__GetColumn(nIndex):__HyperLabel:NameSym
	ENDIF

	RETURN symRet


METHOD FieldValidate( uFieldPos, uValue ) 
	LOCAL nIndex    AS DWORD
	LOCAL lRet      AS LOGIC

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:FieldValidate( "+AsString( uFieldPos )+" )" )
	#ENDIF

	nIndex := SELF:__GetColIndex( uFieldPos, TRUE )
	IF nIndex = 0 .OR. nIndex > nNumCols
		oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADFLD ), #FieldValidate )
	ELSE
		oStmt:__ErrInfo:ErrorFlag := FALSE
        local oDF := aDataFields[nIndex] as DataField
		lRet := oDF:__FieldSpec:PerformValidations( uValue )
		IF !lRet
			IF oDF:__FieldSpec:Status != NULL_OBJECT
				//  Get description from hyperlabel
				oStmt:__GenerateSQLError( oDF:__FieldSpec:__HyperLabel:Description ,  ;
					#FieldValidate )
			ELSE
				oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADVALID ), #FieldValidate )
			ENDIF
		ENDIF
	ENDIF
	RETURN lRet



METHOD FLOCK() 

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:FLock()" )
	#ENDIF
	RETURN TRUE

METHOD FreeStmt( fOption ) 
	//RvdH 070530 Added default value, as suggested by Stavros Spanos
	IF PCount() == 0
		fOption := SQL_CLOSE
	ENDIF

	RETURN oStmt:FreeStmt( fOption )

METHOD GetData( iCol ) 
	LOCAL nIndex    AS DWORD
	LOCAL nODBCType AS SHORTINT
	LOCAL nVal      AS SHORTINT
	LOCAL liVal     AS LONGINT
	LOCAL fVal      AS REAL4
	LOCAL dVal      AS REAL8
	LOCAL cVal      AS STRING
	LOCAL lVal      AS LOGIC
	LOCAL dDate     AS DATE
	LOCAL uiLen     AS DWORD
	LOCAL bVal      AS BYTE
	LOCAL aData     AS ARRAY
	LOCAL xVal      AS USUAL
	LOCAL pTemp     AS PTR
	LOCAL nDec		AS INT
	LOCAL nLen      AS DWORD
	LOCAL oData		AS SqlData

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:GetData( "+AsString( iCol )+" )" )
	#ENDIF

	nIndex := SELF:__GetColIndex( iCol, TRUE )
	IF nIndex = 0 .OR. nIndex > nNumCols
		oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADCOL ), #GetData )
		SELF:Error( oStmt:ErrInfo )
		RETURN NIL
	ENDIF
	oStmt:__ErrInfo:ErrorFlag := FALSE
	nODBCType := SELF:__GetColumn(nIndex):ODBCType

	IF lAppendFlag
		aData := aAppendData
	ELSE
		aData := aSQLData
	ENDIF
	oData := aData[nIndex]
	IF SELF:lEof .OR. oData:Null
		#IFDEF __DEBUG__
			__SQLOutputDebug( "**          :GetData IS NULL" )
		#ENDIF
		//RETURN __GetNULLDataValue( nODBCType, SELF:lNullAsBlank )
		RETURN NIL
	ENDIF

	pTemp := oData:ptrValue
	DO CASE
	CASE nODBCType = SQL_SMALLINT
		nVal  := SHORTINT( pTemp )
		#IFDEF __DEBUG__
			__SQLOutputDebug( "**          :GetData( si )="+AsString( nVal ) )
		#ENDIF
		RETURN nVal

	CASE nODBCType = SQL_INTEGER
		liVal := LONGINT( pTemp )
		#IFDEF __DEBUG__
			__SQLOutputDebug( "**          :GetData( li )="+AsString( liVal ) )
		#ENDIF
		RETURN liVal

	CASE nODBCType = SQL_REAL
		fVal  := REAL4( pTemp )
		#IFDEF __DEBUG__
			__SQLOutputDebug( "**          :GetData( fl )="+AsString( fVal ) )
		#ENDIF
		RETURN fVal

	CASE nODBCType = SQL_FLOAT   .OR. nODBCType = SQL_DOUBLE
		dVal  := REAL8( pTemp )
		#IFDEF __DEBUG__
			__SQLOutputDebug( "**          :GetData( db )="+AsString( dVal ) )
		#ENDIF
		RETURN dVal

	CASE nODBCType = SQL_NUMERIC .OR. nODBCType = SQL_DECIMAL
		nLen  := oData:Length
		cVal  := Mem2String( pTemp, nLen )
		xVal  := Val( cVal )
		IF UsualType( xVal ) = FLOAT
            LOCAL oDF := SELF:DataField( nIndex ) as DataField
			nDec := oDF:__FieldSpec:Decimals
			xVal := Round( xVal, nDec )
		ENDIF
		RETURN xVal

	CASE nODBCType = SQL_BIT
		bVal  := BYTE( pTemp )
		IF bVal = SQL_LOGICAL_TRUE .OR. bVal = 1
			lVal := TRUE
		ELSE
			lVal := FALSE
		ENDIF
		#IFDEF __DEBUG__
			__SQLOutputDebug( "**    :GetData( l )="+AsString( lVal ) )
			__SQLOutputDebug( "      bVal = "+AsString( bVal ) )
		#ENDIF
		RETURN lVal

	CASE nODBCType = SQL_TINYINT
		bVal  := BYTE( pTemp )
		#IFDEF __DEBUG__
			__SQLOutputDebug( "**    :GetData( l )="+AsString( bVal ) )
		#ENDIF
		RETURN bVal

	CASE nODBCType = SQL_DATE
		cVal  := Mem2String( pTemp, SQL_DATE_LEN+1 )
		cVal  := __AdjustString( cVal )
		dDate := CToDAnsi( cVal )
		#IFDEF __DEBUG__
			__SQLOutputDebug( "**          :GetData( d )="+AsString( dDate ) )
		#ENDIF
		RETURN dDate

	CASE nODBCType = SQL_TIMESTAMP
		// UH: Map timestamp to vo string   
		IF SELF:lTimeStampAsDate
			dDate := SELF:GetdateVal(nIndex)
			#IFDEF __DEBUG__
				__SQLOutputDebug( "**          :GetData( tsd )="+ AsString(dDate) )
			#ENDIF                                                                
			RETURN dDate
		ELSE
		
			cVal := SELF:GetTimeStamp( nIndex )
			#IFDEF __DEBUG__
				__SQLOutputDebug( "**          :GetData( tsd )="+ cVal )
			#ENDIF
			RETURN cVal
		ENDIF

	CASE nODBCType = SQL_LONGVARCHAR  .OR.;
		nODBCType = SQL_WLONGVARCHAR .OR. ;
		nODBCType = SQL_LONGVARBINARY
		cVal := oData:LongValue
		RETURN cVal

	OTHERWISE
		pTemp := oData:ptrValue
		uiLen := PszLen( pTemp )
		uiLen := IIF(uiLen < oData:Length, uiLen, oData:Length)
		cVal	:= Mem2String(pTemp, uiLen)
		#IFDEF __DEBUG__
			__SQLOutputDebug( "**          :GetData( st )="+ cVal ) // RTRIM( cVal ) )
		#ENDIF
		RETURN cVal

	ENDCASE



METHOD GetdateVal( uFieldPos ) 
	LOCAL cVal AS STRING
	LOCAL dVal AS DATE

	cVal := SELF:GetTimestamp( uFieldPos )
	IF !IsNil( cVal )
		dVal := CToDAnsi( SubStr3( cVal, 1, 12 ) )
	ENDIF
	RETURN dVal

METHOD GetLookupTable( nMaxRows,uField1,uField2 ) 
	LOCAL aResult := {}         AS ARRAY
	LOCAL wRows := 32767        AS WORD

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:GetLookupTable()" )
	#ENDIF
	IF nMaxRows = NIL
		wRows := 100
	ELSEIF nMaxRows<wRows
		wRows := nMaxRows
	ENDIF
	IF IsNil( uField1 )
		uField1 := 1
	ENDIF
	IF IsNil( uField2 )
		uField2 := 2
	ENDIF
	DO WHILE wRows > 0 .AND. SELF:Status = NULL_OBJECT .AND. !SELF:lEof
		AAdd( aResult, { SELF:FIELDGET( uField1 ), SELF:FIELDGET( uField2 ) } )
		--wRows
		SELF:Skip()
	ENDDO
	SELF:Notify( NotifyRecordChange )
	RETURN aResult

METHOD GetStatementOption( fOption ) 

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:GetStatementOption()" )
	#ENDIF
	RETURN oStmt:GetStatementOption( fOption )


METHOD GetTimeStamp( uFieldPos ) 
	LOCAL nIndex    AS DWORD
	LOCAL nODBCType AS SHORTINT
	LOCAL cVal      AS STRING
	LOCAL uiLen     AS DWORD
	LOCAL oData		 AS SQLData 
	LOCAL oColumn	 AS SQLColumn
	nIndex := SELF:__GetColIndex( uFieldPos, TRUE )
	IF ( nIndex = 0 .OR. nIndex > nNumCols )
		oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADFLD ), #FieldGet )
		SELF:Error( oStmt:ErrInfo )

		RETURN NIL
	ENDIF

	oColumn 	 := aSQLColumns[nIndex]
	nODBCType := oColumn:ODBCType
	IF ( nODBCType != SQL_TIMESTAMP )
		oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADFLD ), #FieldGet )
		SELF:Error( oStmt:ErrInfo )
		RETURN NIL
	ENDIF
	uiLen := oColumn:__FieldSpec:Length
	IF lAppendFlag
		oData := aAppendData[nIndex]
	ELSE
		oData := aSQLData[nIndex]
	ENDIF
	IF oData:Null
		cVal := Space( uiLen )
	ELSE
		cVal := Mem2String( oData:ptrValue , uiLen )
		cVal := __AdjustString( cVal )
	ENDIF
	oStmt:__ErrInfo:ErrorFlag := FALSE
	RETURN cVal

METHOD GetTimeString( uFieldPos ) 
	LOCAL cVal AS STRING

	cVal := SELF:GetTimestamp( uFieldPos )
	IF !IsNil( cVal )
		cVal := SubStr2( cVal, 12 )
	ENDIF
	RETURN cVal

METHOD GoBottom() 
	LOCAL lOk		 AS LOGIC

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:GoBottom()" )
	#ENDIF
	IF ! SELF:Notify( NOTIFYINTENTTOMOVE )
		oStmt:__GenerateSQLError( "IntentToMove returned false.", #GoBottom )
		RETURN FALSE
	ENDIF
	// RvdH 050413 Centralize GoCold behavior
	IF !SELF:__GoCold(FALSE, TRUE)
		RETURN FALSE
	ENDIF
	// Below is the 'Old' __GoBottomCursor method
	IF lAppendFlag
		#IFDEF __DEBUG__
			__SQLOutputDebug( "***  Moved to appended record " )
		#ENDIF

		SELF:__SetRecordFlags( FALSE, TRUE )

		lOk :=  TRUE     // already at append, OK
	ELSE
		lOk := SELF:ExtendedFetch( SQL_FETCH_LAST, 0 )

		IF lOk
			SELF:__SetRecordFlags( NIL, FALSE )
			IF SELF:nLastRecNum > 0
				SELF:__SetRecordFlags( FALSE, NIL )
			ENDIF
		ELSE
			SELF:__SetRecordFlags( TRUE, TRUE )
		ENDIF

	ENDIF
	RETURN lOk


METHOD GoTo( uRecordExpr ) 
	//	LOCAL siIndex       AS INT
	LOCAL nRecno AS LONGINT
	LOCAL lRet := FALSE AS LOGIC
	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:GoTo( "+AsString( uRecordExpr )+" )" )
	#ENDIF
	IF !IsNumeric( uRecordExpr )
		oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADPAR ), #GoTo )
		RETURN NIL
	ENDIF
	IF !SELF:Notify( NOTIFYINTENTTOMOVE )
		oStmt:__GenerateSQLError( "IntentToMove returned false.", #GoTo )
		RETURN FALSE
	ENDIF  
	nRecno := uRecordExpr

   // Below is the 'Old' __GoToCursor method
	IF nRecno = SELF:nLastRecNum + 1
		IF lAppendFlag
			#IFDEF __DEBUG__
				__SQLOutputDebug( "***  Moved to appended record " )
			#ENDIF
			SELF:__SetRecordFlags( FALSE, TRUE )
			nRecNum         := nRecno
			RETURN TRUE     // already at append, OK
		ELSEIF lReadCOlumnInfo
			// Restore lAppendFlag                  
			#IFDEF __DEBUG__
				__SQLOutputDebug( "**  Back to appended record restored" )
			#ENDIF

			lAppendFlag   := TRUE
			nAppendRecNum := nRecno
			nRecNum       := nRecno             
			SELF:__SetRecordFlags( FALSE, FALSE )
			//RvdH 070716 This will fill the new row with blanks again
			SELF:ReReadRow()
			RETURN TRUE
			
		ENDIF       
		
	ENDIF
	IF nRecno = 0
		nRecno := SELF:nLastRecNum + 1
	ENDIF
	// RvdH 050413 Centralize GoCold behavior
	IF !SELF:__GoCold(FALSE, TRUE)
		RETURN FALSE
	ENDIF
	//
	//  Fetch negative recno from bottom
	//  Notifications get send by ExtendedFetch and Gobottom/Skip
	//
	IF nRecno >= 0
		lRet := SELF:ExtendedFetch( SQL_FETCH_ABSOLUTE, nRecno )
	ELSE
		IF SELF:GoBottom()
			lRet := SELF:Skip( nRecno )
		ENDIF
	ENDIF
	IF !lRet
		IF SELF:nRecnum = SELF:nLastRecNum
			SELF:nRecnum := SELF:nLastRecNum + 1
			SELF:__SetRecordFlags( TRUE, TRUE )
		ENDIF
	ENDIF
	RETURN lRet



METHOD GoTop() 
	LOCAL lRet AS LOGIC
	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:GoTop()" )
	#ENDIF
	IF !SELF:Notify( NOTIFYINTENTTOMOVE )
		oStmt:__GenerateSQLError( "IntentToMove returned false.", #GoTop )
		RETURN FALSE
	ENDIF
	// RvdH 050413 Centralize GoCold behavior
	IF !SELF:__GoCold(FALSE, TRUE)
		RETURN FALSE
	ENDIF
   // Below is the 'Old' __GoTopCursor method
	lRet := SELF:ExtendedFetch( SQL_FETCH_FIRST, 0 )

	IF !lRet
		SELF:nLastRecnum := 0
		SELF:nRecnum := 1
		SELF:__SetRecordFlags( TRUE, TRUE )
	ENDIF
	RETURN lRet

END CLASS

