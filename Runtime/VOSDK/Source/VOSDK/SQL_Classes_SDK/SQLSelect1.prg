PARTIAL CLASS SQLSelect INHERIT DataServer
	PROTECT oStmt           AS SQLStatement
	PROTECT oConn				AS SQLConnection
	PROTECT cCursor         AS STRING
	PROTECT lCsrOpenFlag    AS LOGIC
	PROTECT lFetchFlag      AS LOGIC
	PROTECT lBof            AS LOGIC
	PROTECT lEof            AS LOGIC
	PROTECT lRowModified    AS LOGIC
	PROTECT nNumCols        AS DWORD
	PROTECT aSQLColumns     AS ARRAY
	PROTECT aSQLData        AS ARRAY
	PROTECT lSkipFlag       AS LOGIC
	PROTECT nRecNum         AS INT
	PROTECT nBuffRecNum     AS INT
	PROTECT nLastRecNum     AS INT
	PROTECT aAppendData     AS ARRAY
	PROTECT lAppendFlag     AS LOGIC
	PROTECT lNotifyIntentToMove AS LOGIC
	PROTECT nAppendRecNum   AS LONGINT
	PROTECT aIndexCol       AS ARRAY
	PROTECT nScrollUpdateType   AS INT
	PROTECT nRowCount       AS INT		// # of rows for last insert/delete/update
	PROTECT nNumRows			AS INT		// # of rows in select
	PROTECT cTableName      AS STRING
	PROTECT lDeleteFlag     AS LOGIC
	PROTECT nNotifyCount    AS INT
	PROTECT lSuppressNotify AS LOGIC
	
	PROTECT apColMem        AS ARRAY		//RvdH 050429 Now array of Record Buffers
	PROTECT nBufferLength	AS DWORD
	PROTECT nMaxBindColumn  AS DWORD 	//RvdH  Changed from LONG to DWORD
	PROTECT lColBind        AS LOGIC
	PROTECT lLastRecFound   AS LOGIC
	PROTECT aDataRecord     AS ARRAY			// Buffer with SQLData Objects for Data Record (Bound )
	PROTECT aOriginalRecord  AS ARRAY		// Buffer with SQLData Objects with Original values (Not Bound)
	
	PROTECT aColumnAttributes AS ARRAY     // Array of SQLColumnAttributes Objects.
	PROTECT lNullAsBlank    AS LOGIC
	PROTECT lTimeStampAsDate AS LOGIC
	PROTECT lReadColumnInfo	AS LOGIC			//RvdH 070716 Abused to remember lAppendFlag 
	
	//RvdH 050413 Added some support methods
	METHOD __AllocStmt() AS LOGIC STRICT 
	
	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:__AllocStmt()" )
	#ENDIF
	RETURN SELF:oStmt:__AllocStmt()
	

METHOD __BuildUpdateStmt AS STRING STRICT 
	// RvdH 050413 Added
	LOCAL cUpdate       AS STRING
	LOCAL cSet          AS STRING
	LOCAL cQuote		AS STRING
	LOCAL oCol			AS SqlColumn
	LOCAL oData			AS SqlData
	LOCAL nIndex        AS DWORD
	LOCAL nODBCType     AS SHORTINT
	LOCAL nCount        AS INT
	
	cUpdate 	:= "update " + cTableName
	cSet 		:= " set "
	cQuote  	:= oStmt:Connection:IdentifierQuoteChar
	nCount	:= 0
	FOR nIndex := 1 TO nNumCols
		oCol 			:= aSQLColumns[nIndex]
		oData			:= aSQLData[nIndex]
		nODBCType  := oCol:ODBCType
		IF nODBCType = SQL_LONGVARCHAR .OR. nODBCType = SQL_LONGVARBINARY   .OR. nODBCTYPE = SQL_WLONGVARCHAR
			IF !oData:Null
				LOOP
			ENDIF
		ENDIF
		
		IF oData:ValueChanged
			nCount++
			cUpdate += cSet
			cUpdate += cQuote
			cUpdate += oCol:ColName
			cUpdate += cQuote
			
			cUpdate += __GetDataValuePSZ( oCol, ;
				oData,    ;
				TRUE,                ;
				FALSE )
			cSet := ","
		ENDIF
	NEXT
	IF nCount == 0
		cUpdate := NULL_STRING
	ENDIF
	RETURN cUpdate
	
	
	

METHOD  __CopyDataBuffer( aSQLSource AS ARRAY, aSQLTarget AS ARRAY) AS ARRAY STRICT 
	LOCAL nIndex  	AS DWORD
	LOCAL nCols	  AS DWORD
	LOCAL oTarget AS SQLData
	LOCAL oSource AS SQLData
	// Adjust size
	nCols := SELF:nNumCols
	IF nCols > ALen(aSQLSource)
		nCols := ALen(aSQLSource)
	ENDIF
	IF nCols > ALen(aSQLTarget)
		nCols := ALen(aSQLTarget)
	ENDIF
	
	IF nCols >= 1
		FOR nIndex := 1 TO nCols
			oTarget := aSQLTarget[nIndex]
			oSource := aSQLSource[nIndex]
			oTarget:Null 		 	:= oSource:Null
			oTarget:ValueChanged := oSource:ValueChanged
		NEXT
		// Now copy the data
		oTarget := aSQLTarget[1]
		oSource := aSQLSource[1]
		MemCopy(oTarget:ptrValue, oSource:ptrValue, SELF:nBufferLength)
	ENDIF
	RETURN aSQLTarget
	
	

METHOD __CopySQLError( symMethod AS SYMBOL, oErrInfo AS SQLErrorInfo) AS VOID STRICT 
	LOCAL oStmtErr AS SQLErrorInfo
	SELF:oStmt:ErrInfo := SQLErrorInfo{ SELF, symMethod }
	oStmtErr := SELF:oStmt:ErrInfo
	oStmtErr:ReturnCode  	:= oErrInfo:ReturnCode
	oStmtErr:NativeError 	:= oErrInfo:NativeError
	oStmtErr:ErrorFlag   	:= oErrInfo:ErrorFlag
	oStmtErr:ErrorMessage 	:= oErrInfo:ErrorMessage
	oStmtErr:SQLState 	 	:= oErrInfo:SQLState
	RETURN
	
	// METHOD __DeleteCursor() AS LOGIC PASCAL CLASS SQLSelect
	// 	LOCAL nRetcode      AS INT
	// 	LOCAL lConcurrency  AS LOGIC
	// 	LOCAL lRet          AS LOGIC
	//
	// 	#IFDEF __DEBUG__
	// 		__SQLOutputDebug( "** SQLSelect:__DeleteCursor()" )
	// 	#ENDIF
	//
	// 	SELF:nRowCount := 0
	//
	// 	nRetCode := SQL_SUCCESS
	//
	// 	lConcurrency := ( SELF:oStmt:Connection:ScrollConcurrency == SQL_CONCUR_LOCK )
	//
	// 	IF lConcurrency
	// 		nRetCode := SQL_LOCK_RECORD( SELF:oStmt:StatementHandle, 1, SQL_LOCK_EXCLUSIVE )
	// 	ENDIF
	//
	// 	IF nRetCode = SQL_SUCCESS
	// 		lRet := SELF:SetPos( 1, SQL_DELETE, SQL_LOCK_NO_CHANGE )
	// 		IF lRet
	// 			SELF:nRowCount := 1
	// 		ENDIF
	// 		IF lConcurrency
	// 			SQL_LOCK_RECORD( SELF:oStmt:StatementHandle, 1, SQL_LOCK_UNLOCK )
	// 		ENDIF
	// 	ENDIF
	// 	RETURN lRet
	//

METHOD __FigureScrollUpdateType() AS DWORD STRICT 
	LOCAL  nRet AS DWORD
	
	#IFDEF __DEBUG__
		__SQLOutputDebug( "** Select:__FigureScrollUpdateType()" )
	#ENDIF
	
	IF nScrollUpdateType == SQL_SC_UPD_AUTO
		//  UH 07/10/2000
		IF ALen( aIndexCol ) != 0     // do we have primary key info ?
			nRet := SQL_SC_UPD_KEY
		ELSE
			//oConn := oStmt:Connection
			//RvdH 050428 Always try update in cursor. SqlSelect:Update() will fall back to values when that fails.
			//IF ( /*oConn:ScrollCsr  .AND. */ oConn:PositionOps .AND. oConn:nActiveStmts != 1 )
			nRet := SQL_SC_UPD_CURSOR
			//ELSE
			//	nRet := SQL_SC_UPD_VALUE
			//ENDIF
		ENDIF
	ELSE
		nRet := DWORD(SELF:nScrollUpdateType)
	ENDIF
	
	RETURN nRet
	

METHOD __FindTableName() AS VOID STRICT 
	LOCAL nPos   AS DWORD
	LOCAL cClass AS STRING
	LOCAL cStmt	 AS STRING
	// RvdH 050413 Optimized by using local strings
	
	cStmt := SELF:oStmt:SQLString
	IF cStmt != NULL_STRING
		nPos := AtC( "select ", cStmt )
		IF nPos > 0
			cTableName := LTrim( SubStr2( cStmt, nPos + 7 ) )
		ENDIF
		
		nPos := AtC( " from ", cStmt )
		cTableName := LTrim( SubStr2( cStmt, nPos + 6 ) )
		nPos := At2( " ", cTableName )
		IF nPos >= 2
			cTableName := SubStr3( cTableName, 1, nPos - 1 )
		ENDIF
		
		// tell the DataServer the name...
		cClass := Symbol2String( ClassName( SELF ) )
		oHyperLabel := HyperLabel{ cTableName, cTableName,                                   ;
			cClass + ": "+cTableName, ;
			cClass + "_"+cTableName }
		
	ENDIF
	RETURN
	

METHOD __ForceOpen AS LOGIC STRICT 
	// RvdH 050413 Added
	IF ! SELF:lCsrOpenFlag
		IF ! SELF:Execute()
			RETURN FALSE
		ENDIF
	ENDIF
	RETURN TRUE
	

METHOD __FreeStmt( fOption AS WORD ) AS LOGIC STRICT 
	
	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:__FreeStmt()" )
	#ENDIF
	RETURN oStmt:__FreeStmt( fOption )
	

METHOD __GetColIndex( uCol AS USUAL, lAutoFetch AS LOGIC) AS DWORD STRICT 
	//
	// Returns the index of the specified column
	// iCol can be numeric, or the column name as a symbol or a string
	//
	// RvdH 050413 Added Check for Column Count and combined Symbol & String handling
	
	LOCAL nType  AS DWORD
	LOCAL nIndex AS DWORD
	LOCAL sCol   AS STRING
	LOCAL dwCol	 AS DWORD
	LOCAL oCol	 AS SQLColumn
	
	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:__GetColIndex( "+AsString( uCol )+" )" )
	#ENDIF
	
	IF !IsNil( lAutoFetch ) .AND. lAutoFetch
		//RvdH 050413 Centralize opening of cursor
		IF ! SELF:__ForceOpen()
			RETURN 0
		ENDIF
		
		IF !SELF:lFetchFlag
			
			//IF oStmt:Connection:ScrollCsr
			
			SELF:__SkipCursor( 1 )
			
			// 			ELSEIF !SELF:Fetch()
			// 				IF ! SELF:lEof .OR. !SELF:lFetchFlag
			// 					RETURN 0
			// 				ENDIF
			// 			ENDIF
		ENDIF
	ENDIF
	
	nType := UsualType( uCol )
	// Convert SYMBOL to String
	IF nType == SYMBOL
		uCol := Upper( Symbol2String( uCol ) )
		nType := STRING
	ENDIF
	DO CASE
	CASE nType = STRING
		sCol := Upper( uCol )
		FOR nIndex := 1 TO SELF:nNumCols
			oCol := aSQLColumns[nIndex]
			IF oCol:Name == sCol
				RETURN nIndex
			ENDIF
			IF Upper( oCol:AliasName ) == sCol
				RETURN nIndex
			ENDIF
		NEXT
		
	CASE nType = LONGINT
		dwCol := uCol
		IF (dwCol > 0 .AND. dwCol <= SELF:nNumCols)
			RETURN dwCol
		ENDIF
		
	CASE nType = FLOAT
		dwCol := Integer( uCol )
		IF (dwCol > 0 .AND. dwCol <= SELF:nNumCols)
			RETURN dwCol
		ENDIF
		
	OTHERWISE
		// Return 0 below
	ENDCASE
	RETURN 0
	

METHOD __GetCursorName () AS LOGIC STRICT 
	LOCAL nCount        AS SHORTINT // dcaton 070206 was INT
	LOCAL pszBuffer     AS PSZ
	LOCAL nRetCode      AS INT
	LOCAL lRet          AS LOGIC
	
	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:__GetCursorName()" )
	#ENDIF
	
	IF ( oStmt:StatementHandle = SQL_NULL_HSTMT )
		SELF:__AllocStmt()
	ENDIF
	// Statement NOT yet prepared?
	IF ( ! oStmt:PrepFlag )
		IF ( !SELF:Prepare() )
			RETURN FALSE
		ENDIF
	ENDIF
	pszBuffer := MemAlloc( 33 )
	IF pszBuffer <> NULL_PSZ
		MemClear( PTR( _CAST,pszBuffer ), 33 )
		nRetCode := SQLGetCursorName( oStmt:StatementHandle, pszBuffer, ;
			32,/*Len( cCsrName ),*/ ;
			@nCount )
		IF ( nRetCode != SQL_SUCCESS )
			oStmt:MakeErrorInfo(SELF, #__GetCursorName, nRetCode)
		ELSE
			SELF:cCursor := Psz2String( pszBuffer )
			oStmt:ErrInfo:ErrorFlag := FALSE
			lRet := TRUE
		ENDIF
		MemFree( pszBuffer )
	ELSE
		SQLThrowOutOfMemoryError()
		lRet := FALSE
	ENDIF
	RETURN lRet
	

METHOD __GetFieldName( uFieldPosition AS USUAL) AS STRING STRICT 
	//
	// Same AS FieldName but without autoexecute stuff
	// siFieldPosition is numeric
	// Returns fieldname as string
	//
	LOCAL nIndex AS DWORD
	LOCAL cRet   AS STRING
	
	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:__GetFieldName( "+AsString( uFieldPosition )+" )" )
	#ENDIF
	
	nIndex := SELF:__GetColIndex( uFieldPosition, FALSE )
	
	IF nIndex = 0 .OR. nIndex > nNumCols
		oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADFLD ), #FieldName )
	ELSE
		oStmt:ErrInfo:ErrorFlag := FALSE
		cRet := aSQLColumns[nIndex]:ColName
	ENDIF
	
	RETURN cRet
	

METHOD __GetLongData( nODBCType AS SHORTINT, nIndex AS DWORD ) AS LOGIC STRICT  
	LOCAL nRetCode AS INT
	LOCAL lRet     AS LOGIC
	LOCAL cBuffer  AS STRING
	LOCAL pTemp    AS PTR
	LOCAL nLen     AS LONGINT
	LOCAL nLenTot  AS DWORD
	LOCAL nLenNew  AS DWORD
	LOCAL nCType   AS SHORTINT
	LOCAL pBufTot  AS BYTE PTR
	LOCAL pBufTmp  AS BYTE PTR
	LOCAL pTemp1   AS BYTE PTR
	LOCAL nSize    AS INT
	LOCAL oCol     AS SQLColumn
	LOCAL nMax     AS LONGINT
	LOCAL oData	   AS SQLData
	
	pBufTot := NULL_PTR
	nLenTot := 0
	oCol   := SELF:aSQLColumns[nIndex]
	nLen   := oCol:Length
	nMax := __SQLMaxStringSize()
	
	IF nLen > nMax
		nlen := nMax
	ENDIF
	
	pTemp  := MemAlloc( DWORD(nLen ))
	IF pTemp == NULL_PTR
		SQLThrowOutOfMemoryError()
		RETURN FALSE
	ENDIF
    IF (nODBCType == SQL_LONGVARCHAR .OR. nODBCType == SQL_WLONGVARCHAR .OR. ;
        nODBCType == SQL_CHAR .OR. nODBCType == SQL_VARCHAR .OR.             ;
        nODBCType == SQL_WCHAR .OR. nODBCType == SQL_WVARCHAR )

		nCType := SQL_C_CHAR
	ELSE
		nCType := SQL_C_BINARY
	ENDIF
	oData	:= aSqlData[nIndex]
	DO WHILE TRUE
		nRetCode := SQLGetData( oStmt:StatementHandle,  ;
			WORD(nIndex),           ;
			nCType,                 ;
			pTemp,                  ;
			nLen,                   ;
			@nSize )
		
		IF nRetCode == SQL_SUCCESS   .OR. nRetCode == SQL_SUCCESS_WITH_INFO    

			IF nSize = SQL_NULL_DATA
				oData:Null := TRUE
				lRet := TRUE
				EXIT
			ELSE
				oData:Null := FALSE
			ENDIF
			
			//RvdH 070430 If they have a ZERO length string, get out of here
            IF nSize == 0

				oData:LongValue := NULL_STRING
				lRet := TRUE
				EXIT
			ENDIF
			
			IF pBufTot == NULL_PTR
				nLenTot := DWORD(nSize)
                pBufTot := MemAlloc( nLenTot )
				IF pBufTot == NULL_PTR
					SQLThrowOutOfMemoryError()
					lRet := FALSE
					EXIT
				ENDIF
				MemCopy( pBufTot, pTemp, nLenTot )
			ELSE
			    // For Character results we have to delete the zero terminator from the previous call
                // So we adjust the length of the old and new buffers 
                nLenNew := nLenTot + DWORD(nSize)    
                IF (nCType == SQL_C_CHAR)
                    nLenTot -= 1
                    nLenNew -= 1
                ENDIF
                pBufTmp := MemAlloc( nLenNew) 
                IF pBufTmp == NULL_PTR
                    SQLThrowOutOfMemoryError( )
                    lRet := FALSE
                    EXIT
                ENDIF          
                MemCopy( pBufTmp, pBufTot, nLenTot )
                pTemp1 := pBufTmp + nLenTot
                MemCopy( pTemp1, pTemp, DWORD(nSize) )
                MemFree( pBufTot )
                pBufTot := pBufTmp            
                nLenTot := nLenNew

			ENDIF
			IF nRetCode = SQL_SUCCESS  

			    cBuffer := Mem2String( pBufTot, nLenTot )
    			
			    //  UH 05/15/2000
			    //  cBuffer := __AdjustString( cBuffer )
			    IF ( nCType == SQL_C_CHAR ) .AND. ( nODBCType != SQL_LONGVARCHAR )  .AND. ( nODBCType != SQL_WLONGVARCHAR )
				    cBuffer := __AdjustString( cBuffer )
			    ENDIF
    			
			    oData:LongValue := cBuffer
			    lRet := TRUE
			    EXIT
			ENDIF
		ELSE
			oStmt:MakeErrorInfo(SELF, #__GetLongData, nRetCode)
			nSize := oCol:FieldSpec:Length + 1
			oData:LongValue := Space( nSize ) // Buffer( nSize )   dcaton 070330  Vulcan doesn't support Buffer()
			EXIT
		ENDIF
	ENDDO
	
	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:__GetLongData() returns " + cBuffer )
	#ENDIF
	
	MemFree( pTemp )      
	//RvdH 070430 pBufTot may be empty for zero length strings
	IF pBufTot != NULL_PTR
		MemFree( pBufTot )
	ENDIF
	
	RETURN lRet
	

METHOD __GetUpdateKey( lAppend AS LOGIC) AS STRING STRICT 
	LOCAL cWhere        AS STRING
	LOCAL nIndex        AS DWORD
	LOCAL nTemp         AS DWORD
	LOCAL aData         AS ARRAY
	LOCAL cQuoteChar		AS STRING
	LOCAL oCol			  AS SQLColumn
	
	cWhere := " where "
	
	IF lAppend
		aData := SELF:aAppendData
	ELSE
		aData := SELF:aSqlData
	ENDIF
	// RvdH 050413 Moved retrieval of QuoteChar out of the loop and added local oCol
	cQuoteChar 	:= SELF:oConn:IdentifierQuoteChar
	FOR nIndex := 1 TO ALen( aIndexCol )
		//
		//  use the original data value of the index...
		//
		nTemp := aIndexCol[nIndex]
		oCol	:= aSQLColumns[nTemp]
		
		cWhere += cQuoteChar
		cWhere += oCol:ColName
		cWhere += cQuoteChar
		cWhere += __GetDataValuePSZ( oCol,;
			aData[nTemp],;
			TRUE, ;
			TRUE )
		
		IF nIndex != ALen( aIndexCol )
			cWhere += " and "
		ENDIF
	NEXT
	
	RETURN cWhere
	

METHOD __GetUpdateStmt( nIndex AS DWORD, lAppend AS LOGIC) AS SqlStatement STRICT 
	LOCAL cRet  AS STRING
	LOCAL oRet  AS SQLStatement
	LOCAL nType AS DWORD
	
	cRet := "update " + cTableName
	cRet += " set "
	cRet += aSQLColumns[nIndex]:ColName
	cRet += " = ? "
	
	nType := SELF:__FigureScrollUpdateType()
	
	DO CASE
	CASE nType = SQL_SC_UPD_KEY
		cRet += SELF:__GetUpdateKEY( lAppend )
		
	CASE nType = SQL_SC_UPD_CURSOR
		cRet += SELF:__GetUpdateVAL( lAppend )
		//  cRet += __CAVOSTR_SQLCLASS__WHERE
		//  SELF:__GetCursorName()
		//  cRet += __CAVOSTR_SQLCLASS__CURR_OF + SELF:cCursor
		
	CASE nType = SQL_SC_UPD_VALUE
		cRet += SELF:__GetUpdateVAL( lAppend )
		
	ENDCASE
	oRet := oConn:__GetExtraStmt(cRet)
	//oRet := SQLStatement{ cRet, oConn }
	//  UH 07/10/2000
	oRet:SQLString := SELF:PreExecute( cRet )
	RETURN oRet
	

METHOD __GetUpdateVal( lAppend AS LOGIC) AS STRING STRICT 
	LOCAL cWhere  := "" AS STRING
	LOCAL nIndex        AS DWORD
	LOCAL aData         AS ARRAY
	LOCAL lStart        AS LOGIC
	LOCAL nODBCType     AS SHORTINT
	LOCAL cQuote        AS STRING
	LOCAL oCol			  AS SQLColumn
	
	
	IF lAppend
		aData := SELF:aAppendData
	ELSE
		aData   := SELF:aOriginalRecord
	ENDIF
	
	cWhere += " where "
	cQuote := SELF:oStmt:Connection:IdentifierQuoteChar
	
	FOR nIndex := 1 TO nNumCols
		oCol 		  := aSQLColumns[nIndex]
		nODBCType  := oCol:ODBCType
		
		IF ( nODBCType != SQL_LONGVARCHAR )    .AND. ;
				( nODBCType != SQL_LONGVARBINARY ) .AND. ;
				( nODBCType != SQL_VARBINARY )     .AND. ;
				( nODBCType != SQL_BINARY )		.AND. ;
				( nODBCTYPE != SQL_WLONGVARCHAR)
			
			IF lStart
				cWhere += " and "
			ELSE
				lStart := TRUE
			ENDIF
			
         
         cWhere += cQuote + oCol:ColName + cQuote + __GetDataValuePSZ( oCol, aData[nIndex], TRUE, TRUE )
			
			
		ENDIF
	NEXT
	
	RETURN cWhere
	

METHOD __GoCold(lForceAppend AS LOGIC, lForceUpdate AS LOGIC) AS LOGIC STRICT 
	// RvdH 050413 Added
	//  Append pending?
	IF lAppendFlag
		IF !SELF:AppendRow( lForceAppend )
			RETURN FALSE
		ENDIF
	ENDIF
	//  Was the current row modified?
	IF lRowModified
		IF !SELF:Update( lForceUpdate )
			RETURN FALSE
		ENDIF
	ENDIF
	
	RETURN TRUE
	

METHOD __InitColumnDesc() AS LOGIC STRICT 
	LOCAL nODBCType     AS SHORTINT
	LOCAL nPrecision    AS DWORD // dcaton 070206 was LONGINT
	LOCAL nDispSize     AS LONGINT
	LOCAL nScale        AS SHORTINT
	LOCAL nNullable     AS SHORTINT
	LOCAL lNullable     AS LOGIC
	LOCAL nIndex        AS WORD
	LOCAL nMaxColName   AS SHORTINT
	LOCAL pszColName    AS PSZ
	LOCAL cColName      AS STRING
	LOCAL cTrueName     AS STRING
	LOCAL nMax          AS SHORTINT
	LOCAL nDescType     AS WORD
	LOCAL aOldSQLColumns AS ARRAY
	LOCAL cOldAlias     AS STRING
	LOCAL nRetCode      AS INT
	LOCAL lDataField    AS LOGIC
	LOCAL nSize         AS DWORD
	LOCAL nLength       AS DWORD
	LOCAL  cType        AS STRING
	LOCAL cClassName    AS STRING
	LOCAL cCaption      AS STRING
	LOCAL oHlColumn     AS HYPERLABEL
	LOCAL oFieldSpec    AS FIELDSPEC
	LOCAL lRet          AS LOGIC
	LOCAL lLong         AS LOGIC
	LOCAL nTemp         AS DWORD
	LOCAL oColumn       AS SQLColumn
	LOCAL hStmt			  AS PTR
	LOCAL oData			  AS SQLData
	LOCAL pData			  AS BYTE PTR
	LOCAL pLength		  AS LONGINT PTR
	LOCAL nTotalSize	  AS DWORD
	LOCAL aLength		  AS ARRAY
	LOCAL aNull			  AS ARRAY   
	LOCAL nMaxString	  AS LONGINT
	LOCAL nMaxDisp		  AS LONGINT
	LOCAL lBinding			AS LOGIC   
	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:__InitColumnDesc()" )
	#ENDIF
	
	SELF:lColBind := FALSE
	
	SELF:NumResultCols()
   IF (nNumCols == 0)
      RETURN FALSE
   ENDIF
	
	IF ALen( aSQLColumns ) = 0
		aOldSQLColumns := {}
	ELSE
		aOldSQLColumns := aSQLColumns
	ENDIF
	
	SELF:aSQLColumns    := ArrayNew( nNumCols )
	SELF:aDataRecord    := ArrayNew( nNumCols )
	SELF:aSQLData       := SELF:aDataRecord
	
	aLength				  := ArrayNew(nNumCols)
	aNull   				  := ArrayNew(nNumCols)
	nTotalSize			  := 0
	nMaxString			  := __SQLMaxStringSize()
	nMaxDisp				  := __SQLMaxDisplaySize()
	
	SELF:__MemFree()
	SELF:apColmem 		  := {}
	
	//
	//  Init datafield array in super class ( DataServer )
	//  only if not already initialized
	//
	IF ALen( aDataFields ) = 0
		SELF:aDataFields := ArrayNew( nNumCols )
		lDataField := TRUE
	ELSE
		lDataField := FALSE
	ENDIF
	
	pszColName := MemAlloc( MAX_COLNAME_SIZE + 1 )
	IF pszColName == NULL_PSZ
		SQLThrowOutOfMemoryError()
		RETURN FALSE
	ENDIF
	
	lRet := TRUE
	
	
	//apColMem       := {}
	//aplColNulls    := {}
	nMaxBindColumn := 0
	
	SELF:aColumnAttributes := ArrayNew( nNumCols )
	hStmt := oStmt:StatementHandle
	FOR nIndex := 1 TO nNumCols
		
		nRetCode := SQLDescribeCol(hStmt ,  ;
			nIndex,                 ;
			pszColName,             ;
			MAX_COLNAME_SIZE,       ;
			@nMaxColName,           ;
			@nODBCType,             ;
			@nPrecision,            ;
			@nScale,                ;
			@nNullable )
		IF ( nRetCode != SQL_SUCCESS )
			oStmt:MakeErrorInfo(SELF, #__InitColumnDesc, nRetCode)
			lRet := FALSE
			
			EXIT
		ENDIF
		cTrueName := Psz2String( pszColName )
		cColName  := Upper( cTrueName )
		nTemp := At2( ".", cColName )
		IF nTemp > 0
			cColName := SubStr2( cColName, nTemp+1 )
		ENDIF
		
		IF nNullable = SQL_NULLABLE
			lNullable := TRUE
		ELSE
			lNullable := FALSE
		ENDIF
		
		//RvdH 070509 if nScale > 0 add nScale+1 to nPrecision for the decimal separator
		IF nScale > 0
			nPrecision := nPrecision + nScale + 1
		ENDIF
		
		nDescType := SQL_COLUMN_DISPLAY_SIZE
		nMax := 0
		nDispSize := 0
		nRetCode := SQLColAttributes(   hStmt,;
			nIndex,               ;
			nDescType,            ;
			NULL_PTR,             ;
			0,                  ;
			@nMax,                ;
			@nDispSize )
		
		IF nRetCode != SQL_SUCCESS
			oStmt:MakeErrorInfo(SELF, #__InitColumnDesc, nRetCode)
			lRet := FALSE
			EXIT
		ENDIF
		
		
		nSize := ALen( aOldSQLColumns )                     
		IF nSize = 0
			cOldAlias := NULL_STRING
		ELSEIF nSize >= nIndex .AND. aOldSQLColumns[nIndex]:ColName = cColName
			cOldAlias := aOldSQLColumns[nIndex]:AliasName
		ENDIF
		
		nLength := nPrecision
		//RvdH 050429 Change new ODBC types to Older types
		DO CASE
		CASE nODBCType == SQL_TYPE_DATE
			nODBCType := SQL_DATE
		CASE nODBCType == SQL_TYPE_TIME
			nODBCType := SQL_TIME
		CASE nODBCType == SQL_TYPE_TIMESTAMP
			nODBCType := SQL_TIMESTAMP
		OTHERWISE
			// Do nothing
		ENDCASE
		IF SELF:lTimeStampAsDate .AND. nODBCType = SQL_TIMESTAMP
			nODBCType := SQL_DATE
		ENDIF
		
		cType :=  __ODBCType2FSpec( nODBCType, @nLength, @nScale )
		
		#IFDEF __DEBUG__
			__SQLOutputDebug(   " ColName=" + cColName +             ;
				" nODBCType=" + AsString( nODBCType )+ ;
				" Prec=" + AsString( nPrecision ) +    ;
				" nScale=" + AsString( nScale )   +    ;
				" lNullable=" + AsString( lNullable ) )
		#ENDIF
		
		cClassName := Symbol2String( ClassName( SELF ) )

		// For Numeric Types adjust Precision and Disp Size	
		IF cType == "N"
			IF nPrecision > DWORD(nDispSize)
				nPrecision := DWORD(nMaxString)
				nDispSize  := nMaxDisp
			ELSE
				nPrecision := DWORD(nDispSize)  
				nLength	  := nPrecision
			ENDIF
		ELSEIF cType == "M"
			IF nPrecision > DWORD(nMaxString)
				nPrecision := DWORD(nMaxString)
			ENDIF
			IF nDispSize > nMaxDisp .OR. nDispSize < 0
				nDispSize := nMaxDisp
			ENDIF     
// 		ELSEIF cType == "C"
// 			// Add 1 extra character for the terminating ZERO
// 			nLength += 1
		ELSE
			// LEave
		ENDIF
		#IFDEF __DEBUG__
			__SQLOutputDebug( "  Precision  : " + AsString( nPrecision ) )
			__SQLOutputDebug( "  DisplaySize: " + AsString( nDispSize ) )
		#ENDIF
		
		
		cCaption   := cColName
		oHlColumn  := HyperLabel{cColName, cCaption, cClassName + ": " + cColName,  cClassName }
		
		#IFDEF __DEBUG__
			__SQLOutputDebug( " TYPE = " + cType )
		#ENDIF
		
		//RvdH 050429 Take Length from ODBC
		DO CASE
		CASE nODBCType = SQL_TIME
			//  UH 12/12/2001
			//nLength := IIF( nLength == 12, nLength, 12 )
			nLength	:= SQL_TIME_LEN +1     //RvdH 070501 1 byte extra for string delimiter
			IF nPrecision > 0
				nLength += nPrecision + 1
			ENDIF
		CASE nODBCType = SQL_DATE
			nLength	:= SQL_DATE_LEN  +1   //RvdH 070501 1 byte extra for string delimiter
		CASE nODBCType = SQL_TIMESTAMP
			//  UH 12/12/2001
			//nLength := IIF( nLength == 23, nLength, 23 )
			nLength := SQL_TIMESTAMP_LEN  +1 //RvdH 070501 1 byte extra for string delimiter
			IF nPrecision > 0
				nLength += nPrecision + 1
			ENDIF
		ENDCASE
		oFieldSpec := FieldSpec { oHlColumn, cType, nLength, nScale  }
		oFieldSpec:Nullable := lNullable
		DO CASE
		CASE nODBCType = SQL_TIME
			oFieldSpec:Picture := SubStr3( "99:99:99.999999", 1, nLength )
			
		CASE nODBCType = SQL_TIMESTAMP
			oFieldSpec:Picture := SubStr3( "9999-99-99 99:99:99.999999", 1, nLength )
			
		CASE ( nODBCType = SQL_LONGVARCHAR )   .OR. ;
				( nODBCType = SQL_LONGVARBINARY ) .OR. ;
				( nODBCType = SQL_VARBINARY )     .OR. ;
				( nODBCTYPE = SQL_WLONGVARCHAR)   .OR. ;
				( nODBCType = SQL_BINARY )
			lLong := TRUE
		ENDCASE
		
		oColumn := SQLColumn{   oHlColumn , ;
			oFieldSpec, ;
			nODBCType,  ;
			nScale,     ;
			lNullable,  ;
			nIndex,     ;
			cTrueName,  ;
			cOldAlias }
		oColumn:Length := LONGINT(nLength)
		IF ( nODBCType = SQL_LONGVARCHAR ) .OR. ;
				( nODBCType = SQL_LONGVARBINARY ) .OR. ;
				( nODBCTYPE = SQL_WLONGVARCHAR)
			oColumn:Length      := IIF(LONGINT(_CAST, nPrecision) != SQL_NO_TOTAL, LONGINT(nPrecision), nMaxString )   // dcaton 070206 added INT cast on nPrecision to strongly type iif()
			
			//  UH 01/11/2000
			//  JSP 09/05/2000
			//  oColumn:DisplaySize := IIf( nDispSize != SQL_NO_TOTAL, nDispSize, __SQLMaxStringSize() )
			//  oColumn:DisplaySize := IIf( nDispSize != SQL_NO_TOTAL, nDispSize, __SQLMaxDisplaySize() )
			IF nDispSize = SQL_NO_TOTAL
				oColumn:DisplaySize := Min( nDispSize, 0x7FFFFFFF )
			ELSE
				oColumn:DisplaySize := DWORD(nMaxDisp)
			ENDIF
		ENDIF
		// Add 1 to Length for terminators
		nLength += 1		

		aSQLColumns[nIndex] := oColumn
		
		aLength[nIndex]	:= nLength
		aNull  [nIndex]	:= lNullable
		
		// Now Adjust the size info:
      // Increase with nLength rounded at 4 bytes and add 4 bytes 'to be safe'
      // So Round nLength up to mutiple of 4
      nLength    :=  (nLength / _SIZEOF(LONGINT)) * _SIZEOF(LONGINT) + _SIZEOF(LONGINT)         
      nTotalSize +=  nLength  
		
		IF lDataField
			aDataFields[nIndex] := DataField{oHlColumn,oFieldSpec}
		ENDIF
	NEXT
	
	// Now we know the total size needed for column binding
	// Allocate the buffer for the column binding
	SELF:nBufferLength := nTotalSize
	pData					  := MemAlloc(nTotalSize )
	pLength				  := MemAlloc(nNumCols * _SIZEOF(LONGINT))
	IF (pData == NULL_PTR .OR. pLength == NULL_PTR)
		SQLThrowOutOfMemoryError()
	ENDIF

	AAdd(apColMem, pData)
	AAdd(apColMem, pLength)
	
	
	// Build array of SQLData Objects
	lBinding	:= TRUE	
	FOR nIndex := 1 TO nNumCols 
		lNullable := aNull[nIndex]
		nLength	 := aLength[nIndex]
		oData := SqlData{}
		oData:Initialize(pData, pLength, nLength, lNullable, FALSE)
		aSQLData[nIndex] 	:= oData

      nLength  :=  (nLength / _SIZEOF(LONGINT)) * _SIZEOF(LONGINT) + _SIZEOF(LONGINT)                      
      pData    += nLength 
		pLength	+= 1
		
		IF lBinding .AND. SELF:BindColumn( nIndex )
			nMaxBindColumn := nIndex
		ELSE
			lBinding := FALSE
		ENDIF
		
		
	NEXT
	
	// Now setup memory for other arrays
	SELF:aOriginalRecord  	:= ArrayNew( nNumCols )
	
	// Allocate 'Original' record buffers
	pData 	:= MemAlloc(nTotalSize)
	pLength  := MemAlloc(nNumCols * _SIZEOF(LONGINT))
	IF (pData == NULL_PTR .OR. pLength == NULL_PTR)
		SQLThrowOutOfMemoryError()
	ENDIF
	AAdd(apColMem, pData)
	AAdd(apColMem, pLength)
	
	FOR nIndex := 1 TO nNumCols
		nLength 	:= aLength[nIndex]
		// Allocate Object for Original record
		oData := SQLData{}
		oData:Initialize(pData, pLength, nLength, TRUE, FALSE)
		SELF:aOriginalRecord[nIndex] := oData
		
      // Increase Pointer with nLength rounded at 4 bytes and 4 bytes 'to be safe'     
      nLength     := (nLength / _SIZEOF(LONGINT)) * _SIZEOF(LONGINT) + _SIZEOF(LONGINT)
      pData       += nLength 
		pLength	+= 1
	NEXT
	
	// AppendData now used the same array as aOriginalRecord
	//		SELF:aAppendData		 	:= ArrayNew( nNumCols )
	// 	// Allocate 'Append Data ' record buffers
	// 	pData 	:= MemAlloc(nTotalSize)
	// 	pLength  := MemAlloc(nNumCols * _sizeof(LONG))
	// 	AAdd(apColMem, pData)
	// 	AAdd(apColMem, pLength)
	// 	FOR nIndex := 1 TO nNumCols
	// 		nLength 	:= aLength[nIndex]
	// 		// Allocate Object for Append Data
	//       oData := SQLData{}
	//       oData:Initialize(pData, pLength, nLength, TRUE, FALSE)
	// 		SELF:aAppendData[nIndex] := oData
	// 		pData 	+= nLength + 1		// One extra to be sure...
	// 		pLength	+= 1
	// 	NEXT
	SELF:aAppendData := SELF:aOriginalRecord
	
	MemFree( pszColName )
	
	IF nMaxBindColumn > 0
		SELF:lColBind := TRUE
	ENDIF
	
	IF lRet
		oStmt:ErrInfo:ErrorFlag := FALSE
	ENDIF
	
	IF lLong .AND. ( SELF:nScrollUpdateType == SQL_SC_UPD_AUTO )
		SELF:nScrollUpdateType := SQL_SC_UPD_VALUE
	ENDIF
	
	RETURN TRUE
	

METHOD __InitColValue( nIndex AS DWORD ) AS LOGIC STRICT 
	LOCAL nSize         AS INT
	LOCAL lRet          AS LOGIC
	LOCAL nODBCType     AS SHORTINT
	LOCAL nRetCode      AS INT
	LOCAL nNull         AS INT
	LOCAL nLen          AS INT
	LOCAL pLength       AS LONG PTR
	LOCAL oSQLColumn    AS SQLColumn
	LOCAL oData			  AS SqlData
	LOCAL pTemp			  AS PTR
	
	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:__InitColValue( "+AsString( nIndex )+" )" )
	#ENDIF                      
	//RvdH 070716 Moved initialization from appended records from Append()
	IF SELF:lAppendFlag
		oData	   				:= aAppendData[nIndex]
		oData:Null 			:= TRUE
		oData:ValueChanged 	:= FALSE
		oData:LongValue 		:= NULL_STRING
		nSize 					:= aSQLColumns[nIndex]:FieldSpec:Length + 1
		// numeric?
		pTemp := oData:ptrValue
		IF __GetStringFromODBCType( aSQLColumns[nIndex]:ODBCType ) = "N"
			MemSet( pTemp, 0, DWORD(nSize) )
		ELSE
			MemSet( pTemp, SQL_BLANK_CHARACTER, DWORD(nSize) )
		ENDIF
		lRet := TRUE
	ELSE
		oSQLColumn 			:= aSQLColumns[nIndex]
		nODBCType  			:= oSQLColumn:ODBCType
		oData	   			:= aSQLData[nIndex]
		oData:ValueChanged := FALSE
		// if Column bound ?
		IF nIndex > 0 .AND. nIndex <= SELF:nMaxBindColumn
			
			pLength 	:= oData:ptrLength
			nNull 	:= pLength[1]
			
			IF nNull = SQL_NULL_DATA
				oData:Null := TRUE
				IF nODBCType = SQL_LONGVARCHAR   .OR. nODBCType = SQL_WLONGVARCHAR
					oData:LongValue := NULL_STRING // Space( 10 )
				ENDIF
			ELSE
				oData:Null := FALSE
				IF nODBCType = SQL_LONGVARCHAR .OR. nODBCType = SQL_WLONGVARCHAR
					// When Long column is bound we get its value now
					oData:LongValue := Psz2String( oData:ptrValue)
				ENDIF
			ENDIF
			
			#IFDEF __DEBUG__
				__SQLOutputDebug( "** Data found in bound Column" )
			#ENDIF
			lRet := TRUE
		ELSE
			
			IF ( nODBCType = SQL_LONGVARCHAR ) .OR. ( nODBCType = SQL_LONGVARBINARY )   .OR. ( nODBCType = SQL_WLONGVARCHAR )
				nLen := oSQLColumn:Length  + 1 
				lRet := SELF:__GetLongData( nODBCType, nIndex )
				
			ELSE
				nLen := oSQLColumn:FieldSpec:Length + 1
				oData:Clear()
				nODBCType := SQLType2CType( nODBCType )
				
				// Get the column data into the buffer( s )
				nRetCode := SQLGetData( oStmt:StatementHandle, WORD(nIndex),   ;
					nODBCType,                                  ;
					oData:ptrValue ,   ;
					nLen, ;
					@nSize )
				
				IF ( nRetCode != SQL_SUCCESS )
					oStmt:MakeErrorInfo(SELF, #__InitColValue, nRetCode)
					
				ELSE
					IF nSize = SQL_NULL_DATA
						oData:Null := TRUE
					ELSE
						oData:Null := FALSE
					ENDIF
					lRet := TRUE
				ENDIF
			ENDIF
		ENDIF
	ENDIF
	RETURN lRet
	

METHOD __MemFree() AS VOID STRICT 
	LOCAL i AS DWORD
	IF apColMem != NULL_ARRAY
		#IFDEF __DEBUG__
			__SQLOutputDebug( "** SQLSelect:__MemFree()" )
		#ENDIF
		FOR i := 1 TO ALen( apColMem )
			
			IF IsPtr(apColMem[i] ) .AND. apColMem[i] != NULL_PTR
				MemFree( apColMem[i])
			ENDIF
			
		NEXT  //Vulcan.NET-Transporter: i
	ENDIF
	SELF:apColMem    	 := NULL_ARRAY
	SELF:nBufferLength := 0
	RETURN
	

METHOD  __PrepareStmtOptions( oStatement AS SQLStatement ) AS VOID STRICT 
	oStatement:CursorType        := SELF:oStmt:CursorType
	oStatement:ScrollConcurrency := SELF:oStmt:ScrollConcurrency
	oStatement:SimulateCursor    := SELF:oStmt:SimulateCursor
	RETURN
	

METHOD __PutLongData( cValue AS STRING, nODBCType AS SHORTINT, nIndex AS DWORD, ;
		lAppend AS LOGIC ) AS STRING STRICT 
	//
	//  Generate SQL Command:
	//
	//  INSERT INTO <table> ( <colname> ) VALUES ( ? )
	//
	LOCAL nRetCode      AS INT
	LOCAL oUpdate       AS SQLStatement
	//LOCAL oCol          AS SQLColumn
//	LOCAL nPrecision    AS LONGINT
//	LOCAL nScale        AS INT
	LOCAL nLength       AS DWORD
	LOCAL nDataAtExec   AS INT
   //LOCAL nData         AS DWORD
   //LOCAL nMax          AS DWORD
   LOCAL pData         AS BYTE PTR
   //LOCAL pEnd          AS BYTE PTR
	LOCAL hStmt         AS PTR
	LOCAL cStmt         AS STRING
	LOCAL aData         AS ARRAY
	LOCAL nCType        AS SHORTINT
	LOCAL oData			  AS SQLData
	//  __SQLOutputDebug( "__PutLongData() with " + cValue )
	
   //nMax := __SQLMaxStringSize()
	
	
	oUpdate := SELF:__GetUpdateStmt( nIndex, lAppend )
	
	IF oUpdate = NULL_OBJECT
		RETURN NULL_STRING
	ENDIF
	
	//oCol 			:= SELF:aSQLColumns[nIndex]
//	nPrecision 	:= oCol:Length
	nLength    	:= SLen( cValue )
	
//	nScale:= oCol:Scale
	hStmt := oUpdate:StatementHandle
	cStmt := oUpdate:SQLString
	nRetCode := SQLPrepare( hStmt, String2Psz( cStmt ), SQL_NTS )
	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:__PutLongData()" )
		__SQLOutputDebug( "   Data: " + cValue )
		__SQLOutputDebug( "   " + cStmt )
	#ENDIF
	
	IF nRetCode = SQL_SUCCESS
      //       IF nLength >= nMax
      //          nData := nMax
      //       ELSE
      //          nData := nLength
      //       ENDIF
		
		IF nODBCType = SQL_LONGVARCHAR   .OR. nODBCType = SQL_WLONGVARCHAR
			nCType := SQL_C_CHAR
			
			//nData++
			nLength++
		ELSE
			nCType := SQL_C_BINARY
		ENDIF
		
		nIndex := 1
      //RvdH 071030 Fix for Memo fields that did not update
      pData := String2Psz(cValue )
      nDataAtExec := INT(nLength)
      SQLBindParameter(   hStmt,                  ;
         1,                      ; // Position
         SQL_PARAM_INPUT,        ; // Direction
         nCType,                 ; // C Type
         nODBCType,              ; // SQL Type
         nLength,                ; // Length
         0,                      ; // Scale
         pData,                  ; // Value
         INT(nLength),           ; // max length of value
         @nDataAtExec )            // Parameter length
		
		nRetCode := SQLExecute( hStmt )
	ENDIF
	
	IF nRetCode = SQL_SUCCESS
		oUpdate:ErrInfo:ErrorFlag := FALSE
	ELSE
		oUpdate:MakeErrorInfo(SELF, #__PutLongData, nRetCode)
		
		SELF:__CopySQLError( #__PutLongData, oUpdate:ErrInfo )
		cValue := Space( 10 )
		
		#IFDEF __DEBUG__
			__SQLOutputDebug( "   failed, return value: " + NTrim( nRetCode ) )
		#ENDIF
		
		
	ENDIF
	SELF:oConn:__CloseExtraStmt(oUpdate)
	
	IF lAppend
		aData := SELF:aAppendData
	ELSE
		aData := SELF:aSqlData
	ENDIF
	oData	:= aData[nIndex]
	oData:ValueChanged := FALSE  // don't force any other UPDATE
	RETURN cValue
	

METHOD __RecCount() AS LONGINT STRICT 
	LOCAL cStatement    AS STRING
	LOCAL cCount        AS STRING
	LOCAL nPos          AS DWORD
	LOCAL l             AS LONGINT
	LOCAL nRet          AS LONGINT
	LOCAL nRetCode      AS LONGINT
	LOCAL oCount        AS SQLStatement
	LOCAL pLong         AS LONGINT PTR
	LOCAL pData         AS LONGINT PTR 
	LOCAL nCount		  AS DWORD
	LOCAL nRecs			  AS LONGINT
	LOCAL lRecCount	  AS LONGINT
	LOCAL aParams		  AS ARRAY   
	LOCAL cQuoteChar	  AS STRING
	//_DebOut32(String2Psz("Start __RecCount "+NTrim(Seconds())))
	
	//RvdH 050413 Centralize opening of cursor
	IF ! SELF:__ForceOpen()
		RETURN 0
	ENDIF
	
   IF SELF:lLastRecFound
      RETURN SELF:nLastRecNum
   ENDIF 
	
   //RvdH 050413 Some drivers return negative number as 'guestimate' of # of rows
   nRecs := oStmt:RecCount
   IF ( nRecs != -1  .AND. nRecs != 1)
      SELF:nNumRows := Abs(nRecs)  
      SELF:lLastRecFound := TRUE
      SELF:nLastRecNum :=SELF:nNumRows       
      RETURN SELF:nNumRows
   ENDIF
	
	
	BEGIN SEQUENCE
		nRet 			:= 0L
		cStatement 	:= SELF:oStmt:SQLString
		
		// RvdH 061218 Suggestion from Robson, Dec 2006
		// create subselect for more complex commands SQL
		//RvdH 070507 Changed Instr(.. , upper(..)) to Atc to avoid capitalizing the statement 3 times !
		//      IF Instr( "UNION",   Upper(cStatement) ) .OR.;
		//          Instr( "DISTINCT", Upper(cStatement) ) .OR.;
		//          Instr( "GROUP BY", Upper(cStatement) )

		IF AtC( "UNION",   cStatement ) > 0 .OR. ;
				AtC( "DISTINCT", cStatement ) > 0 .OR. ;
				AtC( "GROUP BY", cStatement ) > 0
			//RvdH 070507 Added alias for subselect (suggestion Markus Feser)
			cQuoteChar := oStmt:Connection:IdentifierQuoteChar                       
         cCount := "select count( 1 ) from ( " + AllTrim(cStatement) + " ) "+cQuoteChar+"VOSQLSELECTCOUNTER"+cQuoteChar
			
		ELSE
			//  UH 08/09/2001
			nPos := AtC( " ORDER BY ", cStatement )
			IF nPos > 0
				cStatement := Left( cStatement,nPos-1 )
			ENDIF
			
			//  UH 08/09/2001
			//  nPos :=  AtC( "FROM", cStatement )
			nPos :=  AtC( " FROM ", cStatement )
			
         IF nPos > 0
            cCount := "select count ( * ) " + SubStr2( cStatement, nPos )
         ELSE
            cCount := "select count ( * ) " + SELF:cTableName
         ENDIF
		ENDIF
		oCount := SELF:oConn:__GetExtraStmt(cCount)
		aParams :=SELF:oStmt:__Params  
		nCount := ALen( aParams )
		
		IF nCount > 0
			IF ! oCount:__SetParameters( aParams)
				BREAK
			ENDIF
		ENDIF

		oCount:SQLString := SELF:PreExecute( oCount:SQLString )
		
		pData := MemAlloc( _SIZEOF( LONGINT ) )
		IF pData = NULL_PTR
			SQLThrowOutOfMemoryError()
			BREAK
		ENDIF
		pData[1]	:= 0L
		
		pLong := @lRecCount
		lRecCount := 0
		
		nRetCode := SQLBindCol( oCount:StatementHandle, 1,  ;
			SQL_C_SLONG ,               ;
			pData,                      ;
			4,                          ;
			pLong )
		
		IF nRetCode == SQL_SUCCESS
			oCount:ScrollConcurrency := SQL_CONCUR_READ_ONLY
			l := LONGINT(SLen( cCount ))
			nRetCode := SQLExecDirect( oCount:StatementHandle, String2Psz(cCount ),  l  )
			
			IF nRetCode = SQL_SUCCESS .OR. nRetCode = SQL_SUCCESS_WITH_INFO
				nRetCode := SQLFetch( oCount:StatementHandle )
				IF nRetCode == SQL_SUCCESS
					nRet := pData[1]
					SELF:nlastRecNum   := nRet
					SELF:lLastRecFound := TRUE
				ENDIF
			ENDIF
		ENDIF
	RECOVER
	
	END SEQUENCE
	IF pData != NULL_PTR
		MemFree( pData )
	ENDIF
	SELF:oConn:__CloseExtraStmt(oCount)
	//_DebOut32(String2Psz("End __RecCount "+NTrim(Seconds())))
	SELF:nNumRows := nRet
	RETURN nRet
	
	

METHOD __Reset() AS LOGIC STRICT 
	
	RETURN oStmt:__Reset()
	

METHOD __SetCursorName( cCursorName AS STRING) AS LOGIC STRICT 
	LOCAL nRetCode      AS INT
	LOCAL lRet          AS LOGIC
	
	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:__SetCursorName()" )
	#ENDIF
	
	IF ( oStmt:StatementHandle = SQL_NULL_HSTMT )
		SELF:__AllocStmt()
	ENDIF
	
	nRetCode := SQLSetCursorName(   oStmt:StatementHandle,  ;
		String2Psz(cCursorName),;
		_SLen( cCursorName ) )
	
	IF ( nRetCode != SQL_SUCCESS )
		oStmt:MakeErrorInfo(SELF, #__SetCursorName, nRetCode)
	ELSE
		SELF:cCursor := cCursorName
		oStmt:ErrInfo:ErrorFlag := FALSE
		lRet := TRUE
	ENDIF
	
	RETURN lRet
	
	

METHOD __SetNullData( nODBCType AS SHORTINT, nIndex AS DWORD, aData AS ARRAY) AS VOID STRICT 
	LOCAL oData 	AS SqlData
	
	oData 				 := aData[nIndex]
	oData:Null         := TRUE
	oData:ValueChanged := TRUE
	SELF:lRowModified  := TRUE
	IF nODBCType = SQL_LONGVARCHAR .OR. nODBCType = SQL_LONGVARBINARY .OR. nODBCType = SQL_WLONGVARCHAR
		oData:LongValue := NULL_STRING // Space( 10 )	///RvdH ?
	ELSE
		//RvdH 050429 Clear Data
		oData:Clear()
		//MemClear(PTR(_CAST, oData:Value), oData:Length)
	ENDIF
	RETURN
	

METHOD __SetRecordFlags( lBofNew AS USUAL, lEofNew AS USUAL) AS VOID STRICT 
	
	IF IsLogic( lBofNew )
		SELF:lBof := lBofNew
	ENDIF
	
	IF IsLogic( lEofNew )
		//
		//  Do we move the pointer from
		//  phantom record back to the scope ?
		//
		IF !lEofNew .AND. SELF:lEof
			SELF:aSQLData := SELF:aDataRecord
		ENDIF
		
		SELF:lEof := lEofNew
	ENDIF
	SELF:aSQLData := SELF:aDataRecord
	RETURN
	

METHOD __SetScrollOptions( nConcurrency AS DWORD, nKeySet AS DWORD, lAsync AS LOGIC) AS LOGIC STRICT  
	
	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:__SetScrollOptions()" )
	#ENDIF
	
	RETURN oStmt:__SetScrollOptions( nConcurrency, nKeyset, lAsync )
	

METHOD __SkipCursor( nRecCount AS LONGINT) AS LOGIC STRICT 
	
	LOCAL nFetchType        AS INT
	LOCAL lRet              AS LOGIC
	LOCAL lWasAppending		AS LOGIC
	// RvdH 050413 Optimized by using _GoCold
	
	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:__SkipCursor( " + NTrim( nRecCount )+" )" )
	#ENDIF
	
	IF nRecCount < 0
		IF SELF:lBof .AND. SELF:nLastRecNum > 0
			SELF:__SetRecordFlags( FALSE, NIL )
		ENDIF
	ENDIF
	lWasAppending := SELF:lAppendFlag
	IF ! SELF:__GoCold(TRUE, TRUE)
		RETURN FALSE
	ENDIF
	IF nRecCount = 1
		nFetchType := SQL_FETCH_NEXT
		
		lRet := SELF:ExtendedFetch( nFetchType, nRecCount )
		
		IF SELF:lEof .AND. SELF:nLastRecNum > 0
			SELF:__SetRecordFlags( FALSE, NIL )
		ENDIF
		
	ELSEIF nRecCount = -1
		nFetchType := SQL_FETCH_PREV
		
		IF lWasAppending
			RETURN SELF:ExtendedFetch( SQL_FETCH_LAST, 0 )
		ENDIF
		
		lRet := SELF:ExtendedFetch( nFetchType, 1 )
		
	ELSE
		nFetchType := SQL_FETCH_RELATIVE
		
		
		lRet := SELF:ExtendedFetch( nFetchType, nRecCount )
	ENDIF
	
	RETURN lRet
	
	

METHOD __UpdateLongData( lAppend AS LOGIC ) AS VOID STRICT 
	LOCAL nIndex    AS DWORD
	LOCAL nODBCType AS SHORTINT
	LOCAL cTemp     AS STRING
	LOCAL aData     AS ARRAY
	LOCAL oData 	AS SqlData
	LOCAL oCol		AS SqlColumn
	// RvdH 050413 Optimized by using local oCol
	IF lAppend
		aData := SELF:aAppendData
	ELSE
		aData := SELF:aSqlData
	ENDIF
	FOR nIndex := 1 TO SELF:nNumCols
		oCol := aSQLColumns[nIndex]
		nODBCType := oCol:ODBCType
		IF ( nODBCType = SQL_LONGVARCHAR ) .OR. ( nODBCType = SQL_LONGVARBINARY ) .OR. ( nODBCType = SQL_WLONGVARCHAR )
			oData := aData[nIndex]
			IF oData:ValueChanged .AND. ! oData:Null
				//cTemp := AsString( oData:Value )
				cTemp := oData:LongValue
				SELF:__PutLongData( cTemp, nODBCType, nIndex, lAppend )
			ENDIF
		ENDIF
	NEXT
	RETURN
	
END CLASS

