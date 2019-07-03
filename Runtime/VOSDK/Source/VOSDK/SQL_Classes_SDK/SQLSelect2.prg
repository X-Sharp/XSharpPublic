PARTIAL CLASS SQLSelect

METHOD AddDateVal( uFieldPos, dDate ) 
	//  Adds time string to Timestamp field
	LOCAL cVal  AS STRING
	LOCAL cDate AS STRING
	LOCAL cTime AS STRING

	IF !IsDate( dDate )
		dDate := Today()
	ENDIF

	cVal := SELF:GetTimestamp( uFieldPos )
	IF IsNil( cVal )
		RETURN NIL
	ENDIF

	cDate := DToCSQL( dDate )
	IF SLen( cVal ) = 0
		cTime := Time()
	ELSE
		cTime := SubStr2( cVal, 12 )
	ENDIF
	cVal := cDate + " " + cTime
	RETURN SELF:SetTimeStamp( uFieldPos, cVal )

METHOD AddTimeString( uFieldPos, cTime ) 
	//  Adds time string to Timestamp field
	LOCAL cVal  AS STRING
	LOCAL cDate AS STRING

	IF !IsString( cTime )
		cTime := Time()
	ENDIF

	IF At2( ".", cTime ) == 0
		cTime := cTime + ".000000"
	ENDIF
	cVal := SELF:GetTimestamp( uFieldPos )
	IF IsNil( cVal )
		RETURN NIL
	ENDIF
	IF SLen( cVal ) = 0
		cDate := DToCSQL( Today() )
	ELSE
		cDate := SubStr3( cVal, 1, 11 )
	ENDIF
	cVal := cDate + " " + cTime
	RETURN SELF:SetTimeStamp( uFieldPos, cVal )




METHOD Append() 
	LOCAL nIndex    AS DWORD
// 	LOCAL nSize     AS INT
// 	LOCAL pTemp     AS PTR
// 	LOCAL oData		AS SqlData

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:Append()" )
	#ENDIF
	IF !SELF:Notify( NOTIFYINTENTTOMOVE )
		oStmt:__GenerateSQLError( "IntentToMove returned false.", #Append )
		RETURN FALSE
	ENDIF
	//RvdH 050413 Centralize opening of cursor
	IF ! SELF:__ForceOpen()
		RETURN FALSE
	ENDIF
	// RvdH 050413 Centralize GoCold behavior
	IF !SELF:__GoCold(TRUE, TRUE)
		RETURN FALSE
	ENDIF

	IF !lFetchFlag
		SELF:__GetColIndex( 1, TRUE )
	ENDIF

   // Set Append/Deleted Flags and set EOF/BOF Flags to FALSE
   SELF:lAppendFlag := TRUE
   SELF:lDeleteFlag := FALSE
   SELF:__SetRecordFlags(FALSE, FALSE)

   // initialize SQLData objects...
   FOR nIndex := 1 TO nNumCols
		SELF:__InitColValue(nIndex) 
	NEXT

	SELF:lReadColumnInfo		 := TRUE 		//RvdH 070716 Abused to remember lAppendFlag 
	SELF:lNotifyIntentToMove := FALSE
	SELF:nAppendRecNum 		 := nRecNum    //  save last recno
	SELF:nRecNum 				 := SELF:nLastRecNum + 1
	SELF:Notify( NOTIFYAPPEND )
	#IFDEF __DEBUG__
		__SQLOutputDebug( "   Recnum:      " + NTrim( SELF:nRecnum ) )
		__SQLOutputDebug( "   nLastRecNum: " + NTrim( SELF:nLastRecnum ) )
	#ENDIF

	RETURN TRUE

METHOD AppendRow( lForce ) 

	LOCAL nIndex    AS DWORD
	LOCAL sInsert   AS STRING
	LOCAL cValue    AS STRING
	LOCAL oInsert   AS SQLStatement
	LOCAL nCount    AS SHORTINT
	LOCAL lRet      AS LOGIC
	LOCAL nODBCType AS SHORTINT
	LOCAL aLongData AS ARRAY
	LOCAL nLongData AS DWORD
	LOCAL oColumn 	AS SqlColumn
	LOCAL oData		AS SqlData
	LOCAL cQuote	AS STRING
	LOCAL sValues	AS STRING

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:AppendRow( "+AsString( lForce )+" )" )
	#ENDIF

	lAppendFlag  := FALSE
	IF SELF:lRowModified
		lRowModified := FALSE
	ELSE
		#IFDEF __DEBUG__
			__SQLOutputDebug( "**  suppressed because not modified " )
		#ENDIF

		RETURN TRUE
	ENDIF

	IF IsLogic( lForce ) .AND. !lForce .AND. !SELF:lNotifyIntentToMove
		#IFDEF __DEBUG__
			__SQLOutputDebug( "**  suppressed by !lNotifyIntentToMove" )
		#ENDIF

		RETURN TRUE
	ENDIF

	sInsert 		:= "insert into " + SELF:cTableName + " ( "
	sValues 		:= ""
	nCount  		:= 0
	cQuote 		:= SELF:oConn:IdentifierQuoteChar
	aLongData   := {}
	nLongData   := 0
	//RvdH 050413 Three loops now combined in one
	FOR nIndex := 1 TO nNumCols
		oData := aAppendData[nIndex]
		IF oData:ValueChanged
			oColumn 		:= aSQLColumns[nIndex]
			nODBCType  	:= oColumn:ODBCType
			IF nODBCType = SQL_LONGVARCHAR .OR. nODBCType = SQL_LONGVARBINARY  .OR. nODBCType = SQL_WLONGVARCHAR
				// For Long fields we save the data to aLongData and we write a dummy space character
				cValue := "' '"
				AAdd( aLongData, { nIndex, oData:LongValue } )
				nLongData++

			ELSE
				cValue := __GetDataValuePSZ( oColumn, oData, FALSE, FALSE )
			ENDIF
			IF nCount > 0
				sInsert += ","
				sValues += ","
			ENDIF
			sInsert +=  cQuote + oColumn:ColName  + cQuote
			IF ( oData:Null )
				sValues += "NULL"
			ELSE
				sValues += cValue
			ENDIF
			++nCount

		ENDIF
	NEXT

	IF nCount = 0
		lAppendFlag := FALSE
		SELF:nRecNum := SELF:nAppendRecNum
		RETURN TRUE
	ENDIF

	sInsert += ") values ( "+sValues + " ) "


	oInsert := oConn:__GetExtraStmt(sInsert)

	SELF:__PrepareStmtOptions( oInsert )
	oInsert:SQLString := SELF:PreExecute( oInsert:SQLString )

	IF !oInsert:Execute()
		SELF:__CopySQLError( #AppendRow, oInsert:ErrInfo )

		IF oStmt:ErrInfo:ReturnCode != SQL_SUCCESS_WITH_INFO
			oConn:__CloseExtraStmt(oInsert)
			RETURN FALSE
		ENDIF
	ENDIF

	SELF:nRowCount := oInsert:NumSuccessfulRows
	oConn:__CloseExtraStmt(oInsert)

	IF nRowCount = 0
		oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__NO_INS ), #AppendRow )
		RETURN FALSE
	ENDIF

	SELF:nAppendRecNum := SELF:nRecNum

	IF SELF:__Reset()
		SELF:nLastRecNum := SELF:nLastRecNum + SELF:NumSuccessfulRows
		SELF:Goto( SELF:nAppendRecNum )  
		lReadColumnInfo := FALSE
		lRet := TRUE
	ENDIF

	// Now write the Long Values to the new record
	IF nLongData > 0
		FOR nIndex := 1 TO nLongData
			IF .NOT. SELF:FIELDGET( aLongData[nIndex, 1] ) == aLongData[nIndex, 2]
				SELF:FIELDPUT( aLongData[nIndex, 1], aLongData[nIndex, 2] )
			ENDIF
		NEXT
		// self:Update() will call NotifyFileChange
		SELF:Update()
	ELSE
		SELF:Notify( NOTIFYFILECHANGE )
	ENDIF

	RETURN lRet

DESTRUCTOR() 

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:Axit()" )
	#ENDIF
	SELF:__MemFree()
	UnregisterAxit(SELF)
	RETURN 

METHOD BindColumn( i ) 
	LOCAL nCType    AS SHORTINT
	LOCAL nRetCode  AS SHORTINT
	LOCAL oCol      AS SqlColumn
	LOCAL pLength    AS PTR
	LOCAL pData     AS PTR
	LOCAL nSize     AS LONGINT
	LOCAL lRet      AS LOGIC
	LOCAL nODBCType AS SHORTINT
	LOCAL oData		 AS SQLData

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:BindColumn() " + NTrim( i ) )
	#ENDIF
   
	oCol := SELF:aSQLColumns[i]
	nODBCType := oCol:ODBCType
	IF nODBCType = SQL_LONGVARCHAR .OR. nODBCType = SQL_LONGVARBINARY  .OR. nODBCType = SQL_WLONGVARCHAR
		RETURN FALSE
	ENDIF
	oData := SELF:aSQLData[i]
	oData:Clear()
	pData 	:= oData:ptrValue
	pLength	:= oData:ptrLength
	nSize	   := LONGINT(oData:Length)
	nCType := SQLType2CType( nODBCType )

	nRetCode := SQLBindCol( oStmt:StatementHandle, i,   ;
							nCType,                     ;
							pData,                      ;
							nSize,                      ;
							pLength )

	IF nRetCode != SQL_SUCCESS
		oStmt:MakeErrorInfo(SELF, #BindColumn, nRetCode)

		#IFDEF __DEBUG__
			__SQLOutputDebug(   " SQLBindCol() failed " )
		#ENDIF
	ELSE
		lRet := TRUE
	ENDIF

	RETURN lRet

METHOD Close() 
	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:Close()" )
	#ENDIF

	IF ! SELF:__GoCold(TRUE, TRUE)
		RETURN FALSE
	ENDIF
	IF oStmt:StatementHandle != SQL_NULL_HSTMT
		SELF:__FreeStmt( SQL_DROP )
	ENDIF
#IFNDEF __VULCAN__	
	SELF:Axit()
#ENDIF	


	SELF:lFetchFlag      := FALSE
	SELF:lCsrOpenFlag    := FALSE
	SELF:lBof            := TRUE
	SELF:lEof            := FALSE
	SELF:lRowModified    := FALSE
	SELF:lDeleteFlag     := FALSE
	SELF:nRecNum         := 0
	SELF:nLastRecNum     := 0
	

	RETURN TRUE


METHOD Column( siCol ) 
	LOCAL nIndex    AS DWORD
	LOCAL oColumn   AS OBJECT
	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:Column( "+AsString( siCol )+" )" )
	#ENDIF
	nIndex := SELF:__GetColIndex( siCol, TRUE )
	IF nIndex = 0 .OR. nIndex > nNumCols
		oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADCOL ), #Column )
	ELSE
		oStmt:ErrInfo:ErrorFlag := FALSE
		oColumn := aSQLColumns[nIndex]
	ENDIF

	RETURN oColumn

METHOD ColumnAttributes( siCol ) 
	LOCAL nIndex            AS DWORD
	LOCAL nValue            AS LONGINT
	LOCAL nMax              AS SHORTINT
	LOCAL nRetCode          AS INT
	LOCAL oSQLColAtt        AS SQLColumnAttributes
	LOCAL oSqlCol				AS SQLColumn
	LOCAL aAttribs 			AS ARRAY
	LOCAL aValues				AS ARRAY
	LOCAL nAttrib				AS DWORD
	//RvdH 050413 Optimized  : added local oSqlCol and removed duplicate code by using a Loop

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:ColumnAttributes( "+AsString( siCol )+" )" )
	#ENDIF

	nIndex := SELF:__GetColIndex( siCol, TRUE )

	IF nIndex = 0 .OR. nIndex > nNumCols
		oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADCOL ), #ColumnAttributes )
		RETURN NULL_OBJECT
	ENDIF

	IF ALen( SELF:aColumnAttributes ) >= nIndex
		IF IsObject( SELF:aColumnAttributes[nIndex] )
			RETURN SELF:aColumnAttributes[nIndex]
		ENDIF
	ENDIF

	oSqlCol		:= aSQLColumns[nIndex]
	oSQLColAtt 	:= SQLColumnAttributes{ oSqlCol:HyperLabel , ;
										oSqlCol:FieldSpec,  ;
										oSqlCol:ODBCType,   ;
										oSqlCol:Scale,      ;
										oSqlCol:Nullable,   ;
										oSqlCol:Index,      ;
										oSqlCol:ColName,    ;
										oSqlCol:AliasName }

	oSQLColAtt:DisplaySize		:= oSqlCol:DisplaySize

	//RvdH 050413 Converted this into a loop, to remove tons of duplicate code
   //
	aAttribs := {	SQL_COLUMN_LENGTH, 			;       // 1
						SQL_COLUMN_UNSIGNED  ,     ;       // 2
						SQL_COLUMN_MONEY	,        ;       // 3
						SQL_COLUMN_UPDATABLE,      ;       // 4
						SQL_COLUMN_AUTO_INCREMENT, ;       // 5
						SQL_COLUMN_CASE_SENSITIVE,	;       // 6
						SQL_COLUMN_SEARCHABLE}             // 7

	aValues 	:= ArrayNew(ALen(aAttribs))

	FOR nAttrib := 1 TO ALen(aAttribs)

		nRetCode := SQLColAttributes(   oStmt:StatementHandle,  ;
										WORD( _CAST,nIndex ),   ;
										aAttribs[nAttrib],      ;
										NULL_PTR, 0,            ;
										@nMax,                  ;
										@nValue )

		IF ( nRetCode != SQL_SUCCESS )
			oStmt:MakeErrorInfo(SELF, #ColumnAttributes, nRetCode)
			RETURN NULL_OBJECT
		ENDIF
		aValues[nAttrib] := nValue
	NEXT

	oSQLColAtt:Length 			:= aValues[1]
	oSQLColAtt:Unsigned 			:= aValues[2] != 0
	oSQLColAtt:Money 				:= aValues[3] != 0
   oSQLColAtt:Updatable 		:= aValues[4] != SQL_ATTR_READONLY
   oSQLColAtt:AutoIncrement 	:= aValues[5] != 0
   oSQLColAtt:CaseSensitive 	:= aValues[6] != 0
	oSQLColAtt:Searchable 		:= aValues[7]

	oStmt:ErrInfo:ErrorFlag 	:= FALSE
	SELF:aColumnAttributes[nIndex] := oSQLColAtt
	//RvdH 050413 Replace Column in aSQLColumns since ColumAttributes is a subclass
	SELF:aSQLColumns[nIndex] := oSqlColAtt
	RETURN oSQLColAtt

METHOD Commit() 
	//
	//  UH: Obsolete method, don't support after 2.0 !!!!
	//
	//
	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:Commit()" )
	#ENDIF
	SELF:Update(TRUE)
	RETURN SELF:oStmt:Connection:Commit()

METHOD DataField( uFieldPos ) 
	LOCAL nIndex        AS DWORD
	LOCAL oRet          AS OBJECT

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:DataField( "+AsString( uFieldPos )+" )" )
	#ENDIF
	nIndex := SELF:__GetColIndex( uFieldPos, TRUE )
	IF nIndex = 0 .OR. nIndex > nNumCols
		oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADFLD ), #DataField )
	ELSE
		oStmt:ErrInfo:ErrorFlag := FALSE

		oRet := aDataFields[nIndex]
	ENDIF
	RETURN oRet

METHOD Delete() 
	LOCAL sDelete       AS STRING
	LOCAL oDelete       AS SQLStatement
	LOCAL nIndex        AS DWORD
	LOCAL nType         AS DWORD
	LOCAL aDataBuffer   AS ARRAY
	LOCAL nRecno        AS INT
	LOCAL nODBCType     AS SHORTINT
	LOCAL cQuote		  AS STRING
	LOCAL oCOl			  AS SQLColumn
	LOCAL nCol				AS LONGINT

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:Delete()" )
	#ENDIF

	IF lAppendFlag
		lAppendFlag := FALSE
		lRowModified := FALSE
		nRecNum := nAppendRecNum
		RETURN TRUE
	ENDIF

	IF ! lCsrOpenFlag
		oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__NO_CSR ), #Delete )
		RETURN FALSE
	ENDIF
	nType := SELF:__FigureScrollUpdateType()
	cQuote := oStmt:Connection:IdentifierQuoteChar
	DO CASE
	CASE nType = SQL_SC_UPD_CURSOR

		SELF:__GetCursorName()

		sDelete := "delete from " + cTableName + " where current of " + cCursor


	CASE nType = SQL_SC_UPD_KEY
		IF ALen( aIndexCol ) = 0
			oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__NO_KEY ), #Delete )
			RETURN FALSE
		ENDIF

		sDelete := "delete from " + cTableName + " where "
      //aDataBuffer := aSQLDataBuffer[nBuffIndex, SQL_DATA_BUFFER]
      aDataBuffer   := SELF:aOriginalRecord
      FOR nIndex := 1 TO ALen( aIndexCol )
         nCol       := aIndexCol[nIndex]
         oCol       := aSQLColumns[nCol]
         sDelete    +=  cQuote  + oCol:ColName  + cQuote + __GetDataValuePSZ( oCol, aDataBuffer[nCol], TRUE, TRUE )
         IF nIndex  != ALen( aIndexCol )
            sDelete += " and "
         ENDIF
      NEXT

	CASE nType = SQL_SC_UPD_VALUE
		sDelete := "delete from " + cTableName + " where "
		//aDataBuffer := aSQLDataBuffer[nBuffIndex, SQL_DATA_BUFFER]
		aDataBuffer	  := SELF:aOriginalRecord
		//RvdH 030925 Changed mechanism for Adding AND, because it would fail if the
		//            last column was LONG
      FOR nIndex := 1 TO nNumCols
         oCol       := aSQLColumns[nIndex]
         nODBCType  := oCol:ODBCType
         IF nODBCType == SQL_LONGVARCHAR .OR. nODBCType == SQL_LONGVARBINARY .OR. nODBCType == SQL_WLONGVARCHAR
            LOOP
         ENDIF
         sDelete    +=  cQuote + oCol:ColName +  cQuote  + __GetDataValuePSZ(  oCol, aDataBuffer[nIndex], TRUE, TRUE )
         IF nIndex  != ALen( aIndexCol )
            sDelete += " and "
         ENDIF
      NEXT
	ENDCASE

	oDelete := oConn:__GetExtraStmt(sDelete)

	SELF:__PrepareStmtOptions( oDelete )
	oDelete:SQLString := SELF:PreExecute( oDelete:SQLString )
	IF !oDelete:Execute()
		SELF:__CopySQLError( #Delete, oDelete:ErrInfo )
		IF oStmt:ErrInfo:ReturnCode != SQL_SUCCESS_WITH_INFO
			oConn:__CloseExtraStmt(oDelete)
			RETURN FALSE
		ENDIF
	ENDIF

	nRowCount := oDelete:NumSuccessfulRows
	oConn:__CloseExtraStmt(oDelete)


	IF nRowCount = 0
		oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__NO_ROW ), #Delete )
		RETURN FALSE
	ENDIF

	lRowModified := FALSE
	IF oStmt:Connection:ScrollCsr
		nRecno := SELF:nRecNum
		IF SELF:nRowCount > 0
			IF SetDeleted()
				IF SELF:__Reset()
					SELF:nLastRecNum := SELF:nLastRecNum - SELF:nRowCount

					IF  SELF:nLastRecNum < 1
						SELF:__SetRecordFlags( TRUE, TRUE )
					ELSE
						SELF:GoTop()
						IF nRecno > 2
							SELF:Skip( nRecno - 2 )
						ENDIF
					ENDIF
				ENDIF
			ELSE

				SELF:lDeleteFlag := TRUE
			ENDIF
		ENDIF
	ELSE
		lDeleteFlag := TRUE
		SELF:Skip( -1 )
	ENDIF

	SELF:Notify( NOTIFYDELETE )

	IF SELF:BOF .AND. !SELF:EOF
		SELF:GoTop()
	ENDIF

	RETURN TRUE

//METHOD Destroy() AS VOID
//	SELF:__MemFree()
//	//IF ! InCollect()
//    UnRegisterAxit( SELF )
//    //ENDIF
//    RETURN 
    
METHOD DirectSkip( nSkip ) 
	LOCAL lRet          AS LOGIC
	LOCAL lRowCount     AS DWORD // dcaton 070206 was LONGINT
	LOCAL nRowStat      AS WORD // dcaton 070206 was INT
	LOCAL nRetCode      AS INT

	nRetCode := SQLExtendedFetch( SELF:Statement:StatementHandle, ;
									SQL_FETCH_RELATIVE, ;
									nSkip, ;
									@lRowCount, ;
									@nRowStat )
	lRet :=( nRetCode == SQL_SUCCESS )
	IF lRet
		SELF:ReReadRow()
	ENDIF
	RETURN lRet

METHOD Error( oError ) 
	//  Method for handling error conditions raised during database processing
	//
	//  The standard Error handling method passes the problem to its clients
	//  if there are any, in their standard Exception handing method.
	//
	//  If there is no client who wants to deal with the problem, the user
	//  defined error handler gets called.
	//
	//  Note: if an error comes in while one is being handled, the Error
	//  method immediately breaks without any fancy stuff.

	STATIC LOCAL    lErrorProcessingSemaphor    AS LOGIC
	IF lErrorProcessingSemaphor
		Eval( ErrorBlock(), oError )
	ELSE
		lErrorProcessingSemaphor := TRUE
		IF IsArray( aClients ) .AND. ALen( aClients ) != 0 .AND. IsMethod( aClients[1], #Error )
			aClients[1]:Error( oError )
		ELSE
			lErrorProcessingSemaphor := FALSE
			Eval( ErrorBlock(), oError )
		ENDIF
		lErrorProcessingSemaphor := FALSE
	ENDIF
	RETURN NIL

METHOD Execute( uParam ) 
	LOCAL nCount        AS DWORD
	LOCAL lRet          AS LOGIC
	LOCAL i             AS DWORD
	LOCAL aArgs         AS ARRAY
	//RvdH 050413 Optimized by suppress reading of Column Attributes

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:Execute()" )
	#ENDIF

	IF oStmt:StatementHandle = SQL_NULL_HSTMT
		SELF:__AllocStmt()
	ENDIF

	oStmt:SQLString := SELF:PreExecute( oStmt:SQLString )
	nCount := PCount()
	IF nCount != 0
		IF nCount = 1 .AND. UsualType( uParam ) = ARRAY
			aArgs  := uParam
		ELSE
			aArgs := ArrayCreate( nCount )
			FOR i:=1 TO nCount
				aArgs[i] := _GETMPARAM( i )
			NEXT
		ENDIF 
	ENDIF
	lRet := oStmt:Execute( aArgs)

	IF lRet
		SELF:lCsrOpenFlag := TRUE
		SELF:lRowModified := FALSE
		SELF:lBof         := FALSE
		SELF:lEof         := FALSE
		SELF:lFetchFlag   := FALSE
		SELF:lDeleteFlag  := FALSE
		SELF:nRowCount    := -1
		SELF:nNumRows		:= -1
		SELF:nLastRecNum  := oStmt:NumSuccessfulRows
// 		SELF:lEofCsr      := FALSE		///RvdH Not used
		IF SELF:nlastRecNum < 0
			SELF:lLastRecFound := FALSE
		ELSE
			SELF:lLastRecFound:= TRUE
		ENDIF
		#IFDEF __DEBUG__
			__SQLOutputDebug( "** Number of Rows: " + NTrim( oStmt:NumSuccessfulRows ) )
		#ENDIF

		//SELF:lNoBuffering  := oConn:ScrollCsr
		lRet := SELF:__InitColumnDesc()

      // RvdH 050413 Suppress reading of ColumnAttributes until really necessary
		// 		FOR i := 1 TO SELF:nNumCols
		// 			SELF:ColumnAttributes( i )
		// 		NEXT

		IF SELF:nNumCols = 0
			lRet := FALSE
		ELSE

//          RvdH 080925 This generates a lot of unwanted traffic
//          IF !SELF:lLastRecFound
//             IF SELF:__RecCount() = 0
//                SELF:GoBottom()
//             ENDIF
//          ENDIF

			SELF:GoTop()

			#IFDEF __DEBUG__
				//__SQLOutputDebug( "** Buffering: " + AsString( !SELF:lNoBuffering ) )
				__SQLOutputDebug( "** Connection:ScrollCsr: " + AsString( SELF:oConn:ScrollCsr ) )
				__SQLOutputDebug( "** nRecNum: " + NTrim( nRecNum ) )
			#ENDIF
		ENDIF
	ELSE
		//  UH 07/17/2000
		SELF:lBof         := TRUE
		SELF:lEof         := TRUE
		SELF:nRowCount    := -1
		SELF:nLastRecNum  := -1
	ENDIF
	// Parameters are not freed. They are used in the __RecCount Access
	RETURN lRet

METHOD ExtendedFetch( nFetchType, nRow ) 
	LOCAL nRetCode      AS INT
	LOCAL nIndex        AS DWORD
	LOCAL nNumRows      AS DWORD
	LOCAL nRowStatus    AS WORD // dcaton 070206 was INT
	LOCAL lAppended     AS LOGIC
	LOCAL lRet          AS LOGIC

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:ExtendedFetch( "+AsString( nFetchType )+","+AsString( nRow )+" )" )
	#ENDIF
	//RvdH 050413 Centralize opening of cursor
	IF ! SELF:__ForceOpen()
		RETURN FALSE
	ENDIF

	//  Already at eof?
	IF SELF:lEof .AND. nFetchType = SQL_FETCH_NEXT
		#IFDEF __DEBUG__
			__SQLOutputDebug( "             ExtendedFetch( NEXT ) Already at EOF!" )
		#ENDIF
		//  Empty table?
		IF SELF:lBof
			lDeleteFlag := TRUE     // mark it as deleted
			SELF:Notify( NOTIFYFILECHANGE )  //refresh browser
		ENDIF
		RETURN FALSE
	ENDIF
	//  Already at bof?
	IF SELF:lBof .AND. nFetchType = SQL_FETCH_PREV
		#IFDEF __DEBUG__
			__SQLOutputDebug( "             ExtendedFetch( PREV ) Already at BOF!" )
		#ENDIF
		//  Empty table?
		IF SELF:lEof
			lDeleteFlag := TRUE             // mark it as deleted
			SELF:Notify( NOTIFYFILECHANGE ) //refresh browser
		ENDIF

		RETURN FALSE
	ENDIF
	//  append pending?
   //RvdH 050413 Centralize GoCold behavior
   IF ! SELF:__GoCold(FALSE, TRUE)
      RETURN FALSE
   ENDIF
   IF lAppendFlag
      lAppended := TRUE
   ENDIF

   SELF:__SetRecordFlags( FALSE, NIL )
	//
	// Fetch the row...
	//
	nRetCode := SQLExtendedFetch(   oStmt:StatementHandle,  ;
									nFetchType,             ;
									nRow,                   ;
									@nNumRows,              ;
									@nRowStatus )

	IF nRetCode = SQL_NO_DATA_FOUND
		DO CASE
		CASE nFetchType = SQL_FETCH_NEXT
			SELF:__SetRecordFlags( NIL, TRUE )
			IF !SELF:lLastRecFound
				SELF:nLastRecnum := SELF:nRecnum
				SELF:lLastRecFound := TRUE
			ENDIF
			#IFDEF __DEBUG__
				__SQLOutputDebug( "** SQLSelect:ExtendedFetch( NEXT ) EOF!, last record found!" )
			#ENDIF

		CASE nFetchType = SQL_FETCH_PREV
			SELF:__SetRecordFlags( TRUE, NIL )
			#IFDEF __DEBUG__
				__SQLOutputDebug( "** SQLSelect:ExtendedFetch( PREV ) BOF!" )
			#ENDIF

		CASE nFetchType = SQL_FETCH_FIRST .OR. nFetchType = SQL_FETCH_LAST
			SELF:__SetRecordFlags( TRUE, TRUE)
			#IFDEF __DEBUG__
				__SQLOutputDebug( "** SQLSelect:ExtendedFetch() BOF & EOF!" )
			#ENDIF

		CASE nFetchType = SQL_FETCH_RELATIVE
			IF nRow > 0
				SELF:__SetRecordFlags( NIL, TRUE )
				#IFDEF __DEBUG__
					__SQLOutputDebug( "** SQLSelect:ExtendedFetch( +REL ) EOF!" )
				#ENDIF
			ELSE
				SELF:__SetRecordFlags( TRUE, NIL )
				#IFDEF __DEBUG__
					__SQLOutputDebug( "** SQLSelect:ExtendedFetch( -REL ) BOF!" )
				#ENDIF
			ENDIF
		ENDCASE
		lFetchFlag := TRUE   // say we attempted to fetch
		IF SELF:lBof .AND. !SELF:lEof
			// position csr on the first row...

			nRetCode := SQLExtendedFetch(   oStmt:StatementHandle,  ;
											SQL_FETCH_FIRST,        ;
											0,                      ;
											@nNumRows,              ;
											@nRowStatus )

			//  Allow with info
			IF nRetCode != SQL_SUCCESS .AND. nRetCode != SQL_SUCCESS_WITH_INFO
				IF nRetCode = SQL_NO_DATA_FOUND

					SELF:__SetRecordFlags( NIL, TRUE)

				ELSE
					oStmt:MakeErrorInfo(SELF, #ExtendedFetch, nRetCode)
					RETURN FALSE
				ENDIF
			ENDIF
			IF nFetchType == SQL_FETCH_RELATIVE
				nRecNum := 1
			ENDIF

			#IFDEF __DEBUG__
				__SQLOutputDebug( "   BOF, back on first!" )
			#ENDIF

		ENDIF
		IF !SELF:lBof .AND. SELF:lEof

			nRetCode := SQLExtendedFetch(   oStmt:StatementHandle,  ;
											SQL_FETCH_LAST,         ;
											0,                      ;
											@nNumRows,              ;
											@nRowStatus )

			IF nRetCode != SQL_SUCCESS .AND. nRetCode != SQL_SUCCESS_WITH_INFO
				IF nRetCode = SQL_NO_DATA_FOUND
					SELF:__SetRecordFlags( TRUE, NIL)
				ELSE
					oStmt:MakeErrorInfo(SELF, #ExtendedFetch, nRetCode)
					RETURN FALSE
				ENDIF
			ENDIF 
			#IFDEF __DEBUG__
				__SQLOutputDebug( "   EOF, back on last!" )
			#ENDIF
		ENDIF
		IF SELF:lBof .AND. SELF:lEof
			lDeleteFlag := TRUE
		SELF:Notify( NOTIFYFILECHANGE )
		ELSEIF SELF:lBof
			SELF:nRecNum := 1
		ELSE
			SELF:nRecNum := SELF:nLastRecNum  
		ENDIF
		#IFDEF __DEBUG__
			__SQLOutputDebug( "*** nRecNum = " + NTrim( nRecNum ) )
		#ENDIF

		RETURN FALSE

	ELSEIF nRetCode != SQL_SUCCESS

		oStmt:MakeErrorInfo(SELF, #ExtendedFetch, nRetCode)
		//
		//  Fix for undocumented status 01S06
		//
		IF nRetCode   = SQL_SUCCESS_WITH_INFO .AND. ;
			nFetchType = SQL_FETCH_PREV        .AND. ;
			SubStr3( oStmt:ErrInfo:SQLState,1,5 ) = "01S06"

			oStmt:ErrInfo:ErrorFlag := FALSE

			SELF:__SetRecordFlags( TRUE, NIL)

			RETURN TRUE
		ENDIF
   	SELF:__SetRecordFlags( FALSE, FALSE )
		//RETURN FALSE
	ELSE
		SELF:__SetRecordFlags( FALSE, FALSE )
	ENDIF

	#IFDEF __DEBUG__
		__SQLOutputDebug( "*** nRowStatus: " + NTrim( nRowStatus ) )
	#ENDIF  
	//
	// RvdH 070122 If we get here we always want to read the column values
	//
		//
// 	IF ( nRowStatus = SQL_ROW_SUCCESS ) .OR. ;
// 		( nRowStatus = SQL_ROW_UPDATED ) .OR. ;
// 		( nRowStatus = SQL_ROW_DELETED ) .OR. ;
// 		( nRowStatus = SQL_ROW_ADDED )
		//
		// reset flags for this row & get the col data...
		//
		FOR nIndex := 1 TO nNumCols
			SELF:__InitColValue( nIndex )
		NEXT

		lRet := TRUE
		IF nRowStatus = SQL_ROW_DELETED
			SELF:lDeleteFlag := TRUE
		ELSE
			SELF:lDeleteFlag := FALSE
		ENDIF
// 	ELSE
// 		SELF:__SetRecordFlags( NIL, TRUE )
// 		lFetchFlag  := TRUE
// 		#IFDEF __DEBUG__
// 			__SQLOutputDebug( "** SQLSelect:ExtendedFetch() rowstat EOF!" )
// 		#ENDIF

// 		RETURN FALSE
// 	ENDIF

	//nBuffIndex := 1
	SELF:__CopyDataBuffer( SELF:aSQLData, SELF:aOriginalRecord)

	//aSQLDataBuffer[nBuffIndex, SQL_DATA_DELETE] := SELF:lDeleteFlag

	SELF:lFetchFlag  := TRUE
	SELF:lRowModified:= FALSE
	SELF:nRowCount   := -1
	DO CASE
	CASE nFetchType = SQL_FETCH_NEXT
		++nRecNum
		SELF:Notify( NOTIFYRECORDCHANGE, 1 )

	CASE nFetchType = SQL_FETCH_PREV
		IF ! lAppended  //  appended already restored recnum
			--nRecNum
		ENDIF
		SELF:Notify( NOTIFYRECORDCHANGE, -1 )

	CASE nFetchType = SQL_FETCH_FIRST
		nRecNum := 1
		IF SELF:nLastRecnum = -1
			SELF:nLastRecnum := 1
		ENDIF
		SELF:Notify( NOTIFYGOTOP )

	CASE nFetchType = SQL_FETCH_LAST
		nRecNum := SELF:nLastRecNum
		SELF:Notify( NOTIFYGOBOTTOM )

	CASE nFetchType = SQL_FETCH_ABSOLUTE
		nRecNum := nRow
		SELF:Notify( NOTIFYRECORDCHANGE )

	CASE nFetchType = SQL_FETCH_RELATIVE
		nRecNum += nRow
		SELF:Notify( NOTIFYRECORDCHANGE, nRow )

	ENDCASE

	IF !SELF:lLastRecFound
		IF SELF:nRecnum > SELF:nLastRecNum
			SELF:nLastRecnum := SELF:nRecnum
		ENDIF
	ENDIF

	#IFDEF __DEBUG__
		__SQLOutputDebug( "*** nRecNum = " + NTrim( nRecNum ) )
	#ENDIF
	oStmt:ErrInfo:ErrorFlag := FALSE
	RETURN lRet

METHOD Fetch( ) 
	LOCAL nRetCode  AS SHORTINT
	LOCAL nIndex    AS DWORD

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:Fetch()" )
	#ENDIF
	//RvdH 050413 Centralize opening of cursor
	IF ! SELF:__ForceOpen()
		RETURN FALSE
	ENDIF
	//RvdH 050413 Centralize GoCold behavior
	IF ! SELF:__GoCold(FALSE, TRUE)
		RETURN FALSE
	ENDIF

	IF !SELF:lFetchFlag
		SELF:__SetRecordFlags( TRUE, NIL)
	ENDIF


	IF SELF:lEof .AND. !SELF:lSkipFlag
		#IFDEF __DEBUG__
			__SQLOutputDebug( "             Fetch() Already at EOF!" )
		#ENDIF

		RETURN FALSE
	ENDIF

		nRetCode := SQLFetch( oStmt:StatementHandle )
		IF nRetCode = SQL_NO_DATA_FOUND

			SELF:__SetRecordFlags( NIL, TRUE )
			nLastRecNum := nRecNum
			lFetchFlag  := TRUE
			#IFDEF __DEBUG__
				__SQLOutputDebug( "             Fetch() SQL_NO_DATA_FOUND!" )
			#ENDIF
			RETURN FALSE

		ELSEIF nRetCode != SQL_SUCCESS
			oStmt:MakeErrorInfo(SELF, #Fetch, nRetCode)
			RETURN FALSE
		ENDIF

		FOR nIndex := 1 TO nNumCols
			SELF:__InitColValue( nIndex )
		NEXT

		SELF:__CopyDataBuffer(SELF:aSqlData, SELF:aOriginalRecord)
		++nRecNum

	IF lFetchFlag
		SELF:__SetRecordFlags( FALSE, NIL)
	ENDIF

	SELF:lFetchFlag  := TRUE
	SELF:lRowModified:= FALSE
	SELF:lDeleteFlag := FALSE
	SELF:nRowCount   := -1
	SELF:oStmt:ErrInfo:ErrorFlag := FALSE
	SELF:__SetRecordFlags( NIL, FALSE )
	SELF:Notify( NOTIFYRECORDCHANGE, 1 )

	RETURN TRUE

METHOD FIELDGET( uField ) 
	LOCAL nType  AS DWORD
	LOCAL xRet   AS USUAL
	LOCAL nIndex AS DWORD
	LOCAL cType  AS STRING

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:FieldGet( "+AsString( uField )+" )" )
	#ENDIF

	nIndex := SELF:__GetColIndex( uField, TRUE )
	IF nIndex = 0 .OR. nIndex > SELF:FCount
		oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADFLD ), #FieldGet )
		SELF:Error( oStmt:ErrInfo )

	ELSE
		xRet := SELF:GetData( nIndex )
		IF xRet = NIL .AND. SELF:lNullAsBlank
			cType := SELF:FieldSpec( uField ):ValType
			DO CASE
			CASE cType == "C"
				xRet := NULL_STRING
			CASE cType == "D"
				xRet := NULL_DATE
			CASE cType == "L"
				xRet := FALSE
			CASE cType == "M"
				xRet := NULL_STRING
			CASE cType == "N"
				xRet := 0
			ENDCASE
		ELSE
			nType := UsualType( xRet )
			IF nType == LONGINT
				xRet := Integer( xRet )
			ENDIF

		ENDIF
	ENDIF

	RETURN xRet

METHOD FieldGetFormatted( uFieldPos ) 
	LOCAL nIndex    AS DWORD
	LOCAL xRet      AS USUAL
	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:FieldGetFormatted( "+AsString( uFieldPos )+" )" )
	#ENDIF
	nIndex := SELF:__GetColIndex( uFieldPos, TRUE )
	IF nIndex = 0 .OR. nIndex > nNumCols
		oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADFLD ), #FieldGetFormatted )
		SELF:Error( oStmt:ErrInfo )
	ELSE
		xRet := SELF:FIELDGET( nIndex )
		oStmt:ErrInfo:ErrorFlag := FALSE
		xRet := aDataFields[nIndex]:FieldSpec:Transform( xRet )
	ENDIF

	RETURN xRet
END CLASS

