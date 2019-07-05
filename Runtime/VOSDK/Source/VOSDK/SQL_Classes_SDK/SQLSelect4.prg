PARTIAL CLASS SQLSelect

CONSTRUCTOR( cSQLSelect, oSQLConnection ) 

	SUPER()
	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:Init( "+AsString( cSQLSelect )+" )" )
	#ENDIF
	SELF:oStmt := SQLStatement{ cSQLSelect, oSQLConnection }
	SELF:oConn := SELF:oStmt:Connection


	SELF:lFetchFlag      	:= FALSE
	SELF:lCsrOpenFlag    	:= FALSE
	SELF:lBof            	:= TRUE
	SELF:lEof            	:= FALSE
	SELF:lRowModified    	:= FALSE
	SELF:lSkipFlag       	:= FALSE
	SELF:nRecNum         	:= 0
	SELF:nNotifyCount    	:= 0
	SELF:lSuppressNotify 	:= FALSE
	SELF:lAppendFlag     	:= FALSE
	SELF:lNotifyIntentToMove := FALSE
	SELF:aIndexCol       	:= {}
	SELF:lDeleteFlag     	:= FALSE
	SELF:nRowCount       	:= -1
	//SELF:nScrollUpdateType	:= SQL_SC_UPD_AUTO
	SELF:nScrollUpdateType	:= SQL_SC_UPD_CURSOR
	//SELF:nBuffSize       	:= 0
	SELF:nLastRecNum     	:= 0
	SELF:lReadColumnInfo		:= FALSE	//RvdH 070716 Abused to remember lAppendFlag 

	//SELF:lNoBuffering    := TRUE
	SELF:lLastRecFound   := FALSE
	IF cSQLSelect != NIL
		SELF:__FindTableName()
	ENDIF
	
	RETURN 





METHOD NoIVarGet( symFieldName ) 

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:NoIVarGet( "+AsString( symFieldName )+" )" )
	#ENDIF
	RETURN SELF:GetData( symFieldName )

METHOD NoIVarPut( symFieldName,VALUE ) 

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:NoIVarPut( "+AsString( symFieldName )+" )" )
	#ENDIF

	RETURN SELF:FIELDPUT( symFieldName, VALUE )






METHOD Notify( kNotification, uDescription ) 
	LOCAL lRetValue AS  LOGIC
	LOCAL nClient   := 0 AS  DWORD

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:Notify( "+AsString( kNotification )+            ;
			" ) Count="+AsString( nNotifyCount ) )
	#ENDIF
	lRetValue := TRUE

	IF nNotifyCount = 0 .AND. ! lSuppressNotify
		IF kNotification <= NOTIFYCOMPLETION
			ASend( aClients, #Notify, kNotification, uDescription )

		ELSEIF kNotification = NOTIFYINTENTTOMOVE
			SELF:lNotifyIntentToMove := TRUE
			DO WHILE lRetValue .AND. nClient < ALen( aClients )
				nClient++
				lRetValue := aClients[nClient]:Notify( kNotification )
			ENDDO
			RETURN lRetValue

		ELSEIF kNotification <= NOTIFYFILECHANGE
			ASend( aClients, #Notify, kNotification, uDescription )

		ELSEIF kNotification = NOTIFYRELATIONCHANGE
			ASend( aClients, #Notify, NOTIFYFILECHANGE )

		ELSEIF kNotification = NOTIFYCLEARRELATION
			// lSelectionActive := FALSE

		ELSE                                             // event I don't know about
			ASend( aClients, #Notify, kNotification, uDescription )
		ENDIF
	ENDIF

	RETURN lRetValue

METHOD NumResultCols() 
	LOCAL nNumColsNew   AS SHORTINT // dcaton 070206 was INT
	LOCAL nRetCode      AS INT
	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:NumResultCols()" )
	#ENDIF
	//  Make sure we have a stmt
	IF ( oStmt:StatementHandle = SQL_NULL_HSTMT )
		SELF:__AllocStmt()
	ENDIF
	//RvdH 050413 Centralize opening of cursor
	IF ! SELF:__ForceOpen()
		RETURN -1
	ENDIF


	//  Use retcode
	nRetCode := SQLNumResultCols( oStmt:StatementHandle, @nNumColsNew )
	IF ( nRetCode != SQL_SUCCESS )
		oStmt:MakeErrorInfo(SELF, #NumResultCols, nRetCode)
		RETURN -1
	ENDIF
	#IFDEF __DEBUG__
		__SQLOutputDebug( "             nNumCols="+AsString( nNumColsNew ) )
	#ENDIF
	SELF:nNumCols := nNumColsNew
	oStmt:ErrInfo:ErrorFlag := FALSE
	RETURN nNumCols


METHOD PreExecute( cSQLString ) 

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:PreExecute( "+AsString( cSQLString )+" )" )
	#ENDIF
	RETURN cSQLString

METHOD Prepare() 

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:Prepare()" )
	#ENDIF
	RETURN oStmt:Prepare()

METHOD Refresh() 

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:Refresh()" )
	#ENDIF
	RETURN SELF:Update( FALSE )

METHOD ReReadRow() 
	LOCAL i     AS DWORD
	LOCAL n     AS DWORD
	
	n := SELF:nNumCols
	FOR i := 1 TO n
		SELF:__InitColValue( i )
	NEXT 

	RETURN TRUE

METHOD ResetCursor( nUpdateType ) 
	LOCAL lRet          AS LOGIC
	LOCAL nRecno        AS LONGINT
	LOCAL lKeyChanged   AS LOGIC
	LOCAL i             AS DWORD

	IF nUpdateType = SQL_SC_UPD_KEY

		FOR i := 1 TO nNumCols
			IF aSQLData[i]:ValueChanged

				IF  AScan( SELF:aIndexCol, i ) > 0
					lKeyChanged := TRUE
				ENDIF
			ENDIF
		NEXT
	ELSE
		lKeyChanged := FALSE
	ENDIF

	nRecno := SELF:nRecNum
	//RvdH Should there be a difference when the key changed ?
	IF lKeyChanged

		lRet := oStmt:__Reset()
		IF lRet
			SELF:GoTo( nRecno )
		ENDIF
	ELSE
		lRet := oStmt:__Reset()
		IF lRet
			SELF:GoTo( nRecno )
		ENDIF
	ENDIF

	RETURN lRet

METHOD ResetNotification() 

	--nNotifyCount
	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:ResetNotification() Count="+AsString( nNotifyCount ) )
	#ENDIF

	RETURN NIL

METHOD RLOCK( uRecordNumber ) 

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:RLock()" )
	#ENDIF
	RETURN TRUE

METHOD RLockVerify() 

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:RLockVerify()" )
	#ENDIF
	RETURN TRUE

METHOD RollBack() 
	//
	//  UH: Obsolete method, don't support after 2.0 !!!!
	//
	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:Rollback()" )
	#ENDIF

	RETURN SELF:oStmt:Connection:RollBack()


METHOD SetColumnAttributes(uFieldPos, oColAttributes) 
	LOCAL nIndex    AS DWORD
	LOCAL lOk		 AS LOGIC
	nIndex := SELF:__GetColIndex( uFieldPos, TRUE )
	IF ( nIndex = 0 .OR. nIndex > nNumCols )
		oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADFLD ), #SetColumnAttributes )
		SELF:Error( oStmt:ErrInfo )
		lOk := FALSE
	ELSEIF IsInstanceOfUsual(oColAttributes, #SQLColumnAttributes)
		SELF:aSQLColumns[nIndex] 			:= oColAttributes
		SELF:aColumnAttributes[nIndex]	:= oColAttributes
		lOk := TRUE
	ELSE
		oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__INVALIDCOLATT ), #SetColumnAttributes )
		SELF:Error( oStmt:ErrInfo )
		lOk := FALSE
	ENDIF
	RETURN lOk


METHOD SetDataField( nFieldPos, oField ) 
	LOCAL oDF          AS DataField
	LOCAL lRet         AS LOGIC

	IF aDataFields = NULL_ARRAY
		///RvdH Changed define
		DbError{ SELF,  #SetDataField, EG_SEQUENCE, __CavoStr( __CAVOSTR_SQLCLASS__NODATAFIELDSEXIST ) }:Throw()

	ELSEIF !IsNumeric( nFieldPos )    .OR. ;
		nFieldPos < 1           .OR. ;
		nFieldPos > ALen( aDataFields )
		///RvdH Changed define
		DbError{ SELF, #SetDataField, EG_ARG,  __CavoStr( __CAVOSTR_SQLCLASS__BADFIELDPOSITION ), nFieldPos, "nFieldPos" }:Throw()

	ELSEIF IsNil( oField ) .OR. !IsInstanceOfUsual( oField,#DataField )
		///RvdH Changed define
		DbError{ SELF,  #SetDataField, EG_ARG,  __CavoStr( __CAVOSTR_SQLCLASS__BADFIELDPOSITION ), nFieldPos, "nFieldPos" }:Throw()

	ELSE
		oDF := aDataFields[nFieldPos]
		IF oField:Name == oDF:Name

			aDataFields[nFieldPos] := oField

			lRet := TRUE
		ENDIF
	ENDIF

	RETURN lRet

METHOD SetPos( nPos, nOption, nLock ) 
	LOCAL lRet          AS LOGIC
	LOCAL nRetCode      AS INT

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSetPos( " + NTrim( nPos ) + " )" )
	#ENDIF

	IF !IsNumeric( nPos )
		nPos := 1
	ENDIF
	IF !IsNumeric( nOption )
		nOption := SQL_POSITION
	ENDIF
	IF !IsNumeric( nLock )
		nLock := SQL_LOCK_NO_CHANGE
	ENDIF

	nRetCode := SQLSetPos( SELF:Statement:StatementHandle, ;
							nPos, ;
							nOption,;
							nLock )

	IF nRetCode = SQL_SUCCESS
		lRet := TRUE
		oStmt:ErrInfo:ErrorFlag := FALSE
	ELSE
		oStmt:MakeErrorInfo(SELF, #SQLSetPos, nRetCode)
	ENDIF

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** nRetCode: " + NTrim( nRetCode ) )
	#ENDIF

	RETURN lRet

METHOD SetPrimaryKey( uFieldPos ) 
	LOCAL nIndex    AS DWORD
	LOCAL lRet      AS LOGIC

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:SetPrimaryKey( "+AsString( uFieldPos )+" )" )
	#ENDIF

	nIndex := SELF:__GetColIndex( uFieldPos, TRUE )
	IF nIndex = 0
		// clear the index array
		aIndexCol := {}
		lRet := TRUE
	ELSE
		IF nIndex > nNumCols
			oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADFLD ), #SetPrimaryKey )
		ELSE
			AAdd( aIndexCol, nIndex )
			oStmt:ErrInfo:ErrorFlag := FALSE
			lRet := TRUE
		ENDIF
	ENDIF
	RETURN lRet

METHOD SetStatementOption( fOption, uValue ) 

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:SetStatementOption()" )
	#ENDIF
	RETURN oStmt:SetStatementOption( fOption, uValue, TRUE )

METHOD SetTimeStamp( uFieldPos, cTimestamp ) 
	LOCAL nIndex AS DWORD
	LOCAL nODBCType AS SHORTINT
	LOCAL oData		AS SQLData 
	LOCAL oErr		AS Error

	nIndex := SELF:__GetColIndex( uFieldPos, TRUE )

	IF ( nIndex = 0 .OR. nIndex > nNumCols )
		oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADFLD ), #SetTimeStamp )
		oErr := oStmt:ErrInfo
		oErr:ArgNum := 1
		oErr:Args := {uFieldPos, cTimestamp} 
		SELF:Error( oErr )
		RETURN NIL
	ENDIF

	nODBCType := aSQLColumns[nIndex]:ODBCType
	IF ( nODBCType != SQL_TIMESTAMP )
		oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADFLD ), #SetTimeStamp  )
		oErr := oStmt:ErrInfo
		oErr:ArgNum := 1
		oErr:Args := {uFieldPos, cTimestamp} 
		SELF:Error( oErr )
		RETURN NIL
	ENDIF
                                                        
	IF ! IsString(cTimeStamp )
		oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADPAR ), #SetTimeStamp  )
		oErr := oStmt:ErrInfo
		oErr:ArgNum := 2
		oErr:Args := {uFieldPos, cTimestamp} 
		SELF:Error( oErr )
		RETURN NIL
	ENDIF
                                                        
                                                        
	IF lAppendFlag
		oData := aAppendData[nIndex]
	ELSE
		oData := aSQLData[nIndex]

	ENDIF
	MemCopy( oData:ptrValue, String2Psz(cTimestamp ), Min( SLen( cTimestamp ), oData:Length ) )
	oData:ValueChanged 	:= TRUE
	oData:Null         := FALSE


	lRowModified := TRUE
	SELF:Notify( NOTIFYFIELDCHANGE, aSQLColumns[nIndex]:NameSym )
	oStmt:ErrInfo:ErrorFlag := FALSE
	RETURN cTimestamp

METHOD Skip( nRecordCount ) 
	LOCAL siRecCount        AS INT

	IF ! SELF:Notify( NOTIFYINTENTTOMOVE )
		oStmt:__GenerateSQLError( "IntentToMove returned false.", #Skip )
		RETURN FALSE
	ENDIF

	IF nRecordCount = NIL
		siRecCount := 1
	ELSE
		siRecCount := nRecordCount
	ENDIF

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:Skip( "+NTrim( siRecCount )+" )" )
	#ENDIF

	IF ( siRecCount = 0 )
		RETURN TRUE
	ENDIF

	RETURN SELF:__SkipCursor( siRecCount )

METHOD SuspendNotification() 

	SELF:nNotifyCount := SELF:nNotifyCount + 1
	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:SuspendNotification() Count="+AsString( nNotifyCount ) )
	#ENDIF
	RETURN NIL

METHOD Unlock( nRecordNumber ) 

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:Unlock()" )
	#ENDIF

	RETURN TRUE


METHOD Update( lUpdateFlag ) 
	LOCAL nType         AS DWORD
	LOCAL lRet          AS LOGIC

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:Update( "+AsString( lUpdateFlag )+" )" )
	#ENDIF

	IF ! IsLogic(lUpdateFlag)
		lUpdateFlag := TRUE
	ENDIF

	//RvdH 050413 Centralize opening of cursor
	IF ! SELF:__ForceOpen()
		RETURN FALSE
	ENDIF

	IF lAppendFlag .AND. lUpdateFlag
		IF !SELF:AppendRow( TRUE )
			RETURN FALSE
		ENDIF
	ENDIF

	// Update(FALSE) means don't update...
	IF ! lUpdateFlag
		// When lUpdateFlag = FALSE, Rollback the changes
		IF lRowModified
			SELF:__CopyDataBuffer(aOriginalRecord, aSQLData)

			SELF:lRowModified := FALSE
			SELF:lAppendFlag  := FALSE
			SELF:Notify( NOTIFYRECORDCHANGE, 0 )
			RETURN TRUE
		ENDIF
		RETURN FALSE
	ENDIF

	lRet := TRUE

	IF lRowModified
		nType := SELF:__FigureScrollUpdateType()

		DO CASE
		CASE nType = SQL_SC_UPD_CURSOR
			lRet := SELF:UpdateCursor()
			IF !lRet
				//  Fall back to other methods to update
				IF ALen( SELF:aIndexCol ) != 0
					lRet := SELF:UpdateKey()
				ENDIF
				IF !lRet
					lRet := SELF:UpdateVal()
				ENDIF
			ENDIF

		CASE nType = SQL_SC_UPD_KEY
			lRet := SELF:UpdateKey()
			IF !lRet
				//  Fall back to other method to update
				lRet := SELF:UpdateVal()
			ENDIF

		CASE nType = SQL_SC_UPD_VALUE
			lRet := SELF:UpdateVal()

		ENDCASE

		IF lRet
			SELF:__UpdateLongData( FALSE )
			SELF:lRowModified := FALSE
			SELF:ResetCursor( nType )
         // When update succeeds we copy the current data as 'original'.
			SELF:__CopyDataBuffer(  SELF:aSQLData, SELF:aOriginalRecord)

			IF nRowCount = 0
				oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__NO_ROW ), #Update )
				lRet := FALSE
			ENDIF

			IF lRet
				SELF:Notify( NOTIFYFILECHANGE )
			ENDIF

		ENDIF
	ENDIF
	RETURN lRet

METHOD UpdateCursor() 
	LOCAL oUpdate       AS SQLStatement
	LOCAL lRet := FALSE AS LOGIC
   LOCAL cUpdate 		  AS STRING
	//RvdH 050413 Centralized building the Update Statement
	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:UpdateCURSOR()" )
	#ENDIF
	DO WHILE TRUE
		SELF:__GetCursorName()
		cUpdate := SELF:__BuildUpdateStmt()

		IF SLen(cUpdate) == 0
			lRet := TRUE 
			EXIT
		ENDIF

		cUpdate += " where current of " + cCursor
		#IFDEF __DEBUG__
			__SQLOutputDebug( "   " + cUpDate )
		#ENDIF

		oUpdate := oConn:__GetExtraStmt(cUpdate)
		IF oUpdate == NULL_OBJECT
			EXIT
		ENDIF
		oUpdate:SQLString := SELF:PreExecute( oUpdate:SQLString )
		lRet := oUpdate:Execute()
		IF !lRet
			SELF:__CopySQLError( #Update, oUpdate:ErrInfo )
			IF oStmt:ErrInfo:ReturnCode != SQL_SUCCESS_WITH_INFO
				oConn:__CloseExtraStmt(oUpdate)
				EXIT
			ENDIF
		ENDIF

		// save rowcount
		nRowCount := oUpdate:NumSuccessfulRows
		lRowModified := FALSE
		oConn:__CloseExtraStmt(oUpdate)
		lRet := TRUE 
		EXIT
	ENDDO  
	#IFDEF __DEBUG__
		__SQLOutputDebug( "   __UpdateCursor() returns :" + AsString( lRet ) )
	#ENDIF
	
	RETURN lRet

METHOD UpdateKey() 
	LOCAL nIndex        AS DWORD
	LOCAL cUpdate       AS STRING
	LOCAL oUpdate       AS SQLStatement
	LOCAL aDataBuffer   AS ARRAY
	LOCAL nTemp         AS INT
//	LOCAL nCount    := 0 AS INT
	LOCAL oCol			  AS SqlColumn
	LOCAL cQuote		  AS STRING
	//RvdH 050413 Centralized building the Update Statement
	IF ALen( aIndexCol ) = 0
		oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__NO_KEY ), #Update )
		RETURN FALSE
	ENDIF
	cUpdate := SELF:__BuildUpdateStmt()

	IF SLen(cUpdate) == 0
		RETURN TRUE
	ENDIF

	cUpdate     += " where "
	//aDataBuffer := aSQLDataBuffer[nBuffIndex, SQL_DATA_BUFFER]
	aDataBuffer	:= SELF:aOriginalRecord
	cQuote  		:= oStmt:Connection:IdentifierQuoteChar

	FOR nIndex := 1 TO ALen( aIndexCol )
		nTemp   := aIndexCol[nIndex]
		oCol 		:= aSQLColumns[nTemp]
      cUpdate   += cQuote + oCol:ColName + cQuote +__GetDataValuePSZ( oCol,aDataBuffer[nTemp], TRUE, TRUE  )
		IF nIndex != ALen( aIndexCol )
			cUpdate += " and "
		ENDIF
	NEXT
	oUpdate := oConn:__GetExtraStmt(cUpdate)

	SELF:__PrepareStmtOptions( oUpdate )

	oUpdate:SQLString := SELF:PreExecute( oUpdate:SQLString )

	IF !oUpdate:Execute()

		SELF:__CopySQLError( #Update, oUpdate:ErrInfo )

		IF oStmt:ErrInfo:ReturnCode != SQL_SUCCESS_WITH_INFO
			oConn:__CloseExtraStmt(oUpdate)
			RETURN FALSE
		ENDIF
	ENDIF

	nRowCount := oUpdate:NumSuccessfulRows
	lRowModified := FALSE
	oConn:__CloseExtraStmt(oUpdate)
	RETURN TRUE

METHOD UpdateVal() 
	LOCAL cUpdate       AS STRING
	LOCAL oUpdate       AS SQLStatement
	LOCAL lRet          AS LOGIC
	//LOCAL nRecno        AS LONGINT
//	LOCAL nCount   := 0 AS INT
	//RvdH 050413 Centralized building the Update Statement
	cUpdate := SELF:__BuildUpdateStmt()

	IF SLen(cUpdate) == 0
		RETURN TRUE
	ENDIF

	cUpDate += SELF:__GetUpdateVAL( FALSE )
	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:UpDateVal() " + cUpDate )
	#ENDIF

	oUpdate := oConn:__GetExtraStmt(cUpdate)

	SELF:__PrepareStmtOptions( oUpdate )
	oUpdate:SQLString := SELF:PreExecute( oUpdate:SQLString )
	//nRecno := SELF:Recno
	lRet := oUpdate:Execute()

	IF !lRet
		lRowModified := FALSE
		SELF:__CopySQLError( #Update, oUpdate:ErrInfo )

		IF oStmt:ErrInfo:ReturnCode != SQL_SUCCESS_WITH_INFO
			oConn:__CloseExtraStmt(oUpdate)
			RETURN FALSE
		ENDIF
	ENDIF

	nRowCount := oUpdate:NumSuccessfulRows
	lRowModified := FALSE
	oConn:__CloseExtraStmt(oUpdate)
	RETURN TRUE
END CLASS

