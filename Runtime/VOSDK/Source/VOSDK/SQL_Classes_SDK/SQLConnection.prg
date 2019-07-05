CLASS SQLConnection
	HIDDEN cSourceName    AS STRING
	HIDDEN cUser          AS STRING
	HIDDEN cAuthString    AS STRING
	HIDDEN cConnectString AS STRING
	HIDDEN hEnv           AS PTR
	HIDDEN hDbc           AS PTR
	HIDDEN oErrInfo       AS SQLErrorInfo
	HIDDEN lConnFlag      AS LOGIC
	HIDDEN lScrollCsr     AS LOGIC
	HIDDEN lPositionOps   AS LOGIC
	HIDDEN nConcurrency   AS INT
	HIDDEN lConnOverride  AS LOGIC
	HIDDEN lEnvOverride   AS LOGIC
	HIDDEN aStmts         AS ARRAY
	HIDDEN cIdentifierQuoteChar AS STRING
	EXPORT nActiveStmts         AS INT
	EXPORT lSelect4Update       AS LOGIC
	EXPORT lUseSingleConnection AS LOGIC
	PROTECT nODBCCursors        AS INT
	PROTECT nAccessMode         AS INT
	PROTECT nIsolationOption    AS INT
	METHOD __AllocConnect() AS LOGIC STRICT 
	LOCAL hDbcNew  AS PTR
	LOCAL nRetCode AS INT
	LOCAL lRet     AS LOGIC

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLConnection:__AllocConnect()" )
	#ENDIF

	IF ( hDbc = SQL_NULL_HDBC )
		//RvdH 050413 This should be replaced with SqlAllocHandle
		nRetCode := SQLAllocConnect( hEnv, @hDbcNew )

		IF nRetCode = SQL_SUCCESS
			oErrInfo:ErrorFlag := FALSE
			SELF:hDbc := hDbcNew

			#IFDEF __DEBUG__
				__SQLOutputDebug( "** hDbc="+AsString( hDbc ) )
			#ENDIF

			lRet := TRUE
		ELSE
			oErrInfo := SQLErrorInfo{ SELF,             ;
				#AllocConnect,    ;
				hEnv,             ;
				SQL_NULL_HDBC,    ;
				SQL_NULL_HSTMT }

			oErrInfo:ReturnCode := nRetCode
		ENDIF

	ELSE
		SELF:__GenerateSqlError( __CavoStr( __CAVOSTR_SQLCLASS__CON_ALLOC ), #AllocConnect )
	ENDIF

	RETURN lRet


METHOD __AllocEnv() AS LOGIC STRICT 
	LOCAL lRet AS LOGIC

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLConnection:__AllocEnv()" )
	#ENDIF

	IF hEnv = SQL_NULL_HENV
		SELF:hEnv := __GetMyEnv()
		IF hEnv == SQL_NULL_HENV
			oErrInfo := SQLErrorInfo{   SELF,       ;
				#AllocEnv,      ;
				SQL_NULL_HENV,  ;
				SQL_NULL_HDBC,  ;
				SQL_NULL_HSTMT }
		ELSE
			oErrInfo:ErrorFlag := FALSE
			lRet := TRUE
		ENDIF
	ELSE
		SELF:__GenerateSqlError( __CavoStr( __CAVOSTR_SQLCLASS__ENV_ALLOC ), #AllocEnv )
	ENDIF

	RETURN lRet

METHOD __CheckActiveStmts() AS LOGIC STRICT 

	LOCAL nBytes    AS SHORTINT // dcaton 070206 was INT
	LOCAL nActStmts AS INT
	LOCAL nRetCode  AS INT
	LOCAL lRet      AS LOGIC

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLConnection:__CheckActiveStmts()" )
	#ENDIF

	SELF:nActiveStmts := 0

	IF ( !SELF:lUseSingleConnection )

		nRetCode := SQLGetInfo( hDbc,                   ;
			SQL_ACTIVE_STATEMENTS,  ;
			@nActStmts,             ;
			_SIZEOF( INT ),         ;
			@nBytes )

		IF nRetCode = SQL_SUCCESS
			SELF:nActiveStmts := nActStmts
			oErrInfo:ErrorFlag := FALSE
			#IFDEF __DEBUG__
				__SQLOutputDebug( "                 __CheckActiveStmts()="+AsString( nActiveStmts ) )
			#ENDIF
			lRet := TRUE
		ENDIF

	ENDIF

	RETURN lRet


METHOD __CheckIdentQuoteChar() AS LOGIC STRICT 
	LOCAL nBytes   AS SHORTINT // dcaton 070206 was INT
	LOCAL nRetCode AS INT
	LOCAL lRet     AS LOGIC
	LOCAL DIM bData[10] AS BYTE

	SELF:cIdentifierQuoteChar := NULL_STRING
	IF hDbc != SQL_NULL_HDBC
		nRetCode := SQLGetInfo( hDbc,                       ;
			SQL_IDENTIFIER_QUOTE_CHAR,  ;
			@bData,                      ;
			10,                         ;
			@nBytes )
		IF nRetCode = SQL_SUCCESS
			IF nBytes != 0
				SELF:cIdentifierQuoteChar := Psz2String( @bData )
			ENDIF
			lRet := TRUE
		ENDIF
	ENDIF

	oErrInfo:ErrorFlag := FALSE

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLConnection:__CheckIdentQuoteChar()="+AsString( cIdentifierQuoteChar ) )
	#ENDIF

	RETURN lRet

METHOD __CheckPositionOps() AS LOGIC STRICT 
	LOCAL nPosition AS INT
	LOCAL nRetCode  AS INT
	LOCAL lRet      AS LOGIC
	LOCAL nBytes    AS SHORTINT // dcaton 070206 was INT

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLConnection:__CheckPositionOps()" )
	#ENDIF

	lPositionOps   := TRUE
	lSelect4Update := FALSE
	nRetCode := SQLGetInfo( hDbc,                       ;
		SQL_POSITIONED_STATEMENTS,  ;
		@nPosition,                 ;
		_SIZEOF( LONGINT ),           ;
		@nBytes )

	IF nRetCode = SQL_SUCCESS
		//
		//  Can we do update or delete 'where current of'?
		//
		IF _AND ( nPosition, SQL_PS_POSITIONED_DELETE ) = 0 .OR. ;
				_AND( nPosition, SQL_PS_POSITIONED_UPDATE )  = 0
			lPositionOps := FALSE
		ENDIF
		//
		// Can we select 'for update of'?
		//
		IF _AND( nPosition, SQL_PS_SELECT_FOR_UPDATE ) != 0
			lSelect4Update := TRUE
		ENDIF
		oErrInfo:ErrorFlag := FALSE
		lRet := TRUE
	ENDIF

	RETURN lRet

METHOD __CheckQE() AS LOGIC STRICT 
	//
	//  Check Q+E driver ( say we're CA )
	//
	LOCAL ptrValue AS PTR
	LOCAL nValue   AS DWORD
	LOCAL nOption  AS WORD
	LOCAL nRet     AS INT
#ifdef __VULCAN__	   // bug, Vulcan can't resolve typed func ptrs in locals, fix this
	LOCAL pFunc    AS PTR
#else
	LOCAL pFunc    AS __SQLSetConnectAttr PTR
#endif	
	LOCAL hInst    AS PTR
	LOCAL nSize    AS DWORD

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLConnection:__CheckQE()" )
	#ENDIF

	hInst := LoadLibrary( PSZ( _CAST,"ODBC32.DLL" ) )

	IF hInst != 0
		pFunc := GetProcAddress( hInst, PSZ( _CAST, "SQLSetConnectAttr" ) )
	ENDIF

	IF pFunc = NULL
		nOption  := 1041
		ptrValue := String2Psz( __CAVOSTR_SQLCLASS__QE_LIC )
		nValue   := DWORD( _CAST, ptrValue )

		//RvdH 050413 This should be replaced with a Call to SQLSetConnectAttr()
		SQLSetConnectOption( hDbc, nOption, nValue )

		nOption := 1042
		ptrValue := String2Psz( __CAVOSTR_SQLCLASS__QE_PSWD )
		nValue := DWORD( _CAST, ptrValue )
		//RvdH 050413 This should be replaced with a Call to SQLSetConnectAttr()

		SQLSetConnectOption( hDbc, nOption, nValue )
	ELSE
		nOption  := 1041
		ptrValue := String2Psz( __CAVOSTR_SQLCLASS__QE_LIC )
		nSize    := SLen( __CAVOSTR_SQLCLASS__QE_LIC )

#ifdef __VULCAN__
		nRet := PCallNative<SHORT>( pFunc, hDbc, WORD( _CAST,nOption ), ptrValue, WORD( _CAST,nSize + 1 ) )
#else		
		nRet := PCALL( pFunc, hDbc, WORD( _CAST,nOption ), ptrValue, WORD( _CAST,nSize + 1 ) )
#endif		

		IF nRet == SQL_SUCCESS

			nOption := 1042
			ptrValue := String2Psz( __CAVOSTR_SQLCLASS__QE_PSWD )
			nSize    := SLen( __CAVOSTR_SQLCLASS__QE_PSWD )

#ifdef __VULCAN__
			nRet := PCallNative<SHORT>( pFunc, hDbc, WORD( _CAST,nOption ), ptrValue, WORD( _CAST,nSize + 1 ) )
#else			
			nRet := PCALL( pFunc, hDbc, WORD( _CAST,nOption ), ptrValue, WORD( _CAST,nSize + 1 ) )
#endif			
		ENDIF
	ENDIF

	IF hInst != NULL
		FreeLibrary( hInst )
	ENDIF

	RETURN TRUE

METHOD __CheckScrollable() AS LOGIC STRICT 
	//
	//  Check if a driver supports scrollable cursors; sets lScrollCsr flag
	//  Driver must:  1. Support the SQLExtendedFetch function
	//                2. SQLGetInfo( FETCH_DIRECTION ) supports backwards
	//  Also check & save the Concurrency type supported
	//
	LOCAL nExists    AS WORD // dcaton 070206 was INT
	LOCAL nDirection AS INT
	LOCAL nConcur    AS INT
	LOCAL nBytes     AS SHORTINT // dcaton 070206 was INT
	LOCAL nRetCode   AS INT

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLConnection:__CheckScrollable()" )
	#ENDIF

	IF SELF:lScrollCsr = FALSE
		RETURN FALSE
	ENDIF

	nConcurrency := SQL_CONCUR_READ_ONLY

	IF SQLGetFunctions( hDbc, SQL_API_SQLEXTENDEDFETCH, @nExists ) != SQL_SUCCESS
		RETURN FALSE
	ENDIF

	IF nExists != 1
		RETURN FALSE
	ENDIF

	nRetCode := SQLGetInfo( hDbc, SQL_FETCH_DIRECTION, @nDirection, _SIZEOF( LONGINT ), @nBytes )

	IF nRetCode != SQL_SUCCESS
		RETURN FALSE
	ENDIF

	IF _AND( nDirection, SQL_FD_FETCH_NEXT )   =  SQL_FD_FETCH_NEXT     .AND. ;
			_AND( nDirection, SQL_FD_FETCH_FIRST )   =  SQL_FD_FETCH_FIRST    .AND. ;
			_AND( nDirection, SQL_FD_FETCH_LAST )   =  SQL_FD_FETCH_LAST     .AND. ;
			_AND( nDirection, SQL_FD_FETCH_PRIOR )   =  SQL_FD_FETCH_PRIOR    .AND. ;
			_AND( nDirection, SQL_FD_FETCH_ABSOLUTE )= SQL_FD_FETCH_ABSOLUTE  .AND. ;
			_AND( nDirection, SQL_FD_FETCH_RELATIVE )= SQL_FD_FETCH_RELATIVE

		SELF:lScrollCsr := TRUE           // OK, use the driver!

		nRetCode := SQLGetInfo( hDbc,                           ;
			SQL_SCROLL_CONCURRENCY,         ;
			@nConcur,                       ;
			_SIZEOF( LONGINT ),               ;
			@nBytes )

		IF nRetCode == SQL_SUCCESS

			IF _AND( nConcur,SQL_SCCO_OPT_VALUES ) = SQL_SCCO_OPT_VALUES
				nConcurrency := SQL_CONCUR_VALUES
			ELSEIF _AND( nConcur,SQL_SCCO_OPT_TIMESTAMP )=SQL_SCCO_OPT_TIMESTAMP
				nConcurrency := SQL_CONCUR_TIMESTAMP
			ELSEIF _AND( nConcur,SQL_SCCO_LOCK ) = SQL_SCCO_LOCK
				nConcurrency := SQL_CONCUR_LOCK
			ELSEIF _AND( nConcur,SQL_SCCO_READ_ONLY ) = SQL_SCCO_READ_ONLY
				nConcurrency := SQL_CONCUR_READ_ONLY
			ENDIF

			#IFDEF __DEBUG__
				__SQLOutputDebug( "   nConcur=" + AsString(  nConcurrency ) )
				__SQLOutputDebug( "   lScrollCsr=" + AsString( lScrollCsr ) )
			#ENDIF
		ENDIF
	ENDIF

	RETURN SELF:lScrollCsr

METHOD __CloseExtraStmt(oStmt AS SQLStatement)  AS VOID STRICT 
	LOCAL oNewStmt AS SQLStatement
	oNewStmt := oStmt
	oNewStmt:FreeStmt(SQL_DROP)
	IF oNewStmt:Connection != SELF
		oNewStmt:Connection:__Free()
	ENDIF
	RETURN

METHOD __Free() AS LOGIC STRICT 
	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLConnection:__Free()" )
	#ENDIF

	SELF:Disconnect()
	SELF:__FreeConnect()

	RETURN SELF:__FreeEnv()

METHOD __FreeConnect() AS LOGIC STRICT 
	LOCAL nRetCode AS INT
	LOCAL lRet     AS LOGIC

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLConnection:__FreeConnect() hDbc="+AsString( hDbc ) )
	#ENDIF

	IF hDbc = SQL_NULL_HDBC
		SELF:__GenerateSqlError( __CavoStr( __CAVOSTR_SQLCLASS__CON_FREE ), #FreeConnect )
	ELSE
		nRetCode := SQLFreeConnect( hDbc )

		IF nRetCode = SQL_SUCCESS
			SELF:hDbc := SQL_NULL_HDBC
			oErrInfo:ErrorFlag := FALSE
			lRet := TRUE
		ELSE
			oErrInfo := SQLErrorInfo{ SELF, #FreeConnect, hEnv,      ;
				SQL_NULL_HDBC, SQL_NULL_HSTMT }
			oErrInfo:ReturnCode := nRetCode
		ENDIF
	ENDIF

	RETURN lRet

METHOD __FreeEnv () AS LOGIC STRICT 
	LOCAL lRet AS LOGIC

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLConnection:__FreeEnv hEnv="+AsString( hEnv ) )
	#ENDIF

	IF hEnv = SQL_NULL_HENV
		SELF:__GenerateSqlError( __CavoStr( __CAVOSTR_SQLCLASS__ENV_FREE ), #FreeEnv )
	ELSE
		IF  __DropMyEnv( hEnv )
			SELF:hEnv := SQL_NULL_HENV
			oErrInfo:ErrorFlag := FALSE
			lRet := TRUE
		ELSE
			oErrInfo := SQLErrorInfo{   SELF,           ;
				#FreeEnv,       ;
				SQL_NULL_HENV,  ;
				SQL_NULL_HDBC,  ;
				SQL_NULL_HSTMT }
		ENDIF
	ENDIF

	RETURN lRet

METHOD __GenerateSqlError( cErrorString AS STRING, symMethod AS SYMBOL) AS SQLErrorInfo 

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLConnection:__GenerateSQLError( "+         ;
			AsString( cErrorString )+","+AsString( symMethod )+" )" )
	#ENDIF
	SELF:oErrInfo:ErrorMessage := __CavoStr( __CAVOSTR_SQLCLASS__ODBC_VO ) +   ;
		Symbol2String( ClassName( SELF ) ) +   ;
		":" + Symbol2String( symMethod ) +    ;
		" " + cErrorString
	SELF:oErrInfo:NativeError := 0
	SELF:oErrInfo:SQLState    := __CavoStr( __CAVOSTR_SQLCLASS__GENERAL_ERR )
	SELF:oErrInfo:ErrorFlag   := TRUE
	SELF:oErrInfo:SubSystem   := __CavoStr( __CAVOSTR_SQLCLASS_SUBSYS )
	SELF:oErrInfo:MethodSelf  := SELF
	SELF:oErrInfo:FuncSym     := symMethod
	SELF:oErrInfo:CallFuncSym := symMethod
	SELF:oErrInfo:Severity    := ES_ERROR
	SELF:oErrInfo:ReturnCode  := SQL_SUCCESS
	RETURN oErrInfo

METHOD __GetExtraStmt(cStmtText AS STRING) AS SqlStatement STRICT 
	LOCAL oNewConn AS SQLConnection
	LOCAL oNewStmt AS SQLStatement
	IF SELF:nActiveStmts = 1
		oNewConn := SQLConnection{ SELF:cSourceName, ;
			SELF:cUser,     ;
			SELF:cAuthString }
		IF !oNewConn:Connected
			RETURN NULL_OBJECT
		ENDIF
	ELSE
		oNewConn := SELF
	ENDIF
	oNewStmt := SQLStatement{ cStmtText, oNewConn }
	RETURN oNewStmt

METHOD __RegisterStmt( oStmt AS SQLStatement ) AS LOGIC STRICT 
	LOCAL nIndex AS DWORD
	LOCAL nCount AS DWORD
	LOCAL lFound AS LOGIC

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** __RegisterStmt( "+AsString( oStmt:StatementHandle )+" )" )
	#ENDIF

	nCount := ALen( aStmts )
	//
	//  Check if already registered...
	//
	FOR nIndex := 1 TO nCount
		IF aStmts[nIndex] != NIL .AND. ;
				aStmts[nIndex] = oStmt
			#IFDEF __DEBUG__
				__SQLOutputDebug( "** already registered, index="+AsString( nIndex ) )
			#ENDIF
			lFound := TRUE
			EXIT
		ENDIF
	NEXT
	IF !lFound
		AAdd( aStmts, oStmt )
	ENDIF
	RETURN TRUE

METHOD __Transact( nType AS WORD) AS LOGIC STRICT 
	//
	//  Commit/Rollback transaction; returns TRUE if successful
	//
	LOCAL nRetCode  AS INT
	LOCAL lRet      AS LOGIC

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLConnection:Transact()" )
	#ENDIF

	nRetCode := SQLTransact( SQL_NULL_HENV, SELF:ConnHandle, nType )

	IF  nRetCode = SQL_SUCCESS
		SELF:oErrInfo:ErrorFlag := FALSE
		lRet := TRUE
	ELSE
		oErrInfo := SQLErrorInfo{   SELF,            ;
			#Transact,       ;
			SELF:EnvHandle,  ;
			SELF:ConnHandle, ;
			SQL_NULL_HSTMT }

		oErrInfo:ReturnCode := nRetCode
	ENDIF

	RETURN lRet

METHOD __UnregisterStmt( oStmt AS SQLStatement ) AS LOGIC STRICT 
	LOCAL nIndex AS DWORD
	LOCAL nCount AS DWORD
	LOCAL lRet   AS LOGIC

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** __UnRegisterStmt( "+AsString( oStmt:StatementHandle )+" )" )
	#ENDIF

	nCount := ALen( aStmts )

	FOR nIndex := 1 TO nCount
		IF aStmts[nIndex] != NIL .AND.              ;
				aStmts[nIndex] = oStmt
			#IFDEF __DEBUG__
				__SQLOutputDebug( "** found oStmt, index="+AsString( nIndex ) )
			#ENDIF

			ADel( aStmts, nIndex )
			ASize( aStmts, nCount - 1 )
			lRet := TRUE
			EXIT
		ENDIF
	NEXT

	RETURN lRet

ACCESS AccessMode 

	RETURN SELF:nAccessMode

ASSIGN AccessMode( nVal ) 

	IF IsNumeric( nVal ) .AND. ;
			( nVal == SQL_MODE_READ_ONLY .OR. ;
			nVal == SQL_MODE_READ_WRITE )

		SELF:SetConnectOption( SQL_ACCESS_MODE, nVal )
		RETURN SELF:nAccessMode := nVal
	ELSE
		RETURN SELF:nAccessMode
	ENDIF
	//RETURN 

DESTRUCTOR() 

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLConnection:Axit() hEnv="+ AsString( hEnv ) + ;
			", hDbc="+ AsString( hDbc ) )
	#ENDIF

	RETURN SELF:Disconnect()

METHOD BeginTransaction() 

	RETURN SELF:SetConnectOption( SQL_AUTOCOMMIT, SQL_AUTOCOMMIT_OFF )

METHOD Commit() 

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLConnection:Commit()" )
	#ENDIF
	RETURN SELF:__Transact( SQL_COMMIT )

METHOD Connect( cDataSource, cUserID, cPassword ) 
	LOCAL nRetCode       AS SHORTINT
	LOCAL nTemp          AS DWORD
	LOCAL lRet           AS LOGIC
	LOCAL pUser          AS PSZ
	LOCAL pPassword      AS PSZ
	LOCAL pSource        AS PSZ

	//  UH 11/13/2001
	DEFAULT( @cDataSource, "" )
	DEFAULT( @cUserID, "" )
	DEFAULT( @cPassword, "" )

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLConnection:Connect( "+AsString( cDataSource )+","+  ;
			AsString( cUserID )+","+AsString( cPassword )+" )" )
	#ENDIF

	IF !SELF:lConnFlag
		SELF:cSourceName := cDataSource
		SELF:cUser       := cUserID
		SELF:cAuthString := cPassword

		nTemp   := SLen( cDataSource )
		pSource := MemAlloc( nTemp+1 )
		IF pSource == NULL_PSZ
			SQLThrowOutOfMemoryError()
			RETURN FALSE
		ENDIF
		//RvdH 030925 Fixes for BUG # 60
		//RvdH 030925 Changed to nTemp > 0
		//RvdH 030925 Changed to use MemCopyString
		IF nTemp > 0
			//MemCopy( pSource, PTR( _CAST, cDataSource ), nTemp )
			MemCopyString( pSource, cDataSource , nTemp )
		ELSE
			MemSet( pSource, 0, 1 )
		ENDIF

		nTemp := SLen( SELF:cUser )
		pUser := MemAlloc( nTemp+1 )
		IF pUser == NULL_PSZ
			MemFree( pSource )
			SQLThrowOutOfMemoryError()
			RETURN FALSE
		ENDIF
		//RvdH 030925 Changed to nTemp > 0
		//RvdH 030925 Changed to use MemCopyString
		IF nTemp > 0
			//MemCopy( pUser, PTR( _CAST, SELF:cUser ), nTemp )
			MemCopyString( pUser, SELF:cUser , nTemp )
		ELSE
			MemSet( pUser, 0, 1 )
		ENDIF

		nTemp := SLen( SELF:cAuthString )
		pPassword := MemAlloc( nTemp+1 )
		IF pPassword == NULL_PSZ
			MemFree( pUser )
			MemFree( pSource )
			SQLThrowOutOfMemoryError()
			RETURN FALSE
		ENDIF
		//RvdH 030925 Changed to nTemp > 0
		//RvdH 030925 Changed to use MemCopyString
		IF nTemp > 0
			//MemCopy( pPassword, PTR( _CAST, SELF:cAuthString ), nTemp )
			MemCopyString(pPassword, SELF:cAuthString , nTemp )
		ELSE
			MemSet( pPassword, 0, 1 )
		ENDIF

		nRetCode := SQLConnect( SELF:hDbc, ;
			pSource,   ;
			SQL_NTS,   ;
			pUser,     ;
			SQL_NTS,   ;
			pPassword, ;
			SQL_NTS )
		MemFree( pSource )
		MemFree( pUser )
		MemFree( pPassword )

		IF nRetCode != SQL_SUCCESS .AND. nRetCode != SQL_SUCCESS_WITH_INFO

			oErrInfo := SQLErrorInfo{   SELF,    ;
				#Connect,;
				hEnv,    ;
				hDbc,    ;
				SQL_NULL_HSTMT }

			oErrInfo:ReturnCode := nRetCode
			lConnFlag := FALSE
			IF lConnErrMsg
				oErrInfo:ShowErrorMsg()
			ENDIF
		ELSE
			lConnFlag := TRUE
			SELF:__CheckScrollable()
			SELF:__CheckPositionOps()
			SELF:__CheckQE()
			SELF:__CheckIdentQuoteChar()
			SELF:__CheckActiveStmts()
			SELF:cConnectString := __CAVOSTR_SQLCLASS__DSN + cSourceName
			IF SLen( cUser ) != 0
				cConnectString += Chr( 59 ) + __CAVOSTR_SQLCLASS__UID + cUser
			ENDIF
			IF SLen( cAuthString ) != 0
				cConnectString += Chr( 59 ) + __CAVOSTR_SQLCLASS__PWD + cAuthString
			ENDIF
			oErrInfo:ErrorFlag := FALSE
			lRet := TRUE
		ENDIF
	ELSE
		SELF:__GenerateSqlError( __CavoStr( __CAVOSTR_SQLCLASS__CONNECTED ), #connect )
	ENDIF
	RETURN lRet

ACCESS Connected 

	RETURN SELF:lConnFlag

ACCESS ConnectString 

	RETURN SELF:cConnectString

ACCESS ConnHandle 

	RETURN SELF:hDbc

ASSIGN ConnHandle( uVal ) 

	IF SELF:hDbc = SQL_NULL_HDBC
		SELF:hDbc := uVal
	ELSE
		IF lConnFlag
			IF !SELF:Disconnect()
				oErrInfo:Throw()
			ENDIF
		ENDIF
		IF !SELF:__FreeConnect()
			oErrInfo:Throw()
		ENDIF
		SELF:hDbc := uVal
		#IFDEF __DEBUG__
			__SQLOutputDebug( "** SQLConnection:ConnHandle reassign hDbc="+AsString( hDbc ) )
		#ENDIF
		lConnOverride := TRUE
	ENDIF

	RETURN SELF:hDbc

ACCESS DataSource 

	RETURN cSourceName

ASSIGN DataSource( uVal ) 
	IF !lConnFlag
		SELF:cSourceName := uVal
	ELSE
		SELF:__GenerateSqlError( __CavoStr( __CAVOSTR_SQLCLASS__CONNECTED ), #DataSource )
		oErrInfo:Throw()
	ENDIF

	RETURN SELF:cSourceName

METHOD Disconnect() 
	LOCAL nRetCode  AS SHORTINT
	LOCAL lRet      AS LOGIC
	LOCAL nIndex    AS DWORD

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLConnection:Disconnect(), hDbc="+ AsString( hDbc ) )
	#ENDIF

	FOR nIndex := 1 TO ALen( aStmts )
		IF aStmts[nIndex] != NIL
			aStmts[nIndex]:Destroy()
		ENDIF
	NEXT

	IF lConnFlag .AND. !lConnOverride
		IF hDbc = SQL_NULL_HDBC
			lRet := TRUE
		ELSE
			#IFDEF __DEBUG__
				__SQLOutputDebug( "** Disconnect, hDbc="+ AsString( hDbc ) )
			#ENDIF

			SELF:Rollback()
			nRetCode := SQLDisconnect( hDbc )
			IF nRetCode = SQL_SUCCESS
				#IFDEF __DEBUG__
					__SQLOutputDebug( "** Freeconnect, hDbc="+ AsString( hDbc ) )
				#ENDIF
				//RvdH 050413 This should be replaced with a Call to SQLFreeHandle()

				SQLFreeConnect( hDbc )
				__RemoveConnection( hDbc )
				lRet := TRUE
			ELSE
				oErrInfo := SQLErrorInfo{SELF,          ;
					#Disconnect,    ;
					hEnv,           ;
					hDbc, SQL_NULL_HSTMT }

				oErrInfo:ReturnCode := nRetCode
			ENDIF
		ENDIF
		IF lRet
			lConnFlag := FALSE
			lScrollCsr := FALSE
			nConcurrency := SQL_CONCUR_READ_ONLY
			lPositionOps := TRUE
			oErrInfo:ErrorFlag := FALSE
		ENDIF
	ELSE
		lRet := TRUE
	ENDIF

	UnregisterAxit( SELF )
	IF hEnv != SQL_NULL_HENV .AND. !lEnvOverride
		__DropMyEnv( hEnv )
	ENDIF

	RETURN lRet

METHOD DriverConnect( hWindow, nDriverCompletion, cConnStrIn ) 
	LOCAL nBytes      AS SHORTINT // dcaton 070206 was INT
	LOCAL nRetCode    AS INT
	LOCAL nStart      AS DWORD
	LOCAL nEnd        AS DWORD
	LOCAL pszConnect  AS PSZ
	LOCAL pszConStr   AS PSZ
	LOCAL nCompletion AS DWORD
	LOCAL nSize		  	 AS SHORTINT
	LOCAL lRet        AS LOGIC
	LOCAL hWnd        AS PTR

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLConnection:DriverConnect( "+;
			AsString( hWindow )+","+;
			AsString( nDriverCompletion )+","+ ;
			AsString( cConnStrIn )+" )" )
	#ENDIF

	IF IsNumeric( nDriverCompletion )
		nCompletion := nDriverCompletion
	ELSE
		nCompletion := SQL_DRIVER_PROMPT
	ENDIF

	IF !lConnFlag
		IF IsPtr( hWindow )
			hWnd := hWindow
		ENDIF
		IF hWnd = NULL_PTR
			hWnd := GetActiveWindow()
		ENDIF

		#IFDEF __DEBUG__
			__SQLOutputDebug( "  DriverConnect,hWnd="+AsString( hWnd ) )
		#ENDIF

		pszConnect := MemAlloc( SQL_MAX_MESSAGE_LENGTH + 1 )
		IF pszConnect == NULL_PSZ
			SQLThrowOutOfMemoryError()
			RETURN FALSE
		ENDIF
		IF IsString( cConnStrIn )
			nSize     := SHORTINT( _CAST, SLen( cConnStrIn ) + 1 )
			pszConStr := MemAlloc( nSize )
			IF PszConStr == NULL_PSZ
				MemFree(pszConnect)
				SQLThrowOutOfMemoryError()
				RETURN FALSE
			ENDIF
			IF nSize > 1
				MemCopy( pszConStr , String2Psz(cConnStrIn ), nSize )
			ELSE
				MemSet( pszConStr, 0, 1 )
			ENDIF
			nSize := SQL_NTS
		ELSE
			pszConStr := String2Psz( "" )
			nSize := 0
		ENDIF

		nRetCode := SQLDriverConnect( 	hDbc,                    ;
													hWnd,                    ;
													pszConStr,               ;
													SHORTINT( _CAST,nSize ), ;
													pszConnect,              ;
													SQL_MAX_MESSAGE_LENGTH,  ;
													@nBytes,                 ;
													WORD( _CAST,nCompletion ) )

		IF nSize > 0
			MemFree( pszConStr )
		ENDIF

		IF nRetCode = SQL_NO_DATA_FOUND
			SELF:__GenerateSqlError( __CavoStr( __CAVOSTR_SQLCLASS__CANCELLED ), #DriverConnect )

		ELSEIF nRetCode != SQL_SUCCESS .AND. nRetCode != SQL_SUCCESS_WITH_INFO
			oErrInfo := SQLErrorInfo{SELF,           ;
											#DriverConnect, ;
											hEnv,           ;
											hDbc,           ;
											SQL_NULL_HSTMT }
							
			oErrInfo:ReturnCode := nRetCode

			IF lConnErrMsg
				oErrInfo:ShowErrorMsg()
			ENDIF
		ELSE
			lConnFlag := TRUE
			SELF:__CheckScrollable()
			SELF:__CheckPositionOps()
			SELF:__CheckQE()
			SELF:__CheckIdentQuoteChar()
			SELF:__CheckActiveStmts()

			cConnectString := Psz2String( pszConnect )
			oErrInfo:ErrorFlag := FALSE
			nStart := At2( __CAVOSTR_SQLCLASS__DSN, cConnectString )
			nEnd   := At3( Chr( 59 ), cConnectString, nStart )
			IF nStart != 0
				IF nEnd != 0
					cSourceName := SubStr3( cConnectString, nStart+4, nEnd - nStart - 4 )
				ELSE
					cSourceName := SubStr3( cConnectString, nStart+4, DWORD(nBytes) - nStart - 3 )
				ENDIF
			ENDIF

			nStart := At2( __CAVOSTR_SQLCLASS__UID, cConnectString )
			nEnd   := At3( Chr( 59 ), cConnectString, nStart )

			IF nStart != 0
				IF nEnd != 0
					cUser := SubStr3( cConnectString, nStart+4, nEnd - nStart - 4 )
				ELSE
					cUser := SubStr3( cConnectString, nStart+4, DWORD(nBytes) - nStart - 3  )
				ENDIF
			ENDIF

			nStart  := At2( __CAVOSTR_SQLCLASS__PWD, cConnectString )
			nEnd    := At3( Chr( 59 ), cConnectString, nStart )

			IF nStart != 0
				IF nEnd != 0
					cAuthString := SubStr3( cConnectString, nStart+4, nEnd - nStart - 4 )
				ELSE
					cAuthString := SubStr3( cConnectString, nStart+4, DWORD(nBytes) - nStart - 3    )
				ENDIF
			ENDIF

			#IFDEF __DEBUG__
				__SQLOutputDebug( "  DriverConnect ConnectString="+cConnectString )
			#ENDIF

			lRet := TRUE
		ENDIF
		MemFree( pszConnect )
	ELSE
		SELF:__GenerateSqlError( __CavoStr( __CAVOSTR_SQLCLASS__CONNECTED ), #DriverConnect )
	ENDIF

	RETURN lRet


METHOD EndTransaction() 

	RETURN SELF:SetConnectOption( SQL_AUTOCOMMIT, SQL_AUTOCOMMIT_ON )

ACCESS EnvHandle 

	RETURN hEnv

ASSIGN EnvHandle( uVal ) 

	IF SELF:hEnv = SQL_NULL_HENV
		IF IsPtr( uVal )
			SELF:hEnv := uVal
		ENDIF
	ELSE
		IF hEnv != uVal
			SELF:__FreeEnv()
			IF IsPtr( uVal )
				SELF:hEnv := uVal
			ENDIF

			#IFDEF __DEBUG__
				__SQLOutputDebug( "** SQLConnection:EnvHandle reassign hEnv="+AsString( hEnv ) )
			#ENDIF
			lEnvOverride := TRUE
		ENDIF
	ENDIF
	RETURN SELF:hEnv

ACCESS ErrInfo 
	IF oErrInfo:ErrorFlag
		RETURN oErrInfo
	ENDIF
	RETURN NIL

METHOD GetConnectOption( nOption ) 
	LOCAL nValue        AS INT
	LOCAL pData         AS PTR
	LOCAL xRet          AS USUAL
	LOCAL nRetCode      AS INT

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLConnection:GetConnectOption( "+AsString( nOption )+" )" )
	#ENDIF

	IF hDbc = SQL_NULL_HDBC
		SELF:__GenerateSqlError( __CavoStr( __CAVOSTR_SQLCLASS__NOT_CONN ), #GetConnectOption )
	ELSE
		IF nOption = SQL_OPT_TRACEFILE .OR. nOption = SQL_TRANSLATE_DLL
			pData := MemAlloc( 256 )
			IF pData == NULL_PTR
				SQLThrowOutOfMemoryError()
				RETURN NIL
			ENDIF
		ELSE
			pData := @nValue
		ENDIF
		//RvdH 050413 This should be replaced with a Call to SQLGetConnectAttr()

		nRetCode := SQLGetConnectOption( hDbc, nOption, pData )
		IF nRetCode = SQL_SUCCESS
			oErrInfo:ErrorFlag := FALSE
			IF nOption = SQL_OPT_TRACEFILE .OR. nOption = SQL_TRANSLATE_DLL
				xRet := Psz2String( pData )
				MemFree( pData )
			ELSE
				xRet := nValue
			ENDIF

		ELSE
			oErrInfo := SQLErrorInfo{ SELF,                     ;
				#GetConnectOption, hEnv,  ;
				hDbc,                     ;
				SQL_NULL_HSTMT }
			oErrInfo:ReturnCode := nRetCode
			IF nOption = SQL_OPT_TRACEFILE .OR. nOption = SQL_TRANSLATE_DLL
				MemFree( pData )
			ENDIF
			RETURN NIL
		ENDIF

	ENDIF

	RETURN xRet

ACCESS HyperLabel 
	LOCAL oHL AS HyperLabel

	IF SLen( SELF:cSourceName ) > 0
		oHL := HyperLabel{  cSourceName, ;
			cSourceName, ;
			Symbol2String( ClassName( SELF )  )+ ": " + cSourceName, ;
			Symbol2String( ClassName( SELF ) )+ "_" + cSourceName  }
	ENDIF

	RETURN oHL




ACCESS IdentifierQuoteChar 

	RETURN cIdentifierQuoteChar

ASSIGN IdentifierQuoteChar( uVal ) 

	SELF:cIdentifierQuoteChar := uVal

	RETURN 

METHOD Info( nInfoType ) 
	LOCAL dwValue       AS DWORD
	LOCAL wValue        AS DWORD
	LOCAL pData         AS PTR
	LOCAL nBytes        AS SHORTINT 
	LOCAL nRetCode      AS INT
	LOCAL xRet          AS USUAL
	LOCAL lIsString     AS LOGIC
	LOCAL lIsWord       AS LOGIC
	LOCAL oStmt			  AS SQLStatement

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLConnection:Info( "+AsString( nInfoType )+" )" )
	#ENDIF
	IF hDbc = SQL_NULL_HDBC
		SELF:__GenerateSqlError( __CavoStr( __CAVOSTR_SQLCLASS__NOT_CONN ), #GetInfo )
	ELSE
		IF AScan( aInfoString, nInfoType ) > 0
			lIsString := TRUE
			pData := MemAlloc( MAX_CONNECT_INFO_STRING + 1 )
			IF pData == NULL_PTR
				SQLThrowOutOfMemoryError()
				RETURN NIL
			ENDIF
		ELSEIF AScan( aInfoWord, nInfoType ) > 0
			pData := @wValue
			lIsWord := TRUE
		ELSE
			IF nInfoType = SQL_DRIVER_HSTMT
				IF ALen( aStmts ) > 0
					oStmt   := SELF:aStmts[1]
					dwValue := DWORD( _CAST,oStmt:StatementHandle )
				ENDIF
			ENDIF
			pData := @dwValue

		ENDIF
		nRetCode := SQLGetInfo( hDbc,                   ;
			nInfoType,              ;
			pData,                  ;
			MAX_CONNECT_INFO_STRING,;
			@nBytes )

		IF nRetCode = SQL_SUCCESS
			oErrInfo:ErrorFlag := FALSE
			IF lIsString
				xRet := Psz2String( pData )
				MemFree( pData )
			ELSEIF lIsWord
				xRet := wValue
			ELSE
				xRet := dwValue
			ENDIF
		ELSE
			oErrInfo := SQLErrorInfo{   SELF,       ;
				#GetInfo,   ;
				hEnv,       ;
				hDbc,       ;
				SQL_NULL_HSTMT }

			oErrInfo:ReturnCode := nRetCode
			IF lIsString
				MemFree( pData )
			ENDIF
		ENDIF
	ENDIF

	RETURN xRet

CONSTRUCTOR ( cDataSourceName, cUserID, cPassword ) 

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLConnection:Init( "+AsString( cDataSourceName )+","+     ;
			AsString( cUserID )+","+AsString( cPassword )+" )" )
	#ENDIF

	aStmts := {}
	SELF:lConnFlag    := FALSE
	SELF:lScrollCsr   := FALSE
	SELF:lPositionOps := TRUE
	SELF:cConnectString := ""
	SELF:lEnvOverride  := FALSE
	SELF:lConnOverride := FALSE
	oErrInfo := SQLErrorInfo{}
	
	IF !SELF:__AllocEnv()
		RETURN
	ENDIF
	IF !SELF:__AllocConnect()
		SELF:__FreeEnv()
		RETURN
	ENDIF

	SELF:lUseSingleConnection := TRUE
	IF SELF:nODBCCursors == 0
		SELF:nODBCCursors := __CAVO_SQL_ODBC_CURSORS
	ENDIF
	SELF:ODBCCursors := SELF:nODBCCursors

	IF SELF:nAccessMode == 0
		SELF:nAccessMode := __CAVO_SQL_MODE_READ_WRITE
	ENDIF
	SELF:AccessMode := SELF:nAccessMode

	IF SELF:nIsolationOption == 0
		SELF:nIsolationOption := __CAVO_SQL_TXN_READ_COMMITTED
	ENDIF
	SELF:IsolationOption := SELF:nIsolationOption

	#IFDEF __DEBUG__
		__SQLOutputDebug( "**** Scrollable: " + AsString( SELF:lScrollCsr ) )
	#ENDIF

	IF IsString( cUserID )
		SELF:cUser := cUserID
	ENDIF

	IF IsString( cPassword )
		SELF:cAuthString := cPassword
	ENDIF

	IF IsString( cDataSourceName )
		SELF:cSourceName := cDataSourceName

		SELF:connect( cSourceName, cUser, cAuthString )
	ENDIF

	RETURN 

METHOD isFunction( nFunction ) 
	//
	//  Determine if a function is supported; returns TRUE/FALSE, NIL if err
	//
	LOCAL nExists   AS WORD // dcaton 070206 was INT
	LOCAL xRet      AS USUAL
	LOCAL nRetCode  AS INT

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLConnection:IsFunction( "+AsString( nFUNCTION )+" )" )
	#ENDIF

	IF hDbc = SQL_NULL_HDBC
		SELF:__GenerateSqlError( __CavoStr( __CAVOSTR_SQLCLASS__NOT_CONN ), #IsFunction )
	ELSE
		nRetCode := SQLGetFunctions( hDbc, nFunction, @nExists )

		IF nRetCode = SQL_SUCCESS
			oErrInfo:ErrorFlag := FALSE
			IF nExists = 0
				xRet := FALSE
			ELSE
				xRet := TRUE
			ENDIF

		ELSE
			oErrInfo := SQLErrorInfo{   SELF,           ;
				#IsFunction,    ;
				hEnv,           ;
				hDbc,           ;
				SQL_NULL_HSTMT }
			oErrInfo:ReturnCode := nRetCode
		ENDIF
	ENDIF

	RETURN xRet



ACCESS IsolationOption 

	RETURN SELF:nIsolationOption

ASSIGN IsolationOption( nVal ) 

	IF IsNumeric( nVal ) .AND. ;
			( nVal == SQL_TXN_READ_UNCOMMITTED .OR. ;
			nVal == SQL_TXN_READ_COMMITTED .OR. ;
			nVal == SQL_TXN_REPEATABLE_READ .OR. ;
			nVal == SQL_TXN_SERIALIZABLE .OR. ;
			nVal == SQL_TXN_VERSIONING )

		SELF:SetConnectOption( SQL_TXN_ISOLATION_OPTION, nVal )
		SELF:nIsolationOption := nVal
	ENDIF
	RETURN SELF:nIsolationOption

ACCESS ODBCCursors 

	RETURN SELF:nODBCCursors

ASSIGN ODBCCursors( nVal ) 

	IF IsNumeric( nVal ) .AND. ;
			( nVal == SQL_CUR_USE_IF_NEEDED .OR. ;
			nVal == SQL_CUR_USE_ODBC .OR. ;
			nVal == SQL_CUR_USE_DRIVER )
		SELF:lScrollCsr := SELF:SetConnectOption( SQL_ODBC_CURSORS, nVal )
		SELF:nODBCCursors := nVal
	ENDIF
	RETURN SELF:nODBCCursors

ACCESS Password 

	RETURN cAuthString

ASSIGN Password( uVal ) 
	IF  !lConnFlag
		SELF:cAuthString := uVal
	ELSE
		SELF:__GenerateSqlError( __CavoStr( __CAVOSTR_SQLCLASS__CONNECTED ), #Password )
		oErrInfo:Throw()
	ENDIF

	RETURN SELF:cAuthString

ACCESS PositionOps 

	RETURN lPositionOps

METHOD Reconnect() 

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLConnection:Reconnect()" )
	#ENDIF
	IF lConnFlag
		SELF:Disconnect()
	ENDIF
	RETURN SELF:connect()

METHOD Rollback() 

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLConnection:Rollback()" )
	#ENDIF

	RETURN SELF:__Transact( SQL_ROLLBACK )

ACCESS ScrollConcurrency 

	RETURN nConcurrency

ASSIGN ScrollConcurrency( nVal ) 

	IF IsNumeric( nVal ) .AND. ;
			( nVal == SQL_CONCUR_READ_ONLY .OR. ;
			nVal == SQL_CONCUR_LOCK .OR. ;
			nVal == SQL_CONCUR_ROWVER .OR. ;
			nVal == SQL_CONCUR_VALUES )
		SELF:nConcurrency := nVal
	ENDIF
	RETURN SELF:nConcurrency

ACCESS ScrollCsr 

	RETURN SELF:lScrollCsr

ASSIGN ScrollCsr( uVal ) 

	IF IsLogic( uVal )
		SELF:lScrollCsr := uVal
	ENDIF

	RETURN 

METHOD SetConnectOption( nOption, uValue ) 
	LOCAL cValue        AS STRING
	LOCAL nValue        AS DWORD
	LOCAL nRetCode      AS INT
	LOCAL lRet          AS LOGIC

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLConnection:SetConnectOption( "+AsString( nOption )+ ;
			","+AsString( uValue )+" )" )
	#ENDIF
	IF hDbc = SQL_NULL_HDBC
		SELF:__GenerateSqlError( __CavoStr( __CAVOSTR_SQLCLASS__NOT_CONN ), #SetConnectOption )
	ELSE
		IF nOption = SQL_OPT_TRACEFILE .OR. nOption = SQL_TRANSLATE_DLL
			cValue := uValue
			nValue := DWORD( _CAST, String2Psz( cValue ) )
		ELSE
			nValue := uValue
		ENDIF
		//RvdH 050413 This should be replaced with a Call to SQLSetConnectAttr()

		nRetCode := SQLSetConnectOption( hDbc, nOption, nValue )
		#IFDEF __DEBUG__
			__SQLOutputDebug( "** nRetCode : " + NTrim( nRetCode ) )
		#ENDIF
		IF nRetCode = SQL_SUCCESS
			oErrInfo:ErrorFlag := FALSE
			lRet := TRUE
		ELSE
			oErrInfo := SQLErrorInfo{   SELF,               ;
				#SetConnectOption,  ;
				hEnv,               ;
				hDbc,               ;
				SQL_NULL_HSTMT }
			oErrInfo:ReturnCode := nRetCode
		ENDIF
	ENDIF
	RETURN lRet

ACCESS Status 
	LOCAL oStatus AS HyperLabel

	IF oErrInfo:ErrorFlag
		oStatus := HyperLabel{  oErrInfo:FuncSym,  ;
			oErrInfo:SQLState, ;
			Symbol2String( ClassName( oErrInfo:MethodSelf ) ) + ": " + ;
			oErrInfo:ErrorMessage }
	ENDIF

	RETURN oStatus

ACCESS UserID 

	RETURN SELF:cUser

ASSIGN UserID( uVal ) 

	IF  !lConnFlag
		SELF:cUser := uVal
	ELSE
		SELF:__GenerateSqlError( __CavoStr( __CAVOSTR_SQLCLASS__CONNECTED ), #UserID )
		oErrInfo:Throw()
	ENDIF

	RETURN SELF:cUser

//RvdH 2010-12-03: Some extra accesses
ACCESS ActiveStmts 

	RETURN aStmts

END CLASS

FUNCTION SQLDropMyConnection( cMySourceName, cMyUserID, cMyPassword )
	LOCAL nIndex       AS DWORD
	LOCAL hTask        AS PTR
	LOCAL lRet         AS LOGIC

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLDropMyConnection( "+AsString( cMySourceName )+","+ ;
			AsString( cMyUserID )+","+AsString( cMyPassword )+" )" )
	#ENDIF

	hTask := GetCurrentProcess()

	FOR nIndex := 1 TO ALen( aMyConn )

		IF  aMyConn[nIndex] != NIL .AND.                ;
				aMyConn[nIndex,1] == cMySourceName .AND.    ;
				aMyConn[nIndex,2] == cMyUserId .AND.        ;
				aMyConn[nIndex,3] == cMyPassword .AND.      ;
				aMyConn[nIndex,6] == hTask

			#IFDEF __DEBUG__
				__SQLOutputDebug( "** found Connection, index="+AsString( nIndex )+     ;
					", count="+ AsString( aMyConn[nIndex,5] )+    ;
					", hTask="+ AsString( hTask ) )
			#ENDIF
			IF aMyConn[nIndex,5] == 1
				aMyConn[nIndex,4]:Disconnect()
				aMyConn[nIndex,4]:__FreeConnect()
				ADel( aMyConn, nIndex )
				ASize( aMyConn, ALen( aMyConn ) - 1 )
			ELSE
				aMyConn[nIndex,5] := aMyConn[nIndex,5] - 1
			ENDIF

			lRet := TRUE
		ENDIF
	NEXT

	#IFDEF __DEBUG__
		IF !lRet
			__SQLOutputDebug( "** notfound, hTask="+ AsString( hTask ) )
		ENDIF
	#ENDIF

	RETURN lRet

FUNCTION SQLGetMyConnection( cMySourceName, cMyUserID, cMyPassword )
	LOCAL oConn         AS SQLConnection
	LOCAL nIndex        AS DWORD
	LOCAL hTask         AS PTR

	#IFDEF __DEBUG__
		__SQLOutputDebug(   "** SQLGetMyConnection( "    + ;
			AsString( cMySourceName )     + ","+   ;
			AsString( cMyUserID ) +","    + ;
			AsString( cMyPassword ) + " )" )
	#ENDIF

	hTask := GetCurrentProcess()

	FOR nIndex := 1 TO ALen( aMyConn )
		IF  aMyConn[nIndex] != NIL              .AND.   ;
				aMyConn[nIndex,1] == cMySourceName  .AND.   ;
				aMyConn[nIndex,2] == cMyUserId      .AND.   ;
				aMyConn[nIndex,3] == cMyPassword    .AND.   ;
				aMyConn[nIndex,6] == hTask
			// found it
			++aMyConn[nIndex,5]                      // bump use count
			//  more debug
			#IFDEF __DEBUG__
				__SQLOutputDebug( "** found Connection, index="+AsString( nIndex )+     ;
					", count="+ AsString( aMyConn[nIndex,5] )+    ;
					", hTask="+ AsString( hTask ) )
			#ENDIF
			RETURN aMyConn[nIndex,4]
		ENDIF
	NEXT
	//  more debug
	#IFDEF __DEBUG__
		__SQLOutputDebug( "** notfound, adding connection, index="+AsString( ALen( aMyConn )+1 )+ ;
			", hTask="+ AsString( hTask ) )
	#ENDIF
	oConn := SQLConnection{ cMySourceName, cMyUserID, cMyPassword }
	AAdd( aMyConn, { cMySourceName, cMyUserID, cMyPassword, oConn, 1, hTask } )
	RETURN oConn

FUNCTION SQLOpenConnection()                  AS SQLConnection
	//
	// Displays a dialog box that displays a list of available ODBC data sources.
	// Note: if only one ODBC driver is installed on the system, it immediately
	//  opens it without prompting the developer.
	// returns an SQLConnection object
	//
	LOCAL oConn              AS SQLConnection

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLOpenConnection()" )
	#ENDIF

	oConn := SQLConnection{}

	oConn:DriverConnect()

	RETURN oConn



FUNCTION SQLSetConnection( oSQLConnection ) AS SQLConnection
	//
	// Specify default SQL driver to be used by any function or class that
	//  requires one, if one is not provided.
	// Returns the old SQLConnection, if one was specified.
	// If called without a parameter, it only returns the default SQLConnection,
	// if one was specified;
	// if none was specified, it calls SQLOpenConnection().
	//
	LOCAL oDefConn      AS SQLConnection

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSetConnection()" )
	#ENDIF

	// get current default connection
	oDefConn := __GetMyDefConn()

	// any connection passed?
	IF oSQLConnection = NIL
		//
		// no, any default connection?
		//
		IF oDefConn = NULL_OBJECT
			//
			// no, try to open a connection...
			//
			oDefConn := SQLOpenConnection()
		ELSE
			IF !oDefConn:Connected
				//
				//  prompt the user for a source...
				//
				oDefConn:DriverConnect()
			ENDIF
		ENDIF

		__SetMyDefConn( oDefConn )
	ELSE
		oDefConn := __SetMyDefConn( oSQLConnection )
	ENDIF

	RETURN oDefConn


STATIC FUNCTION __DropMyEnv( hEnv )
	//
	// Lookup my environment in static array,
	//  if found, decrement use count, if count = 1, free it
	//
	//  Returns TRUE if successful, else FALSE
	//
	LOCAL nIndex       AS DWORD
	LOCAL hTask        AS PTR
	LOCAL nCount       AS DWORD
	LOCAL lRet         AS LOGIC

	nCount := ALen( aMyEnv )

	hTask := GetCurrentProcess()

	FOR nIndex := 1 TO nCount
		IF aMyEnv[nIndex] != NIL    .AND.    ;
				aMyEnv[nIndex,1] = hTask .AND.    ;
				aMyEnv[nIndex,3] = hEnv

			#IFDEF __DEBUG__
				__SQLOutputDebug( "** found env, hEnv="+ AsString( aMyEnv[nIndex,3] ) + ;
					", index="+AsString( nIndex ) +                              ;
					", count="+ AsString( aMyEnv[nIndex,2] )+            ;
					", hTask="+ AsString( hTask ) )
			#ENDIF

			IF aMyEnv[nIndex,2] == 1
				//RvdH 050413 This should be replaced with a Call to SQLFreeHandle()
				IF SQLFreeEnv( hEnv ) = SQL_SUCCESS
					ADel( aMyEnv, nIndex )              // delete from array
					ASize( aMyEnv, ALen( aMyEnv ) - 1 )
					lRet := TRUE
				ENDIF
			ELSE
				--aMyEnv[nIndex,2]      // decrement use count
			ENDIF
		ENDIF
	NEXT
	#IFDEF __DEBUG__
		__SQLOutputDebug( "** __DropMyEnv()" )
		IF !lRet
			__SQLOutputDebug( "** notfound, hEnv="+ AsString( hEnv )  +    ;
				", hTask="+ AsString( hTask ) )
		ENDIF
	#ENDIF
	RETURN lRet

STATIC FUNCTION __GetMyDefConn() AS SQLConnection
	//
	//  Lookup my task in static array, if found,
	//      return existing default connection
	//  Returns NULL_OBJECT if not found, else oConn
	//
	LOCAL nIndex       AS DWORD
	LOCAL hTask        AS PTR
	LOCAL nCount       AS DWORD
	LOCAL oRet         AS OBJECT

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** __GetMyDefConn()" )
	#ENDIF
	hTask := GetCurrentProcess()
	nCount := ALen( aMyDefConn )
	FOR nIndex := 1 TO nCount
		IF  ( aMyDefConn[nIndex]  != NIL ) .AND. ( aMyDefConn[nIndex,1] = hTask )
			#IFDEF __DEBUG__
				__SQLOutputDebug( "** found DefConnection, index="+AsString( nIndex )+ ;
					", hTask="+ AsString( hTask ) )
			#ENDIF

			oRet := aMyDefConn[nIndex,2]             // return it
		ENDIF
	NEXT
	RETURN oRet

STATIC FUNCTION __GetMyEnv() AS USUAL STRICT
	LOCAL nIndex       AS DWORD
	LOCAL hTask        AS PTR
	LOCAL hEnv         AS PTR
	LOCAL nCount       AS DWORD

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** __GetMyEnv()" )
	#ENDIF

	hTask := GetCurrentProcess()
	nCount := ALen( aMyEnv )
	FOR nIndex := 1 TO nCount
		IF ( aMyEnv[nIndex] != NIL ) .AND. ( aMyEnv[nIndex,1] = hTask )
			aMyEnv[nIndex,2] := aMyEnv[nIndex,2] + 1  // bump use count

			#IFDEF __DEBUG__
				__SQLOutputDebug( "** found env, hEnv="+ AsString( aMyEnv[nIndex,3] ) + ;
					", index="+AsString( nIndex ) +                              ;
					", count="+ AsString( aMyEnv[nIndex,2] )+         ;
					", hTask="+ AsString( hTask ) )
			#ENDIF
			hEnv := aMyEnv[nIndex,3]
		ENDIF
	NEXT
	IF hEnv = 0
		//RvdH 050413 This should be replaced with SqlAllocHandle
		IF SQLAllocEnv( @hEnv ) = SQL_SUCCESS
			AAdd( aMyEnv, { hTask, 1, hEnv } )
			#IFDEF __DEBUG__
				__SQLOutputDebug( "** notfound, alloc hEnv="+ AsString( hEnv )  +              ;
					", index="+ NTrim( ALen( aMyEnv ) ) + ;
					", hTask="+ AsString( hTask ) )
			#ENDIF
		ELSE
			hEnv := SQL_NULL_HENV
		ENDIF
	ENDIF
	RETURN hEnv

FUNCTION __RemoveConnection( hDbc )
	LOCAL nIndex       AS DWORD
	LOCAL lRet         AS LOGIC

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** __RemoveConnection( "+AsString( hDbc )+" )" )
	#ENDIF

	FOR nIndex := 1 TO ALen( aMyConn )
		IF  ( aMyConn[nIndex] != NIL ) .AND. ( aMyConn[nIndex,4]:ConnHandle = hDbc )
			#IFDEF __DEBUG__
				__SQLOutputDebug( "** found Connection, index="+AsString( nIndex )+     ;
					", count="+ AsString( aMyConn[nIndex,5] ) )
			#ENDIF

			ADel( aMyConn, nIndex )
			ASize( aMyConn, ALen( aMyConn ) - 1 )
			lRet := TRUE
		ENDIF
	NEXT

	#IFDEF __DEBUG__
		IF !lRet
			__SQLOutputDebug( "** notfound" )
		ENDIF
	#ENDIF

	RETURN lRet


STATIC FUNCTION __SetMyDefConn( oConn )
	//
	//  Lookup my task in static array, if found, set default connection
	//  otherwise, add it to array & set default connection
	//
	LOCAL nIndex       AS DWORD
	LOCAL nCount       AS DWORD
	LOCAL hTask        AS PTR
	LOCAL lFound       AS LOGIC

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** __SetMyDefConn()" )
	#ENDIF
	hTask := GetCurrentProcess()
	nCount := ALen( aMyDefConn )
	FOR nIndex := 1 TO nCount
		IF aMyDefConn[nIndex] != NIL .AND.                          ;
				aMyDefConn[nIndex,1] = hTask

			aMyDefConn[nIndex,2] := oConn   // set it
			lFound := TRUE
		ENDIF
	NEXT
	IF !lFound
		AAdd( aMyDefConn, { hTask, oConn } )
	ENDIF

	RETURN oConn

STATIC GLOBAL aInfoString 	AS ARRAY
STATIC GLOBAL aInfoWord 	AS ARRAY
STATIC GLOBAL aInfoDWord 	AS ARRAY
PROCEDURE InitGlobals _INIT1
	// Initialize
	aInfoString := {     ;
		SQL_ACCESSIBLE_PROCEDURES,      ;
		SQL_ACCESSIBLE_TABLES,          ;
		SQL_CATALOG_NAME,					  ;
		SQL_CATALOG_NAME_SEPARATOR,	  ;
		SQL_CATALOG_TERM,					  ;
		SQL_COLLATION_SEQ,              ;
		SQL_COLUMN_ALIAS,               ;
		SQL_DATA_SOURCE_NAME,           ;
		SQL_DATA_SOURCE_READ_ONLY,      ;
		SQL_DATABASE_NAME,              ;
		SQL_DBMS_NAME,                  ;
		SQL_DBMS_VER,                   ;
		SQL_DESCRIBE_PARAMETER,         ;
		SQL_DM_VER,                     ;
		SQL_DRIVER_NAME,                ;
		SQL_DRIVER_ODBC_VER,            ;
		SQL_DRIVER_VER,                 ;
		SQL_EXPRESSIONS_IN_ORDERBY,     ;
		SQL_IDENTIFIER_QUOTE_CHAR,      ;
		SQL_INTEGRITY,						  ;
		SQL_KEYWORDS,						  ;
		SQL_LIKE_ESCAPE_CLAUSE,         ;
		SQL_MAX_ROW_SIZE_INCLUDES_LONG, ;
		SQL_MULT_RESULT_SETS,			;
		SQL_MULTIPLE_ACTIVE_TXN,        ;
		SQL_NEED_LONG_DATA_LEN,			  ;
		SQL_ODBC_SQL_OPT_IEF,           ;
		SQL_ODBC_VER,                   ;
		SQL_ORDER_BY_COLUMNS_IN_SELECT, ;
		SQL_OUTER_JOINS,                ;
		SQL_OWNER_TERM,                 ;
		SQL_PROCEDURE_TERM,             ;
		SQL_PROCEDURES,                 ;
		SQL_QUALIFIER_TERM,             ;
		SQL_QUALIFIER_NAME_SEPARATOR,   ;
		SQL_ROW_UPDATES,                ;
		SQL_SCHEMA_TERM,					  ;
		SQL_SEARCH_PATTERN_ESCAPE,      ;
		SQL_SERVER_NAME,                ;
		SQL_SPECIAL_CHARACTERS,         ;
		SQL_TABLE_TERM,                 ;
		SQL_USER_NAME ,						;
		SQL_XOPEN_CLI_YEAR}

	aInfoWord := {							;
		SQL_AGGREGATE_FUNCTIONS,         ;
		SQL_ACTIVE_ENVIRONMENTS,        	;
		SQL_CATALOG_LOCATION,;
		SQL_CONCAT_NULL_BEHAVIOR,;
		SQL_CORRELATION_NAME,;
		SQL_CURSOR_COMMIT_BEHAVIOR,     ;
		SQL_CURSOR_ROLLBACK_BEHAVIOR,   ;
		SQL_FILE_USAGE,;
		SQL_GROUP_BY,;
		SQL_IDENTIFIER_CASE,            ;
		SQL_MAX_CATALOG_NAME_LEN,;
		SQL_MAX_COLUMN_NAME_LEN,        ;
		SQL_MAX_COLUMNS_IN_GROUP_BY,;
		SQL_MAX_COLUMNS_IN_INDEX,;
		SQL_MAX_COLUMNS_IN_SELECT,;
		SQL_MAX_COLUMNS_IN_TABLE,;
		SQL_MAX_CONCURRENT_ACTIVITIES,;
		SQL_MAX_CURSOR_NAME_LEN,        ;
		SQL_MAX_DRIVER_CONNECTIONS,;
		SQL_MAX_IDENTIFIER_LEN,;
		SQL_MAX_PROCEDURE_NAME_LEN,     ;
		SQL_MAX_SCHEMA_NAME_LEN,;
		SQL_MAX_TABLE_NAME_LEN, ;
		SQL_MAX_TABLES_IN_SELECT,;
		SQL_MAX_USER_NAME_LEN,;
		SQL_NON_NULLABLE_COLUMNS,;
		SQL_NULL_COLLATION,;
		SQL_QUOTED_IDENTIFIER_CASE,;
		SQL_TXN_CAPABLE ;
		}

	aInfoDWord  := {        ;
		SQL_ACTIVE_CONNECTIONS,         	;
		SQL_ACTIVE_STATEMENTS,          	;
		SQL_ALTER_DOMAIN,                ;
		SQL_ALTER_TABLE,                 ;
		SQL_ASYNC_MODE,						;
		SQL_BATCH_ROW_COUNT ,;
		SQL_BATCH_SUPPORT,;
		SQL_BOOKMARK_PERSISTENCE,;
		SQL_CATALOG_USAGE,;
		SQL_CONVERT_BIGINT	,;
		SQL_CONVERT_BINARY	,;
		SQL_CONVERT_BIT	   ,;
		SQL_CONVERT_CHAR	   ,;
		SQL_CONVERT_DATE	   ,;
		SQL_CONVERT_DECIMAL  ,;
		SQL_CONVERT_DOUBLE	,;
		SQL_CONVERT_FLOAT	   ,;
		SQL_CONVERT_INTEGER	,;
		SQL_CONVERT_INTERVAL_DAY_TIME	,;
		SQL_CONVERT_INTERVAL_YEAR_MONTH,;
		SQL_CONVERT_LONGVARBINARY,;
		SQL_CONVERT_LONGVARCHAR,;
		SQL_CONVERT_NUMERIC	  ,;
		SQL_CONVERT_REAL	     ,;
		SQL_CONVERT_SMALLINT	  ,;
		SQL_CONVERT_TIME	     ,;
		SQL_CONVERT_TIMESTAMP  ,;
		SQL_CONVERT_TINYINT	  ,;
		SQL_CONVERT_VARBINARY  ,;
		SQL_CONVERT_VARCHAR	  ,;
		SQL_CONVERT_INTERVAL_YEAR_MONTH,;
		SQL_CONVERT_WLONGVARCHAR	,;
		SQL_CONVERT_WVARCHAR	,;
		SQL_CONVERT_FUNCTIONS,;
		SQL_CREATE_ASSERTION,;
		SQL_CREATE_CHARACTER_SET,;
		SQL_CREATE_COLLATION,;
		SQL_CREATE_DOMAIN,;
		SQL_CREATE_SCHEMA,;
		SQL_CREATE_TABLE,;
		SQL_CREATE_TRANSLATION,;
		SQL_CREATE_VIEW,;
		SQL_DATETIME_LITERALS,;
		SQL_DDL_INDEX,;
		SQL_DEFAULT_TXN_ISOLATION,;
		SQL_DRIVER_HDBC,;
		SQL_DRIVER_HENV,;
		SQL_DRIVER_HLIB,;
		SQL_DRIVER_HSTMT,;
		SQL_DROP_ASSERTION,;
		SQL_DROP_CHARACTER_SET,;
		SQL_DROP_COLLATION,;
		SQL_DROP_DOMAIN,;
		SQL_DROP_SCHEMA,;
		SQL_DROP_TABLE,;
		SQL_DROP_TRANSLATION,;
		SQL_DROP_VIEW,;
		SQL_DYNAMIC_CURSOR_ATTRIBUTES1,;
		SQL_DYNAMIC_CURSOR_ATTRIBUTES2,;
		SQL_FORWARD_ONLY_CURSOR_ATTRIBUTES1,;
		SQL_FORWARD_ONLY_CURSOR_ATTRIBUTES2,;
		SQL_GETDATA_EXTENSIONS,;
		SQL_INDEX_KEYWORDS,;
		SQL_INFO_SCHEMA_VIEWS,;
		SQL_INSERT_STATEMENT,;
		SQL_KEYSET_CURSOR_ATTRIBUTES1,;
		SQL_KEYSET_CURSOR_ATTRIBUTES2,;
		SQL_MAX_ASYNC_CONCURRENT_STATEMENTS,;
		SQL_MAX_BINARY_LITERAL_LEN,;
		SQL_MAX_CHAR_LITERAL_LEN,;
		SQL_MAX_INDEX_SIZE,;
		SQL_MAX_OWNER_NAME_LEN,         ;
		SQL_MAX_QUALIFIER_NAME_LEN,     ;
		SQL_MAX_ROW_SIZE,;
		SQL_MAX_STATEMENT_LEN,;
		SQL_NUMERIC_FUNCTIONS,;
		SQL_ODBC_API_CONFORMANCE,       ;
		SQL_ODBC_INTERFACE_CONFORMANCE,;
		SQL_ODBC_SAG_CLI_CONFORMANCE,   ;
		SQL_ODBC_SQL_CONFORMANCE,       ;
		SQL_OJ_CAPABILITIES,;
		SQL_PARAM_ARRAY_ROW_COUNTS,;
		SQL_PARAM_ARRAY_SELECTS,;
		SQL_POS_OPERATIONS,;
		SQL_SCHEMA_USAGE,;
		SQL_SCROLL_OPTIONS,;
		SQL_SQL_CONFORMANCE,;
		SQL_SQL92_DATETIME_FUNCTIONS,;
		SQL_SQL92_FOREIGN_KEY_DELETE_RULE,;
		SQL_SQL92_FOREIGN_KEY_UPDATE_RULE,;
		SQL_SQL92_GRANT,;
		SQL_SQL92_NUMERIC_VALUE_FUNCTIONS,;
		SQL_SQL92_PREDICATES,;
		SQL_SQL92_RELATIONAL_JOIN_OPERATORS,;
		SQL_SQL92_REVOKE,;
		SQL_SQL92_ROW_VALUE_CONSTRUCTOR,;
		SQL_SQL92_STRING_FUNCTIONS,;
		SQL_SQL92_VALUE_EXPRESSIONS,;
		SQL_STANDARD_CLI_CONFORMANCE ,;
		SQL_STATIC_CURSOR_ATTRIBUTES1,;
		SQL_STATIC_CURSOR_ATTRIBUTES2,;
		SQL_STRING_FUNCTIONS,;
		SQL_SUBQUERIES,;
		SQL_SYSTEM_FUNCTIONS,;
		SQL_TIMEDATE_ADD_INTERVALS,;
		SQL_TIMEDATE_DIFF_INTERVALS,;
		SQL_TIMEDATE_FUNCTIONS,;
		SQL_TXN_ISOLATION_OPTION,;
		SQL_UNION}
	RETURN
STATIC GLOBAL aMyConn   := {} AS ARRAY  // array of connection info ( per task )

STATIC GLOBAL aMyDefConn:= {} AS ARRAY  // array of default conn. ( per task )

STATIC GLOBAL aMyEnv    := {} AS ARRAY  // array of environments ( per task )

STATIC GLOBAL lConnErrMsg := TRUE    AS LOGIC   // default is TRUE!
FUNCTION SQLConnectErrorMsg( lValue ) AS LOGIC
	//
	// Set connection error messagebox flag
	//
	DEFAULT( @lValue, FALSE )

	lConnErrMsg := lValue

	RETURN lConnErrMsg
STATIC FUNCTION __SQLSetConnectAttr( hDbc AS PTR, ;
		fOption AS WORD, vParam AS PTR, nSize AS WORD ) AS SHORTINT STRICT
	// Used For typed function pointer in __CheckQE
	RETURN 0

FUNCTION SQLGetDataSources() AS ARRAY
	//
	// Returns an ARRAY OF strings identifying the available ODBC data sources.
	// else returns NULL_ARRAY if error
	//
	LOCAL hEnv    AS PTR
	LOCAL pszSource         AS PSZ
	LOCAL pszDescription    AS PSZ
	LOCAL aSources          AS ARRAY
	LOCAL nBytes            AS SHORTINT // dcaton 070206 was DWORD
	LOCAL nDBytes           AS SHORTINT // dcaton 070206 was DWORD
	LOCAL nRetCode          AS INT
	LOCAL cNewSource        AS STRING

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLGetDataSources()" )
	#ENDIF

	aSources := {}

	// get an environment
	hEnv := __GetMyEnv()

	IF hEnv != SQL_NULL_HENV

		pszSource      := MemAlloc( SQL_MAX_DSN_LENGTH + 1 )
		pszDescription := MemAlloc( SQL_MAX_MESSAGE_LENGTH + 1 )
		IF pszSource == NULL_PSZ .OR. pszDescription == NULL_PSZ
			MemFree(pszSource)
			MemFree(pszDescription)
			SQLThrowOutOfMemoryError()
			RETURN {}
		ENDIF


		// get the first source...
		nRetCode := SQLDataSources( hEnv, SQL_FETCH_FIRST,  ;
			pszSource,              ;
			SQL_MAX_DSN_LENGTH + 1, ;
			@nBytes,                ;
			pszDescription,         ;
			SQL_MAX_MESSAGE_LENGTH, ;
			@nDBytes )

		// get the rest...
		DO WHILE nRetCode = SQL_SUCCESS
			cNewSource := Psz2String( pszSource )

			AAdd( aSources, cNewSource )

			nRetCode := SQLDataSources( hEnv,                   ;
				SQL_FETCH_NEXT,         ;
				pszSource,              ;
				SQL_MAX_DSN_LENGTH + 1, ;
				@nBytes,                ;
				pszDescription,         ;
				SQL_MAX_MESSAGE_LENGTH, ;
				@nDBytes )
		ENDDO

		MemFree( pszSource )
		MemFree( pszDescription )

		__DropMyEnv( hEnv )

	ENDIF

	RETURN aSources



