PARTIAL CLASS SQLStatement
	
	HIDDEN  hStmt   			AS PTR
	HIDDEN  cStatement      AS STRING
	HIDDEN  oConn           AS SQLConnection
	HIDDEN  oErrInfo        AS SQLErrorInfo
	HIDDEN  lPrepFlag       AS LOGIC
	HIDDEN  aParams[0]      AS ARRAY
	PROTECT nRowSet         AS INT
	HIDDEN  nCursorType     AS INT
	HIDDEN  nSimulateCursor AS INT
	PROTECT nKeySet         AS INT
//	HIDDEN  nRecCount       AS LONGINT // Unused
	PROTECT aUserOption     AS ARRAY
	METHOD __AllocStmt() AS LOGIC STRICT 
	LOCAL hStmtNew AS PTR
	LOCAL nRetCode AS INT
	LOCAL lRet     AS LOGIC
	
	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLStatement:__AllocStmt()" )
	#ENDIF
	
	IF hStmt = SQL_NULL_HSTMT
		//RvdH 050413 This should be replaced with SqlAllocHandle
		nRetCode := SQLAllocStmt( oConn:ConnHandle, @hStmtNew )
		IF nRetCode = SQL_SUCCESS
			oErrInfo:ErrorFlag := FALSE
			SELF:hStmt := hStmtNew
			#IFDEF __DEBUG__
				__SQLOutputDebug( "** hDbc="+AsString( oConn:ConnHandle )+", hStmt="+AsString( hStmt ) )
			#ENDIF
			oConn:__RegisterStmt( SELF )
			lRet := TRUE
		ELSE
			SELF:MakeErrorInfo(SELF, #AllocStmt, nRetCode)
		ENDIF
		
	ELSE
		// Statement already allocated
		SELF:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__STMT_ALLOC ), #AllocStmt )
	ENDIF
	
	RETURN lRet

METHOD __FreeParameters() 
	LOCAL nPar AS DWORD
	LOCAL oPar AS SqlParameter
	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLStatement:Axit(), hStmt = "+AsString( hStmt ) )
	#ENDIF           
	IF ! SELF:__FreeStmt(SQL_RESET_PARAMS)
		RETURN FALSE
	ENDIF            
	// Destroy 'Our' Parameter objects	
	FOR nPar := 1 TO ALen(SELF:aParams)
		IF (IsInstanceOfUsual(aParams[nPar], #SQLParameter))
			oPar := aParams[nPar] 
			IF oPar:InternalParam                              
				oPar:Destroy()
			ENDIF
		ENDIF
	NEXT                      
	ASize(SELF:aParams,0)           
	RETURN TRUE
	

METHOD __FreeStmt( nOption AS WORD) AS LOGIC STRICT 
	LOCAL nRetCode AS INT
	LOCAL lRet     AS LOGIC
		
	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLStatement:__FreeStmt( "+AsString( nOption )+" ) hStmt="+AsString( hStmt ) )
	#ENDIF

	IF hStmt != SQL_NULL_HSTMT

		IF nOption = SQL_DROP
			SQLFreeStmt( hStmt, SQL_UNBIND )
			nRetCode := SQLFreeStmt( hStmt, SQL_CLOSE )
			//RvdH 050413 This should be replaced with a Call to SQLFreeHandle()
			nRetCode := SQLFreeStmt( hStmt, SQL_DROP )
		ELSE
			nRetCode := SQLFreeStmt( hStmt, nOption )
		ENDIF
		
		IF nRetCode = SQL_SUCCESS
			
			oErrInfo:ErrorFlag := FALSE
			IF nOption = SQL_DROP
				SELF:oConn:__UnRegisterStmt( SELF )
				SELF:hStmt := SQL_NULL_HSTMT
				lPrepFlag := FALSE
				
			ELSEIF nOption = SQL_CLOSE
				lPrepFlag := FALSE
			ENDIF
			
			lRet := TRUE
		ELSE 
			SELF:MakeErrorInfo(SELF, #FreeStmt, nRetCode)
		ENDIF
	ENDIF
	RETURN lRet
	

METHOD __GenerateSQLError( cErrorString AS STRING, symMethod AS SYMBOL) AS SQLErrorInfo STRICT 
	
	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLStatement:__GenerateSQLError( "+         ;
			AsString( cErrorString )+","+AsString( symMethod )+" )" )
	#ENDIF
	
	oErrInfo:ErrorMessage := __CavoStr( __CAVOSTR_SQLCLASS__ODBC_VO ) +      ;
		Symbol2String( ClassName( SELF ) ) +   ;
		":" + Symbol2String( symMethod ) +    ;
		" " + cErrorString
	oErrInfo:NativeError:= 0
	oErrInfo:SQLState   := __CavoStr( __CAVOSTR_SQLCLASS__GENERAL_ERR )
	oErrInfo:ErrorFlag  := TRUE
	oErrInfo:SubSystem  := __CavoStr( __CAVOSTR_SQLCLASS_SUBSYS )
	oErrInfo:MethodSelf := SELF
	oErrInfo:FuncSym    := symMethod
	oErrInfo:CallFuncSym:= symMethod
	oErrInfo:Severity   := ES_ERROR
	oErrInfo:ReturnCode := SQL_SUCCESS 
	
	RETURN oErrInfo
	

ACCESS __Params AS ARRAY STRICT 

	RETURN SELF:aParams

ASSIGN __Params( aNewParams AS ARRAY)  STRICT 
	RETURN 

METHOD __Reset( ) AS LOGIC STRICT 
	LOCAL lRet AS LOGIC
	
	IF hStmt != SQL_NULL_HSTMT
		IF SELF:__FreeStmt( SQL_CLOSE )
			lRet := SELF:Execute()
		ENDIF
	ENDIF
	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLStatement:__ResetStmt() returns " + AsString( lRet ) )
	#ENDIF
	
	RETURN lRet
	

METHOD __SetDefaultStatementOptions() AS LOGIC STRICT 
	LOCAL n     AS INT
	
	IF SELF:nCursorType > 0
      n := SELF:nCursorType 
      SELF:SetStatementOption( SQL_CURSOR_TYPE, n , TRUE)
	ENDIF
	
	// If keyset driven, set keyset size
	IF ( SELF:nCursorType == SQL_CURSOR_KEYSET_DRIVEN )
		IF SELF:nKeySet > 0
         n := SELF:nKeySet 
         SELF:SetStatementOption( SQL_KEYSET_SIZE, n , TRUE)
		ENDIF
	ENDIF
	
	IF SELF:nRowSet > 0
      n := SELF:nRowSet
      SELF:SetStatementOption( SQL_ROWSET_SIZE, n , TRUE)
	ENDIF
	
	// Set concurrency level
	n := SELF:ScrollConcurrency
	
	IF n > 0
      SELF:SetStatementOption( SQL_CONCURRENCY, n , TRUE)
	ENDIF
	
	RETURN TRUE
	

METHOD __SetParameters( aNewParams AS ARRAY) AS LOGIC STRICT 
	LOCAL nIndex        AS DWORD
	LOCAL nNumParams    AS DWORD
	LOCAL nRetCode      AS INT
	LOCAL uParam		  AS USUAL
	LOCAL oParam		  AS SqlParameter

	IF ( ! SELF:__FreeStmt( SQL_RESET_PARAMS ) )
		RETURN FALSE
	ENDIF

	nNumParams := ALen( aNewParams )
	ASize(SELF:aParams, nNumParams)
	FOR nIndex := 1 UPTO nNumParams
		uParam 	:= aNewParams[nIndex] 
		IF IsInstanceOfUsual(uParam, #SqlParameter)
			oParam := uParam
			// Do not set the 'internalparam' flag
		ELSE
			oParam := SqlParameter{uParam}
			oParam:InternalParam := TRUE
		ENDIF    
		SELF:aParams[nIndex] := oParam		
      nRetCode := oParam:bind(SELF, nIndex)

		IF ( nRetCode != SQL_SUCCESS )
			SELF:MakeErrorInfo(SELF, #SetParameters, nRetCode)
			RETURN FALSE
		ENDIF

	NEXT  
	SELF:ErrInfo:ErrorFlag := FALSE
	RETURN TRUE
	

METHOD __SetScrollOptions( nConcType AS DWORD, nKeySet AS DWORD, lAsync AS LOGIC) AS LOGIC STRICT 
	LOCAL lRet AS LOGIC
	
	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLStatement:SetScrollOptions()" )
	#ENDIF
	
	lRet := TRUE
	
	lRet := SELF:SetStatementOption( SQL_CURSOR_TYPE, nKeySet, .F. )
	IF lRet
		lRet := SELF:SetStatementOption( SQL_CONCURRENCY, nConcType, .F. )
	ENDIF
	IF lRet
		lRet := SELF:SetStatementOption( SQL_ROWSET_SIZE, SELF:nRowSet, .F. )
	ENDIF
	IF lRet
		lRet := SELF:SetStatementOption( SQL_ASYNC_ENABLE, IIF( lAsync, 1, 0 ), .F. )
	ENDIF
	
	IF lRet
		oErrInfo:ErrorFlag := FALSE
	ELSE
		SELF:MakeErrorInfo(SELF,   #__SetScrollOptions,0)
	ENDIF
	
	RETURN lRet
	

DESTRUCTOR() 
	SELF:Destroy()
	RETURN 

METHOD Commit() 
	//
	//  UH: Obsolete method, don't support after 2.0 !!!!
	//
	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLStatement:Commit()" )
	#ENDIF
	
	RETURN SELF:Connection:Commit()
	

ACCESS Connection 
	
	RETURN SELF:oConn

ACCESS CursorType 
	
	RETURN SELF:GetStatementOption( SQL_CURSOR_TYPE )
	

ASSIGN CursorType( nVal ) 
	
	IF IsNumeric( nVal ) .AND. ;
			( nVal == SQL_CURSOR_FORWARD_ONLY .OR. ;
			nVal == SQL_CURSOR_KEYSET_DRIVEN .OR. ;
			nVal == SQL_CURSOR_DYNAMIC .OR. ;
			nVal == SQL_CURSOR_STATIC )
		RETURN SELF:nCursorType := nVal
	ENDIF
	RETURN SELF:nCursorType

METHOD Destroy() 
	IF hStmt != SQL_NULL_HSTMT .AND. oConn != NULL_OBJECT 
		IF oConn:Connected
		   SELF:__FreeParameters()
			SELF:__FreeStmt( SQL_DROP )
			//IF ! InCollect()
				SELF:hStmt := SQL_NULL_HSTMT
				UnregisterAxit(SELF)
			//ENDIF
		ENDIF
	ENDIF
	RETURN NIL	
	

ACCESS ErrInfo 
	
	RETURN oErrInfo
	

ASSIGN ErrInfo( uVal ) 
	
	SELF:oErrInfo := uVal
	
	RETURN SELF:oErrInfo
	

METHOD Execute( uParm ) 
	LOCAL nCount     AS DWORD
	LOCAL aArg       AS ARRAY
	LOCAL nRetCode   AS INT
	LOCAL l          AS LONGINT
	LOCAL lRet       AS LOGIC
	LOCAL lNewParams AS LOGIC
	
	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLStatement:Execute()" )
	#ENDIF
	
	IF hStmt = SQL_NULL_HSTMT
		SELF:__AllocStmt()
	ENDIF
	
	nCount := ALen( SELF:aParams )
	
	IF PCount() != 0
		IF PCount() = 1 .AND. UsualType( uParm ) = ARRAY
			aArg   := uParm
			nCount := ALen( uParm )
		ELSE
			aArg := {}
			
			FOR nCount := 1 UPTO PCount()
				AAdd( aArg, _GETMPARAM( nCount ) )
			NEXT
		ENDIF
		lNewParams := TRUE
	ENDIF
	
	IF nCount != 0
		IF ! lNewParams
			aArg := SELF:aParams
		ENDIF
		IF !SELF:__SetParameters( aArg )
			SELF:__FreeParameters()
			RETURN FALSE
		ENDIF
	ENDIF
	IF ALen(SELF:aParams) > 0 .AND. ! lPrepFlag
		SELF:Prepare()
	ENDIF	
	IF !lPrepFlag 
		IF !SELF:__SetDefaultStatementOptions()
			IF !( oErrInfo:SQLState = __CavoStr( __CAVOSTR_SQLCLASS__FUNCSEQ_ERR ) )
				SELF:oConn:ScrollCsr := FALSE
			ENDIF
		ENDIF
		
		l := LONGINT( _CAST,SLen( cStatement ) )

		#IFDEF __DEBUG__
			IF DWORD( _CAST, l ) != PszLen( String2Psz( cStatement ) )
				__SQLOutputDebug( "**      STATEMENT CONTAINS embedded ZERORs" )
				//  "Embedded zeros found"
				MemoWrit( "c:\sqlstat.log", cStatement )
			ENDIF
			__SQLOutputDebug( "**              MemTotal() -> " + NTrim( MemTotal() ) )
		#ENDIF
		//_DebOut32(String2Psz("SQLExecDirect start "+NTrim(Seconds())))
		IF l > 0
			nRetCode := SQLExecDirect( hStmt, String2Psz( cStatement ), l )
		ENDIF
		//_DebOut32(String2Psz("SQLExecDirect ok "+NTrim(Seconds())))
		#IFDEF __DEBUG__
			__SQLOutputDebug( "**              SQLExecDirect()" )
			__SQLOutputDebug( "**              MemTotal() -> " + NTrim( MemTotal() ) )
		#ENDIF
		
	ELSE
		//_DebOut32(String2Psz("SQLExecute start "+NTrim(Seconds())))
		nRetCode := SQLExecute( hStmt )
		//_DebOut32(String2Psz("SQLExecute ok "+NTrim(Seconds())))
	ENDIF
	
	// SQL_SUCCESS_WITH_INFO is no error
	// We should still check the results here though,
	// I have refrained from doing this here to keep the
	// changes small.
	IF nRetCode = SQL_SUCCESS .OR. nRetCode = SQL_SUCCESS_WITH_INFO
		lPrepFlag := TRUE
		lRet      := TRUE
		
		// Try to determine the number of rows
		// SQLRowCount is only guaranteed to work after UPDATE,
		// INSERT or DELETE operations. Some drivers however
		// may also be able to return the number of rows
		// of a SELECT statement even before fetching any rows.
		// So this is still worth a try.
		
	ELSE
		SELF:MakeErrorInfo(SELF, #Execute, nRetCode)
	ENDIF
	
	
	#IFDEF __DEBUG__
		__SQLOutputDebug( "**** returns : " + AsString( lRet ) )
	#ENDIF
	//SELF:__FreeParameters()
	RETURN lRet
	

METHOD FreeStmt( fOption ) 
	//RvdH 070530 Added default value, as suggested by Stavros Spanos
	IF PCount() == 0
		fOption := SQL_CLOSE
	ENDIF

 	
	RETURN SELF:__FreeStmt( fOption )
	

METHOD GetStatementOption( fOption ) 
	LOCAL nValue   AS INT
	LOCAL pData    AS PTR
	LOCAL nRetCode AS INT
	LOCAL xRet     AS USUAL
	//
	//  Get current setting for a statement option; returns NIL if err
	//
	
	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLStatement:GetStatementOption()" )
	#ENDIF
	
	IF hStmt != SQL_NULL_HSTMT
		pData := @nValue
		//RvdH 050413 This should be replaced with a Call to SQLGetStmtAttr()
		
		nRetCode := SQLGetStmtOption( hStmt, fOption, pData )
		
		IF nRetCode = SQL_SUCCESS
			oErrInfo:ErrorFlag := FALSE
			xRet := nValue
		ELSE
			SELF:MakeErrorInfo(SELF,#GetStatementOption, nRetCode)
		ENDIF
	ELSE
		SELF:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__NO_STMT ), #GetStatementOption )
	ENDIF
	RETURN xRet
	

ACCESS HyperLabel 
	LOCAL oHL       AS HyperLabel
	
	oHL := HyperLabel{  #Statement,     ;
							cStatement,     ;
							Symbol2String( ClassName( SELF ) )+ ": " + cStatement,  ;
							Symbol2String( ClassName( SELF ) )+ "_" + cStatement }
	RETURN oHL
	
	
	

CONSTRUCTOR( cSQLStatement, oSQLConnection ) 
	
	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLStatement:Init( "+AsString( cSQLStatement )+" )" )
	#ENDIF
	lPrepFlag := FALSE
	// Filled in with the number of rows affected by the next SQLExecDirect call.
	oErrInfo  := SQLErrorInfo{}
	IF IsString( cSQLStatement ) .AND. SLen( cSQLStatement ) > 0
      SELF:cStatement := cSQLStatement 
   ENDIF
	IF oSQLConnection = NIL
		SELF:oConn := SQLSetConnection()
	ELSE
		SELF:oConn := oSQLConnection
	ENDIF
	
	SELF:ScrollConcurrency  := SqlSetStmtConcurrency()
	SELF:CursorType         := SqlSetStmtCursorType()
	SELF:SimulateCursor     := SqlSetStmtSimulateCursor()
	
	IF oConn:Connected
		SELF:__AllocStmt()
	ELSE
		SELF:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__NOT_CONN ), #Init )
	ENDIF
	
	nRowSet := 1
	nKeySet := 128
	SELF:aUserOption := {}
	SELF:aParams := {}	
	RETURN 
	

ACCESS KeySet 
	RETURN SELF:nKeySet
	

ASSIGN KeySet( n ) 
	IF IsNumeric( n )
		SELF:nKeySet := Integer( n )
	ENDIF
	RETURN SELF:nKeySet
	

METHOD MakeErrorInfo(oObject, symMethod, nRetCode) 
	SELF:ErrInfo := SQLErrorInfo{  oObject, symMethod,            ;
											SELF:oConn:EnvHandle,         ;
											SELF:oConn:ConnHandle,        ;
											SELF:hStmt }
	SELF:ErrInfo:ReturnCode := nRetCode
	RETURN SELF:ErrInfo
	
	

ACCESS NativeSQL 
	LOCAL nBytes        AS INT // dcaton 070206 was DWORD
	LOCAL nSize         AS DWORD
	LOCAL cValueNew     AS STRING
	LOCAL nRetCode      AS INT
	LOCAL pszStmt       AS PSZ
	
	IF cStatement == NULL_STRING .OR. hStmt = SQL_NULL_HSTMT
		SELF:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__NO_STMT ), #NativeSQL )
	ELSE
		
		nSize 	:= SLen( cStatement ) * 2 + 1
		pszStmt := MemAlloc( nSize )
		IF pszStmt == NULL_PSZ
			SQLThrowOutOfMemoryError()
			nRetCode := SQL_ERROR
		ELSE
			nRetCode := SQLNativeSql(   oConn:ConnHandle,       ;
											String2Psz( cStatement ), ;
											_SLen( cStatement ) ,       ;
											pszStmt,                ;
											SHORTINT( _CAST,nSize - 1 ),              ;
											@nBytes )
			
		ENDIF
		IF nRetCode = SQL_SUCCESS
			oErrInfo:ErrorFlag := FALSE
			cValueNew := Psz2String( pszStmt )
		ELSE
			SELF:MakeErrorInfo(SELF, #NativeSql, nRetCode)
		ENDIF
		MemFree( pszStmt )
	ENDIF
	RETURN cValueNew
	

ACCESS NumParameters 
	LOCAL nCount    AS SHORTINT // dcaton 070206 was INT
	LOCAL nRetCode  AS INT
	
	IF ( hStmt = SQL_NULL_HSTMT )
		SELF:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__NO_STMT ), #NumParameters )
		nCount := -1
	ELSE
		nRetCode := SQLNumParams( hStmt, @nCount )
		
		IF nRetCode = SQL_SUCCESS
			oErrInfo:ErrorFlag := FALSE
			
		ELSE
			SELF:MakeErrorInfo(SELF, #NumParameters, nRetCode)
			nCount := -1
		ENDIF
	ENDIF
	
	RETURN nCount
	

ACCESS NumSuccessfulRows 
	LOCAL nCount    AS INT
	LOCAL nRetCode  AS INT
	
	IF hStmt = SQL_NULL_HSTMT
		SELF:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__NO_STMT ), #NumSuccessfulRows )
		nCount := -1
	ELSE
		
		nRetCode := SQLRowCount( hStmt, @nCount )
		
		#IFDEF __DEBUG__
			__SQLOutputDebug( "** SQLRowCount() returns " + NTrim( nRetCode ) )
			__SQLOutputDebug( "** Number of rows " + NTrim( nCount ) )
		#ENDIF
		
		IF nRetCode = SQL_SUCCESS
			oErrInfo:ErrorFlag := FALSE
		ELSE
			SELF:MakeErrorInfo(SELF,#NumSuccessfulRows, nRetCode)
			nCount := -1
		ENDIF
		
	ENDIF
	
	RETURN nCount
	

METHOD Prepare() 
	
	LOCAL nRetCode AS INT
	LOCAL lRet     AS LOGIC
	
	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLStatement:Prepare()" )
	#ENDIF
	
	IF hStmt = SQL_NULL_HSTMT
		IF !SELF:__AllocStmt()
			RETURN lRet
		ENDIF
	ENDIF
	
	IF SELF:oConn:ScrollCsr
		// Set all required options
		IF !SELF:__SetDefaultStatementOptions()
			IF !( oErrInfo:SQLState = __CavoStr( __CAVOSTR_SQLCLASS__FUNCSEQ_ERR ) )
				SELF:oConn:ScrollCsr := FALSE
			ENDIF
		ENDIF
		
	ENDIF
	
	nRetCode := SQLPrepare( hStmt, String2Psz( cStatement ), LONGINT( _CAST, SLen( cStatement ) ) )
	///RvdH
	// SQL_SUCCESS_WITH_INFO is no error
	// We should still check the results here though,
	// I have refrained from doing this here to keep the
	// changes small.
	IF nRetCode = SQL_SUCCESS .OR. nRetCode = SQL_SUCCESS_WITH_INFO
		oErrInfo:ErrorFlag := FALSE
		lPrepFlag   := TRUE
		lRet        := TRUE
	ELSE
		SELF:MakeErrorInfo(SELF, #Prepare,  nRetCode)
	ENDIF
	
	RETURN lRet
	
	

ACCESS PrepFlag 
	
	RETURN SELF:lPrepFlag
	

ASSIGN PrepFlag( lNew ) 
	
	IF IsLogic( lNew )
		SELF:lPrepFlag := lNew
	ENDIF
	
	RETURN SELF:lPrepFlag
	

ACCESS RecCount 
	
	RETURN SELF:NumSuccessfulRows
	

METHOD RollBack() 
	//
	//  UH: Obsolete method, don't support after 2.0 !!!!
	//
	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLStatement:Rollback()" )
	#ENDIF
	
	RETURN SELF:Connection:RollBack()

ACCESS RowSet 
	
	RETURN SELF:nRowSet
	

ASSIGN RowSet( nVal ) 
	
	IF IsNumeric( nVal ) .AND. nVal > 0
		
		//  UH: To implement buffering for scrollable cursors
		//  SELF:nRowSet := nVal
		SELF:nRowSet := 1
		
	ENDIF
	RETURN SELF:nRowSet
	

ACCESS ScrollConcurrency 
	RETURN SELF:oConn:ScrollConcurrency
	

ASSIGN ScrollConcurrency( nVal ) 
	
	RETURN SELF:oConn:ScrollConcurrency := nVal
	

METHOD SetStatementOption( fOption, uValue, lUser ) 
	
	//
	//  Sets current setting for a statement option; returns TRUE if successful
	//
	LOCAL nRetCode AS INT
	LOCAL nExist   AS INT
	LOCAL lRet     AS LOGIC
   LOCAL nOption  AS DWORD
	
	#IFDEF __DEBUG__
		__SQLOutputDebug(   "** SQLStatement:SetStatementOption( " + ;
			AsString( fOption ) + "," + AsString( uValue ) + " )" )
	#ENDIF
	
	Default( @lUser, TRUE )
	
	nExist := AScan( SELF:aUserOption, fOption )
	
	IF lUser
		IF nExist = 0
			AAdd( aUserOption, fOption )
		ENDIF
	ELSE
		IF nExist > 0
			//
			//  Don't overwrite option set by user
			//
			RETURN FALSE
		ENDIF
	ENDIF
	
	IF hStmt = SQL_NULL_HSTMT
		SELF:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__NO_STMT ), #SetStatementOption )
	ELSE
		//RvdH 050413 This should be replaced with a Call to SQLSetStmtAttr()
      nOption  := uValue 
      nRetCode := SQLSetStmtOption( hStmt, fOption, nOption )
		
		#IFDEF __DEBUG__
			__SQLOutputDebug( "** nRetCode : " + NTrim( nRetCode ) )
		#ENDIF
		
		IF ( nRetCode = SQL_SUCCESS ) .OR. ( nRetCode = SQL_SUCCESS_WITH_INFO )
			oErrInfo:ErrorFlag := FALSE
			lRet := TRUE
		ELSE
			SELF:MakeErrorInfo(SELF, #SetStatementOption, nRetCode)
		ENDIF
	ENDIF
	RETURN lRet
	

ACCESS SimulateCursor 
	RETURN SELF:GetStatementOption( SQL_SIMULATE_CURSOR )
	

ASSIGN SimulateCursor( nVal ) 
	
	IF IsNumeric( nVal ) .AND. ;
			( nVal == SQL_SC_NON_UNIQUE .OR. ;
			nVal == SQL_SC_TRY_UNIQUE .OR. ;
			nVal == SQL_SC_UNIQUE )
		
		RETURN SELF:nSimulateCursor := nVal
	ENDIF
	RETURN SELF:nSimulateCursor
	

ACCESS SQLString 
	
	RETURN cStatement
	

ASSIGN SQLString( uVal ) 
	LOCAL cRet  AS STRING
	
	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLStatement:SQLString uVal="+AsString( uVal ) )
	#ENDIF
	EnforceType(@uVal, STRING)
	cRet := SqlDeleteWhiteSpace(uVal )
	// RvdH 061002 No need to reassign if there are no differences
	//	This prevents unneccesarry dropping of prepared statements
	// RvdH 070118 Optimization removed because of problems with parameterized queries
	//IF ! (SELF:cStatement == cRet)
	// statement NOT yet prepared?
	IF ( !lPrepFlag )
		SELF:cStatement := cRet
	ELSE
		// Attempt to drop, then re-prepare the new stmt...
		SELF:FreeStmt( SQL_DROP )
		IF SELF:__AllocStmt()
			SELF:cStatement := cRet
		ENDIF
	ENDIF
	//ENDIF
	RETURN 

ACCESS StatementHandle 
	
	RETURN hStmt

ASSIGN StatementHandle( uVal ) 
	
	IF hStmt = SQL_NULL_HSTMT
		SELF:hStmt := uVal
		RETURN SELF:hStmt
	ELSE
		SELF:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__STMT_NOT_ALLOC ), #StatementHandle )
	ENDIF
	RETURN 
	
	

ACCESS Status 
	LOCAL   oRet    AS OBJECT
	
	IF SELF:oErrInfo:ErrorFlag
		oRet := HyperLabel{ oErrInfo:FuncSym,                           ;
			oErrInfo:SQLState,                          ;
			Symbol2String( ClassName( oErrInfo:MethodSelf ) ) + ": " + ;
			oErrInfo:ErrorMessage }
	ENDIF
	
	RETURN oRet

//RvdH 2010-12-03: Some extra properties
ACCESS Params 
	RETURN SELF:aParams

	
END CLASS

