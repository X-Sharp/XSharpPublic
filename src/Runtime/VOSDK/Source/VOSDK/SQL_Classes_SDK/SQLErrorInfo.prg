/// <include file="SQL.xml" path="doc/SQLErrorInfo/*" />
CLASS SQLErrorInfo  INHERIT Error
	HIDDEN cSQLState     AS STRING
	HIDDEN nNativeError  AS INT
	HIDDEN cErrorMessage AS STRING
	HIDDEN nErrorMsgLen  AS INT
	HIDDEN lErrorFlag    AS LOGIC
	HIDDEN nRetCode  	 AS INT


/// <include file="SQL.xml" path="doc/SQLErrorInfo.ErrorFlag/*" />
ACCESS ErrorFlag 


	RETURN SELF:lErrorFlag


/// <include file="SQL.xml" path="doc/SQLErrorInfo.ErrorFlag/*" />
ASSIGN ErrorFlag( uVal ) 


	IF IsLogic( lErrorFlag )
		SELF:lErrorFlag := uVal
	ENDIF


	RETURN 




/// <include file="SQL.xml" path="doc/SQLErrorInfo.ErrorMessage/*" />
ACCESS ErrorMessage 


	RETURN SELF:cErrorMessage


/// <include file="SQL.xml" path="doc/SQLErrorInfo.ErrorMessage/*" />
ASSIGN ErrorMessage( uVal ) 


	IF IsString( uVal )
		SELF:cErrorMessage := uVal
	ENDIF
	RETURN 


/// <include file="SQL.xml" path="doc/SQLErrorInfo.ErrorMessageLen/*" />
ACCESS ErrorMessageLen 


	RETURN SELF:nErrorMsgLen


/// <include file="SQL.xml" path="doc/SQLErrorInfo.ctor/*" />
CONSTRUCTOR( oOriginator, symMethod, hEnv, hDbc, hStmt ) 


	LOCAL nNativeErrorNew     AS SHORTINT // dcaton 070206 was INT
	LOCAL nErrorMsgLenNew     AS SHORTINT // dcaton 070206 was INT
	LOCAL DIM bErrState[ SQL_SQLSTATE_SIZE + 1] AS BYTE
	LOCAL DIM bErrMsg [SQL_MAX_MESSAGE_LENGTH + 1] AS BYTE
   SUPER()
	SELF:SubSystem := __CavoStr( __CAVOSTR_SQLCLASS_SUBSYS )


	IF IsObject( oOriginator )
		SELF:MethodSelf := oOriginator
	ENDIF


	IF IsSymbol( symMethod )
		SELF:FuncSym     := symMethod
		SELF:CallFuncSym := symMethod
	ENDIF


	// GenCode := EG_ ??
	SELF:Severity := ES_ERROR
	SELF:cErrorMessage := NULL_STRING
	SELF:lErrorFlag := FALSE
	SELF:nNativeError := 0


	IF __CheckHandles( hEnv, hDbc, hStmt )


			//RvdH 050413 This should be replaced with a Call to SQLGetDiagRec()
			nRetCode := SQLError( hEnv,               ;
									hDbc,               ;
									hStmt,              ;
									@bErrState,        ;
									@nNativeErrorNew,   ;
									@bErrMsg,          ;
									SQL_MAX_MESSAGE_LENGTH - 1, ;
									@nErrorMsgLenNew )
		IF nRetCode != SQL_ERROR


			lErrorFlag := TRUE
			SELF:nNativeError := nNativeErrorNew
			SELF:nErrorMsgLen := nErrorMsgLenNew
			SELF:cErrorMessage:= Mem2String( @bErrMsg[1] , nErrorMsgLenNew)
			SELF:cSQLState    := Mem2String( @bErrState[1], SQL_SQLSTATE_SIZE )


			#IFDEF __DEBUG__
				__SQLOutputDebug( "** SQLErrorInfo:Init @" +          ;
								Symbol2String( ClassName( oOriginator ) ) +        ;
								":" + Symbol2String( symMethod ) + " " +           ;
								AsString( cSQLState ) )
				__SQLOutputDebug( " >>>>Msg=" + cErrorMessage )
			#ENDIF
		ENDIF




	ENDIF
	RETURN 


/// <include file="SQL.xml" path="doc/SQLErrorInfo.NativeError/*" />
ACCESS NativeError 


	RETURN nNativeError


/// <include file="SQL.xml" path="doc/SQLErrorInfo.NativeError/*" />
ASSIGN NativeError( uVal ) 


	IF IsNumeric( nNativeError )
		SELF:nNativeError := uVal
	ENDIF
	RETURN 




/// <include file="SQL.xml" path="doc/SQLErrorInfo.ReturnCode/*" />
ACCESS ReturnCode 


	//  SQL return code ( ie: SQL_SUCCESS_WITH_INFO, SQL_ERROR, etc. )
	RETURN SELF:nRetCode


/// <include file="SQL.xml" path="doc/SQLErrorInfo.ReturnCode/*" />
ASSIGN ReturnCode( uVal ) 


	IF IsNumeric( uVal )
		SELF:nRetCode := uVal
	ENDIF
	RETURN SELF:nRetCode


/// <include file="SQL.xml" path="doc/SQLErrorInfo.ShowErrorMsg/*" />
METHOD ShowErrorMsg() 
	LOCAL cTitle AS STRING


	IF lErrorFlag
		cTitle :=  AsString( FuncSym )
		cTitle +=  "  NativeError: "
		cTitle +=  AsString( nNativeError )
		cTitle +=  "  SQLState: " + cSQLState
		MessageBox( 0, String2Psz(cErrorMessage ), String2Psz(cTitle ), MB_ICONSTOP )
	ENDIF


	RETURN NIL


/// <include file="SQL.xml" path="doc/SQLErrorInfo.SQLState/*" />
ACCESS SQLState 


	RETURN cSQLState


/// <include file="SQL.xml" path="doc/SQLErrorInfo.SQLState/*" />
ASSIGN SQLState( uVal ) 


	IF IsString( cSQLState )
		SELF:cSQLState := uVal
	ENDIF
	RETURN 
#ifdef __XSHARP_RT__
/// <include file="SQL.xml" path="doc/SQLErrorInfo.Throw/*" />
METHOD Throw() AS VOID  STRICT
#else
/// <include file="SQL.xml" path="doc/SQLErrorInfo.Throw/*" />
METHOD Throw() 
#endif


	RETURN Eval( ErrorBlock(), SELF )




//RvdH 2010-12-03: Some extra properties
/// <include file="SQL.xml" path="doc/SQLErrorInfo.ErrorList/*" />
ACCESS ErrorList


	LOCAL oErr 		AS SQLErrorInfo
	LOCAL oStmt		AS SQLStatement
	LOCAL aRet		AS ARRAY


   aRet		:= {} 


	IF IsInstanceOf( SELF:MethodSelf , #SqlStatement)


      oStmt		:= SELF:MethodSelf 		
	   oErr 		:= SELF


		WHILE oErr != NULL_OBJECT .AND. (Val(oErr:SQLState) <> 0 .OR. oErr:NativeError <> 0)
	
	
			AAdd( aRet, { oErr:NativeError, oErr:SQLState, oErr:ErrorMessage } )
		   oErr		:= SQLErrorInfo{ oStmt, SELF:FuncSym,            ;
	                                 oStmt:__Connection:EnvHandle,         ;
	                                 oStmt:__Connection:ConnHandle,        ;
	                                 oStmt:StatementHandle }
		ENDDO		
	
	
	ENDIF


	RETURN aRet




END CLASS


