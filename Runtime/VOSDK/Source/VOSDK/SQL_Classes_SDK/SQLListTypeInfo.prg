CLASS SQLListTypeInfo INHERIT SQLCatalogQuery
	EXPORT SqlType          AS SHORTINT

METHOD Execute()  
	LOCAL   nRet    AS SHORTINT

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLListTypeInfo:Execute()" )
	#ENDIF

	IF  oStmt:StatementHandle = SQL_NULL_HSTMT
		SELF:__AllocStmt()
	ENDIF

	nRet := SQLGetTypeInfo( oStmt:StatementHandle, SqlType )

	IF nRet != SQL_SUCCESS
		oStmt:ErrInfo := SQLErrorInfo{  SELF,                       ;
										#Execute,                   ;
										oStmt:__Connection:EnvHandle, ;
										oStmt:__Connection:ConnHandle,;
										oStmt:StatementHandle }
		RETURN FALSE
	ENDIF
	RETURN SUPER:Execute()

CONSTRUCTOR( nSqlType, oSQLConnection ) 

	SUPER( oSQLConnection )

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLListTypeInfo:Init()" )
	#ENDIF

	IF IsNumeric( nSqlType )
		SELF:SqlType := nSqlType
	ELSE
		SELF:SqlType := SQL_ALL_TYPES
	ENDIF

	SELF:Execute()
	RETURN 

END CLASS

