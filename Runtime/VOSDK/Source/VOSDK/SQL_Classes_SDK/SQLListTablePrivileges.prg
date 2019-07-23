CLASS SQLListTablePrivileges INHERIT SQLCatalogQuery
	EXPORT Qualifier AS STRING
	EXPORT Owner     AS STRING
	EXPORT TableName AS STRING

METHOD Execute() 
	LOCAL   nRet    AS INT
    LOCAL psz1, psz2, psz3 AS PSZ

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLListTablePrivileges:Execute()" )
	#ENDIF

	IF oStmt:StatementHandle = SQL_NULL_HSTMT
		SELF:__AllocStmt()
	ENDIF
    IF SELF:Qualifier != NULL_STRING
        psz1 := String2Psz(Qualifier)
    ENDIF
    IF SELF:Owner != NULL_STRING
        psz2 := String2Psz(Owner)
    ENDIF
    IF SELF:TableName != NULL_STRING
        psz3 := String2Psz(TableName)
    ENDIF

    nRet := SQLTablePrivileges( oStmt:StatementHandle,  ;
                    psz1,  _SLen( Qualifier ) ,;
                    psz2,  _SLen( Owner ),     ;
                    psz3,  _SLen( TableName ) )

	IF nRet != SQL_SUCCESS
		oStmt:ErrInfo := SQLErrorInfo{  SELF,       ;
										#Execute,      ;
										oStmt:__Connection:EnvHandle,  ;
										oStmt:__Connection:ConnHandle, ;
										oStmt:StatementHandle }
		RETURN FALSE
	ENDIF
	RETURN SUPER:Execute()

CONSTRUCTOR( cQualifier, cOwner, cTableName, oSQLConnection ) 

	SUPER( oSQLConnection )

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLListTablePrivileges:Init()" )
	#ENDIF

	IF IsString( cQualifier )
		SELF:Qualifier := cQualifier
	ELSE
		SELF:Qualifier := NULL_STRING
	ENDIF

	IF IsString( cOwner )
		SELF:Owner := cOwner
	ELSE
		SELF:Owner := NULL_STRING
	ENDIF

	IF IsString( cTableName )
		SELF:TableName := cTableName
	ELSE
		SELF:TableName := NULL_STRING
	ENDIF

	SELF:Execute()

	RETURN 

END CLASS

