CLASS SQLListForeignKeys INHERIT SQLCatalogQuery
	EXPORT PQualifier AS STRING
	EXPORT POwner     AS STRING
	EXPORT PTableName AS STRING
	EXPORT FQualifier AS STRING
	EXPORT FOwner     AS STRING
	EXPORT FTableName AS STRING


METHOD Execute() 
	LOCAL   nRet    AS INT
    LOCAL psz1, psz2, psz3, psz4, psz5, psz6 AS PSZ

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLListForeignKeys:Execute()" )
	#ENDIF

	IF oStmt:StatementHandle = SQL_NULL_HSTMT
		SELF:__AllocStmt()
	ENDIF
    IF PQualifier != NULL_STRING
        psz1 := String2Psz(PQualifier)
    ENDIF
    IF POwner != NULL_STRING
        psz2 := String2Psz(POwner)
    ENDIF
    IF PTableName != NULL_STRING
        psz3 := String2Psz(PTableName)
    ENDIF
    IF FQualifier != NULL_STRING
        psz4 := String2Psz(FQualifier)
    ENDIF
    IF FOwner != NULL_STRING
        psz5 := String2Psz(FOwner)
    ENDIF
    IF FTableName != NULL_STRING
        psz6 := String2Psz(FTableName)
    ENDIF

    nRet := SQLForeignKeys( oStmt:StatementHandle,      ;
                               psz1,  _SLen( PQualifier ) ,;
                               psz2,  _SLen( POwner ) ,    ;
                               psz3,  _SLen( PTableName  ),;
                               psz4,  _SLen( FQualifier  ), ;
                               psz5,  _SLen( FOwner ) ,    ;
                               psz6,  _SLen( FTableName  ) )

	IF nRet != SQL_SUCCESS
		oStmt:ErrInfo := SQLErrorInfo{  SELF,                       ;
										#Excute,                    ;
										oStmt:__Connection:EnvHandle, ;
										oStmt:__Connection:ConnHandle,;
										oStmt:StatementHandle }
		RETURN FALSE
	ENDIF
	RETURN SUPER:Execute()

CONSTRUCTOR( cPQualifier, cpOwner, cPTableName, cFQualifier, cFOwner, ;
			  cFTableName, oSQLConnection ) 

	SUPER( oSQLConnection )

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLListForeignKeys:Init()" )
	#ENDIF

	IF IsString( cPQualifier )
		SELF:PQualifier := cPQualifier
	ELSE
		SELF:PQualifier := NULL_STRING
	ENDIF

	IF IsString( cPOwner )
		SELF:POwner := cPOwner
	ELSE
		SELF:POwner := NULL_STRING
	ENDIF

	IF IsString( cPTableName )
		SELF:PTableName := cPTableName
	ELSE
		SELF:PTableName := NULL_STRING
	ENDIF

	IF IsString( cFQualifier )
		SELF:FQualifier := cFQualifier
	ELSE
		SELF:FQualifier := NULL_STRING
	ENDIF

	IF IsString( cFOwner )
		SELF:FOwner := cFOwner
	ELSE
		SELF:FOwner := NULL_STRING
	ENDIF

	IF IsString( cFTableName )
		SELF:FTableName := cFTableName
	ELSE
		SELF:FTableName := NULL_STRING
	ENDIF

	SELF:Execute()

	RETURN 
END CLASS

