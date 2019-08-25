CLASS SQLListTables INHERIT SQLCatalogQuery
	EXPORT Qualifier AS STRING
	EXPORT Owner     AS STRING
	EXPORT TableName AS STRING
	EXPORT TableType AS STRING


METHOD Execute() 
	LOCAL   nRet    AS INT
    LOCAL psz1, psz2, psz3, psz4 AS PSZ
    #IFDEF __DEBUG__
        __SQLOutputDebug( "** SQLListTables:Execute()" )
    #ENDIF

    IF  oStmt:StatementHandle = SQL_NULL_HSTMT
        SELF:__AllocStmt()
    ENDIF       
    
    IF SELF:Qualifier != NULL_STRING
        psz1 := String2Psz(SELF:Qualifier)
    ENDIF
    IF SELF:Owner != NULL_STRING
        psz2 := String2Psz(SELF:Owner)
    ENDIF
    IF SELF:TableName != NULL_STRING
        psz3 := String2Psz(SELF:TableName)
    ENDIF
    IF SELF:TableType != NULL_STRING
        psz4 := String2Psz(SELF:TableType)
    ENDIF
    
    nRet := SQLTables(  oStmt:StatementHandle,                       ;
                    psz1, _SLen( SELF:Qualifier ), ;
                    psz2, _SLen( SELF:Owner ),         ;
                    psz3, _SLen( SELF:TableName ), ;
                    psz4, _SLen( SELF:TableType ) )

	IF nRet != SQL_SUCCESS
		oStmt:ErrInfo := SQLErrorInfo{  SELF,                       ;
										#Execute,                   ;
										oStmt:__Connection:EnvHandle, ;
										oStmt:__Connection:ConnHandle,;
										oStmt:StatementHandle       }
		RETURN FALSE
	ENDIF

	RETURN SUPER:Execute()

CONSTRUCTOR( cQualifier, cOwner, cTableName, cTableType, oSQLConnection ) 

	SUPER( oSQLConnection )

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLListTables:Init( "+AsString( cQualifier )+","+   ;
							AsString( cOwner )+","+        ;
							AsString( cTableName )+","+    ;
							AsString( cTableType )+" )" )
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

	IF  IsString( cTableType )
		SELF:TableType := cTableType
	ELSE
		SELF:TableType := NULL_STRING
	ENDIF

	SELF:Execute()
	RETURN 


END CLASS

