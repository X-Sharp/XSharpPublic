PARTIAL CLASS SQLListSpecialColumns INHERIT SQLCatalogQuery
	EXPORT Qualifier AS STRING
	EXPORT Owner     AS STRING
	EXPORT TableName AS STRING
	EXPORT ColType   AS WORD
	EXPORT Nullable  AS WORD
	EXPORT Scope     AS WORD


METHOD Execute() 
	LOCAL   nRet    AS SHORTINT
    LOCAL psz1, psz2, psz3 AS PSZ
    #IFDEF __DEBUG__
        __SQLOutputDebug( "** SQLListSpecialColumns:Execute()" )
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

    nRet := SQLSpecialColumns(  oStmt:StatementHandle,    ColType , ;
        psz1,  _SLen( Qualifier ),    ;
        psz2,  _SLen( Owner ),        ;
        psz3,  _SLen( TableName ),    ;
        SELF:Scope , SELF:Nullable  )
	IF nRet != SQL_SUCCESS
		oStmt:ErrInfo := SQLErrorInfo{  SELF,                       ;
										#Execute,                   ;
										oStmt:Connection:EnvHandle, ;
										oStmt:Connection:ConnHandle,;
										oStmt:StatementHandle   }
		RETURN FALSE
	ENDIF
	RETURN SUPER:Execute()

CONSTRUCTOR( nColType, cQualifier, cOwner, cTableName, nScope, ;
			 nNullable, oSQLConnection ) 

	SUPER( oSQLConnection )

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLListSpecialColumns:Init()" )
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

	IF IsNumeric( nColType )
		SELF:ColType := nColType
	ELSE
		SELF:ColType := SQL_BEST_ROWID
	ENDIF

	IF IsNumeric( nScope )
		SELF:Scope := nScope
	ELSE
		SELF:Scope :=  SQL_SCOPE_CURROW
	ENDIF

	IF IsNumeric( nNullable )
		SELF:Nullable := nNullable
	ELSE
		SELF:Nullable := SQL_NULLABLE
	ENDIF

	SELF:Execute()

	RETURN 

END CLASS

