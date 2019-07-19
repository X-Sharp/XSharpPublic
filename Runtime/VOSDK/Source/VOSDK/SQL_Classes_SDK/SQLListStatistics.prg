CLASS SQLListStatistics INHERIT SQLCatalogQuery
	EXPORT Qualifier AS STRING
	EXPORT Owner     AS STRING
	EXPORT TableName AS STRING
	EXPORT Unique    AS SHORTINT
	EXPORT Accuracy  AS SHORTINT


METHOD Execute() 
    LOCAL psz1, psz2, psz3 AS PSZ
	LOCAL   nRet    AS INT

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLListStatistics:Execute()" )
	#ENDIF

	IF  oStmt:StatementHandle = SQL_NULL_HSTMT
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
    
    nRet := SQLStatistics(  oStmt:StatementHandle,  ;
                            psz1, _SLen( Qualifier ), ;
                            psz2, _SLen( Owner ),     ;
                            psz3, _SLen( TableName ), ;
                            WORD(SELF:Unique), WORD(SELF:Accuracy ))

	IF nRet != SQL_SUCCESS
		oStmt:ErrInfo := SQLErrorInfo{  SELF,                       ;
            #Execute,                      ;
            oStmt:__Connection:EnvHandle, ;
            oStmt:__Connection:ConnHandle,;
            oStmt:StatementHandle       }
        RETURN FALSE
	ENDIF

	RETURN SUPER:Execute()

CONSTRUCTOR( cQualifier, cOwner, cTableName, nUnique, nAccuracy, oSQLConnection ) 

	SUPER( oSQLConnection )

	#IFDEF __DEBUG__
        __SQLOutputDebug( "** SQLListStatistics:Init()" )
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

	IF IsNumeric( nUnique )
		SELF:Unique  := nUnique
	ELSE
		SELF:Unique  := SQL_INDEX_ALL
	ENDIF

	IF IsNumeric( nAccuracy )
		SELF:Accuracy := nAccuracy
	ELSE
		SELF:Accuracy := SQL_QUICK
	ENDIF
	SELF:Execute()
	RETURN 
END CLASS

