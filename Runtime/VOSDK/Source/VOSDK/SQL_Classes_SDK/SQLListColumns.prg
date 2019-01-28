CLASS SQLListColumns INHERIT SQLCatalogQuery
    EXPORT Qualifier    AS STRING
    EXPORT Owner        AS STRING
    EXPORT TableName    AS STRING
    EXPORT ColName      AS STRING



METHOD Execute  () 
    LOCAL psz1, psz2, psz3, psz4 AS PSZ
    LOCAL   nRet    AS INT

    #IFDEF __DEBUG__
        __SQLOutputDebug("** SQLListColumns:Execute()")
    #ENDIF

    IF oStmt:StatementHandle = SQL_NULL_HSTMT
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
    IF SELF:ColName != NULL_STRING
        psz4 := String2Psz(SELF:ColName)
    ENDIF
    // get the list of columns...
    nRet := SQLColumns( oStmt:StatementHandle,   ;
                        psz1, _SLen(SELF:Qualifier),    ;
                        psz2, _SLen(SELF:Owner),        ;
                        psz3, _SLen(SELF:TableName),    ;
                        psz4, _SLen(SELF:ColName)               )

    IF nRet != SQL_SUCCESS
        oStmt:ErrInfo := SQLErrorInfo{  SELF,           ;
                                        #Init,          ;
                                        oStmt:Connection:EnvHandle,   ;
                                        oStmt:Connection:ConnHandle,      ;
                                        oStmt:StatementHandle }

        RETURN FALSE
    ENDIF

    RETURN SUPER:Execute()

CONSTRUCTOR     ( cQualifier, cOwner, cTableName, cColName, oSQLConnection) 

    SUPER(oSQLConnection)

    #IFDEF __DEBUG__
        __SQLOutputDebug("** SQLListColumns:Init("+AsString(cQualifier)+","+   ;
                            AsString(cOwner) + "," +          ;
                            AsString(cTableName) + "," +      ;
                            AsString(cColName)+")" )
    #ENDIF

    IF IsString(cQualifier)
        SELF:Qualifier := cQualifier
    ELSE
        SELF:Qualifier := NULL_STRING
    ENDIF

    IF IsString(cOwner)
        SELF:Owner := cOwner
    ELSE
        SELF:Owner := NULL_STRING
    ENDIF

    IF IsString(cTableName)
        SELF:TableName := cTableName
    ELSE
        SELF:TableName := NULL_STRING
    ENDIF

    IF IsString(cColName)
        SELF:ColName := cColName
    ELSE
        SELF:ColName := NULL_STRING
    ENDIF

    SELF:Execute()

    RETURN 


END CLASS

