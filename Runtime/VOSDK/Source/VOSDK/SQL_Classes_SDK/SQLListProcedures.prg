CLASS SQLListProcedures INHERIT SQLCatalogQuery
	EXPORT Qualifier AS STRING
	EXPORT Owner     AS STRING
	EXPORT ProcName  AS STRING

METHOD Execute() 
	LOCAL   nRet    AS INT
    LOCAL psz1, psz2, psz3 AS PSZ

    #IFDEF __DEBUG__
        __SQLOutputDebug( "** SQLListProcedures:Execute()" )
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
    IF SELF:ProcName != NULL_STRING
        psz3 := String2Psz(ProcName)
    ENDIF
    
    nRet := SQLProcedures( oStmt:StatementHandle,             ;
        psz1, _SLen( Qualifier ), ;
        psz2, _SLen( Owner ),     ;
        psz3, _SLen( ProcName ) )

	IF nRet != SQL_SUCCESS
		oStmt:ErrInfo := SQLErrorInfo{  SELF,                       ;
										#Execute,                   ;
										oStmt:Connection:EnvHandle, ;
										oStmt:Connection:ConnHandle,;
										oStmt:StatementHandle }

		RETURN FALSE
	ENDIF
	RETURN SUPER:Execute()

CONSTRUCTOR( cQualifier, cOwner, cProcName, oSQLConnection ) 

	SUPER( oSQLConnection )

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLListProcedures:Init()" )
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

	IF IsString( cProcName )
		SELF:ProcName := cProcName
	ELSE
		SELF:ProcName := NULL_STRING
	ENDIF
	SELF:Execute()
	RETURN 

END CLASS

