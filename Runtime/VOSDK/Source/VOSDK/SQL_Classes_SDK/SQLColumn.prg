PARTIAL CLASS SQLColumn         INHERIT SQLValue
	//  UH 08/09/2001
	//  EXPORT DisplaySize  AS INT
	EXPORT DisplaySize  AS DWORD
	EXPORT Index        AS INT
	EXPORT ColName      AS STRING
	EXPORT AliasName    AS STRING
	EXPORT Length       AS LONGINT

CONSTRUCTOR( oHyperLabel, oFieldSpec, nODBCType, nScale, lNullable, ;
	nIndex, cColName, cAlias )  

	SUPER( oHyperLabel, oFieldSpec, nODBCType, nScale, lNullable )

	IF IsNumeric( nIndex )
		SELF:Index := nIndex
	ENDIF

	IF IsString( cColName )
		SELF:ColName := cColName
	ENDIF

	IF IsString( cAlias )
		SELF:AliasName := cAlias
	ENDIF

	RETURN 
END CLASS

