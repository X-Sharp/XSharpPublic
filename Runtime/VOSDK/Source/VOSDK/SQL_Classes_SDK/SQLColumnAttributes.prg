CLASS SQLColumnAttributes INHERIT SQLColumn
	EXPORT Unsigned      AS LOGIC
	EXPORT Money         AS LOGIC
	EXPORT Updatable     AS LOGIC
	EXPORT AutoIncrement AS LOGIC
	EXPORT CaseSensitive AS LOGIC
	EXPORT Searchable    AS INT


CONSTRUCTOR ( oHyperLabel, oFieldSpec, nODBCType, nScale, ;
	lNullable, nIndex, cColName, cAliasName ) 

	SUPER( oHyperLabel, oFieldSpec, nODBCType, nScale, ;
				lNullable, nIndex, cColName, cAliasName )

	RETURN 
END CLASS

