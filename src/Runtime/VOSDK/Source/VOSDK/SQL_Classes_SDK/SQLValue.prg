/// <include file="SQL.xml" path="doc/SQLValue/*" />
CLASS SQLValue INHERIT DataField
	EXPORT ODBCType AS SHORTINT
	EXPORT Scale    AS INT
	EXPORT Nullable AS LOGIC


/// <include file="SQL.xml" path="doc/SQLValue.ctor/*" />
CONSTRUCTOR( oHyperLabel, oFieldSpec, nODBCType, nScale, lNullable ) 


	IF IsInstanceOfUsual(oFieldspec, #FieldSpec) 	///RvdH Added Check
		((FieldSpec)oFieldSpec):Nullable := lNullable
	ENDIF


	SUPER( oHyperLabel, oFieldSpec )


	IF IsNumeric( nODBCType )
		SELF:ODBCType := nODBCType
	ENDIF


	IF IsNumeric( nScale )
		SELF:Scale := nScale
	ENDIF


	IF IsLogic( lNullable )
		SELF:Nullable := lNullable
	ENDIF


	RETURN 




/// <include file="SQL.xml" path="doc/SQLValue.UsualType/*" />
ACCESS UsualType () 
	LOCAL nRet AS INT
   IF SELF:oFieldSpec != NULL_OBJECT
		nRet := SELF:oFieldSpec:UsualType
	ELSE
		NRet := VOID
	ENDIF
	RETURN nRet
END CLASS


