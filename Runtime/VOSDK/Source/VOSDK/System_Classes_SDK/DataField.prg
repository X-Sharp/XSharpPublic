CLASS DataField
	PROTECT oFieldSpec		AS FieldSpec
	PROTECT oHyperLabel	AS HyperLabel

METHOD AsString( ) 
	RETURN oHyperLabel:AsString( )

ACCESS FieldSpec 
	RETURN oFieldSpec

ACCESS HyperLabel 
	RETURN oHyperLabel

CONSTRUCTOR( oHLName, oFS ) 

	IF IsInstanceOfUsual( oHLName, #HyperLabel )
		oHyperLabel := oHLName
	ELSEIF IsString( oHLName ) .OR. IsSymbol( oHLName )
		oHyperLabel := HyperLabel{ oHLName }
	ELSE
		BREAK DbError{ SELF, #Init, EG_ARG, ;
			__CavoStr(__CAVOSTR_DBFCLASS_BADHLNAME), oHLName, "oHLName" }
	ENDIF
	IF IsInstanceOfUsual( oFS, #FieldSpec )
		oFieldSpec := oFS
	ELSE
		BREAK DbError{ SELF, #Init, EG_ARG, __CavoStr(__CAVOSTR_DBFCLASS_BADFS), oFS, "oFS" }
	ENDIF

	RETURN 

ACCESS Name 
	RETURN oHyperLabel:Name

ACCESS NameSym 
	RETURN oHyperLabel:NameSym


END CLASS

