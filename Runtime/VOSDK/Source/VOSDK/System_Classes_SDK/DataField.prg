CLASS DataField
	PROTECT oFieldSpec		AS FieldSpec
	PROTECT oHyperLabel	AS HyperLabel

PROPERTY __FieldSpec as FieldSpec GET oFieldSpec
PROPERTY __HyperLabel as HyperLabel GET oHyperLabel

METHOD AsString( ) 
	RETURN oHyperLabel:AsString( )

ACCESS FieldSpec 
	RETURN oFieldSpec

ACCESS HyperLabel 
	RETURN oHyperLabel

CONSTRUCTOR( oHLName, oFS ) 

	IF IsObject( oHLName) .and. __USual.ToObject(oHLName) IS HyperLabel 
		oHyperLabel := oHLName
	ELSEIF IsString( oHLName ) .OR. IsSymbol( oHLName )
		oHyperLabel := HyperLabel{ oHLName }
	ELSE
		BREAK DbError{ SELF, #Init, EG_ARG, ;
			__CavoStr(__CAVOSTR_DBFCLASS_BADHLNAME), oHLName, "oHLName" }
	ENDIF
	IF IsObject(oFS) .and. __Usual.ToObject(oFS) IS FieldSpec 
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

