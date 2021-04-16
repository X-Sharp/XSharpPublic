/// <include file="System.xml" path="doc/DataField/*" />
CLASS DataField
	PROTECT oFieldSpec		AS FieldSpec
	PROTECT oHyperLabel	AS HyperLabel


 /// <exclude />
PROPERTY __FieldSpec as FieldSpec GET oFieldSpec
 /// <exclude />
PROPERTY __HyperLabel as HyperLabel GET oHyperLabel


/// <include file="System.xml" path="doc/DataField.AsString/*" />
METHOD AsString( ) 
	RETURN oHyperLabel:AsString( )


/// <include file="System.xml" path="doc/DataField.FieldSpec/*" />
ACCESS FieldSpec 
	RETURN oFieldSpec


/// <include file="System.xml" path="doc/DataField.HyperLabel/*" />
ACCESS HyperLabel 
	RETURN oHyperLabel


/// <include file="System.xml" path="doc/DataField.ctor/*" />
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


/// <include file="System.xml" path="doc/DataField.Name/*" />
ACCESS Name 
	RETURN oHyperLabel:Name


/// <include file="System.xml" path="doc/DataField.NameSym/*" />
ACCESS NameSym 
	RETURN oHyperLabel:NameSym




END CLASS


