//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


USING System.Data
USING System.Reflection


/// <include file="Sql.xml" path="doc/SQLValue/*" />
[XSharp.Internal.TypesChanged];
CLASS SQLValue INHERIT DataField
/// <include file="Sql.xml" path="doc/SQLValue.Type/*" />
	PROPERTY Type         AS System.Type       AUTO
/// <include file="Sql.xml" path="doc/SQLValue.Scale/*" />
	PROPERTY Scale        AS SHORT             AUTO
/// <include file="Sql.xml" path="doc/SQLValue.Nullable/*" />
	PROPERTY Nullable     AS LOGIC             AUTO
	//PROTECT nODBCType   AS SHORT             AUTO


/// <include file="Sql.xml" path="doc/SQLValue.ctor/*" />
CONSTRUCTOR( oHyperLabel as HyperLabel, oFieldSpec as FieldSpec, nType as System.Type, nScale as SHORT, lNullable  AS LOGIC) 
	oFieldSpec:Nullable := lNullable
	SUPER( oHyperLabel, oFieldSpec )
	IF !IsNil(nType)
		SELF:Type := nType
	ENDIF
	IF IsNumeric( nScale )
		SELF:Scale := nScale
	ENDIF
	IF IsLogic( lNullable )
		SELF:Nullable := lNullable
	ENDIF
	RETURN 


/// <include file="Sql.xml" path="doc/SQLValue.UsualType/*" />
ACCESS UsualType () AS DWORD
	LOCAL nRet AS DWORD
    IF SELF:oFieldSpec != NULL_OBJECT
		nRet := SELF:oFieldSpec:UsualType
	ELSE
		nRet := VOID
	ENDIF
	RETURN nRet


//[ObsoleteAttribute( "ODBCType is no longer used. Use Type in stead", FALSE )] ;
//PROPERTY ODBCType AS SHORTINT GET nODBCType SET nODBCType := value 
    
    
END CLASS


