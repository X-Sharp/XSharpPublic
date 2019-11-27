//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Data
using System.Reflection

PARTIAL CLASS SQLValue INHERIT DataField
	EXPORT Type         AS System.Type
	EXPORT Scale        AS SHORT
	EXPORT Nullable     AS LOGIC
    PROTECT nODBCType   AS SHORT

CONSTRUCTOR( oHyperLabel as HyperLabel, oFieldSpec as FieldSpec, nType as System.Type, nScale as SHORT, lNullable  AS LOGIC) 
	IF IsInstanceOfUsual(oFieldSpec, #FieldSpec) 	
		((FieldSpec)oFieldSpec):Nullable := lNullable
	ENDIF
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

ACCESS UsualType () AS DWORD
	LOCAL nRet AS DWORD
    IF SELF:oFieldSpec != NULL_OBJECT
		nRet := SELF:oFieldSpec:UsualType
	ELSE
		nRet := VOID
	ENDIF
	RETURN nRet

[ObsoleteAttribute( "ODBCType is no longer used. Use Type in stead", FALSE )] ;
PROPERTY ODBCType AS SHORTINT GET nODBCType SET nODBCType := value 
    
END CLASS

