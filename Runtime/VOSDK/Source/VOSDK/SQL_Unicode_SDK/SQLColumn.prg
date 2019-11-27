//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Data
USING System.Diagnostics
[DebuggerDisplay( "SqlColumn {ColName,nq}" )] ;
PARTIAL CLASS SQLColumn    INHERIT SQLValue
	EXPORT DisplaySize  AS DWORD
	EXPORT Index        AS INT
	EXPORT ColName      AS STRING
	EXPORT AliasName    AS STRING
	EXPORT Length       AS LONGINT

CONSTRUCTOR( oHyperLabel AS HyperLabel, oFieldSpec as FieldSpec, oType as System.Type, nScale as SHORT, lNullable as LOGIC, nIndex as INT, cColName as STRING, cAlias  as STRING)  

	SUPER( oHyperLabel, oFieldSpec, oType, nScale, lNullable )
	SELF:Index := nIndex
	SELF:ColName := cColName
	SELF:AliasName := cAlias

	RETURN 
END CLASS

