//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Data
USING System.Diagnostics

[DebuggerDisplay( "SqlColumn {ColName,nq}" )] ;
CLASS SQLColumn    INHERIT SQLValue
	PROPERTY DisplaySize  AS DWORD       AUTO
	PROPERTY Index        AS INT         AUTO
	PROPERTY ColName      AS STRING      AUTO
	PROPERTY AliasName    AS STRING      AUTO
	PROPERTY Length       AS LONGINT     AUTO

CONSTRUCTOR( oHyperLabel AS HyperLabel, oFieldSpec as FieldSpec, oType as System.Type, nScale as SHORT, lNullable as LOGIC, nIndex as INT, cColName as STRING, cAlias  as STRING)  

	SUPER( oHyperLabel, oFieldSpec, oType, nScale, lNullable )
	SELF:Index := nIndex
	SELF:ColName := cColName
	SELF:AliasName := cAlias

	RETURN 
END CLASS

