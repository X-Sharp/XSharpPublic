//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


USING System.Data
USING System.Diagnostics
/// <include file="Sql.xml" path="doc/SQLColumn/*" />
[XSharp.Internal.TypesChanged];
[DebuggerDisplay( "SqlColumn {ColName,nq}" )] ;
CLASS SQLColumn    INHERIT SQLValue
/// <include file="Sql.xml" path="doc/SQLColumn.DisplaySize/*" />
	PROPERTY DisplaySize  AS DWORD       AUTO
/// <include file="Sql.xml" path="doc/SQLColumn.Index/*" />
	PROPERTY Index        AS INT         AUTO
/// <include file="Sql.xml" path="doc/SQLColumn.ColName/*" />
	PROPERTY ColName      AS STRING      AUTO
/// <include file="Sql.xml" path="doc/SQLColumn.AliasName/*" />
	PROPERTY AliasName    AS STRING      AUTO
/// <include file="Sql.xml" path="doc/SQLColumn.Length/*" />
	PROPERTY Length       AS LONGINT     AUTO


/// <include file="Sql.xml" path="doc/SQLColumn.ctor/*" />
CONSTRUCTOR( oHyperLabel AS HyperLabel, oFieldSpec as FieldSpec, oType as System.Type, nScale as SHORT, lNullable as LOGIC, nIndex as INT, cColName as STRING, cAlias  as STRING)  


	SUPER( oHyperLabel, oFieldSpec, oType, nScale, lNullable )
	SELF:Index := nIndex
	SELF:ColName := cColName
	SELF:AliasName := cAlias


	RETURN 
END CLASS


