//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


using System.Diagnostics
/// <include file="Sql.xml" path="doc/SQLColumnAttributes/*" />
[XSharp.Internal.TypesChanged];
[DebuggerDisplay( "SqlColumnAttribute {ColName,nq}" )] ;
CLASS SQLColumnAttributes INHERIT SQLColumn
/// <include file="Sql.xml" path="doc/SQLColumnAttributes.Unsigned/*" />
	PROPERTY Unsigned      AS LOGIC   AUTO
/// <include file="Sql.xml" path="doc/SQLColumnAttributes.Money/*" />
	PROPERTY Money         AS LOGIC   AUTO
/// <include file="Sql.xml" path="doc/SQLColumnAttributes.Updatable/*" />
	PROPERTY Updatable     AS LOGIC   AUTO
/// <include file="Sql.xml" path="doc/SQLColumnAttributes.AutoIncrement/*" />
	PROPERTY AutoIncrement AS LOGIC   AUTO
/// <include file="Sql.xml" path="doc/SQLColumnAttributes.CaseSensitive/*" />
	PROPERTY CaseSensitive AS LOGIC   AUTO
/// <include file="Sql.xml" path="doc/SQLColumnAttributes.Searchable/*" />
	PROPERTY Searchable    AS INT     AUTO


/// <include file="Sql.xml" path="doc/SQLColumnAttributes.ctor/*" />
CONSTRUCTOR ( oHyperLabel AS HyperLabel, oFieldSpec AS FieldSpec, oType AS System.Type, nScale AS SHORT, lNullable AS LOGIC, nIndex AS INT, cColName AS STRING, cAliasName AS STRING) 
	SUPER( oHyperLabel, oFieldSpec, oType, nScale, lNullable, nIndex, cColName, cAliasName )
	RETURN 


END CLASS


