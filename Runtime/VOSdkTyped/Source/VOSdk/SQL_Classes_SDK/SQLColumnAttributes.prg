//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using System.Diagnostics
[DebuggerDisplay( "SqlColumnAttribute {ColName,nq}" )] ;
CLASS SQLColumnAttributes INHERIT SQLColumn
	PROPERTY Unsigned      AS LOGIC   AUTO
	PROPERTY Money         AS LOGIC   AUTO
	PROPERTY Updatable     AS LOGIC   AUTO
	PROPERTY AutoIncrement AS LOGIC   AUTO
	PROPERTY CaseSensitive AS LOGIC   AUTO
	PROPERTY Searchable    AS INT     AUTO

CONSTRUCTOR ( oHyperLabel AS HyperLabel, oFieldSpec AS FieldSpec, oType AS System.Type, nScale AS SHORT, lNullable AS LOGIC, nIndex AS INT, cColName AS STRING, cAliasName AS STRING) 
	SUPER( oHyperLabel, oFieldSpec, oType, nScale, lNullable, nIndex, cColName, cAliasName )
	RETURN 

END CLASS

