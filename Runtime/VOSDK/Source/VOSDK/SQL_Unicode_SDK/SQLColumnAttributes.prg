//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using System.Diagnostics
[DebuggerDisplay( "SqlColumnAttribute {ColName,nq}" )] ;
PARTIAL CLASS SQLColumnAttributes INHERIT SQLColumn
	EXPORT Unsigned      AS LOGIC
	EXPORT Money         AS LOGIC
	EXPORT Updatable     AS LOGIC
	EXPORT AutoIncrement AS LOGIC
	EXPORT CaseSensitive AS LOGIC
	EXPORT Searchable    AS INT

CONSTRUCTOR ( oHyperLabel AS HyperLabel, oFieldSpec AS FieldSpec, oType AS System.Type, nScale AS SHORT, lNullable AS LOGIC, nIndex AS INT, cColName AS STRING, cAliasName AS STRING) 
	SUPER( oHyperLabel, oFieldSpec, oType, nScale, lNullable, nIndex, cColName, cAliasName )
	RETURN 

END CLASS

