//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


USING System.Data
USING System.Data.Common
USING System.Diagnostics

/// <include file="Sql.xml" path="doc/SqlParameter/*" />

[XSharp.Internal.TypesChanged];
[DebuggerDisplay( "SqlParameter({Value})" )] ;
CLASS SqlParameter
	EXPORT IO				AS SHORTINT
	EXPORT UsualType		AS DWORD
	EXPORT CType			AS SHORTINT
	EXPORT ODBCType		    AS SHORTINT
	EXPORT Precision		AS DWORD
	EXPORT Scale			AS SHORTINT
	EXPORT InternalParam	AS LOGIC			
    PROTECT uValue           AS USUAL


/// <include file="Sql.xml" path="doc/SqlParameter.ctor/*" />
CONSTRUCTOR(xValue,nIO,nODBCType,nSize)  CLIPPER
	@@Default(REF nIO, SQL_PARAM_INPUT)
	@@Default(REF nODBCType, SQL_TYPE_UNKNOWN)
	@@Default(REF nSize, 0)
	SELF:IO			:= nIO
	SELF:ODBCType	:= nODBCType
	SELF:SetValue(xValue,nSize)
	RETURN 
	
	
/// <include file="Sql.xml" path="doc/SqlParameter.Bind/*" />
METHOD Bind(oStatement AS SQLStatement,nPos AS DWORD) AS LONGINT STRICT 
    RETURN 0	


/// <include file="Sql.xml" path="doc/SqlParameter.Destroy/*" />
METHOD Destroy()  AS USUAL CLIPPER
	RETURN TRUE
	
	
/// <include file="Sql.xml" path="doc/SqlParameter.SetValue/*" />
METHOD SetValue(xValue AS USUAL,nSize AS LONGINT) AS USUAL STRICT 
    SELF:uValue := xValue
	RETURN NIL


/// <include file="Sql.xml" path="doc/SqlParameter.Value/*" />
ACCESS Value AS USUAL STRICT 
    RETURN uValue




END CLASS


DEFINE SQL_TYPE_UNKNOWN:=-9999


