//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Data
USING System.Data.Common
USING System.Diagnostics

[DebuggerDisplay( "SqlParameter({Value})" )] ;
PARTIAL CLASS SqlParameter
	EXPORT IO				AS SHORTINT
	EXPORT UsualType		AS DWORD
	EXPORT CType			AS SHORTINT
	EXPORT ODBCType		    AS SHORTINT
	EXPORT Precision		AS DWORD
	EXPORT Scale			AS SHORTINT
	EXPORT InternalParam	AS LOGIC			
    PROTECT uValue           AS USUAL

CONSTRUCTOR(xValue,nIO,nODBCType,nSize) 
	DEFAULT nIO 		TO SQL_PARAM_INPUT
	DEFAULT nODBCType	TO SQL_TYPE_UNKNOWN
	DEFAULT nSize		TO 0
	SELF:IO			:= nIO
	SELF:ODBCType	:= nODBCType
	SELF:SetValue(xValue,nSize)
	RETURN 
	
METHOD Bind(oStatement AS SQLStatement,nPos AS DWORD) AS LONGINT STRICT 
    RETURN 0	

METHOD Destroy() 
	RETURN TRUE
	
METHOD SetValue(xValue AS USUAL,nSize AS LONGINT) AS USUAL STRICT 
    SELF:uValue := xValue
	RETURN NIL

ACCESS Value AS USUAL STRICT 
    RETURN uValue


END CLASS

DEFINE SQL_TYPE_UNKNOWN:=-9999

