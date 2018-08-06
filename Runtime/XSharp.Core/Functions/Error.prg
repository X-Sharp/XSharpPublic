//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


/// <summary>
/// Return an error message associated with a system-generated error code.
/// </summary>
/// <param name="nGenCode">The error code exported by an error object. One of the GenCodes enum values</param>
/// <returns>The message string associated with the error code.  Error messages are nation-dependent.</returns>
FUNCTION ErrString(nGenCode AS DWORD) AS STRING
	LOCAL cResource AS STRING
	IF nGenCode > XSharp.GenCode.EG_MAX
		cResource := "RT_MSG_ERR_UNKNOWN"
	ELSE
		cResource := "RT_MSG_ERR_" + nGenCode:ToString()
	ENDIF
	RETURN Messages.GetString(cResource)
	
	
	
	/// <summary>
	/// Return a description string for a DOS error number.
	/// </summary>
	/// <param name="nDosErr">The DOS error number that you want a description for.</param>
	/// <returns>The message string associated with the error number.</returns>
FUNCTION DosErrString(nDosErr AS DWORD) AS STRING
	LOCAL cResource AS STRING
	LOCAL cResult AS STRING
	cResource := "RT_MSG_DOSERR_" + nDosErr:ToString() 
	// when not found return string from RT_MSG_DOSERR_UNKNOWN
	cResult := Messages.GetString(cResource)
	IF String.IsNullOrEmpty(cResult)
		cResult := Messages.GetString("RT_MSG_DOSERR_UNKNOWN")
	ENDIF
	RETURN cResult




/// <summary>
/// </summary>
/// <param name="dwType"></param>
/// <returns>
/// </returns>
FUNCTION TypeString( nType AS DWORD ) AS STRING
   LOCAL ret AS STRING
   SWITCH nType
   CASE 0
      ret := "NIL"
   CASE  LONG
      ret := "LONGINT"
   CASE  DATE
      ret := "DATE"
   CASE  FLOAT
      ret := "FLOAT"
   CASE  4
      ret := "FIXED"
   CASE  ARRAY
      ret := "ARRAY"
   CASE  OBJECT
      ret := "OBJECT"
   CASE  STRING
      ret := "STRING"
   CASE  LOGIC
      ret := "LOGIC"
   CASE  CODEBLOCK
      ret := "CODEBLOCK"
   CASE  SYMBOL
      ret := "SYMBOL"
   CASE  BYTE
      ret := "BYTE"
   CASE  SHORT
      ret := "SHORTINT"
   CASE  WORD
      ret := "WORD"
   CASE  DWORD
      ret := "DWORD"
   CASE  REAL4
      ret := "REAL4"
   CASE  REAL8
      ret := "REAL8"
   CASE  PSZ
      ret := "PSZ"
   CASE  PTR
      ret := "PTR"
   CASE  USUAL
      ret := "USUAL"
   CASE  20
      ret := "UNKNOWN"
   CASE  21
      ret := "@"
   CASE  INT64
      ret := "INT64"
   CASE  UINT64
      ret := "UINT64"
   CASE  24
      ret := "CHAR"
   CASE  25
      ret := "DYNAMIC"
   CASE  26
      ret := "DATETIME"
   CASE  27
      ret := "DECIMAL"
   CASE  32
      ret := "MEMO"
   OTHERWISE
      ret := "UNKNOWN"
   END SWITCH 
   RETURN ret   