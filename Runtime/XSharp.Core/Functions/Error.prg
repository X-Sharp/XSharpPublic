//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/errstring/*" />
FUNCTION ErrString(dwNewReturnCode AS DWORD) AS STRING
	LOCAL cResource AS STRING
	IF dwNewReturnCode > XSharp.GenCode.EG_MAX
		cResource := "RT_MSG_ERR_UNKNOWN"
	ELSE
		cResource := "RT_MSG_ERR_" + dwNewReturnCode:ToString()
	ENDIF
	RETURN Messages.GetString(cResource)
	
	
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/doserrstring/*" />
FUNCTION DosErrString(dwError AS DWORD) AS STRING
    RETURN System.ComponentModel.Win32Exception{ (INT) dwError }.Message




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
   CASE  __UsualType.Long
      ret := "LONGINT"
   CASE  __UsualType.Date
      ret := "DATE"
   CASE  __UsualType.Float
      ret := "FLOAT"
   CASE  4
      ret := "FIXED"
   CASE  __UsualType.Array
      ret := "ARRAY"
   CASE  __UsualType.Object
      ret := "OBJECT"
   CASE  __UsualType.String
      ret := "STRING"
   CASE  __UsualType.Logic
      ret := "LOGIC"
   CASE  __UsualType.Codeblock
      ret := "CODEBLOCK"
   CASE  __UsualType.Symbol
      ret := "SYMBOL"
   CASE  __UsualType.Byte
      ret := "BYTE"
   CASE  __UsualType.Shortint
      ret := "SHORTINT"
   CASE  __UsualType.Word
      ret := "WORD"
   CASE  __UsualType.DWord
      ret := "DWORD"
   CASE  __UsualType.Real4
      ret := "REAL4"
   CASE  __UsualType.Real8
      ret := "REAL8"
   CASE  __UsualType.Psz
      ret := "PSZ"
   CASE  __UsualType.Ptr
      ret := "PTR"
   CASE  __UsualType.Usual
      ret := "USUAL"
   CASE  __UsualType.Int64
      ret := "INT64"
   CASE  __UsualType.UInt64
      ret := "UINT64"
   CASE  __UsualType.Char
      ret := "CHAR"
   CASE  __UsualType.Dynamic
      ret := "DYNAMIC"
   CASE  __UsualType.DateTime
      ret := "DATETIME"
   CASE  __UsualType.Decimal
      ret := "DECIMAL"
   CASE  __UsualType.Memo
      ret := "MEMO"
   CASE  __UsualType.Invalid
      ret := "INVALID"
   OTHERWISE
      ret := "UNKNOWN"
   END SWITCH 
   RETURN ret   
