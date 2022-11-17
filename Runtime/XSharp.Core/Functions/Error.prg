//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/errstring/*" />
FUNCTION ErrString(dwNewReturnCode AS DWORD) AS STRING
	LOCAL cResource AS STRING
	IF dwNewReturnCode > XSharp.Gencode.EG_MAX
		cResource := "RT_MSG_ERR_UNKNOWN"
	ELSE
		cResource := "RT_MSG_ERR_" + dwNewReturnCode:ToString()
	ENDIF
	RETURN Messages.GetString(cResource)



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/doserrstring/*" />
FUNCTION DosErrString(dwError AS DWORD) AS STRING
    RETURN System.ComponentModel.Win32Exception{ (INT) dwError }.Message




/// <summary>Return a descriptive name (in upper case) for a X# (Usual) Type</summary>
/// <param name="dwType">Type number to check for</param>
/// <returns>
/// </returns>
FUNCTION TypeString( nType AS DWORD ) AS STRING
   LOCAL ret AS STRING
   SWITCH (__UsualType) nType
   CASE __UsualType.Void
      ret := "NIL"
   CASE  __UsualType.Long
      ret := "LONGINT"
   CASE  __UsualType.Date
      ret := "DATE"
   CASE  __UsualType.Float
      ret := "FLOAT"
   CASE  __UsualType.Fixed
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
   CASE  __UsualType.ShortInt
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
   CASE  __UsualType.Currency
      ret := "CURRENCY"
   CASE  __UsualType.Memo
      ret := "MEMO"
   CASE  __UsualType.Invalid
      ret := "INVALID"
   case  __UsualType.Null
      ret := "NULL"
   otherwise
      ret := "UNKNOWN"
   END SWITCH
   RETURN ret


FUNCTION __ErrString(resid AS DWORD , args PARAMS OBJECT[]) AS STRING
    // Strings are stored in a Managed resource with a name
    // the name matches the enum names
    // convert the id to the enum and get its name
    LOCAL strId  AS STRING
    LOCAL strMessage AS STRING
    strId := Enum.GetName( TYPEOF(VOErrors) , resid)
    IF !String.IsNullOrEmpty(strId)
        strMessage := XSharp.Messages.GetString( strId )
        IF String.IsNullOrEmpty( strMessage )
            strMessage := ": canot load string resource '" + strId + "'"
        ELSEIF args != null .and. args:Length > 0
            strMessage := String.Format(strMessage, args)
        ENDIF
    ELSE
        strMessage := "Cannot find string for error number "+resid:ToString()
    ENDIF
    RETURN strMessage
