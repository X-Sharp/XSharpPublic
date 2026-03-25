//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Runtime.InteropServices
USING System.Text
// This code assumes 0 based arrays
#pragma options("az",on)
#region functions

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/oem2ansibuff/*" />
/// <include file="RTComments.xml" path="Comments/PSZ/*" />
FUNCTION Oem2AnsiBuff(pszTarget AS PSZ,pszSource AS PSZ,dwCount AS DWORD) AS PSZ
	VAR aSource := BYTE[]{dwCount}
	VAR aDest   := BYTE[]{dwCount}
	Marshal.Copy(pszTarget:Address,aSource,0, (INT) dwCount)
	XSharp.Core.Functions.Oem2AnsiBuff(aDest, aSource, dwCount)
	Marshal.Copy(aDest,pszTarget:Address,0, (INT) dwCount)
	RETURN pszTarget

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ansi2oembuff/*" />
/// <include file="RTComments.xml" path="Comments/PSZ/*" />
FUNCTION Ansi2OemBuff(pszTarget AS PSZ,pszSource AS PSZ,dwCount AS DWORD) AS PSZ
	VAR aSource := BYTE[]{dwCount}
	VAR aDest   := BYTE[]{dwCount}
	Marshal.Copy(pszTarget:Address,aSource,0, (INT) dwCount)
	XSharp.Core.Functions.Ansi2OemBuff(aDest, aSource, dwCount)
	Marshal.Copy(aDest,pszTarget:Address,0, (INT) dwCount)
	RETURN pszTarget




    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/bin2logic/*" />
    /// <include file="RTComments.xml" path="Comments/PSZ/*" />
	FUNCTION Bin2Logic(pszLogical AS PSZ) AS LOGIC
		IF pszLogical != NULL_PSZ
			RETURN pszLogical:Item[0] != 0
		ENDIF
		RETURN FALSE


 /// <include file="XSharp.RT.Docs.xml" path="doc/EmptyPSZ/*" />
	FUNCTION EmptyPSZ(PSZValue AS PSZ) AS LOGIC
		   RETURN PSZValue:IsEmpty



	/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/psz2string/*" />
    /// <include file="RTComments.xml" path="Comments/PSZ/*" />
	FUNCTION Psz2String(pszString AS PSZ) AS STRING
		// The compiler converts to a call to the implicit converter
		RETURN pszString:ToString()

	/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/psz2string/*" />
    /// <include file="RTComments.xml" path="Comments/PSZ/*" />
	FUNCTION Psz2Usual(pszString AS PSZ) AS USUAL
		// The compiler converts to a call to the implicit converter
		RETURN pszString

	/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/pszalloc/*" />
    /// <include file="RTComments.xml" path="Comments/PSZ/*" />
	FUNCTION PszAlloc(pszString AS PSZ) AS PSZ
		RETURN PSZ{ pszString:Address }

	/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/pszalloc/*" />
    /// <include file="RTComments.xml" path="Comments/PSZ/*" />
	FUNCTION PszLen(pszString AS PSZ) AS DWORD
		RETURN pszString:Length



	/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/stringalloc/*" />
    /// <include file="RTComments.xml" path="Comments/PSZ/*" />
	FUNCTION StringAlloc(cString AS STRING) AS PSZ
		LOCAL pMem := String2Mem(cString) AS IntPtr
        RETURN PSZ{pMem}




	/// <inheritdoc cref="Time" />"
    /// <include file="RTComments.xml" path="Comments/PSZ/*" />
	FUNCTION TimePsz() AS PSZ
		RETURN String2Psz(Time())

    /// <inheritdoc cref="Time24" />"
    /// <include file="RTComments.xml" path="Comments/PSZ/*" />
	FUNCTION TimePsz24() AS PSZ
		RETURN String2Psz(Time24())

#endregion



/// <exclude />
FUNCTION __UpperPsz(pszSource AS PSZ) AS PSZ
	LOCAL bp AS BYTE PTR
	bp := pszSource
	DO WHILE bp[0] != 0
		IF bp[0] >= 97 .AND. bp[0] <= 122       // between 'a' and 'z'
			bp[0] -= 32
		ENDIF
		bp++
	ENDDO
	RETURN pszSource

/// <exclude/>
FUNCTION String2Mem(s AS STRING) AS IntPtr
	LOCAL result := 0 AS IntPtr
	IF s != NULL
		VAR encoding := StringHelpers.WinEncoding
		VAR bytes    := encoding:GetBytes(s)
		VAR len      := bytes:Length
		result	     := MemAlloc((DWORD) (len+1))
		Marshal.Copy(bytes,0,result, len)
        Marshal.WriteByte(result, len, 0)	 // end of string
	ENDIF
	RETURN result

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/mem2string/*" />
FUNCTION Mem2String(ptrSource AS IntPtr, dwCount AS DWORD) AS STRING
	IF ptrSource == IntPtr.Zero .OR. dwCount == 0
		RETURN String.Empty
	ENDIF
	VAR encoding := StringHelpers.WinEncoding
	VAR numchars := encoding:GetCharCount( (BYTE PTR) ptrSource, (INT) dwCount)
	VAR buffer   := (CHAR PTR) MemAlloc( (DWORD) (numchars * SIZEOF(CHAR)) )
	numchars     := encoding:GetChars((BYTE PTR) ptrSource, (INT) dwCount, buffer, numchars)
	VAR result   := STRING {buffer, 0, numchars}
	MemFree(buffer)
RETURN result




// The following functions are used by Crypt
/// <exclude/>
FUNCTION __String2MemRaw( s AS STRING ) AS PSZ
   LOCAL ret AS BYTE PTR
   LOCAL x   AS INT
   LOCAL len AS INT
   ret := NULL_PTR
   IF s != NULL
      len := s:Length
      ret := (BYTE PTR) MemAlloc( (DWORD)( len + 1 ) )
      FOR x := 0 UPTO len -1
         ret[x] := (BYTE)( s[x] & 0xFF )
      NEXT
   ENDIF

   RETURN PSZ{ ret }

/// <exclude/>
FUNCTION __Mem2StringRaw( p AS PSZ, len AS DWORD ) AS STRING
   LOCAL sb AS StringBuilder
   LOCAL x  AS DWORD

   IF len > 0
       sb  := StringBuilder{ (INT) len }

       FOR x := 0 UPTO len - 1
          sb:Append( (CHAR) p:Item[(INT)x] )
       NEXT

       RETURN sb:ToString()
    ELSE
       RETURN ""
    ENDIF



// parameters are 0 based
/// <exclude/>
FUNCTION _NGet( p AS PSZ, dwOffset AS DWORD ) AS BYTE
   LOCAL ret := 0 AS BYTE
   IF p != NULL_PSZ
      ret := ((BYTE PTR)p)[dwOffset]
   ENDIF
   RETURN ret

// parameters are 0 based
/// <exclude/>
FUNCTION _NPut( p AS PSZ, dwOffset AS DWORD, b AS BYTE ) AS VOID
   IF p != NULL_PSZ
      ((BYTE PTR)p)[dwOffset] := b
   ENDIF
   RETURN


/*

These functions are not needed. There is an implicit conversion from PSZ to STRING

/// <include file="XSharp.RT.Docs.xml" path="doc/IsAlNum/*" />
FUNCTION IsAlNum(pszSource AS PSZ) AS LOGIC
    VAR cSource := Psz2String(pszSource)
	RETURN XSharp.Core.Functions.IsAlNum(cSource)

/// <include file="XSharp.RT.Docs.xml" path="doc/IsAlpha/*" />
FUNCTION IsAlpha(pszSource AS PSZ) AS LOGIC
    VAR cSource := Psz2String(pszSource)
	RETURN XSharp.Core.Functions.IsAlpha(cSource)

/// <include file="XSharp.RT.Docs.xml" path="doc/IsAlphaNum/*" />
FUNCTION IsAlphaNum(pszSource AS PSZ) AS LOGIC
	RETURN XSharp.RT.Functions.IsAlNum(pszSource)

/// <include file="XSharp.RT.Docs.xml" path="doc/IsBDigit/*" />
FUNCTION IsBDigit(pszSource AS PSZ) AS LOGIC
    VAR cSource := Psz2String(pszSource)
	RETURN XSharp.Core.Functions.IsBDigit(cSource)

/// <include file="XSharp.RT.Docs.xml" path="doc/IsXDigit/*" />
FUNCTION IsXDigit(pszSource AS PSZ) AS LOGIC
    VAR cSource := Psz2String(pszSource)
	RETURN XSharp.Core.Functions.IsXDigit(cSource)


/// <include file="XSharp.RT.Docs.xml" path="doc/IsSpace/*" />
FUNCTION IsSpace(pszSource AS PSZ) AS LOGIC
    VAR cSource := Psz2String(pszSource)
	RETURN XSharp.Core.Functions.IsSpace(cSource)

/// <include file="XSharp.RT.Docs.xml" path="doc/IsUpper/*" />
FUNCTION IsUpper(pszSource AS PSZ) AS LOGIC
    VAR cSource := Psz2String(pszSource)
	RETURN XSharp.Core.Functions.IsUpper(cSource)

/// <include file="XSharp.RT.Docs.xml" path="doc/IsLower/*" />
FUNCTION IsLower(pszSource AS PSZ) AS LOGIC
    VAR cSource := Psz2String(pszSource)
	RETURN XSharp.Core.Functions.IsLower(cSource)
*/
