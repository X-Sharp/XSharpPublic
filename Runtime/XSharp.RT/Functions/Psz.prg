//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Runtime.InteropServices
USING System.Text

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


	/// <summary>
	/// </summary>
	/// <param name="PSZValue"></param>
	/// <returns>
	/// </returns>
    /// <include file="RTComments.xml" path="Comments/PSZ/*" />
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




	/// <inheritdoc cref="M:XSharp.Core.Functions.Time()" />"
    /// <include file="RTComments.xml" path="Comments/PSZ/*" />
	FUNCTION TimePsz() AS PSZ
		RETURN String2Psz(Time())

    /// <inheritdoc cref="M:XSharp.Core.Functions.Time24()" />"
    /// <include file="RTComments.xml" path="Comments/PSZ/*" />
	FUNCTION TimePsz24() AS PSZ
		RETURN String2Psz(Time24())

#endregion



/// <exclude />
FUNCTION __UpperPsz(pszSource AS PSZ) AS PSZ
	LOCAL bp AS BYTE PTR
	bp := pszSource
	DO WHILE bp[1] != 0
		IF bp[1] >= 97 .AND. bp[1] <= 122       // between 'a' and 'z'
			bp[1] -= 32
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
      FOR x := 1 UPTO len
         ret[x] := (BYTE)( s[x-1] & 0xFF )
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
FUNCTION _NGet( p AS PSZ, dwOffset AS DWORD ) AS DWORD
   LOCAL ret := 0 AS DWORD
   IF p != NULL_PSZ
      ret := ((BYTE PTR)p)[dwOffset+1]
   ENDIF
   RETURN ret

// parameters are 0 based
/// <exclude/>
FUNCTION _NPut( p AS PSZ, dwOffset AS DWORD, b AS BYTE ) AS VOID
   IF p != NULL_PSZ
      ((BYTE PTR)p)[dwOffset+1] := b
   ENDIF
   RETURN


/*

These functions are not needed. There is an implicit conversion from PSZ to STRING

/// <summary>Determine if the leftmost character in a PSZ is alphanumeric.</summary>
/// <param name="pszSource">The string to examine.</param>
/// <returns>TRUE if the first character is either alphabetic or numeric otherwise FALSE.</returns>
FUNCTION IsAlNum(pszSource AS PSZ) AS LOGIC
    VAR cSource := Psz2String(pszSource)
	RETURN XSharp.Core.Functions.IsAlNum(cSource)

/// <summary>Determine if the leftmost character in a PSZ is alphabetic.</summary>
/// <param name="pszSource">The string to examine.</param>
/// <returns>TRUE if the first character is alphabetic.</returns>
FUNCTION IsAlpha(pszSource AS PSZ) AS LOGIC
    VAR cSource := Psz2String(pszSource)
	RETURN XSharp.Core.Functions.IsAlpha(cSource)

/// <summary>Determine if the leftmost character in a PSZ is alphanumeric.</summary>
/// <param name="pszSource">The string to examine.</param>
/// <returns>TRUE if the first character is alphanumeric.</returns>
FUNCTION IsAlphaNum(pszSource AS PSZ) AS LOGIC
	RETURN XSharp.RT.Functions.IsAlNum(pszSource)

/// <summary>Determine if the leftmost character in a PSZ is a binary digit  (0 or 1)).</summary>
/// <param name="pszSource">The string to examine.</param>
/// <returns>TRUE if the first character of the string is 0 or 1 otherwise FALSE.</returns>
FUNCTION IsBDigit(pszSource AS PSZ) AS LOGIC
    VAR cSource := Psz2String(pszSource)
	RETURN XSharp.Core.Functions.IsBDigit(cSource)

/// <summary>Determine if the leftmost character in a PSZ is a hex character (that is, digits from 1 through 9 and letters from A through F).</summary>
/// <param name="pszSource">The string to examine.</param>
/// <returns>TRUE if the first character is hex otherwise FALSE.</returns>
FUNCTION IsXDigit(pszSource AS PSZ) AS LOGIC
    VAR cSource := Psz2String(pszSource)
	RETURN XSharp.Core.Functions.IsXDigit(cSource)


/// <summary>Determine if the leftmost character in a PSZ is a blank (that is, Chr(9) through Chr(13) or Chr(32)).</summary>
/// <param name="pszSource">The string to examine.</param>
/// <returns>TRUE if the first character of the string blank otherwise FALSE.</returns>
FUNCTION IsSpace(pszSource AS PSZ) AS LOGIC
    VAR cSource := Psz2String(pszSource)
	RETURN XSharp.Core.Functions.IsSpace(cSource)

/// <summary>Determine if the leftmost character in a PSZ is uppercase.</summary>
/// <param name="pszSource">The string to examine.</param>
/// <returns>TRUE if the first character is an uppercase letter otherwise, FALSE.</returns>
FUNCTION IsUpper(pszSource AS PSZ) AS LOGIC
    VAR cSource := Psz2String(pszSource)
	RETURN XSharp.Core.Functions.IsUpper(cSource)

/// <summary>Determine if the leftmost character in a PSZ is lower.</summary>
/// <param name="pszSource">The string to examine.</param>
/// <returns>TRUE if the first character is a lowercase letter otherwise, FALSE.</returns>
FUNCTION IsLower(pszSource AS PSZ) AS LOGIC
    VAR cSource := Psz2String(pszSource)
	RETURN XSharp.Core.Functions.IsLower(cSource)
*/
