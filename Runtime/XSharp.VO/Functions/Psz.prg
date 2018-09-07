//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Runtime.InteropServices
USING System.Text

#region functions

/// <summary>
/// Convert a specified number of OEM characters in a source buffer to a buffer of corresponding, if any, ANSI characters.
/// </summary>
/// <param name="pszDest"></param>
/// <param name="pszSource"></param>
/// <param name="dwCount"></param>
/// <returns>
/// </returns>
FUNCTION Oem2AnsiBuff(pszDest AS PSZ,pszSource AS PSZ,dwCount AS DWORD) AS PSZ
	VAR aSource := BYTE[]{dwCount}
	VAR aDest   := BYTE[]{dwCount}
	Marshal.Copy(pszDest:Address,aSource,0, (INT) dwCount)
	XSharp.Core.Functions.Oem2AnsiBuff(aDest, aSource, dwCount)
	Marshal.Copy(aDest,pszDest:Address,0, (INT) dwCount)
	RETURN pszDest

/// <summary>
/// Convert a specified number of ANSI characters in a source buffer to a buffer of corresponding OEM characters.
/// </summary>
/// <param name="pszDest"></param>
/// <param name="pszSource"></param>
/// <param name="dwCount"></param>
/// <returns>
/// </returns>
FUNCTION Ansi2OemBuff(pszDest AS PSZ,pszSource AS PSZ,dwCount AS DWORD) AS PSZ
	VAR aSource := BYTE[]{dwCount}
	VAR aDest   := BYTE[]{dwCount}
	Marshal.Copy(pszDest:Address,aSource,0, (INT) dwCount)
	XSharp.Core.Functions.Ansi2OemBuff(aDest, aSource, dwCount)
	Marshal.Copy(aDest,pszDest:Address,0, (INT) dwCount)
	RETURN pszDest




	/// <summary>
	/// Convert a string containing an 8-bit logical into a logical value.
	/// </summary>
	/// <param name="pszC"></param>
	/// <returns>
	/// </returns>
	FUNCTION Bin2Logic(pszC AS PSZ) AS LOGIC
		IF pszC != NULL_PSZ
			RETURN pszC:Item[0] != 0
		ENDIF
		RETURN FALSE


	/// <summary>
	/// </summary>
	/// <param name="PSZValue"></param>
	/// <returns>
	/// </returns>
	FUNCTION EmptyPSZ(PSZValue AS PSZ) AS LOGIC
		   RETURN PSZValue:IsEmpty



	/// <summary>
	/// Convert a null-terminated string to a strongly typed string.
	/// </summary>
	/// <param name="pszSource"></param>
	/// <returns>
	/// </returns>
	FUNCTION Psz2String(pszSource AS PSZ) AS STRING
		// The compiler converts to a call to the implicit converter
		RETURN pszSource:ToString()

	/// <summary>
	/// Convert a null-terminated string to a Usual with a string value
	/// </summary>
	/// <param name="pszSource"></param>
	/// <returns>
	/// </returns>
	FUNCTION Psz2Usual(pszSource AS PSZ) AS USUAL
		// The compiler converts to a call to the implicit converter
		RETURN pszSource

	/// <summary>
	/// Copy a buffer pointed to by a Psz to a newly allocated block of memory and return a new pointer to that memory.
	/// </summary>
	/// <param name="ptrSource"></param>
	/// <returns>
	/// </returns>
	FUNCTION PszAlloc(ptrSource AS PSZ) AS PSZ
		RETURN PSZ{ ptrSource:Address }

	/// <summary>
	/// Return the length of a Psz.
	/// </summary>
	/// <param name="pszX"></param>
	/// <returns>
	/// </returns>
	FUNCTION PszLen(pszX AS PSZ) AS DWORD
		RETURN pszX:Length



	/// <summary>
	/// Copy a string to a newly allocated block of memory and return a Psz to the memory.
	/// </summary>
	/// <param name="cSource"></param>
	/// <returns>
	/// </returns>
	FUNCTION StringAlloc(cSource AS STRING) AS PSZ
		RETURN PSZ{cSource}




	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION TimePsz() AS PSZ
		RETURN String2Psz(Time())

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION TimePsz24() AS PSZ
		RETURN String2Psz(Time24())

#endregion



/// <summary>
/// </summary>
/// <param name="pszSource"></param>
/// <returns>
/// </returns>
FUNCTION __UpperPsz(pszSource AS PSZ) AS PSZ
	LOCAL bp AS BYTE PTR
	bp := pszSource
	DO WHILE bp[1] != 0
		IF bp[1] >= 97 .AND. bp[1] <= 122
			bp[1] -= 32
		ENDIF
		bp++
	ENDDO
	RETURN pszSource

FUNCTION String2Mem(s AS STRING) AS IntPtr
	LOCAL result := 0 AS IntPtr
	IF s != NULL
		VAR encoding := System.Text.Encoding.Default
		VAR bytes    := encoding:GetBytes(s)
		VAR len      := bytes:Length
		result	     := MemAlloc((DWORD) (len+1))
		Marshal.Copy(bytes,0,result, len)
	ENDIF
	RETURN result

UNSAFE FUNCTION Mem2String(pString AS IntPtr, nLen AS DWORD) AS STRING
	IF pString == IntPtr.Zero .OR. nLen == 0
		RETURN String.Empty
	ENDIF
	VAR encoding := System.Text.Encoding.Default
	VAR numchars := encoding:GetCharCount( (BYTE PTR) pString, (INT) nLen)
	VAR buffer   := (CHAR PTR) MemAlloc( (DWORD) (numchars * SIZEOF(CHAR)) )
	numchars     := encoding:GetChars((BYTE PTR) pString, (INT) nLen, buffer, numchars)
	VAR result   := STRING {buffer, 0, numchars}
	MemFree(buffer)
RETURN result




// The following functions are used by Crypt
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
FUNCTION _NGet( p AS PSZ, dwOffset AS DWORD ) AS DWORD
   LOCAL ret := 0 AS DWORD
   IF p != NULL_PSZ
      ret := ((BYTE PTR)p)[dwOffset+1]
   ENDIF
   RETURN ret

// parameters are 0 based
FUNCTION _NPut( p AS PSZ, dwOffset AS DWORD, b AS BYTE ) AS VOID
   IF p != NULL_PSZ
      ((BYTE PTR)p)[dwOffset+1] := b
   ENDIF
   RETURN



/// <summary>Determine if the leftmost character in a string is alphanumeric.</summary>
/// <param name="pszSource">The string to examine.</param>
/// <returns>TRUE if the first character is either alphabetic or numeric otherwise FALSE.</returns>
FUNCTION IsAlNum(pszSource AS PSZ) AS LOGIC
    VAR cSource := Psz2String(pszSource)
	RETURN XSharp.Core.Functions.IsAlNum(cSource)

/// <summary>Determine if the leftmost character in a string is alphabetic.</summary>
/// <param name="pszSource">The string to examine.</param>
/// <returns>TRUE if the first character is alphabetic.</returns>
FUNCTION IsAlpha(pszSource AS PSZ) AS LOGIC
    VAR cSource := Psz2String(pszSource)
	RETURN XSharp.Core.Functions.IsAlpha(cSource)

FUNCTION IsAlphaNum(pszSource AS PSZ) AS LOGIC
	RETURN XSharp.VO.Functions.IsAlNum(pszSource)

/// <summary>Determine if the leftmost character in a string is a binary digit  (0 or 1)).</summary>
/// <param name="pszSource">The string to examine.</param>
/// <returns>TRUE if the first character of the string is 0 or 1 otherwise FALSE.</returns>
FUNCTION IsBDigit(pszSource AS PSZ) AS LOGIC
    VAR cSource := Psz2String(pszSource)
	RETURN XSharp.Core.Functions.IsBDigit(cSource)

/// <summary>Determine if the leftmost character in a string is a hex character (that is, digits from 1 through 9 and letters from A through F).</summary>
/// <param name="pszSource">The string to examine.</param>
/// <returns>TRUE if the first character is hex otherwise FALSE.</returns>
FUNCTION IsXDigit(pszSource AS PSZ) AS LOGIC
    VAR cSource := Psz2String(pszSource)
	RETURN XSharp.Core.Functions.IsXDigit(cSource)


/// <summary>Determine if the leftmost character in a string is a blank (that is, Chr(9) through Chr(13) or Chr(32)).</summary>
/// <param name="pszSource">The string to examine.</param>
/// <returns>TRUE if the first character of the string blank otherwise FALSE.</returns>
FUNCTION IsSpace(pszSource AS PSZ) AS LOGIC
    VAR cSource := Psz2String(pszSource)
	RETURN XSharp.Core.Functions.IsSpace(cSource)

/// <summary>Determine if the leftmost character in a string is uppercase.</summary>
/// <param name="pszSource">The string to examine.</param>
/// <returns>TRUE if the first character is an uppercase letter otherwise, FALSE.</returns>
FUNCTION IsUpper(pszSource AS PSZ) AS LOGIC
    VAR cSource := Psz2String(pszSource)
	RETURN XSharp.Core.Functions.IsUpper(cSource)

/// <summary>Determine if the leftmost character in a string is lower.</summary>
/// <param name="pszSource">The string to examine.</param>
/// <returns>TRUE if the first character is a lowercase letter otherwise, FALSE.</returns>
FUNCTION IsLower(pszSource AS PSZ) AS LOGIC
    VAR cSource := Psz2String(pszSource)
	RETURN XSharp.Core.Functions.IsLower(cSource)
