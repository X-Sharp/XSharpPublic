//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using System.Runtime.InteropServices
using System.Text

#region functions

/// <summary>
/// Convert a specified number of OEM characters in a source buffer to a buffer of corresponding, if any, ANSI characters.
/// </summary>
/// <param name="pszDest"></param>
/// <param name="pszSource"></param>
/// <param name="dwCount"></param>
/// <returns>
/// </returns>
function Oem2AnsiBuff(pszDest as psz,pszSource as psz,dwCount as dword) as psz
	var aSource := byte[]{dwCount}
	var aDest   := byte[]{dwCount}
	Marshal.Copy(pszDest:Address,aSource,0, (int) dwCount)
	global::Functions.Oem2AnsiBuff(aDest, aSource, dwCount)
	Marshal.Copy(aDest,pszDest:Address,0, (int) dwCount)
	return pszDest

/// <summary>
/// Convert a specified number of ANSI characters in a source buffer to a buffer of corresponding OEM characters.
/// </summary>
/// <param name="pszDest"></param>
/// <param name="pszSource"></param>
/// <param name="dwCount"></param>
/// <returns>
/// </returns>
function Ansi2OemBuff(pszDest as Psz,pszSource as Psz,dwCount as dword) as Psz
	var aSource := byte[]{dwCount}
	var aDest   := byte[]{dwCount}
	Marshal.Copy(pszDest:Address,aSource,0, (int) dwCount)
	global::Functions.Ansi2OemBuff(aDest, aSource, dwCount)
	Marshal.Copy(aDest,pszDest:Address,0, (int) dwCount)
	return pszDest
	
	
	/// <summary>
	/// Convert a value to a static Psz.
	/// </summary>
	/// <param name="u"></param>
	/// <returns>
	/// </returns>
	function AsPsz(u as __Usual) as Psz
		/// THROW NotImplementedException{}
		return (Psz) IntPtr.Zero
	
	/// <summary>
	/// Convert a string containing an 8-bit logical into a logical value.
	/// </summary>
	/// <param name="pszC"></param>
	/// <returns>
	/// </returns>
	function Bin2Logic(pszC as Psz) as logic
		if pszC != null_psz
			return pszC:Item[0] != 0
		endif	
		return false
	

	/// <summary>
	/// </summary>
	/// <param name="PSZValue"></param>
	/// <returns>
	/// </returns>
	function EmptyPSZ(PSZValue as Psz) as logic
		   RETURN PSZValue:IsEmpty
	
	/// <summary>
	/// </summary>
	/// <param name="pszText"></param>
	/// <param name="pszCapt"></param>
	/// <param name="dwB1"></param>
	/// <param name="dwB2"></param>
	/// <param name="dwB3"></param>
	/// <returns>
	/// </returns>
	function ErrorMessageBox(pszText as Psz,pszCapt as Psz,dwB1 as dword,dwB2 as dword,dwB3 as dword) as dword
		/// THROW NotImplementedException{}
		return 0     
	
	/// <summary>
	/// Determine if the leftmost character in a string is alphanumeric.
	/// </summary>
	/// <param name="pszC"></param>
	/// <returns>
	/// </returns>
	function IsAlNum(pszSource as Psz) as logic
		if pszSource != NULL_PSZ
			return System.Char.IsLetterorDigit(pszSource:Item[0])
		endif
		return false   
	
	/// <summary>
	/// Determine if the leftmost character in a string is alphabetic.
	/// </summary>
	/// <param name="pszSource"></param>
	/// <returns>
	/// </returns>
	function IsAlpha(pszSource as psz) as logic
		if pszSource != NULL_PSZ
			return System.Char.IsLetter(pszSource:Item[0])
		endif
		return false
	
	/// <summary>
	/// </summary>
	/// <param name="pszSource"></param>
	/// <returns>
	/// </returns>
	function IsAlphaNum(pszSource as Psz) as logic
		if pszSource != NULL_PSZ
			return System.Char.IsLetterOrDigit(pszSource:Item[0])	
		endif
		return false
	/// <summary>
	/// Determine if the leftmost character in a string is a binary digit (0 or 1).
	/// </summary>
	/// <param name="pszC"></param>
	/// <returns>
	/// </returns>
	function IsBDigit(pszSource as Psz) as logic
		if pszSource != NULL_PSZ
			return pszSource:Item[0] == '0' .or. pszSource:Item[0] == '1'
		endif
		return false
	/// <summary>
	/// Determine if the leftmost character in a string is a digit (that is, a numeric digit between 0 and 9).
	/// </summary>
	/// <param name="pszC"></param>
	/// <returns>
	/// </returns>
	function IsDigit(pszSource as Psz) as logic
		if pszSource != NULL_PSZ
			return System.Char.IsDigit(pszSource:Item[0])	
		endif
		return false
	
	/// <summary>
	/// Determine if the leftmost character in a string is a lowercase letter.
	/// </summary>
	/// <param name="pszSource"></param>
	/// <returns>
	/// </returns>
	function IsLower(pszSource as Psz) as logic
		if pszSource != NULL_PSZ
			return System.Char.IsLower(pszSource:Item[0])	
		endif
		return false

	
	/// <summary>
	/// Determine if the leftmost character in a string is a blank (that is, Chr(9) through Chr(13) or Chr(32)).
	/// </summary>
	/// <param name="pszC"></param>
	/// <returns>
	/// </returns>
	function IsSpace(pszSource as Psz) as logic
		if pszSource != NULL_PSZ
			return System.Char.IsWhiteSpace(pszSource:Item[0])	
		endif
		return false
	/// <summary>
	/// Determine if the leftmost character in a string is uppercase.
	/// </summary>
	/// <param name="pszSource"></param>
	/// <returns>
	/// </returns>
	function IsUpper(pszSource as Psz) as logic
		if pszSource != NULL_PSZ
			return System.Char.IsUpper(pszSource:Item[0])	
		endif
		return false
	
	/// <summary>
	/// Determine if the leftmost character in a string is a hex character (that is, digits from 1 through 9 and letters from A through F).
	/// </summary>
	/// <param name="pszC"></param>
	/// <returns>
	/// </returns>
	function IsXDigit(pszSource as Psz) as logic
		if pszSource != null_psz
			local ch := pszSource:Item[0] as char
			return System.Char.IsDigit(ch) .or. (ch >= 'A' .and. ch <= 'F') .or. (ch >= 'a' .and. ch <= 'f')
		endif
		return false
		
	/// <summary>
	/// Convert a null-terminated string to a strongly typed string.
	/// </summary>
	/// <param name="pszSource"></param>
	/// <returns>
	/// </returns>
	function Psz2String(pszSource as psz) as string
		// The compiler converts to a call to the implicit converter
		return pszSource

	/// <summary>
	/// Convert a null-terminated string to a Usual with a string value
	/// </summary>
	/// <param name="pszSource"></param>
	/// <returns>
	/// </returns>
	function Psz2Usual(pszSource as psz) as Usual
		// The compiler converts to a call to the implicit converter
		return pszSource
	
	/// <summary>
	/// Copy a buffer pointed to by a Psz to a newly allocated block of memory and return a new pointer to that memory.
	/// </summary>
	/// <param name="ptrSource"></param>
	/// <returns>
	/// </returns>
	function PszAlloc(ptrSource as Psz) as Psz
		RETURN Psz{ ptrSource:Address }
	
	/// <summary>
	/// Return the length of a Psz.
	/// </summary>
	/// <param name="pszX"></param>
	/// <returns>
	/// </returns>
	function PszLen(pszX as Psz) as dword
		return pszX:Length
	


	/// <summary>
	/// Copy a string to a newly allocated block of memory and return a Psz to the memory.
	/// </summary>
	/// <param name="cSource"></param>
	/// <returns>
	/// </returns>
	function StringAlloc(cSource as string) as Psz
		return Psz{cSource}
	


	
	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	function TimePsz() as Psz
		return String2Psz(Time())
	
	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	function TimePsz24() as Psz
		return String2Psz(Time24())
	
#endregion



/// <summary>
/// </summary>
/// <param name="pszSource"></param>
/// <returns>
/// </returns>
function __UpperPsz(pszSource as Psz) as Psz
	return pszSource	

function String2Mem(s as string) as IntPtr
	local result := 0 as IntPtr
	if s != null
		var encoding := System.Text.Encoding.Default
		var bytes    := encoding:GetBytes(s)
		var len      := bytes:Length
		result	     := MemAlloc((dword) (len+1))
		Marshal.Copy(bytes,0,result, len)	
	endif
	return result 

unsafe function Mem2String(pString as IntPtr, nLen as dword) as string
	if pString == IntPtr.Zero .or. nLen == 0
		return String.Empty
	endif
	var encoding := System.Text.Encoding.Default
	var numchars := encoding:GetCharCount( (byte ptr) pString, (int) nLen) 
	var buffer   := (char ptr) MemAlloc( (dword) (numchars * sizeof(char)) )
	numchars     := encoding:GetChars((byte ptr) pString, (int) nLen, buffer, numchars)
	var result   := string {buffer, 0, numchars}
	MemFree(buffer)
return result




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
   
   RETURN Psz{ ret }
   
FUNCTION __Mem2StringRaw( p AS PSZ, len AS DWORD ) AS STRING
   LOCAL sb AS StringBuilder
   LOCAL x  AS DWORD
   
   IF len > 0
       sb  := StringBuilder{ (INT) len }
       
       FOR x := 0 UPTO len - 1
          sb:Append( (Char) p:Item[(int)x] )
       NEXT
   
       RETURN sb:ToString()
    ELSE
       RETURN ""
    endif   


