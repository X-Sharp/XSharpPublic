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
	XSharp.Core.Functions.Oem2AnsiBuff(aDest, aSource, dwCount)
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
	XSharp.Core.Functions.Ansi2OemBuff(aDest, aSource, dwCount)
	Marshal.Copy(aDest,pszDest:Address,0, (int) dwCount)
	return pszDest
	
	

	
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
	/// Convert a null-terminated string to a strongly typed string.
	/// </summary>
	/// <param name="pszSource"></param>
	/// <returns>
	/// </returns>
	function Psz2String(pszSource as psz) as string
		// The compiler converts to a call to the implicit converter
		return pszSource:ToString()

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
   