//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using XSharp


#region functions
	/// <summary>
	/// Remove spaces from a file name specified as a __Psz, changing the contents of the original file name as well as the returned file name.
	/// </summary>
	/// <param name="pszFileName"></param>
	/// <returns>
	/// </returns>
	function AdjustFNamePSZ(pszFileName as __Psz) as __Psz
		/// THROW NotImplementedException{}
		return (__Psz) IntPtr.Zero
	
	/// <summary>
	/// Convert a specified number of ANSI characters in a source buffer to a buffer of corresponding OEM characters.
	/// </summary>
	/// <param name="pszDest"></param>
	/// <param name="pszSource"></param>
	/// <param name="dwCount"></param>
	/// <returns>
	/// </returns>
	function Ansi2OemBuff(pszDest as __Psz,pszSource as __Psz,dwCount as dword) as __Psz
		/// THROW NotImplementedException{}
		return (__Psz) IntPtr.Zero
	
	/// <summary>
	/// Convert a value to a static __Psz.
	/// </summary>
	/// <param name="u"></param>
	/// <returns>
	/// </returns>
	function AsPsz(u as __Usual) as __Psz
		/// THROW NotImplementedException{}
		return (__Psz) IntPtr.Zero
	
	/// <summary>
	/// Convert a string containing an 8-bit logical into a logical value.
	/// </summary>
	/// <param name="pszC"></param>
	/// <returns>
	/// </returns>
	function Bin2Logic(pszC as __Psz) as logic
		/// THROW NotImplementedException{}
		return false   
	
	/// <summary>
	/// Check a strongly typed string for String.Empty and return a valid (non-zero) __Psz.
	/// </summary>
	/// <param name="cSource"></param>
	/// <returns>
	/// </returns>
	function Cast2Psz(cSource as string) as __Psz
		/// THROW NotImplementedException{}
		return (__Psz) IntPtr.Zero
	
	/// <summary>
	/// Change the current Windows directory.
	/// </summary>
	/// <param name="pszDir"></param>
	/// <returns>
	/// </returns>
	function DirChange(pszDir as __Psz) as int
		/// THROW NotImplementedException{}
		return 0   
	
	/// <summary>
	/// Create a directory.
	/// </summary>
	/// <param name="pszDir"></param>
	/// <returns>
	/// </returns>
	function DirMake(pszDir as __Psz) as int
		/// THROW NotImplementedException{}
		return 0   
	
	/// <summary>
	/// Remove a directory.
	/// </summary>
	/// <param name="pszDir"></param>
	/// <returns>
	/// </returns>
	function DirRemove(pszDir as __Psz) as int
		/// THROW NotImplementedException{}
		return 0   
	
	/// <summary>
	/// Change the current disk drive.
	/// </summary>
	/// <param name="pszDisk"></param>
	/// <returns>
	/// </returns>
	function DiskChange(pszDisk as __Psz) as logic
		/// THROW NotImplementedException{}
		return false   
	
	/// <summary>
	/// </summary>
	/// <param name="PSZValue"></param>
	/// <returns>
	/// </returns>
	function EmptyPSZ(PSZValue as __Psz) as logic
		/// THROW NotImplementedException{}
		return false   
	
	/// <summary>
	/// </summary>
	/// <param name="pszText"></param>
	/// <param name="pszCapt"></param>
	/// <param name="dwB1"></param>
	/// <param name="dwB2"></param>
	/// <param name="dwB3"></param>
	/// <returns>
	/// </returns>
	function ErrorMessageBox(pszText as __Psz,pszCapt as __Psz,dwB1 as dword,dwB2 as dword,dwB3 as dword) as dword
		/// THROW NotImplementedException{}
		return 0     
	
	/// <summary>
	/// Determine if the leftmost character in a string is alphanumeric.
	/// </summary>
	/// <param name="pszC"></param>
	/// <returns>
	/// </returns>
	function IsAlNum(pszC as __Psz) as logic
		/// THROW NotImplementedException{}
		return false   
	
	/// <summary>
	/// Determine if the leftmost character in a string is alphabetic.
	/// </summary>
	/// <param name="pszSource"></param>
	/// <returns>
	/// </returns>
	function IsAlpha(pszSource as __Psz) as logic
		/// THROW NotImplementedException{}
		return false   
	
	/// <summary>
	/// </summary>
	/// <param name="pszSource"></param>
	/// <returns>
	/// </returns>
	function IsAlphaNum(pszSource as __Psz) as logic
		/// THROW NotImplementedException{}
		return false   
	
	/// <summary>
	/// Determine if the leftmost character in a string is a binary digit (0 or 1).
	/// </summary>
	/// <param name="pszC"></param>
	/// <returns>
	/// </returns>
	function IsBDigit(pszC as __Psz) as logic
		/// THROW NotImplementedException{}
		return false   
	
	/// <summary>
	/// Determine if the leftmost character in a string is a digit (that is, a numeric digit between 0 and 9).
	/// </summary>
	/// <param name="pszC"></param>
	/// <returns>
	/// </returns>
	function IsDigit(pszC as __Psz) as logic
		/// THROW NotImplementedException{}
		return false   
	
	/// <summary>
	/// Determine if the leftmost character in a string is a lowercase letter.
	/// </summary>
	/// <param name="pszSource"></param>
	/// <returns>
	/// </returns>
	function IsLower(pszSource as __Psz) as logic
		/// THROW NotImplementedException{}
		return false   
	
	/// <summary>
	/// Determine if the leftmost character in a string is a blank (that is, Chr(9) through Chr(13) or Chr(32)).
	/// </summary>
	/// <param name="pszC"></param>
	/// <returns>
	/// </returns>
	function IsSpace(pszC as __Psz) as logic
		/// THROW NotImplementedException{}
		return false   
	
	/// <summary>
	/// Determine if the leftmost character in a string is uppercase.
	/// </summary>
	/// <param name="pszSource"></param>
	/// <returns>
	/// </returns>
	function IsUpper(pszSource as __Psz) as logic
		/// THROW NotImplementedException{}
		return false   
	
	/// <summary>
	/// Determine if the leftmost character in a string is a hex character (that is, digits from 1 through 9 and letters from A through F).
	/// </summary>
	/// <param name="pszC"></param>
	/// <returns>
	/// </returns>
	function IsXDigit(pszC as __Psz) as logic
		/// THROW NotImplementedException{}
		return false   
	
	
	/// <summary>
	/// </summary>
	/// <param name="pObj"></param>
	/// <returns>
	/// </returns>
	function ObjAsPsz(pObj as IntPtr) as __Psz
		/// THROW NotImplementedException{}
		return (__Psz) IntPtr.Zero
	
	/// <summary>
	/// Convert a specified number of OEM characters in a source buffer to a buffer of corresponding, if any, ANSI characters.
	/// </summary>
	/// <param name="pszDest"></param>
	/// <param name="pszSource"></param>
	/// <param name="dwCount"></param>
	/// <returns>
	/// </returns>
	function Oem2AnsiBuff(pszDest as __Psz,pszSource as __Psz,dwCount as dword) as __Psz
		/// THROW NotImplementedException{}
		return __PSZ._NULL_PSZ 
	

	
	/// <summary>
	/// Convert a null-terminated string to a strongly typed string.
	/// </summary>
	/// <param name="pszSource"></param>
	/// <returns>
	/// </returns>
	function Psz2String(pszSource as __Psz) as string
		/// THROW NotImplementedException{}
		return String.Empty   
	
	/// <summary>
	/// Copy a buffer pointed to by a __Psz to a newly allocated block of memory and return a new pointer to that memory.
	/// </summary>
	/// <param name="ptrSource"></param>
	/// <returns>
	/// </returns>
	function PszAlloc(ptrSource as IntPtr) as __Psz
		/// THROW NotImplementedException{}
		return __PSZ._NULL_PSZ 
	
	/// <summary>
	/// Return the length of a __Psz.
	/// </summary>
	/// <param name="pszX"></param>
	/// <returns>
	/// </returns>
	function PszLen(pszX as __Psz) as dword
		/// THROW NotImplementedException{}
		return 0   
	
	/// <summary>
	/// </summary>
	/// <param name="pszUnicode"></param>
	/// <returns>
	/// </returns>
	function PszLenW(pszUnicode as __Psz) as dword
		/// THROW NotImplementedException{}
		return 0   
	
	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	function ReleaseString() as __Psz
		/// THROW NotImplementedException{}
		return __PSZ._NULL_PSZ 
	

	
	/// <summary>
	/// Convert a __VOFloat expression to a __Psz.
	/// </summary>
	/// <param name="flSource"></param>
	/// <param name="dwLen"></param>
	/// <param name="dwDec"></param>
	/// <returns>
	/// </returns>
	function StrFloat(flSource as __VOFloat,dwLen as dword,dwDec as dword) as __Psz
		/// THROW NotImplementedException{}
		return __PSZ._NULL_PSZ 
	
	/// <summary>
	/// Convert a strongly typed string to a null-terminated string.
	/// </summary>
	/// <param name="cSource"></param>
	/// <returns>
	/// </returns>
	function String2Psz(cSource as string) as __Psz
		/// THROW NotImplementedException{}
		return __PSZ._NULL_PSZ 
	
	/// <summary>
	/// Copy a string to a newly allocated block of memory and return a __Psz to the memory.
	/// </summary>
	/// <param name="cSource"></param>
	/// <returns>
	/// </returns>
	function StringAlloc(cSource as string) as __Psz
		/// THROW NotImplementedException{}
		return __PSZ._NULL_PSZ 
	
	/// <summary>
	/// Convert an integer expression to a __Psz.
	/// </summary>
	/// <param name="l"></param>
	/// <param name="dwLen"></param>
	/// <param name="dwDec"></param>
	/// <returns>
	/// </returns>
	function StrInt(l as long,dwLen as dword,dwDec as dword) as __Psz
		/// THROW NotImplementedException{}
		return __PSZ._NULL_PSZ 
	
	/// <summary>
	/// Convert a long integer expression to a __Psz.
	/// </summary>
	/// <param name="l"></param>
	/// <param name="dwLen"></param>
	/// <param name="dwDec"></param>
	/// <returns>
	/// </returns>
	function StrLong(l as long,dwLen as dword,dwDec as dword) as __Psz
		/// THROW NotImplementedException{}
		return __PSZ._NULL_PSZ 
	
	/// <summary>
	/// Convert a null-terminated string to a __Symbol and add it to the atom table.
	/// </summary>
	/// <param name="p"></param>
	/// <returns>
	/// </returns>
	function SysAddAtom(p as __Psz) as __Symbol
		/// THROW NotImplementedException{}
		return null_symbol   
	
	/// <summary>
	/// Convert a null-terminated string to an uppercase __Symbol and add it to the atom table.
	/// </summary>
	/// <param name="p"></param>
	/// <returns>
	/// </returns>
	function SysAddAtomUpperA(p as __Psz) as __Symbol
		/// THROW NotImplementedException{}
		return null_symbol   
	
	/// <summary>
	/// </summary>
	/// <param name="p"></param>
	/// <returns>
	/// </returns>
	function SysAddAtomUpperBuff(p as __Psz) as __Symbol
		/// THROW NotImplementedException{}
		return null_symbol   
	
	/// <summary>
	/// Determine whether a __Symbol is in the atom table.
	/// </summary>
	/// <param name="p"></param>
	/// <returns>
	/// </returns>
	function SysFindAtom(p as __Psz) as __Symbol
		/// THROW NotImplementedException{}
		return null_symbol   
	
	/// <summary>
	/// Convert a __Symbol to a null-terminated string.
	/// </summary>
	/// <param name="s"></param>
	/// <returns>
	/// </returns>
	function SysGetAtomName(s as __Symbol) as __Psz
		/// THROW NotImplementedException{}
		return __PSZ._NULL_PSZ 
	
	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	function TimePsz() as __Psz
		/// THROW NotImplementedException{}
		return __PSZ._NULL_PSZ 
	
	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	function TimePsz24() as __Psz
		/// THROW NotImplementedException{}
		return __PSZ._NULL_PSZ 
	
#endregion



/// <summary>
/// </summary>
/// <param name="pszSource"></param>
/// <returns>
/// </returns>
function __UpperPsz(pszSource as __Psz) as __Psz
	/// THROW NotImplementedException{}
	return	 (__Psz) IntPtr.Zero  