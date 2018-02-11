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
	FUNCTION AdjustFNamePSZ(pszFileName AS __Psz) AS __Psz
		/// THROW NotImplementedException{}
	RETURN __PSZ._NULL_PSZ 

	/// <summary>
	/// Convert a specified number of ANSI characters in a source buffer to a buffer of corresponding OEM characters.
	/// </summary>
	/// <param name="pszDest"></param>
	/// <param name="pszSource"></param>
	/// <param name="dwCount"></param>
	/// <returns>
	/// </returns>
	FUNCTION Ansi2OemBuff(pszDest AS __Psz,pszSource AS __Psz,dwCount AS DWORD) AS __Psz
		/// THROW NotImplementedException{}
	RETURN __PSZ._NULL_PSZ 

	/// <summary>
	/// Convert a value to a static __Psz.
	/// </summary>
	/// <param name="u"></param>
	/// <returns>
	/// </returns>
	FUNCTION AsPsz(u AS __Usual) AS __Psz
		/// THROW NotImplementedException{}
	RETURN __PSZ._NULL_PSZ 

	/// <summary>
	/// Convert a string containing an 8-bit logical into a logical value.
	/// </summary>
	/// <param name="pszC"></param>
	/// <returns>
	/// </returns>
	FUNCTION Bin2Logic(pszC AS __Psz) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Check a strongly typed string for String.Empty and return a valid (non-zero) __Psz.
	/// </summary>
	/// <param name="cSource"></param>
	/// <returns>
	/// </returns>
	FUNCTION Cast2Psz(cSource AS STRING) AS __Psz
		/// THROW NotImplementedException{}
	RETURN __PSZ._NULL_PSZ 

	/// <summary>
	/// Change the current Windows directory.
	/// </summary>
	/// <param name="pszDir"></param>
	/// <returns>
	/// </returns>
	FUNCTION DirChange(pszDir AS __Psz) AS INT
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Create a directory.
	/// </summary>
	/// <param name="pszDir"></param>
	/// <returns>
	/// </returns>
	FUNCTION DirMake(pszDir AS __Psz) AS INT
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Remove a directory.
	/// </summary>
	/// <param name="pszDir"></param>
	/// <returns>
	/// </returns>
	FUNCTION DirRemove(pszDir AS __Psz) AS INT
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Change the current disk drive.
	/// </summary>
	/// <param name="pszDisk"></param>
	/// <returns>
	/// </returns>
	FUNCTION DiskChange(pszDisk AS __Psz) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="PSZValue"></param>
	/// <returns>
	/// </returns>
	FUNCTION EmptyPSZ(PSZValue AS __Psz) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="pszText"></param>
	/// <param name="pszCapt"></param>
	/// <param name="dwB1"></param>
	/// <param name="dwB2"></param>
	/// <param name="dwB3"></param>
	/// <returns>
	/// </returns>
	FUNCTION ErrorMessageBox(pszText AS __Psz,pszCapt AS __Psz,dwB1 AS DWORD,dwB2 AS DWORD,dwB3 AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0     

	/// <summary>
	/// Determine if the leftmost character in a string is alphanumeric.
	/// </summary>
	/// <param name="pszC"></param>
	/// <returns>
	/// </returns>
	FUNCTION IsAlNum(pszC AS __Psz) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Determine if the leftmost character in a string is alphabetic.
	/// </summary>
	/// <param name="pszSource"></param>
	/// <returns>
	/// </returns>
	FUNCTION IsAlpha(pszSource AS __Psz) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="pszSource"></param>
	/// <returns>
	/// </returns>
	FUNCTION IsAlphaNum(pszSource AS __Psz) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Determine if the leftmost character in a string is a binary digit (0 or 1).
	/// </summary>
	/// <param name="pszC"></param>
	/// <returns>
	/// </returns>
	FUNCTION IsBDigit(pszC AS __Psz) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Determine if the leftmost character in a string is a digit (that is, a numeric digit between 0 and 9).
	/// </summary>
	/// <param name="pszC"></param>
	/// <returns>
	/// </returns>
	FUNCTION IsDigit(pszC AS __Psz) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Determine if the leftmost character in a string is a lowercase letter.
	/// </summary>
	/// <param name="pszSource"></param>
	/// <returns>
	/// </returns>
	FUNCTION IsLower(pszSource AS __Psz) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Determine if the leftmost character in a string is a blank (that is, Chr(9) through Chr(13) or Chr(32)).
	/// </summary>
	/// <param name="pszC"></param>
	/// <returns>
	/// </returns>
	FUNCTION IsSpace(pszC AS __Psz) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Determine if the leftmost character in a string is uppercase.
	/// </summary>
	/// <param name="pszSource"></param>
	/// <returns>
	/// </returns>
	FUNCTION IsUpper(pszSource AS __Psz) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Determine if the leftmost character in a string is a hex character (that is, digits from 1 through 9 and letters from A through F).
	/// </summary>
	/// <param name="pszC"></param>
	/// <returns>
	/// </returns>
	FUNCTION IsXDigit(pszC AS __Psz) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   


	/// <summary>
	/// </summary>
	/// <param name="pObj"></param>
	/// <returns>
	/// </returns>
	FUNCTION ObjAsPsz(pObj AS IntPtr) AS __Psz
		/// THROW NotImplementedException{}
	RETURN __PSZ._NULL_PSZ 

	/// <summary>
	/// Convert a specified number of OEM characters in a source buffer to a buffer of corresponding, if any, ANSI characters.
	/// </summary>
	/// <param name="pszDest"></param>
	/// <param name="pszSource"></param>
	/// <param name="dwCount"></param>
	/// <returns>
	/// </returns>
	FUNCTION Oem2AnsiBuff(pszDest AS __Psz,pszSource AS __Psz,dwCount AS DWORD) AS __Psz
		/// THROW NotImplementedException{}
	RETURN __PSZ._NULL_PSZ 

	/// <summary>
	/// Return the name of an activated entity.
	/// </summary>
	/// <param name="dwActivation"></param>
	/// <returns>
	/// </returns>
	FUNCTION ProcName(dwActivation AS __Usual) AS __Psz
		/// THROW NotImplementedException{}
	RETURN __PSZ._NULL_PSZ 

	/// <summary>
	/// Convert a null-terminated string to a strongly typed string.
	/// </summary>
	/// <param name="pszSource"></param>
	/// <returns>
	/// </returns>
	FUNCTION Psz2String(pszSource AS __Psz) AS STRING
		/// THROW NotImplementedException{}
	RETURN String.Empty   

	/// <summary>
	/// Copy a buffer pointed to by a __Psz to a newly allocated block of memory and return a new pointer to that memory.
	/// </summary>
	/// <param name="ptrSource"></param>
	/// <returns>
	/// </returns>
	FUNCTION PszAlloc(ptrSource AS IntPtr) AS __Psz
		/// THROW NotImplementedException{}
	RETURN __PSZ._NULL_PSZ 

	/// <summary>
	/// Return the length of a __Psz.
	/// </summary>
	/// <param name="pszX"></param>
	/// <returns>
	/// </returns>
	FUNCTION PszLen(pszX AS __Psz) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="pszUnicode"></param>
	/// <returns>
	/// </returns>
	FUNCTION PszLenW(pszUnicode AS __Psz) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION ReleaseString() AS __Psz
		/// THROW NotImplementedException{}
	RETURN __PSZ._NULL_PSZ 

	/// <summary>
	/// Break a path name into its components.
	/// </summary>
	/// <param name="pzsPath"></param>
	/// <param name="pszDrive"></param>
	/// <param name="pszDir"></param>
	/// <param name="pszName"></param>
	/// <param name="pszExt"></param>
	/// <returns>
	/// </returns>
	FUNCTION SplitPath(pzsPath AS __Psz,pszDrive AS __Psz,pszDir AS __Psz,pszName AS __Psz,pszExt AS __Psz) AS VOID
		/// THROW NotImplementedException{}
	RETURN

	/// <summary>
	/// Convert a __VOFloat expression to a __Psz.
	/// </summary>
	/// <param name="flSource"></param>
	/// <param name="dwLen"></param>
	/// <param name="dwDec"></param>
	/// <returns>
	/// </returns>
	FUNCTION StrFloat(flSource AS __VOFloat,dwLen AS DWORD,dwDec AS DWORD) AS __Psz
		/// THROW NotImplementedException{}
	RETURN __PSZ._NULL_PSZ 

	/// <summary>
	/// Convert a strongly typed string to a null-terminated string.
	/// </summary>
	/// <param name="cSource"></param>
	/// <returns>
	/// </returns>
	FUNCTION String2Psz(cSource AS STRING) AS __Psz
		/// THROW NotImplementedException{}
	RETURN __PSZ._NULL_PSZ 

	/// <summary>
	/// Copy a string to a newly allocated block of memory and return a __Psz to the memory.
	/// </summary>
	/// <param name="cSource"></param>
	/// <returns>
	/// </returns>
	FUNCTION StringAlloc(cSource AS STRING) AS __Psz
		/// THROW NotImplementedException{}
	RETURN __PSZ._NULL_PSZ 

	/// <summary>
	/// Convert an integer expression to a __Psz.
	/// </summary>
	/// <param name="l"></param>
	/// <param name="dwLen"></param>
	/// <param name="dwDec"></param>
	/// <returns>
	/// </returns>
	FUNCTION StrInt(l AS LONG,dwLen AS DWORD,dwDec AS DWORD) AS __Psz
		/// THROW NotImplementedException{}
	RETURN __PSZ._NULL_PSZ 

	/// <summary>
	/// Convert a long integer expression to a __Psz.
	/// </summary>
	/// <param name="l"></param>
	/// <param name="dwLen"></param>
	/// <param name="dwDec"></param>
	/// <returns>
	/// </returns>
	FUNCTION StrLong(l AS LONG,dwLen AS DWORD,dwDec AS DWORD) AS __Psz
		/// THROW NotImplementedException{}
	RETURN __PSZ._NULL_PSZ 

	/// <summary>
	/// Convert a null-terminated string to a __Symbol and add it to the atom table.
	/// </summary>
	/// <param name="p"></param>
	/// <returns>
	/// </returns>
	FUNCTION SysAddAtom(p AS __Psz) AS __Symbol
		/// THROW NotImplementedException{}
	RETURN NULL_SYMBOL   

	/// <summary>
	/// Convert a null-terminated string to an uppercase __Symbol and add it to the atom table.
	/// </summary>
	/// <param name="p"></param>
	/// <returns>
	/// </returns>
	FUNCTION SysAddAtomUpperA(p AS __Psz) AS __Symbol
		/// THROW NotImplementedException{}
	RETURN NULL_SYMBOL   

	/// <summary>
	/// </summary>
	/// <param name="p"></param>
	/// <returns>
	/// </returns>
	FUNCTION SysAddAtomUpperBuff(p AS __Psz) AS __Symbol
		/// THROW NotImplementedException{}
	RETURN NULL_SYMBOL   

	/// <summary>
	/// Determine whether a __Symbol is in the atom table.
	/// </summary>
	/// <param name="p"></param>
	/// <returns>
	/// </returns>
	FUNCTION SysFindAtom(p AS __Psz) AS __Symbol
		/// THROW NotImplementedException{}
	RETURN NULL_SYMBOL   

	/// <summary>
	/// Convert a __Symbol to a null-terminated string.
	/// </summary>
	/// <param name="s"></param>
	/// <returns>
	/// </returns>
	FUNCTION SysGetAtomName(s AS __Symbol) AS __Psz
		/// THROW NotImplementedException{}
	RETURN __PSZ._NULL_PSZ 

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION TimePsz() AS __Psz
		/// THROW NotImplementedException{}
	RETURN __PSZ._NULL_PSZ 

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION TimePsz24() AS __Psz
		/// THROW NotImplementedException{}
	RETURN __PSZ._NULL_PSZ 

	#endregion
