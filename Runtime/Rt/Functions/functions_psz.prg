//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
begin namespace XSharp.IO
	#region functions
	/// <summary>
	/// Remove spaces from a file name specified as a PSZ, changing the contents of the original file name as well as the returned file name.
	/// </summary>
	/// <param name="pszFileName"></param>
	/// <returns>
	/// </returns>
	FUNCTION AdjustFNamePSZ(pszFileName AS PSZ) AS PSZ
		/// THROW NotImplementedException{}
	RETURN NULL   

	/// <summary>
	/// Convert a specified number of ANSI characters in a source buffer to a buffer of corresponding OEM characters.
	/// </summary>
	/// <param name="pszDest"></param>
	/// <param name="pszSource"></param>
	/// <param name="dwCount"></param>
	/// <returns>
	/// </returns>
	FUNCTION Ansi2OemBuff(pszDest AS PSZ,pszSource AS PSZ,dwCount AS DWORD) AS PSZ
		/// THROW NotImplementedException{}
	RETURN NULL   

	/// <summary>
	/// Convert a value to a static PSZ.
	/// </summary>
	/// <param name="u"></param>
	/// <returns>
	/// </returns>
	FUNCTION AsPsz(u AS USUAL) AS PSZ
		/// THROW NotImplementedException{}
	RETURN NULL   

	/// <summary>
	/// Convert a string containing an 8-bit logical into a logical value.
	/// </summary>
	/// <param name="pszC"></param>
	/// <returns>
	/// </returns>
	FUNCTION Bin2Logic(pszC AS PSZ) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Check a strongly typed string for NULL_STRING and return a valid (non-zero) PSZ.
	/// </summary>
	/// <param name="cSource"></param>
	/// <returns>
	/// </returns>
	FUNCTION Cast2Psz(cSource AS STRING) AS PSZ
		/// THROW NotImplementedException{}
	RETURN NULL   

	/// <summary>
	/// Change the current Windows directory.
	/// </summary>
	/// <param name="pszDir"></param>
	/// <returns>
	/// </returns>
	FUNCTION DirChange(pszDir AS PSZ) AS INT
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Create a directory.
	/// </summary>
	/// <param name="pszDir"></param>
	/// <returns>
	/// </returns>
	FUNCTION DirMake(pszDir AS PSZ) AS INT
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Remove a directory.
	/// </summary>
	/// <param name="pszDir"></param>
	/// <returns>
	/// </returns>
	FUNCTION DirRemove(pszDir AS PSZ) AS INT
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Change the current disk drive.
	/// </summary>
	/// <param name="pszDisk"></param>
	/// <returns>
	/// </returns>
	FUNCTION DiskChange(pszDisk AS PSZ) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="PSZValue"></param>
	/// <returns>
	/// </returns>
	FUNCTION EmptyPSZ(PSZValue AS PSZ) AS LOGIC
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
	FUNCTION ErrorMessageBox(pszText AS PSZ,pszCapt AS PSZ,dwB1 AS DWORD,dwB2 AS DWORD,dwB3 AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return the number of files that match a given file specification and attribute.
	/// </summary>
	/// <param name="pszFile"></param>
	/// <param name="nAttr"></param>
	/// <returns>
	/// </returns>
	FUNCTION FFCount(pszFile AS PSZ,nAttr AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Find the first file that matches a given file specification or attribute.
	/// </summary>
	/// <param name="pszFile"></param>
	/// <param name="nAttr"></param>
	/// <returns>
	/// </returns>
	FUNCTION FFirst(pszFile AS PSZ,nAttr AS DWORD) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Determine if the leftmost character in a string is alphanumeric.
	/// </summary>
	/// <param name="pszC"></param>
	/// <returns>
	/// </returns>
	FUNCTION IsAlNum(pszC AS PSZ) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Determine if the leftmost character in a string is alphabetic.
	/// </summary>
	/// <param name="pszSource"></param>
	/// <returns>
	/// </returns>
	FUNCTION IsAlpha(pszSource AS PSZ) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="pszSource"></param>
	/// <returns>
	/// </returns>
	FUNCTION IsAlphaNum(pszSource AS PSZ) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Determine if the leftmost character in a string is a binary digit (0 or 1).
	/// </summary>
	/// <param name="pszC"></param>
	/// <returns>
	/// </returns>
	FUNCTION IsBDigit(pszC AS PSZ) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Determine if the leftmost character in a string is a digit (that is, a numeric digit between 0 and 9).
	/// </summary>
	/// <param name="pszC"></param>
	/// <returns>
	/// </returns>
	FUNCTION IsDigit(pszC AS PSZ) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Determine if the leftmost character in a string is a lowercase letter.
	/// </summary>
	/// <param name="pszSource"></param>
	/// <returns>
	/// </returns>
	FUNCTION IsLower(pszSource AS PSZ) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Determine if the leftmost character in a string is a blank (that is, Chr(9) through Chr(13) or Chr(32)).
	/// </summary>
	/// <param name="pszC"></param>
	/// <returns>
	/// </returns>
	FUNCTION IsSpace(pszC AS PSZ) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Determine if the leftmost character in a string is uppercase.
	/// </summary>
	/// <param name="pszSource"></param>
	/// <returns>
	/// </returns>
	FUNCTION IsUpper(pszSource AS PSZ) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Determine if the leftmost character in a string is a hex character (that is, digits from 1 through 9 and letters from A through F).
	/// </summary>
	/// <param name="pszC"></param>
	/// <returns>
	/// </returns>
	FUNCTION IsXDigit(pszC AS PSZ) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="pszVal"></param>
	/// <param name="ptrAny"></param>
	/// <param name="dwLen"></param>
	/// <returns>
	/// </returns>
	FUNCTION ItemCopyC(pszVal AS PSZ,ptrAny AS PTR,dwLen AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="pszVal"></param>
	/// <returns>
	/// </returns>
	FUNCTION ItemFreeC(pszVal AS PSZ) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="ptrAny"></param>
	/// <returns>
	/// </returns>
	FUNCTION ItemGetC(ptrAny AS PTR) AS PSZ
		/// THROW NotImplementedException{}
	RETURN NULL   

	/// <summary>
	/// </summary>
	/// <param name="ptrAny"></param>
	/// <param name="pszDate"></param>
	/// <returns>
	/// </returns>
	FUNCTION ItemGetDS(ptrAny AS PTR,pszDate AS PSZ) AS PSZ
		/// THROW NotImplementedException{}
	RETURN NULL   

	/// <summary>
	/// </summary>
	/// <param name="ptrAny"></param>
	/// <param name="dwLen"></param>
	/// <param name="dwDec"></param>
	/// <returns>
	/// </returns>
	FUNCTION ItemGetNS(ptrAny AS PTR,dwLen AS DWORD,dwDec AS DWORD) AS PSZ
		/// THROW NotImplementedException{}
	RETURN NULL   

	/// <summary>
	/// </summary>
	/// <param name="ptrAny"></param>
	/// <param name="pszVal"></param>
	/// <returns>
	/// </returns>
	FUNCTION ItemPutC(ptrAny AS PTR,pszVal AS PSZ) AS PTR
		/// THROW NotImplementedException{}
	RETURN NULL   

	/// <summary>
	/// </summary>
	/// <param name="ptrAny"></param>
	/// <param name="pszVal"></param>
	/// <param name="uilen"></param>
	/// <returns>
	/// </returns>
	FUNCTION ItemPutCL(ptrAny AS PTR,pszVal AS PSZ,uilen AS DWORD) AS PTR
		/// THROW NotImplementedException{}
	RETURN NULL   

	/// <summary>
	/// </summary>
	/// <param name="ptrAny"></param>
	/// <param name="pszDate"></param>
	/// <returns>
	/// </returns>
	FUNCTION ItemPutDS(ptrAny AS PTR,pszDate AS PSZ) AS PTR
		/// THROW NotImplementedException{}
	RETURN NULL   

	/// <summary>
	/// </summary>
	/// <param name="ptrAny"></param>
	/// <param name="pszVal"></param>
	/// <param name="dwLen"></param>
	/// <param name="dwDec"></param>
	/// <returns>
	/// </returns>
	FUNCTION ItemPutNS(ptrAny AS PTR,pszVal AS PSZ,dwLen AS DWORD,dwDec AS DWORD) AS PTR
		/// THROW NotImplementedException{}
	RETURN NULL   

	/// <summary>
	/// </summary>
	/// <param name="pObj"></param>
	/// <returns>
	/// </returns>
	FUNCTION ObjAsPsz(pObj AS PTR) AS PSZ
		/// THROW NotImplementedException{}
	RETURN NULL   

	/// <summary>
	/// Convert a specified number of OEM characters in a source buffer to a buffer of corresponding, if any, ANSI characters.
	/// </summary>
	/// <param name="pszDest"></param>
	/// <param name="pszSource"></param>
	/// <param name="dwCount"></param>
	/// <returns>
	/// </returns>
	FUNCTION Oem2AnsiBuff(pszDest AS PSZ,pszSource AS PSZ,dwCount AS DWORD) AS PSZ
		/// THROW NotImplementedException{}
	RETURN NULL   

	/// <summary>
	/// Return the name of an activated entity.
	/// </summary>
	/// <param name="dwActivation"></param>
	/// <returns>
	/// </returns>
	FUNCTION ProcName(dwActivation AS USUAL) AS PSZ
		/// THROW NotImplementedException{}
	RETURN NULL   

	/// <summary>
	/// Convert a null-terminated string to a strongly typed string.
	/// </summary>
	/// <param name="pszSource"></param>
	/// <returns>
	/// </returns>
	FUNCTION Psz2String(pszSource AS PSZ) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Copy a buffer pointed to by a PSZ to a newly allocated block of memory and return a new pointer to that memory.
	/// </summary>
	/// <param name="ptrSource"></param>
	/// <returns>
	/// </returns>
	FUNCTION PszAlloc(ptrSource AS PTR) AS PSZ
		/// THROW NotImplementedException{}
	RETURN NULL   

	/// <summary>
	/// Return the length of a PSZ.
	/// </summary>
	/// <param name="pszX"></param>
	/// <returns>
	/// </returns>
	FUNCTION PszLen(pszX AS PSZ) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="pszUnicode"></param>
	/// <returns>
	/// </returns>
	FUNCTION PszLenW(pszUnicode AS PSZ) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION ReleaseString() AS PSZ
		/// THROW NotImplementedException{}
	RETURN NULL   

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
	FUNCTION SplitPath(pzsPath AS PSZ,pszDrive AS PSZ,pszDir AS PSZ,pszName AS PSZ,pszExt AS PSZ) AS VOID
		/// THROW NotImplementedException{}
	RETURN

	/// <summary>
	/// Convert a float expression to a PSZ.
	/// </summary>
	/// <param name="flSource"></param>
	/// <param name="dwLen"></param>
	/// <param name="dwDec"></param>
	/// <returns>
	/// </returns>
	FUNCTION StrFloat(flSource AS FLOAT,dwLen AS DWORD,dwDec AS DWORD) AS PSZ
		/// THROW NotImplementedException{}
	RETURN NULL   

	/// <summary>
	/// Convert a strongly typed string to a null-terminated string.
	/// </summary>
	/// <param name="cSource"></param>
	/// <returns>
	/// </returns>
	FUNCTION String2Psz(cSource AS STRING) AS PSZ
		/// THROW NotImplementedException{}
	RETURN NULL   

	/// <summary>
	/// Copy a string to a newly allocated block of memory and return a PSZ to the memory.
	/// </summary>
	/// <param name="cSource"></param>
	/// <returns>
	/// </returns>
	FUNCTION StringAlloc(cSource AS STRING) AS PSZ
		/// THROW NotImplementedException{}
	RETURN NULL   

	/// <summary>
	/// Convert an integer expression to a PSZ.
	/// </summary>
	/// <param name="l"></param>
	/// <param name="dwLen"></param>
	/// <param name="dwDec"></param>
	/// <returns>
	/// </returns>
	FUNCTION StrInt(l AS LONG,dwLen AS DWORD,dwDec AS DWORD) AS PSZ
		/// THROW NotImplementedException{}
	RETURN NULL   

	/// <summary>
	/// Convert a long integer expression to a PSZ.
	/// </summary>
	/// <param name="l"></param>
	/// <param name="dwLen"></param>
	/// <param name="dwDec"></param>
	/// <returns>
	/// </returns>
	FUNCTION StrLong(l AS LONG,dwLen AS DWORD,dwDec AS DWORD) AS PSZ
		/// THROW NotImplementedException{}
	RETURN NULL   

	/// <summary>
	/// Convert a null-terminated string to a symbol and add it to the atom table.
	/// </summary>
	/// <param name="p"></param>
	/// <returns>
	/// </returns>
	FUNCTION SysAddAtom(p AS PSZ) AS SYMBOL
		/// THROW NotImplementedException{}
	RETURN NULL_SYMBOL   

	/// <summary>
	/// Convert a null-terminated string to an uppercase symbol and add it to the atom table.
	/// </summary>
	/// <param name="p"></param>
	/// <returns>
	/// </returns>
	FUNCTION SysAddAtomUpperA(p AS PSZ) AS SYMBOL
		/// THROW NotImplementedException{}
	RETURN NULL_SYMBOL   

	/// <summary>
	/// </summary>
	/// <param name="p"></param>
	/// <returns>
	/// </returns>
	FUNCTION SysAddAtomUpperBuff(p AS PSZ) AS SYMBOL
		/// THROW NotImplementedException{}
	RETURN NULL_SYMBOL   

	/// <summary>
	/// Determine whether a symbol is in the atom table.
	/// </summary>
	/// <param name="p"></param>
	/// <returns>
	/// </returns>
	FUNCTION SysFindAtom(p AS PSZ) AS SYMBOL
		/// THROW NotImplementedException{}
	RETURN NULL_SYMBOL   

	/// <summary>
	/// Convert a symbol to a null-terminated string.
	/// </summary>
	/// <param name="s"></param>
	/// <returns>
	/// </returns>
	FUNCTION SysGetAtomName(s AS SYMBOL) AS PSZ
		/// THROW NotImplementedException{}
	RETURN NULL   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION TimePsz() AS PSZ
		/// THROW NotImplementedException{}
	RETURN NULL   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION TimePsz24() AS PSZ
		/// THROW NotImplementedException{}
	RETURN NULL   

	#endregion
end namespace