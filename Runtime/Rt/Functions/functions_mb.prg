//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using Vulcan
begin namespace XSharp.Runtime
	#region functions
	/// <summary>
	/// Remove leading and trailing spaces — including double-byte spaces — from a string.
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION MBALLTRIM(c AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN String.Empty   

	/// <summary>
	/// Return the position of the first occurrence of a substring within a string — both the substring and the string can contain double-byte characters.
	/// </summary>
	/// <param name="cSearch"></param>
	/// <param name="cTarget"></param>
	/// <returns>
	/// </returns>
	FUNCTION MBAT(cSearch AS STRING,cTarget AS STRING) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return the position of the first occurrence of a substring within a string — both the substring and the string can contain double-byte characters.
	/// </summary>
	/// <param name="cSearch"></param>
	/// <param name="cTarGet"></param>
	/// <returns>
	/// </returns>
	FUNCTION MBAT2(cSearch AS STRING,cTarGet AS STRING) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return the position of the first occurrence of a substring within a string, starting at a specific position — both the substring and the string can contain double-byte characters.
	/// </summary>
	/// <param name="cSearch"></param>
	/// <param name="cTarget"></param>
	/// <param name="wOffset"></param>
	/// <returns>
	/// </returns>
	FUNCTION MBAT3(cSearch AS STRING,cTarget AS STRING,wOffset AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return the position of the first occurrence of a sub string within a string, without regard for case — both the substring and the string can contain double-byte characters.
	/// </summary>
	/// <param name="cSearch"></param>
	/// <param name="cTarget"></param>
	/// <returns>
	/// </returns>
	FUNCTION MBATC(cSearch AS STRING,cTarget AS STRING) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return the position of the first occurrence of a substring within a string, without regard for case — both the substring and the string can contain double-byte characters.
	/// </summary>
	/// <param name="cSearch"></param>
	/// <param name="cTarget"></param>
	/// <returns>
	/// </returns>
	FUNCTION MBATC2(cSearch AS STRING,cTarget AS STRING) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return the line number of the first occurrence of a substring within a multiple line string — both the substring and the string can contain double-byte characters.
	/// </summary>
	/// <param name="cSearch"></param>
	/// <param name="cTarget"></param>
	/// <returns>
	/// </returns>
	FUNCTION MBATLINE(cSearch AS STRING,cTarget AS STRING) AS LONGINT
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return the line number of the first occurrence of a substring within a multiple line string — both the substring and the string can contain double-byte characters.
	/// </summary>
	/// <param name="cSearch"></param>
	/// <param name="cTarget"></param>
	/// <returns>
	/// </returns>
	FUNCTION MBATLINE2(cSearch AS STRING,cTarget AS STRING) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return a substring beginning with the first character of a string containing double-byte characters.
	/// </summary>
	/// <param name="c"></param>
	/// <param name="wLen"></param>
	/// <returns>
	/// </returns>
	FUNCTION MBLEFT(c AS STRING,wLen AS DWORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN String.Empty   

	/// <summary>
	/// Return the length of a string containing double-byte characters or an __Array.
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION MBLEN(c AS __Usual) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Remove leading spaces — including double-byte spaces — from a string.
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION MBLTRIM(c AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN String.Empty   

	/// <summary>
	/// Return the length of a __Psz containing double-byte characters.
	/// </summary>
	/// <param name="pszC"></param>
	/// <returns>
	/// </returns>
	FUNCTION MBPSZLEN(pszC AS __Psz) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return the position of the last occurrence of a substring within a string — both the substring and the string can contain double-byte characters.
	/// </summary>
	/// <param name="cSearch"></param>
	/// <param name="cTarget"></param>
	/// <returns>
	/// </returns>
	FUNCTION MBRAT(cSearch AS STRING,cTarget AS STRING) AS LONGINT
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return the position of the last occurrence of a substring within a string — both the substring and the string can contain double-byte characters.
	/// </summary>
	/// <param name="cSearch"></param>
	/// <param name="cTarget"></param>
	/// <returns>
	/// </returns>
	FUNCTION MBRAT2(cSearch AS STRING,cTarget AS STRING) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return the position of the last occurrence of a substring within a string, starting at a specific position — both the substring and the string can contain double-byte characters.
	/// </summary>
	/// <param name="cSearch"></param>
	/// <param name="cTarget"></param>
	/// <param name="wOffset"></param>
	/// <returns>
	/// </returns>
	FUNCTION MBRAT3(cSearch AS STRING,cTarget AS STRING,wOffset AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return a substring beginning with the last character of a string containing double-byte characters.
	/// </summary>
	/// <param name="c"></param>
	/// <param name="wLen"></param>
	/// <returns>
	/// </returns>
	FUNCTION MBRIGHT(c AS STRING,wLen AS DWORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN String.Empty   

	/// <summary>
	/// Remove trailing spaces — including double-byte spaces — from a string.
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION MBRTRIM(c AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN String.Empty   

	/// <summary>
	/// Return the length of a strongly typed string containing double-byte characters.
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION MBSLEN(c AS STRING) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Insert a string into another string, optionally deleting a specified number of characters from the original string — both strings can contain double-byte characters.
	/// </summary>
	/// <param name="cSearch"></param>
	/// <param name="wStart"></param>
	/// <param name="wDel"></param>
	/// <param name="cIns"></param>
	/// <returns>
	/// </returns>
	FUNCTION MBSTUFF(cSearch AS STRING,wStart AS DWORD,wDel AS DWORD,cIns AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN String.Empty   

	/// <summary>
	/// Extract a substring from a string — both the substring and the string can contain double-byte characters.
	/// </summary>
	/// <param name="c"></param>
	/// <param name="iStart"></param>
	/// <param name="wLen"></param>
	/// <returns>
	/// </returns>
	FUNCTION MBSUBSTR(c AS __Usual,iStart AS __Usual,wLen AS __Usual) AS STRING
		/// THROW NotImplementedException{}
	RETURN String.Empty   

	/// <summary>
	/// Extract a substring from a string, using strong typing and only two arguments — both the substring and the string can contain double-byte characters.
	/// </summary>
	/// <param name="c"></param>
	/// <param name="wStart"></param>
	/// <returns>
	/// </returns>
	FUNCTION MBSUBSTR2(c AS STRING,wStart AS DWORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN String.Empty   

	/// <summary>
	/// Extract a substring from a string, using strong typing and three required arguments — both the substring and the string can contain double-byte characters.
	/// </summary>
	/// <param name="c"></param>
	/// <param name="wStart"></param>
	/// <param name="wLen"></param>
	/// <returns>
	/// </returns>
	FUNCTION MBSUBSTR3(c AS STRING,wStart AS DWORD,wLen AS DWORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN String.Empty   

	/// <summary>
	/// Remove trailing spaces — including double-byte spaces — from a string.
	/// </summary>
	/// <param name="c"></param>
	/// <returns>
	/// </returns>
	FUNCTION MBTRIM(c AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN String.Empty   

	#endregion
end namespace