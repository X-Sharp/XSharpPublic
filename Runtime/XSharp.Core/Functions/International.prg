//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System.Globalization
USING System.Threading
USING System.Security.Permissions

/// <summary>
/// Remove leading and trailing spaces — including double-byte spaces — from a string.
/// </summary>
/// <param name="cMBString">The string to trim</param>
/// <returns>A trimmed string, with leading and trailing spaces removed.</returns>
/// <remarks>This function is the same as AllTrim() since .Net has unicode strings</remarks>
FUNCTION MBAllTrim(cMBString AS STRING) AS STRING
	RETURN Alltrim(cMBString)

/// <summary>
/// Return the position of the first occurrence of a substring within a string — both the substring and the string can contain double-byte characters.
/// </summary>
/// <param name="cSearch">The substring to search for.</param>
/// <param name="cTarget">The string in which to search.  (To start at a specific offset, use MBAt3().)</param>
/// <returns>The position of the first occurrence of cMBSearch within cMBTarget.  If cMBSearch is not found, MBAt() returns zero.</returns>
/// <remarks>This function is the same as At() since .Net has unicode strings</remarks>
FUNCTION MBAt(cSearch AS STRING,cTarget AS STRING) AS DWORD
	RETURN At( cSearch, cTarget ) 

/// <summary>
/// Return the position of the first occurrence of a substring within a string — both the substring and the string can contain double-byte characters.
/// </summary>
/// <param name="cSearch">The substring for which to search.</param>
/// <param name="cTarget">The string in which to search.  (To start at a specific offset, use MBAt3().)</param>
/// <returns>The position of the first occurrence of cMBSearch within cMBTarget.  If cMBSearch is not found, MBAt2() returns zero.</returns>
/// <remarks>This function is the same as At2 since .Net has unicode strings</remarks>
FUNCTION MBAt2(cSearch AS STRING,cTarget AS STRING) AS DWORD
	RETURN At2( cSearch, cTarget )

/// <summary>
/// Return the position of the first occurrence of a substring within a string, starting at a specific position — both the substring and the string can contain double-byte characters.
/// </summary>
/// <param name="cSearch">The substring to search for.</param>
/// <param name="cTarget">The string in which to search.</param>
/// <param name="wOffset">The position in the string at which to start searching.  A value of zero corresponds to the first character.</param>
/// <returns>The position of the first occurrence of cMBSearch within cMBTarget.  If cMBSearch is not found, MBAt3() returns zero.</returns>
/// <remarks>This function is the same as At3 since .Net has unicode strings</remarks>
FUNCTION MBAt3(cSearch AS STRING,cTarget AS STRING,wOffset AS DWORD) AS DWORD
	RETURN At3( cSearch, cTarget, wOffset ) 

/// <summary>
/// Return the position of the first occurrence of a sub string within a string, without regard for case — both the substring and the string can contain double-byte characters.
/// </summary>
/// <param name="cSearch">The substring to search for.</param>
/// <param name="cTarget">The string in which to search.</param>
/// <returns>The position of the first occurrence of cMBSearch within cMBTarget.  If cMBSearch is not found, MBAtC() returns zero.</returns>
/// <remarks>This function is the same as AtC since .Net has unicode strings</remarks>
FUNCTION MBAtC(cSearch AS STRING,cTarget AS STRING) AS DWORD
	RETURN AtC(cSearch, cTarget)   

/// <summary>
/// Return the position of the first occurrence of a substring within a string, without regard for case — both the substring and the string can contain double-byte characters.
/// </summary>
/// <param name="cSearch">The substring to search for.</param>
/// <param name="cTarget">The string in which to search.</param>
/// <returns>The position of the first occurrence of cMBSearch within cMBTarget.  If cMBSearch is not found, MBAtC2() returns zero.</returns>
/// <remarks>This function is the same as AtC2 since .Net has unicode strings</remarks>
FUNCTION MBAtC2(cSearch AS STRING,cTarget AS STRING) AS DWORD
	RETURN AtC2(cSearch, cTarget)   

/// <summary>
/// Return the line number of the first occurrence of a substring within a multiple line string — both the substring and the string can contain double-byte characters.
/// </summary>
/// <param name="cSearch">The substring to search for.</param>
/// <param name="cTarget">The string in which to search.</param>
/// <returns>The position of the first occurrence of cMBSearch within cMBTarget.  If cMBSearch is not found, MBAtLine() returns zero.</returns>
/// <remarks>This function is the same as AtLine() since .Net has unicode strings</remarks>
FUNCTION MBAtLine(cSearch AS STRING,cTarget AS STRING) AS LONGINT
	RETURN (LONG) AtLine(cSearch, cTarget)   

/// <summary>
/// Return the line number of the first occurrence of a substring within a multiple line string — both the substring and the string can contain double-byte characters.
/// </summary>
/// <param name="cSearch">The substring to search for.</param>
/// <param name="cTarget">The string in which to search.</param>
/// <returns>The position of the first occurrence of cMBSearch within cMBTarget.  If cMBSearch is not found, MBAtLine() returns zero.</returns>
/// <remarks>This function is the same as AtLine2() since .Net has unicode strings</remarks>
FUNCTION MBAtLine2(cSearch AS STRING,cTarget AS STRING) AS DWORD
	RETURN AtLine2(cSearch, cTarget)   

/// <summary>
/// Return a substring beginning with the first character of a string containing double-byte characters.
/// </summary>
/// <param name="c">The string from which to extract characters. </param>
/// <param name="wLen">The number of characters to extract.</param>
/// <returns>The leftmost wCount characters of cMBString — each character counts as one character.  If wCount is zero, MBLeft() returns a NULL_STRING.  If wCount is larger than the length of the string, MBLeft() returns the entire string.</returns>
/// <remarks>This function is the same as Left() since .Net has unicode strings</remarks>
FUNCTION MBLEFT(c AS STRING,wLen AS DWORD) AS STRING
	RETURN Left(c, wLen)   

/// <summary>
/// Return the length of a string containing double-byte characters.
/// </summary>
/// <param name="cMBString">The string to measure.  Each character counts as one .</param>
/// <returns>The length of cMBString in which each character counts as one.</returns>
/// <remarks>This function is the same as SLen() since .Net has unicode strings</remarks>
FUNCTION MBLEN(cMBString AS STRING) AS DWORD
	RETURN SLen(cMBString)

/// <summary>
/// Remove leading spaces — including double-byte spaces — from a string.
/// </summary>
/// <param name="cMBString"></param>
/// <returns>cMBString with the leading spaces removed.  If cMBString is a NULL_STRING or all spaces, MBLTrim() returns a NULL_STRING.</returns>
/// <remarks>This function is the same as LTrim() since .Net has unicode strings</remarks>
FUNCTION MBLTrim(cMBString AS STRING) AS STRING
	RETURN LTrim(cMBString)

/// <summary>
/// Return the position of the last occurrence of a substring within a string — both the substring and the string can contain double-byte characters.
/// </summary>
/// <param name="cSearch">The substring for which to search.</param>
/// <param name="cTarget">The string in which to search.  (To start at a specific offset, use MBRAt3().)</param>
/// <returns>The position of cMBSearch within cMBTarget.  If cMBSearch is not found, MBRAt() returns zero.</returns>
/// <remarks>This function is the same as Rat() since .Net has unicode strings</remarks>
FUNCTION MBRat(cSearch AS STRING,cTarget AS STRING) AS LONGINT
	RETURN (LONG) Rat(cSearch, cTarget)

/// <summary>
/// Return the position of the last occurrence of a substring within a string — both the substring and the string can contain double-byte characters.
/// </summary>
/// <param name="cSearch">The substring for which to search.</param>
/// <param name="cTarget">The string in which to search.  (To start at a specific offset, use MBRAt3().)</param>
/// <returns>The position of cMBSearch within cMBTarget.  If cMBSearch is not found, MBRAt() returns zero.</returns>
/// <remarks>This function is the same as Rat2() since .Net has unicode strings</remarks>
FUNCTION MBRat2(cSearch AS STRING,cTarget AS STRING) AS DWORD
	RETURN Rat2(cSearch, cTarget)

/// <summary>
/// Return the position of the last occurrence of a substring within a string, starting at a specific position — both the substring and the string can contain double-byte characters.
/// </summary>
/// <param name="cSearch">The substring for which to search.</param>
/// <param name="cTarget">The string in which to search.  (To start at a specific offset, use MBRAt3().)</param>
/// <param name="wOffset">The position in the string at which to start searching.  A value of zero specifies the first character.</param>
/// <returns>The position of cMBSearch within cMBTarget.  If cMBSearch is not found, MBRAt() returns zero.</returns>
/// <remarks>This function is the same as Rat3() since .Net has unicode strings</remarks>
FUNCTION MBRat3(cSearch AS STRING,cTarget AS STRING,wOffset AS DWORD) AS DWORD
	RETURN Rat3(cSearch, cTarget, wOffSet)

/// <summary>
/// Return a substring beginning with the last character of a string containing double-byte characters.
/// </summary>
/// <param name="cMbString">The string from which to extract characters.</param>
/// <param name="wLen">The number of characters to extract. </param>
/// <returns>The rightmost wCount characters of cMBString.  If wCount is zero, MBRight() returns a NULL_STRING.  If wCount is larger than the length of the string, MBRight() returns cMBString.</returns>
/// <remarks>This function is the same as Right() since .Net has unicode strings</remarks>
FUNCTION MBRight(cMbString AS STRING,wLen AS DWORD) AS STRING
	RETURN Right(cMbString, wLen)

/// <summary>
/// Remove trailing spaces — including double-byte spaces — from a string.
/// </summary>
/// <param name="cMbString">The string to trim.</param>
/// <returns>cMBString with the trailing spaces removed.  If cMBString is a NULL_STRING or all spaces, MBRTrim() returns a NULL_STRING.</returns>
/// <remarks>This function is the same as RTrim() since .Net has unicode strings</remarks>
FUNCTION MBRTrim(cMbString AS STRING) AS STRING
	RETURN RTrim(cMbString)

/// <summary>
/// Return the length of a strongly typed string containing double-byte characters.
/// </summary>
/// <param name="cMbString">The string to count.</param>
/// <returns>The length of cMBString in which character counts as one.  If the string is a NULL_STRING, MBSLen() returns zero.</returns>
/// <remarks>This function is the same as Slen() since .Net has unicode strings</remarks>
FUNCTION MBSLen(cMbString AS STRING) AS DWORD
	RETURN SLen(cMbString)

/// <summary>
/// Insert a string into another string, optionally deleting a specified number of characters from the original string — both strings can contain double-byte characters.
/// </summary>
/// <param name="cMBTarget">The string into which characters are inserted and deleted. </param>
/// <param name="wStart">The position in cMBTarget where the insertion/deletion occurs.</param>
/// <param name="wDel">The number of characters to delete.</param>
/// <param name="cIns">The string to insert.</param>
/// <returns>cMBTarget with the specified characters deleted and cMBInsert inserted.</returns>
/// <remarks>This function is the same as Stuff() since .Net has unicode strings</remarks>
FUNCTION MBStuff(cMBTarget AS STRING,wStart AS DWORD,wDel AS DWORD,cIns AS STRING) AS STRING
	RETURN Stuff(cMBTarget, wStart, wDel, cIns)



/// <summary>
/// Extract a substring from a string, using strong typing and only two arguments — both the substring and the string can contain double-byte characters.
/// </summary>
/// <param name="cMbString">The string from which to extract a substring.</param>
/// <param name="wStart">The starting position relative to the leftmost character in cMBTarget. </param>
/// <returns>MBSubstr2() is a typed version of MBSubstr().</returns>
/// <remarks>This function is the same as Substr2() since .Net has unicode strings</remarks>
FUNCTION MBSubstr2(cMbString AS STRING,wStart AS DWORD) AS STRING
	RETURN Substr2(cMbString, wStart)

/// <summary>
/// Extract a substring from a string, using strong typing and three required arguments — both the substring and the string can contain double-byte characters.
/// </summary>
/// <param name="cMbString">The string from which to extract a substring.</param>
/// <param name="wStart">The starting position relative to the leftmost character in cMBTarget. </param>
/// <param name="wLen">The number of characters to extract — double-byte characters count as one character.  If wCount is greater than the number of characters from wStart to the end of cMBTarget, the extra is ignored.</param>
/// <returns>The substring.  If the substring is not present, or if you specify wStart as zero, MBSubstr3() returns a NULL_STRING.</returns>
/// <remarks>This function is the same as Substr3() since .Net has unicode strings</remarks>
FUNCTION MBSubstr3(cMbString AS STRING,wStart AS DWORD,wLen AS DWORD) AS STRING
	RETURN Substr3(cMbString, wStart, wLen)

/// <summary>
/// Remove trailing spaces — including double-byte spaces — from a string.
/// </summary>
/// <param name="cMBString">The string to trim.</param>
/// <returns>cMBString with the trailing spaces removed.  If cMBString is a NULL_STRING or all spaces, MBTrim() returns a NULL_STRING.</returns>
/// <remarks>This function is the same as Trim() since .Net has unicode strings</remarks>
FUNCTION MBTrim(cMBString AS STRING) AS STRING
	RETURN Trim(cMBString)


/// <summary>
/// Gets the locale ID that the runtime uses for comparing strings when running in Windows collation mode (SetCollation(#Windows)).
/// </summary>
/// <returns>Locale ID: System.Globalization.CultureInfo.CurrentCulture:LCID
/// </returns>
FUNCTION GetAppLocaleID() AS DWORD
	LOCAL oCI AS CultureInfo
	oCI := CultureInfo.CurrentCulture
	RETURN (DWORD) oCI:LCID


/// <summary>
/// Sets the locale that the runtime uses for comparing strings when running in Windows collation mode (SetCollation(#Windows)).
/// </summary>
/// <param name="dwLocaleId"></param>
/// <returns>Previous Locale ID: System.Globalization.CultureInfo.CurrentCulture:LCID
/// </returns>
[SecurityPermissionAttribute(SecurityAction.Demand, ControlThread := TRUE)];
FUNCTION SetAppLocaleID(dwLocaleId AS DWORD) AS DWORD
	VAR ci := CultureInfo{ (INT) dwLocaleID}
	Thread.CurrentThread:CurrentCulture	  := ci
	Thread.CurrentThread:CurrentUICulture := ci
	RETURN dwLocaleID


/// <summary>
/// </summary>
/// <param name="pFunc"></param>
/// <returns>
/// </returns>
FUNCTION SetClipCompFunc(pFunc AS OBJECT) AS IntPtr
	THROW NotImplementedException{}
	RETURN IntPtr.Zero



/// <summary>
/// </summary>
/// <param name="n"></param>
/// <returns>
/// </returns>
FUNCTION SetWinCompFlags(n AS OBJECT) AS LONG
	THROW NotImplementedException{}
	RETURN 0   

/// <summary>
/// </summary>
/// <param name="pFunc"></param>
/// <returns>
/// </returns>
FUNCTION SetWinCompFunc(pFunc AS OBJECT) AS IntPtr
	THROW NotImplementedException{}
	RETURN IntPtr.Zero

[Obsolete];
FUNCTION NationInit(dwInst AS DWORD) AS INT
	RETURN 0   

[Obsolete];
FUNCTION NationExit() AS INT
	RETURN 0   


/// <summary>
/// Identify a character set by its nation driver.
/// </summary>
/// <returns>
/// </returns>
FUNCTION NVersion() AS STRING
	THROW NotImplementedException{}
	RETURN String.Empty 


FUNCTION MAKELANGID( p AS WORD, s AS WORD ) AS WORD
	RETURN (WORD) ( ( s << 10 ) | p )

FUNCTION MAKELCID( lgid AS WORD, srtid AS WORD ) AS DWORD
	RETURN (DWORD) ( ( ( (DWORD)(srtid) ) << 16) | ( (INT)(DWORD) lgid ) )


FUNCTION IsBiDi() AS LOGIC
   RETURN System.Windows.Forms.SystemInformation.MidEastEnabled   


_DLL FUNCTION String2W( sz AS STRING ) AS IntPtr PASCAL:OLEAUT32.SysAllocString


[Obsolete];
FUNCTION GetNatDllHandle() AS IntPtr STRICT
	RETURN IntPtr.Zero
