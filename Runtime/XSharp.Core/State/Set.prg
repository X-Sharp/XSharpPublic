//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
// Most of these settings will Get and Set properties of the Runtime.State class
#include "GetSet.xh"

USING XSharp

/// <summary>
/// Returns a string representing the morning extension for time strings in 12-hour format.
/// </summary>
/// <returns>
/// </returns>
FUNCTION GetAMExt() AS STRING
	GETSTATE STRING Set.AmExt 

/// <summary>
/// Returns a string representing the morning extension for time strings in 12-hour format.
/// </summary>
/// <returns>
/// </returns>
FUNCTION SetAMExt() AS STRING
	RETURN GetAmExt()

/// <summary>
/// Set the morning extension for time strings in 12-hour format.
/// </summary>
/// <param name="cExt"></param>
/// <returns>
/// </returns>
FUNCTION SetAMExt(cExt AS STRING) AS STRING
	SETSTATE STRING Set.AmExt cExt


/// <summary>
/// Returns the setting that determines whether time strings are in 12-hour or 24-hour format.
/// </summary>
/// <returns>
/// </returns>
FUNCTION GetAmPm() AS LOGIC
	GETSTATE LOGIC Set.AmPm

/// <summary>
/// Return and optionally change the setting that determines whether time strings are in 12-hour or 24-hour format.
/// </summary>
/// <param name="lSet"></param>
/// <returns>
/// </returns>
FUNCTION SetAmPm(lSet AS LOGIC) AS LOGIC
	SETSTATE LOGIC Set.AmPm lSet

/// <summary>
/// Return and the setting that determines whether database files are created using ANSI or OEM format and whether certain text file operations convert between the two character sets.
/// </summary>
/// <returns>
/// </returns>
function SetAnsi() as logic
	local lOld := RuntimeState.Ansi as LOGIC
	return lOld

/// <summary>
/// Return and optionally change the setting that determines whether database files are created using ANSI or OEM format and whether certain text file operations convert between the two character sets.
/// </summary>
/// <param name="lSet"></param>
/// <returns>
/// </returns>
function SetAnsi(lSet as logic) as logic
	local lOld := RuntimeState.Ansi as LOGIC
	RuntimeState.Ansi := lSet
	return lOld

/// <summary>
/// Sets the locale that the runtime uses for comparing strings when running in Windows collation mode (SetCollation(#Windows)).
/// </summary>
/// <param name="dwLocaleId"></param>
/// <returns>
/// </returns>
FUNCTION SetAppLocaleID(dwLocaleId AS DWORD) AS DWORD
	/// THROW NotImplementedException{}
	RETURN 0   

/// <summary>
/// Return and optionally change the setting that determines whether a beep is sounded by the error system when an error occurs.
/// </summary>
/// <param name="lSet"></param>
/// <returns>
/// </returns>
FUNCTION SetBeep(lSet AS OBJECT) AS LOGIC
	/// THROW NotImplementedException{}
	RETURN FALSE   

/// <summary>
/// Return and optionally change the setting that determines whether to include or omit century digits in the date format.
/// </summary>
/// <param name="lSet"></param>
/// <returns>
/// </returns>
FUNCTION SetCentury(lSet AS LOGIC) AS LOGIC
	SETSTATE LOGIC Set.Century lSet

/// <summary>
/// </summary>
/// <param name="pFunc"></param>
/// <returns>
/// </returns>
FUNCTION SetClipCompFunc(pFunc AS OBJECT) AS IntPtr
	/// THROW NotImplementedException{}
	RETURN IntPtr.Zero

/// <summary>
/// Return and optionally change the setting that determines the type of central processor you have.
/// </summary>
/// <param name="nCpu"></param>
/// <returns>
/// </returns>
FUNCTION SetCpu(nCpu AS OBJECT) AS DWORD
	/// THROW NotImplementedException{}
	RETURN 0   

/// <summary>
/// Return and optionally change the setting that determines the <%APP%> date format by selecting from a list of constants with corresponding date formats.
/// </summary>
/// <param name="dwCountry"></param>
/// <returns>
/// </returns>
FUNCTION SetDateCountry(dwCountry AS OBJECT) AS DWORD
	/// THROW NotImplementedException{}
	RETURN 0   


/// <summary>
/// Return the current __VODate format.
/// </summary>
/// <returns>
/// </returns>
FUNCTION GetDateFormat() AS STRING
	GETSTATE STRING Set.DateFormat

/// <summary>
/// Change the setting that determines the <%APP%> date format.
/// </summary>
/// <param name="cDateFormat"></param>
/// <returns>
/// </returns>
FUNCTION SetDateFormat(cDateFormat AS STRING) AS STRING
	SETSTATE STRING Set.DateFormat cDateFormat
/// <summary>
/// Return and optionally change the setting that determines the number of decimal places used to display numbers.
/// </summary>
/// <param name="nDec"></param>
/// <returns>
/// </returns>
FUNCTION SetDecimal(nDec AS DWORD) AS DWORD
	SETSTATE DWORD Set.Decimals nDec

/// <summary>
/// Return and optionally change the setting that determines the decimal separation character to be used in numeric-to-string conversion functions.
/// </summary>
/// <param name="wSep"></param>
/// <returns>
/// </returns>

FUNCTION SetDecimalSep() AS WORD
	GETSTATE WORD Set.DecimalSep 

FUNCTION SetDecimalSep(wSep AS WORD) AS WORD
	SETSTATE WORD Set.DecimalSep wSep

/// <summary>
/// Change the setting that determines the <%APP%> default drive and directory.
/// </summary>
/// <param name="cDefault"></param>
/// <returns>
/// </returns>
FUNCTION SetDefault() AS STRING
	GETSTATE STRING Set.Default 

FUNCTION SetDefault(cDefault AS STRING) AS STRING
	SETSTATE STRING Set.Default cDefault

/// <summary>
/// Change the setting that determines whether to ignore or include records that are marked for deletion.
/// </summary>
/// <param name="lSet"></param>
/// <returns>
/// </returns>
FUNCTION SetDeleted() AS LOGIC
	GETSTATE LOGIC Set.Deleted 
FUNCTION SetDeleted(lSet AS LOGIC) AS LOGIC
	SETSTATE LOGIC Set.Deleted lSet

/// <summary>
/// Return and optionally change the setting that determines the number of digits that will be shown to the left of the decimal point when a number is displayed.
/// </summary>
/// <param name="nDig"></param>
/// <returns>
/// </returns>
FUNCTION SetDigit() AS DWORD
	GETSTATE DWORD Set.DIGITS 

FUNCTION SetDigit(nDig AS DWORD) AS DWORD
	SETSTATE DWORD Set.DIGITS nDIg

/// <summary>
/// Return and optionally change the setting that fixes the number of digits used to display numeric output.
/// </summary>
/// <param name="f"></param>
/// <returns>
/// </returns>
FUNCTION SetDigitFixed() AS LOGIC
	GETSTATE LOGIC Set.DigitFixed 

FUNCTION SetDigitFixed(f AS LOGIC) AS LOGIC
	SETSTATE LOGIC Set.DigitFixed f

/// <summary>
/// Update or replace the contents of a DOS environment variable.
/// </summary>
/// <param name="cVar"></param>
/// <param name="cValue"></param>
/// <param name="lAppend"></param>
/// <returns>
/// </returns>
FUNCTION SetEnv(cVar AS STRING,cValue AS STRING,lAppend AS LOGIC) AS LOGIC
	/// THROW NotImplementedException{}
	RETURN FALSE   

/// <summary>
/// Return and optionally change the setting that determines how dates without century digits are interpreted.
/// </summary>
/// <param name="wYear"></param>
/// <returns>
/// </returns>
FUNCTION SetEpoch() AS DWORD
	GETSTATE DWORD Set.Epoch 

FUNCTION SetEpoch(wYear AS DWORD) AS DWORD
	SETSTATE DWORD Set.Epoch wYear

/// <summary>
/// Return and optionally change the setting that determines whether error information is written to the error log file by the default runtime error handler.
/// </summary>
/// <param name="lSet"></param>
/// <returns>
/// </returns>
FUNCTION SetErrorLog(lSet AS OBJECT) AS LOGIC
	/// THROW NotImplementedException{}
	RETURN FALSE   

/// <summary>
/// Toggles an exact match for character string comparisons.
/// </summary>
/// <param name="fExact"></param>
/// <returns>
/// </returns>
FUNCTION SetExact() AS LOGIC
	GETSTATE LOGIC Set.Exact 

FUNCTION SetExact(fExact AS LOGIC) AS LOGIC
	SETSTATE LOGIC Set.Exact fExact

/// <summary>
/// Return and optionally change the setting that determines whether to open database files in exclusive or shared mode.
/// </summary>
/// <param name="lSet"></param>
/// <returns>
/// </returns>
FUNCTION SetExclusive() AS LOGIC
	GETSTATE LOGIC Set.Exclusive 

FUNCTION SetExclusive(lSet AS LOGIC) AS LOGIC
	SETSTATE LOGIC Set.Exclusive lSet

/// <summary>
/// Return and optionally change the setting that determines whether assignments are made to fields or to memory variables.
/// </summary>
/// <param name="lSet"></param>
/// <returns>
/// </returns>
FUNCTION SetFieldStore() AS LOGIC
	GETSTATE LOGIC Set.FieldStore
FUNCTION SetFieldStore(lSet AS LOGIC) AS LOGIC
	SETSTATE LOGIC Set.FieldStore lSet

/// <summary>
/// Return and optionally change the setting that fixes the number of decimal digits used to display numbers.
/// </summary>
/// <param name="fFixed"></param>
/// <returns>
/// </returns>
FUNCTION SetFixed() AS LOGIC
	GETSTATE LOGIC Set.Fixed 

FUNCTION SetFixed(fFixed AS LOGIC) AS LOGIC
	SETSTATE LOGIC Set.Fixed fFixed

/// <summary>
/// Return and optionally change the setting that determines the internal operational characteristics of the underlying floating-point system.
/// </summary>
/// <param name="nFPU"></param>
/// <returns>
/// </returns>
FUNCTION SetMath() AS DWORD
	RETURN 0
FUNCTION SetMath(nFPU AS DWORD) AS DWORD
	RETURN 0   



/// <summary>
/// Change the setting that determines the <%APP%> search path for opening files.
/// </summary>
/// <param name="cPath"></param>
/// <returns>
/// </returns>
FUNCTION SetPath(cPath AS STRING) AS STRING
	SETSTATE STRING Set.Path cPath


/// <summary>
/// Returns a string representing the evening extension for time strings in 12-hour format.
/// </summary>
/// <returns>
/// </returns>
FUNCTION GetPMExt() AS STRING
	GETSTATE STRING Set.PmExt

/// <summary>
/// Returns a string representing the evening extension for time strings in 12-hour format.
/// </summary>
/// <returns>
/// </returns>
FUNCTION SetPMExt() AS STRING
	RETURN GetPmExt()

/// <summary>
/// Set the evening extension for time strings in 12-hour format.
/// </summary>
/// <param name="cExt"></param>
/// <returns>
/// </returns>
FUNCTION SetPMExt(cExt AS STRING) AS STRING
	SETSTATE STRING Set.PmExt cExt

/// <summary>
/// Save a numeric value to the Registry.
/// </summary>
/// <param name="cSubKey"></param>
/// <param name="cKeyName"></param>
/// <param name="nKeyVal"></param>
/// <returns>
/// </returns>
FUNCTION SetRTRegInt(cSubKey AS STRING,cKeyName AS STRING,nKeyVal AS DWORD) AS LOGIC
	/// THROW NotImplementedException{}
	RETURN FALSE   

/// <summary>
/// Save a string value to the Registry.
/// </summary>
/// <param name="cSubKey"></param>
/// <param name="cKeyName"></param>
/// <param name="cKeyVal"></param>
/// <returns>
/// </returns>
FUNCTION SetRTRegString(cSubKey AS STRING,cKeyName AS STRING,cKeyVal AS STRING) AS LOGIC
	/// THROW NotImplementedException{}
	RETURN FALSE   

/// <summary>
/// Return and optionally change the setting that displays numbers in scientific notation.
/// </summary>
/// <param name="f"></param>
/// <returns>
/// </returns>
FUNCTION SetScience() AS LOGIC
	GETSTATE LOGIC Set.Science 

FUNCTION SetScience(lSet AS LOGIC) AS LOGIC
	SETSTATE LOGIC Set.Science lSet

/// <summary>
/// Return and optionally change the setting that determines whether a seek operation will find a close match when no exact match is found.
/// </summary>
/// <param name="lSet"></param>
/// <returns>
/// </returns>
FUNCTION SetSoftSeek() AS LOGIC
	GETSTATE LOGIC Set.SoftSeek 

FUNCTION SetSoftSeek(lSet AS LOGIC) AS LOGIC
	SETSTATE LOGIC Set.SoftSeek lSet

/// <summary>
/// Return and optionally change the setting that determines the thousands separation character to be used in numeric-to-string conversion functions.
/// </summary>
/// <param name="wSep"></param>
/// <returns>
/// </returns>
FUNCTION SetThousandSep() AS WORD
	GETSTATE WORD Set.ThousandSep 

FUNCTION SetThousandSep(wSep AS WORD) AS WORD
	SETSTATE WORD Set.ThousandSep wSep

/// <summary>
/// Change the setting that determines the separation character to be used in time strings.
/// </summary>
/// <param name="dwChar"></param>
/// <returns>
/// </returns>
FUNCTION SetTimeSep(dwChar AS DWORD) AS DWORD
	SETSTATE DWORD Set.TimeSep dwChar

/// <summary>
/// Return and optionally change the setting that determines whether to include unique record keys in an order.
/// </summary>
/// <param name="lSet"></param>
/// <returns>
/// </returns>

FUNCTION SetUnique() AS LOGIC
	GETSTATE LOGIC Set.Unique 

FUNCTION SetUnique(lSet AS LOGIC) AS LOGIC
	SETSTATE LOGIC Set.Unique lSet

/// <summary>
/// </summary>
/// <param name="n"></param>
/// <returns>
/// </returns>
FUNCTION SetWinCompFlags(n AS OBJECT) AS LONG
	/// THROW NotImplementedException{}
	RETURN 0   

/// <summary>
/// </summary>
/// <param name="pFunc"></param>
/// <returns>
/// </returns>
FUNCTION SetWinCompFunc(pFunc AS OBJECT) AS IntPtr
	/// THROW NotImplementedException{}
	RETURN IntPtr.Zero


/// <summary>
/// </summary>
/// <param name="lSet"></param>
/// <returns>
/// </returns>
FUNCTION GetYield() AS LOGIC
	GETSTATE LOGIC Set.Yield 

FUNCTION SetYield(lSet AS LOGIC) AS LOGIC
	SETSTATE LOGIC Set.Yield lSet

