//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using Vulcan

begin namespace XSharp.Runtime
	#region functions
	/// <summary>
	/// Set the morning extension for time strings in 12-hour format.
	/// </summary>
	/// <param name="cExt"></param>
	/// <returns>
	/// </returns>
	FUNCTION SetAMExt(cExt AS STRING) AS VOID
		/// THROW NotImplementedException{}
	RETURN

	/// <summary>
	/// Return and optionally change the setting that determines whether time strings are in 12-hour or 24-hour format.
	/// </summary>
	/// <param name="lSet"></param>
	/// <returns>
	/// </returns>
	FUNCTION SetAmPm(lSet AS __Usual) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Return and optionally change the setting that determines whether database files are created using ANSI or OEM format and whether certain text file operations convert between the two character sets.
	/// </summary>
	/// <param name="lSet"></param>
	/// <returns>
	/// </returns>
	FUNCTION SetAnsi(lSet AS __Usual) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

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
	FUNCTION SetBeep(lSet AS __Usual) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Return and optionally change the setting that determines whether to include or omit century digits in the __VODate format.
	/// </summary>
	/// <param name="lSet"></param>
	/// <returns>
	/// </returns>
	FUNCTION SetCentury(lSet AS __Usual) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="pFunc"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION SetClipCompFunc(pFunc AS __Usual) AS PTR
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero

	/// <summary>
	/// Return and optionally change the setting that determines the type of central processor you have.
	/// </summary>
	/// <param name="nCpu"></param>
	/// <returns>
	/// </returns>
	FUNCTION SetCpu(nCpu AS __Usual) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return and optionally change the setting that determines the <%APP%> __VODate format by selecting from a list of constants with corresponding __VODate formats.
	/// </summary>
	/// <param name="dwCountry"></param>
	/// <returns>
	/// </returns>
	FUNCTION SetDateCountry(dwCountry AS __Usual) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Change the setting that determines the <%APP%> __VODate format.
	/// </summary>
	/// <param name="cDateFormat"></param>
	/// <returns>
	/// </returns>
	FUNCTION SetDateFormat(cDateFormat AS STRING) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Return and optionally change the setting that determines the number of decimal places used to display numbers.
	/// </summary>
	/// <param name="nDec"></param>
	/// <returns>
	/// </returns>
	FUNCTION SetDecimal(nDec AS __Usual) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return and optionally change the setting that determines the decimal separation character to be used in numeric-to-string conversion functions.
	/// </summary>
	/// <param name="wSep"></param>
	/// <returns>
	/// </returns>
	FUNCTION SetDecimalSep(wSep AS __Usual) AS WORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Change the setting that determines the <%APP%> default drive and directory.
	/// </summary>
	/// <param name="cDefault"></param>
	/// <returns>
	/// </returns>
	FUNCTION SetDefault(cDefault AS STRING) AS VOID
		/// THROW NotImplementedException{}
	RETURN 

	/// <summary>
	/// Change the setting that determines the location of the error log file.
	/// </summary>
	/// <param name="pszNew"></param>
	/// <returns>
	/// </returns>
	FUNCTION SetDefaultDir(pszNew AS __Psz) AS VOID
		/// THROW NotImplementedException{}
	RETURN 

	/// <summary>
	/// Return and optionally change the setting that determines whether to ignore or include records that are marked for deletion.
	/// </summary>
	/// <param name="lSet"></param>
	/// <returns>
	/// </returns>
	FUNCTION SetDeleted(lSet AS __Usual) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Return and optionally change the setting that determines the number of digits that will be shown to the left of the decimal point when a number is displayed.
	/// </summary>
	/// <param name="nDig"></param>
	/// <returns>
	/// </returns>
	FUNCTION SetDigit(nDig AS __Usual) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return and optionally change the setting that fixes the number of digits used to display numeric output.
	/// </summary>
	/// <param name="f"></param>
	/// <returns>
	/// </returns>
	FUNCTION SetDigitFixed(f AS __Usual) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

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
	FUNCTION SetEpoch(wYear AS __Usual) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return and optionally change the setting that determines whether error information is written to the error log file by the default runtime error handler.
	/// </summary>
	/// <param name="lSet"></param>
	/// <returns>
	/// </returns>
	FUNCTION SetErrorLog(lSet AS __Usual) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Toggles an exact match for character string comparisons.
	/// </summary>
	/// <param name="fExact"></param>
	/// <returns>
	/// </returns>
	FUNCTION SetExact(fExact AS __Usual) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Return and optionally change the setting that determines whether to open database files in exclusive or shared mode.
	/// </summary>
	/// <param name="lSet"></param>
	/// <returns>
	/// </returns>
	FUNCTION SetExclusive(lSet AS __Usual) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Return and optionally change the setting that determines whether assignments are made to fields or to memory variables.
	/// </summary>
	/// <param name="lSet"></param>
	/// <returns>
	/// </returns>
	FUNCTION SetFieldStore(lSet AS __Usual) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Return and optionally change the setting that fixes the number of decimal digits used to display numbers.
	/// </summary>
	/// <param name="fFixed"></param>
	/// <returns>
	/// </returns>
	FUNCTION SetFixed(fFixed AS __Usual) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Return and optionally change the setting that determines the point at which 2 __VOFloating point numbers would be considered equal even though they are different.
	/// </summary>
	/// <param name="fDelta"></param>
	/// <returns>
	/// </returns>
	FUNCTION Set__VOFloatDelta(fDelta AS __Usual) AS __VOFloat
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Save the stack environment.
	/// </summary>
	/// <param name="strucMark"></param>
	/// <returns>
	/// </returns>
	FUNCTION SetJmp(strucMark AS _JMP_BUF) AS INT
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="dwBytes"></param>
	/// <returns>
	/// </returns>
	FUNCTION SetKidStackSize(dwBytes AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return and optionally change the setting that determines the internal operational characteristics of the underlying __VOFloating-point system.
	/// </summary>
	/// <param name="nFPU"></param>
	/// <returns>
	/// </returns>
	FUNCTION SetMath(nFPU AS __Usual) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Set the maximum amount of dynamic memory for the main thread.
	/// </summary>
	/// <param name="dwBytes"></param>
	/// <returns>
	/// </returns>
	FUNCTION SetMaxDynSize(dwBytes AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="dwCount"></param>
	/// <returns>
	/// </returns>
	FUNCTION SetMaxRegisteredAxitMethods(dwCount AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Set the maximum number of KID's that can be registered.
	/// </summary>
	/// <param name="dwCount"></param>
	/// <returns>
	/// </returns>
	FUNCTION SetMaxRegisteredKids(dwCount AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Set the maximum amount of dynamic memory for the new thread.
	/// </summary>
	/// <param name="dwBytes"></param>
	/// <returns>
	/// </returns>
	FUNCTION SetMaxThreadDynSize(dwBytes AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Change the setting that determines the <%APP%> search path for opening files.
	/// </summary>
	/// <param name="cPath"></param>
	/// <returns>
	/// </returns>
	FUNCTION SetPath(cPath AS STRING) AS VOID
		/// THROW NotImplementedException{}
	RETURN

	/// <summary>
	/// Set the evening extension for time strings in 12-hour format.
	/// </summary>
	/// <param name="cExt"></param>
	/// <returns>
	/// </returns>
	FUNCTION SetPMExt(cExt AS STRING) AS VOID
		/// THROW NotImplementedException{}
	RETURN 

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
	FUNCTION SetScience(f AS __Usual) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Return and optionally change the setting that determines whether a seek operation will find a close match when no exact match is found.
	/// </summary>
	/// <param name="lSet"></param>
	/// <returns>
	/// </returns>
	FUNCTION SetSoftSeek(lSet AS __Usual) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Return and optionally change the setting that determines the thousands separation character to be used in numeric-to-string conversion functions.
	/// </summary>
	/// <param name="wSep"></param>
	/// <returns>
	/// </returns>
	FUNCTION SetThousandSep(wSep AS __Usual) AS WORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Change the setting that determines the separation character to be used in time strings.
	/// </summary>
	/// <param name="dwChar"></param>
	/// <returns>
	/// </returns>
	FUNCTION SetTimeSep(dwChar AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Return and optionally change the setting that determines whether to include unique record keys in an order.
	/// </summary>
	/// <param name="lSet"></param>
	/// <returns>
	/// </returns>
	FUNCTION SetUnique(lSet AS __Usual) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="n"></param>
	/// <returns>
	/// </returns>
	FUNCTION SetWinCompFlags(n AS __Usual) AS LONG
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="pFunc"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION SetWinCompFunc(pFunc AS __Usual) AS PTR
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero

	/// <summary>
	/// Set the value of WipeDynSpace.
	/// </summary>
	/// <param name="lWipe"></param>
	/// <returns>
	/// </returns>
	FUNCTION SetWipeDynSpace(lWipe AS LOGIC) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="lSet"></param>
	/// <returns>
	/// </returns>
	FUNCTION SetYield(lSet AS __Usual) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	#endregion
end namespace