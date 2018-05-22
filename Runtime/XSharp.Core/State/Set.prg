//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
// Most of these settings will Get and Set properties of the Runtime.State class
#include "GetSet.xh"

using XSharp

/// <summary>
/// Returns a string representing the morning extension for time strings in 12-hour format.
/// </summary>
/// <returns>
/// </returns>
function GetAMExt() as string
	getstate string Set.AmExt 

/// <summary>
/// Returns a string representing the morning extension for time strings in 12-hour format.
/// </summary>
/// <returns>
/// </returns>
function SetAMExt() as string
	getstate string Set.AmExt 

/// <summary>
/// Set the morning extension for time strings in 12-hour format.
/// </summary>
/// <param name="cExt"></param>
/// <returns>
/// </returns>
function SetAMExt(cExt as string) as string
	setstate string Set.AmExt cExt


/// <summary>
/// Returns the setting that determines whether time strings are in 12-hour or 24-hour format.
/// </summary>
/// <returns>
/// </returns>
function GetAmPm() as logic
	getstate logic Set.AmPm

/// <summary>
/// Returns the setting that determines whether time strings are in 12-hour or 24-hour format.
/// </summary>
/// <returns>
/// </returns>
function SetAmPm() as logic
	getstate logic Set.AmPm

/// <summary>
/// Change the setting that determines whether time strings are in 12-hour or 24-hour format.
/// </summary>
/// <param name="lSet"></param>
/// <returns>
/// </returns>
function SetAmPm(lSet as logic) as logic
	setstate logic Set.AmPm lSet

/// <summary>
/// Return and the setting that determines whether database files are created using ANSI or OEM format and whether certain text file operations convert between the two character sets.
/// </summary>
/// <returns>
/// </returns>
function SetAnsi() as logic
	return RuntimeState.Ansi

/// <summary>
/// Change the setting that determines whether database files are created using ANSI or OEM format and whether certain text file operations convert between the two character sets.
/// </summary>
/// <param name="lSet"></param>
/// <returns>
/// </returns>
function SetAnsi(lSet as logic) as logic
	local lOld := RuntimeState.Ansi as logic
	RuntimeState.Ansi := lSet
	return lOld



/// <summary>
/// Return the setting that determines whether a beep is sounded by the error system when an error occurs.
/// </summary>
/// <param name="lSet"></param>
/// <returns>
/// </returns>
function SetBeep() as logic
	getstate logic Set.BELL

/// <summary>
/// Change the setting that determines whether a beep is sounded by the error system when an error occurs.
/// </summary>
/// <param name="lSet"></param>
/// <returns>
/// </returns>
function SetBeep(lSet as logic) as logic
	setstate logic Set.BELL lSet


/// <summary>
/// Return the setting that determines whether to include or omit century digits in the date format.
/// </summary>
/// <param name="lSet"></param>
/// <returns>
/// </returns>
function SetCentury() as logic
	getstate logic Set.Century 

/// <summary>
/// Change the setting that determines whether to include or omit century digits in the date format.
/// </summary>
/// <param name="lSet"></param>
/// <returns>
/// </returns>
function SetCentury(lSet as logic) as logic
	setstate logic Set.Century lSet


/// <summary>
/// Return the setting that determines the type of central processor you have.
/// </summary>
/// <param name="nCpu"></param>
/// <returns>
/// </returns>
function SetCpu() as dword
	getstate dword Set.CPU


/// <summary>
/// Change the setting that determines the type of central processor you have.
/// </summary>
/// <param name="nCpu"></param>
/// <returns>
/// </returns>
function SetCpu(nCpu as dword) as dword
	setstate dword Set.CPU nCPU


/// <summary>
/// Return the setting that determines the X# date format by selecting from a list of constants with corresponding date formats.
/// </summary>
/// <param name="dwCountry"></param>
/// <returns>
/// </returns>
function SetDateCountry() as long
	getstate long Set.DATECOUNTRY

/// <summary>
/// Return and optionally change the setting that determines the X# date format by selecting from a list of constants with corresponding date formats.
/// </summary>
/// <param name="dwCountry"></param>
/// <returns>
/// </returns>
function SetDateCountry(dwCountry as long) as long
	/// THROW NotImplementedException{}
	setstate long Set.DATECOUNTRY dwCountry



/// <summary>
/// Return the current Date format.
/// </summary>
/// <returns>
/// </returns>
function GetDateFormat() as string
	getstate string Set.DateFormat

/// <summary>
/// Change the setting that determines the X# date format.
/// </summary>
/// <param name="cDateFormat"></param>
/// <returns>
/// </returns>
function SetDateFormat(cDateFormat as string) as string
	local cOld as string
	// Changing Dateformat also changes DateCountry and Century
	cOld := RuntimeState.DateFormat
	RuntimeState.DateFormat := cDateFormat
	return cOld

/// <summary>
/// Return the setting that determines the number of decimal places used to display numbers.
/// </summary>
/// <returns>
/// </returns>
function SetDecimal() as dword
	getstate dword Set.Decimals 

/// <summary>
/// Return and change the setting that determines the number of decimal places used to display numbers.
/// </summary>
/// <param name="nDec"></param>
/// <returns>
/// </returns>
function SetDecimal(nDec as dword) as dword
	setstate dword Set.Decimals nDec

/// <summary>
/// Return the setting that determines the decimal separation character to be used in numeric-to-string conversion functions.
/// </summary>
/// <returns>
/// </returns>

function SetDecimalSep() as word
	getstate word Set.DecimalSep 

/// <summary>
/// Return and change the setting that determines the decimal separation character to be used in numeric-to-string conversion functions.
/// </summary>
/// <param name="wSep"></param>
/// <returns>
/// </returns>
function SetDecimalSep(wSep as word) as word
	setstate word Set.DecimalSep wSep

/// <summary>
/// Return the setting that determines the default drive and directory.
/// </summary>
/// <param name="cDefault"></param>
/// <returns>
/// </returns>
function SetDefault() as string
	getstate string Set.Default 

/// <summary>
/// Change the setting that determines the default drive and directory.
/// </summary>
/// <param name="cDefault"></param>
/// <returns>
/// </returns>
function SetDefault(cDefault as string) as string
	__SetPathArray(null)
	setstate string Set.Default cDefault
	

/// <summary>
/// Return the setting that determines whether to ignore or include records that are marked for deletion.
/// </summary>
/// <returns>
/// </returns>
function SetDeleted() as logic
	getstate logic Set.Deleted 

/// <summary>
/// Change the setting that determines whether to ignore or include records that are marked for deletion.
/// </summary>
/// <param name="lSet"></param>
/// <returns>
/// </returns>
function SetDeleted(lSet as logic) as logic
	setstate logic Set.Deleted lSet

/// <summary>
/// Return the setting that determines the number of digits that will be shown to the left of the decimal point when a number is displayed.
/// </summary>
/// <returns>
/// </returns>
function SetDigit() as dword
	getstate dword Set.DIGITS 

/// <summary>
/// Change the setting that determines the number of digits that will be shown to the left of the decimal point when a number is displayed.
/// </summary>
/// <param name="nDig"></param>
/// <returns>
/// </returns>
function SetDigit(nDig as dword) as dword
	setstate dword Set.DIGITS nDIg

/// <summary>
/// Return the setting that fixes the number of digits used to display numeric output.
/// </summary>
/// <param name="f"></param>
/// <returns>
/// </returns>
function SetDigitFixed() as logic
	getstate logic Set.DigitFixed 

/// <summary>
/// Change the setting that fixes the number of digits used to display numeric output.
/// </summary>
/// <param name="f"></param>
/// <returns>
/// </returns>
function SetDigitFixed(f as logic) as logic
	setstate logic Set.DigitFixed f


/// <summary>
/// Return the setting that determines how dates without century digits are interpreted.
/// </summary>
/// <returns>
/// </returns>
function SetEpoch() as dword
	getstate dword Set.Epoch 

/// <summary>
/// Change the setting that determines how dates without century digits are interpreted.
/// </summary>
/// <param name="wYear"></param>
/// <returns>
/// </returns>
function SetEpoch(wEpoch as dword) as dword
	local wYear as dword
	local wCent as dword
	wYear := wEpoch % 100
	wCent := (( wEpoch / 100) +1) * 100
	XSharp.RuntimeState.SetValue<dword> (Set.EpochYear, wYear)
	XSharp.RuntimeState.SetValue<DWORD> (Set.EpochCent, wCent)
	setstate dword Set.Epoch	 wEpoch

/// <summary>
/// Return the setting that determines whether error information is written to the error log file by the default runtime error handler.
/// </summary>
/// <returns>
/// </returns>
function SetErrorLog() as logic
	getstate logic Set.ERRRORLOG 

/// <summary>
/// Change the setting that determines whether error information is written to the error log file by the default runtime error handler.
/// </summary>
/// <param name="lSet"></param>
/// <returns>
/// </returns>
function SetErrorLog(lSet as logic) as logic
	setstate logic Set.ERRRORLOG lSet

/// <summary>
/// Return the setting for an exact match for character string comparisons.
/// </summary>
/// <returns>
/// </returns>
function SetExact() as logic
	getstate logic Set.Exact 

/// <summary>
/// Change the setting for an exact match for character string comparisons.
/// </summary>
/// <param name="fExact"></param>
/// <returns>
/// </returns>
function SetExact(fExact as logic) as logic
	setstate logic Set.Exact fExact

/// <summary>
/// Return the setting that determines whether to open database files in exclusive or shared mode.
/// </summary>
/// <returns>
/// </returns>
function SetExclusive() as logic
	getstate logic Set.Exclusive 

/// <summary>
/// Change the setting that determines whether to open database files in exclusive or shared mode.
/// </summary>
/// <param name="lSet"></param>
/// <returns>
/// </returns>
function SetExclusive(lSet as logic) as logic
	setstate logic Set.Exclusive lSet

/// <summary>
/// Return the setting that determines whether assignments are made to fields or to memory variables.
/// </summary>
/// <returns>
/// </returns>
function SetFieldStore() as logic
	getstate logic Set.FieldStore

/// <summary>
/// Change the setting that determines whether assignments are made to fields or to memory variables.
/// </summary>
/// <param name="lSet"></param>
/// <returns>
/// </returns>
function SetFieldStore(lSet as logic) as logic
	setstate logic Set.FieldStore lSet

/// <summary>
/// Return the setting that fixes the number of decimal digits used to display numbers.
/// </summary>
/// <returns>
/// </returns>
function SetFixed() as logic
	getstate logic Set.Fixed 

/// <summary>
/// Change the setting that fixes the number of decimal digits used to display numbers.
/// </summary>
/// <param name="fFixed"></param>
/// <returns>
/// </returns>
function SetFixed(fFixed as logic) as logic
	setstate logic Set.Fixed fFixed

/// <summary>
/// Return the setting that determines the internal operational characteristics of the underlying floating-point system.
/// </summary>
/// <returns>
/// </returns>
function SetMath() as dword
	getstate dword Set.MATH

/// <summary>
/// Change the setting that determines the internal operational characteristics of the underlying floating-point system.
/// </summary>
/// <param name="nFPU"></param>
/// <returns>
/// </returns>
function SetMath(nFPU as dword) as dword
	setstate dword Set.MATH nFPU

/// <summary>
/// Activate a new DLL for nation-dependent operations and messages.
/// </summary>
/// <returns>
/// </returns>
FUNCTION SetNatDLL(cNewDLL AS STRING) AS LOGIC
	LOCAL cBase AS STRING
	_SetNatDLL(cnewDLL)
	cBase := System.IO.Path.GetFileNameWithoutExtension(cNewDLL)
	_SetCollation(cBase)
	return String.Compare(Messages.CurrentLanguageName, cBase, true) == 0

INTERNAL FUNCTION _SetCollation(cBase AS STRING) AS LOGIC
	VAR rm := System.Resources.ResourceManager{ "XSharp.Collations", typeof(Functions):Assembly }
	VAR obj := rm:GetObject(cBase) 
	if obj != NULL
		VAR bytes := obj ASTYPE BYTE[]
		if bytes != null
			XSharp.RuntimeState.SetValue< BYTE[] >( Set.CollationTable, bytes )
			return true
		endif
	ENDIF
	return false
	
internal Function	_SetNatDLL(cNewDLL as STRING) as STRING
	LOCAL cBase AS STRING
	cBase := System.IO.Path.GetFileNameWithoutExtension(cNewDLL)
	Messages.SetCurrentLanguage(cBase)
	setstate string Set.NatDLL cNewDLL

/// <summary>
/// Return the setting that determines the search path for opening files. This may be a semi colon separated list of folders.
/// </summary>
/// <returns>
/// </returns>
function SetPath() as string
	getstate string Set.Path 


/// <summary>
/// Change the setting that determines the search path for opening files.
/// </summary>
/// <param name="cPath">New path. This may be a semi colon separated list of folders.</param>
/// <returns>
/// </returns>
function SetPath(cPath as string) as string
	__SetPathArray(null)
	setstate string Set.Path cPath


/// <summary>
/// Return the Path array that is used by the File() function to locate files outside of the current directory.
/// This is a combination of the SetDefault() and SetPath() variables
/// This may be null if the file function has never been called or never been called for files outside of the current
/// directory.
/// </summary>
/// <returns>
/// </returns>
function __SetPathArray() as string[]
	getstate string[] Set.PathArray 

/// <summary>
/// Set the Path array that is used by the File() function to locate files outside of the current directory.
/// This is a combination of the SetDefault() and SetPath() variables
/// </summary>
/// <param name="aPath"></param>
/// <returns>
/// </returns>
function __SetPathArray(aPath as string[]) as string[]
	setstate string[] Set.PathArray aPath

/// <summary>
/// Returns a string representing the evening extension for time strings in 12-hour format.
/// </summary>
/// <returns>
/// </returns>
function SetPMExt() as string
	getstate string Set.PmExt

/// <summary>
/// Set the evening extension for time strings in 12-hour format.
/// </summary>
/// <param name="cExt"></param>
/// <returns>
/// </returns>
function SetPMExt(cExt as string) as string
	setstate string Set.PmExt cExt

/// <summary>
/// Return the setting that displays numbers in scientific notation.
/// </summary>
/// <returns>
/// </returns>
function SetScience() as logic
	getstate logic Set.Science 

/// <summary>
/// Change the setting that displays numbers in scientific notation.
/// </summary>
/// <param name="f"></param>
/// <returns>
/// </returns>
function SetScience(lSet as logic) as logic
	setstate logic Set.Science lSet

/// <summary>
/// Return the setting that determines whether a seek operation will find a close match when no exact match is found.
/// </summary>
/// <returns>
/// </returns>
function SetSoftSeek() as logic
	getstate logic Set.SoftSeek 

/// <summary>
/// Change the setting that determines whether a seek operation will find a close match when no exact match is found.
/// </summary>
/// <param name="lSet"></param>
/// <returns>
/// </returns>
function SetSoftSeek(lSet as logic) as logic
	setstate logic Set.SoftSeek lSet

/// <summary>
/// Return the setting that determines the thousands separation character to be used in numeric-to-string conversion functions.
/// </summary>
/// <returns>
/// </returns>
function SetThousandSep() as word
	getstate word Set.ThousandSep 

/// <summary>
/// Change the setting that determines the thousands separation character to be used in numeric-to-string conversion functions.
/// </summary>
/// <param name="wSep"></param>
/// <returns>
/// </returns>
function SetThousandSep(wSep as word) as word
	setstate word Set.ThousandSep wSep

/// <summary>
/// Return the setting that determines the separation character to be used in time strings.
/// </summary>
/// <returns>
/// </returns>
function SetTimeSep() as word
	getstate word Set.TimeSep 

/// <summary>
/// Change the setting that determines the separation character to be used in time strings.
/// </summary>
/// <param name="dwChar"></param>
/// <returns>
/// </returns>
function SetTimeSep(dwChar as word) as word
	setstate word Set.TimeSep dwChar

/// <summary>
/// Return the setting that determines whether to include unique record keys in an order.
/// </summary>
/// <returns>
/// </returns>

function SetUnique() as logic
	getstate logic Set.Unique 

/// <summary>
/// Change the setting that determines whether to include unique record keys in an order.
/// </summary>
/// <param name="lSet"></param>
/// <returns>
/// </returns>

function SetUnique(lSet as logic) as logic
	setstate logic Set.Unique lSet

/// <summary>
/// </summary>
/// <returns>
/// </returns>
function SetYield() as logic
	getstate logic Set.Yield 

/// <summary>
/// </summary>
/// <param name="lSet"></param>
/// <returns>
/// </returns>
function SetYield(lSet as logic) as logic
	setstate logic Set.Yield lSet


/// <summary>
/// Retrieve and set the X# return code.
/// </summary>
/// <param name="dw"></param>
/// <returns>
/// </returns>
function ErrorLevel(dw as dword) as dword 
	setstate DWORD Set.ErrorLevel dw


/// <summary>
/// Retrieve the X# return code.
/// </summary>
/// <returns>
/// </returns>
function ErrorLevel() as dword 
	getstate DWORD Set.ErrorLevel