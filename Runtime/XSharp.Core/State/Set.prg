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
	GETSTATE STRING Set.AmExt 

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
/// Returns the setting that determines whether time strings are in 12-hour or 24-hour format.
/// </summary>
/// <returns>
/// </returns>
FUNCTION SetAmPm() AS LOGIC
	GETSTATE LOGIC Set.AmPm

/// <summary>
/// Change the setting that determines whether time strings are in 12-hour or 24-hour format.
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
FUNCTION SetAnsi() AS LOGIC
	RETURN RuntimeState.Ansi

/// <summary>
/// Change the setting that determines whether database files are created using ANSI or OEM format and whether certain text file operations convert between the two character sets.
/// </summary>
/// <param name="lSet"></param>
/// <returns>
/// </returns>
FUNCTION SetAnsi(lSet AS LOGIC) AS LOGIC
	LOCAL lOld := RuntimeState.Ansi AS LOGIC
	RuntimeState.Ansi := lSet
	RETURN lOld



/// <summary>
/// Return the setting that determines whether a beep is sounded by the error system when an error occurs.
/// </summary>
/// <param name="lSet"></param>
/// <returns>
/// </returns>
FUNCTION SetBeep() AS LOGIC
	GETSTATE LOGIC Set.BELL

/// <summary>
/// Change the setting that determines whether a beep is sounded by the error system when an error occurs.
/// </summary>
/// <param name="lSet"></param>
/// <returns>
/// </returns>
FUNCTION SetBeep(lSet AS LOGIC) AS LOGIC
	SETSTATE LOGIC Set.BELL lSet


/// <summary>
/// Return the setting that determines whether to include or omit century digits in the date format.
/// </summary>
/// <param name="lSet"></param>
/// <returns>
/// </returns>
FUNCTION SetCentury() AS LOGIC
	GETSTATE LOGIC Set.Century 

/// <summary>
/// Change the setting that determines whether to include or omit century digits in the date format.
/// </summary>
/// <param name="lSet"></param>
/// <returns>
/// </returns>
FUNCTION SetCentury(lSet AS LOGIC) AS LOGIC
	LOCAL lOld AS LOGIC
    lOld := XSharp.RuntimeState.GetValue<LOGIC>(Set.CENTURY)
    IF lOld != lSet
      VAR cFormat := XSharp.RuntimeState.DateFormat
      
      IF lSet
        IF ! cFormat:Contains("YYYY")
            cFormat := cFormat:Replace( "YY", "YYYY" )
        ENDIF
        ELSE
            IF cFormat:Contains("YYYY")
                cFormat := cFormat:Replace( "YYYY", "YY" )
            ENDIF
      ENDIF
      
      SetDateFormat( cFormat )
      
    ENDIF
    RETURN lOld


/// <summary>
/// Return the setting that determines the type of central processor you have.
/// </summary>
/// <param name="nCpu"></param>
/// <returns>
/// </returns>
FUNCTION SetCpu() AS DWORD
	GETSTATE DWORD Set.CPU


/// <summary>
/// Change the setting that determines the type of central processor you have.
/// </summary>
/// <param name="nCpu"></param>
/// <returns>
/// </returns>
FUNCTION SetCpu(nCpu AS DWORD) AS DWORD
	SETSTATE DWORD Set.CPU nCPU


/// <summary>
/// Return the setting that determines the X# date format by selecting from a list of constants with corresponding date formats.
/// </summary>
/// <param name="dwCountry"></param>
/// <returns>
/// </returns>
FUNCTION SetDateCountry() AS DWORD
	GETSTATE DWORD Set.DATECOUNTRY

/// <summary>
/// Return and optionally change the setting that determines the X# date format by selecting from a list of constants with corresponding date formats.
/// </summary>
/// <param name="dwCountry"></param>
/// <returns>
/// </returns>
FUNCTION SetDateCountry(dwCountry AS DWORD) AS DWORD
//	setstate DWORD Set.DATECOUNTRY dwCountry
	LOCAL dwOld := RuntimeState.DateCountry AS DWORD
	RuntimeState.DateCountry := dwCountry
	RETURN dwOld



/// <summary>
/// Return the current Date format.
/// </summary>
/// <returns>
/// </returns>
FUNCTION GetDateFormat() AS STRING
	GETSTATE STRING Set.DateFormat

/// <summary>
/// Change the setting that determines the X# date format.
/// </summary>
/// <param name="cDateFormat"></param>
/// <returns>
/// </returns>
FUNCTION SetDateFormat(cDateFormat AS STRING) AS STRING
	LOCAL cOld AS STRING
	// Changing Dateformat also changes DateCountry and Century
	cOld := RuntimeState.DateFormat
	RuntimeState.DateFormat := cDateFormat
	RETURN cOld

/// <summary>
/// Return the setting that determines the number of decimal places used to display numbers.
/// </summary>
/// <returns>
/// </returns>
FUNCTION SetDecimal() AS DWORD
	GETSTATE DWORD Set.Decimals 

/// <summary>
/// Return and change the setting that determines the number of decimal places used to display numbers.
/// </summary>
/// <param name="nDec"></param>
/// <returns>
/// </returns>
FUNCTION SetDecimal(nDec AS DWORD) AS DWORD
	SETSTATE DWORD Set.Decimals nDec

/// <summary>
/// Return the setting that determines the decimal separation character to be used in numeric-to-string conversion functions.
/// </summary>
/// <returns>
/// </returns>

FUNCTION SetDecimalSep() AS DWORD
	GETSTATE DWORD Set.DecimalSep 

/// <summary>
/// Return and change the setting that determines the decimal separation character to be used in numeric-to-string conversion functions.
/// </summary>
/// <param name="wSep"></param>
/// <returns>
/// </returns>
FUNCTION SetDecimalSep(wSep AS DWORD) AS DWORD
	SETSTATE DWORD Set.DecimalSep wSep

/// <summary>
/// Return the setting that determines the default drive and directory.
/// </summary>
/// <param name="cDefault"></param>
/// <returns>
/// </returns>
FUNCTION SetDefault() AS STRING
	GETSTATE STRING Set.Default 

/// <summary>
/// Change the setting that determines the default drive and directory.
/// </summary>
/// <param name="cDefault"></param>
/// <returns>
/// </returns>
FUNCTION SetDefault(cDefault AS STRING) AS STRING
	__SetPathArray(NULL)
	SETSTATE STRING Set.Default cDefault
	

/// <summary>
/// Return the setting that determines whether to ignore or include records that are marked for deletion.
/// </summary>
/// <returns>
/// </returns>
FUNCTION SetDeleted() AS LOGIC
	GETSTATE LOGIC Set.Deleted 

/// <summary>
/// Change the setting that determines whether to ignore or include records that are marked for deletion.
/// </summary>
/// <param name="lSet"></param>
/// <returns>
/// </returns>
FUNCTION SetDeleted(lSet AS LOGIC) AS LOGIC
	SETSTATE LOGIC Set.Deleted lSet

/// <summary>
/// Return the setting that determines the number of digits that will be shown to the left of the decimal point when a number is displayed.
/// </summary>
/// <returns>
/// </returns>
FUNCTION SetDigit() AS DWORD
	GETSTATE DWORD Set.DIGITS 

/// <summary>
/// Change the setting that determines the number of digits that will be shown to the left of the decimal point when a number is displayed.
/// </summary>
/// <param name="nDig"></param>
/// <returns>
/// </returns>
FUNCTION SetDigit(nDig AS DWORD) AS DWORD
	SETSTATE DWORD Set.DIGITS nDIg

/// <summary>
/// Return the setting that fixes the number of digits used to display numeric output.
/// </summary>
/// <param name="f"></param>
/// <returns>
/// </returns>
FUNCTION SetDigitFixed() AS LOGIC
	GETSTATE LOGIC Set.DigitFixed 

/// <summary>
/// Change the setting that fixes the number of digits used to display numeric output.
/// </summary>
/// <param name="f"></param>
/// <returns>
/// </returns>
FUNCTION SetDigitFixed(f AS LOGIC) AS LOGIC
	SETSTATE LOGIC Set.DigitFixed f


/// <summary>
/// Return the setting that determines how dates without century digits are interpreted.
/// </summary>
/// <returns>
/// </returns>
FUNCTION SetEpoch() AS DWORD
	GETSTATE DWORD Set.Epoch 

/// <summary>
/// Change the setting that determines how dates without century digits are interpreted.
/// </summary>
/// <param name="wYear"></param>
/// <returns>
/// </returns>
FUNCTION SetEpoch(wEpoch AS DWORD) AS DWORD
	LOCAL wYear AS DWORD
	LOCAL wCent AS DWORD
	wYear := wEpoch % 100
	wCent := (( wEpoch / 100) +1) * 100
	XSharp.RuntimeState.SetValue<DWORD> (Set.EpochYear, wYear)
	XSharp.RuntimeState.SetValue<DWORD> (Set.EpochCent, wCent)
	SETSTATE DWORD Set.Epoch	 wEpoch

/// <summary>
/// Return the setting that determines whether error information is written to the error log file by the default runtime error handler.
/// </summary>
/// <returns>
/// </returns>
FUNCTION SetErrorLog() AS LOGIC
	GETSTATE LOGIC Set.ERRRORLOG 

/// <summary>
/// Change the setting that determines whether error information is written to the error log file by the default runtime error handler.
/// </summary>
/// <param name="lSet"></param>
/// <returns>
/// </returns>
FUNCTION SetErrorLog(lSet AS LOGIC) AS LOGIC
	SETSTATE LOGIC Set.ERRRORLOG lSet

/// <summary>
/// Return the setting for an exact match for character string comparisons.
/// </summary>
/// <returns>
/// </returns>
FUNCTION SetExact() AS LOGIC
	GETSTATE LOGIC Set.Exact 

/// <summary>
/// Change the setting for an exact match for character string comparisons.
/// </summary>
/// <param name="fExact"></param>
/// <returns>
/// </returns>
FUNCTION SetExact(fExact AS LOGIC) AS LOGIC
	SETSTATE LOGIC Set.Exact fExact

/// <summary>
/// Return the setting that determines whether to open database files in exclusive or shared mode.
/// </summary>
/// <returns>
/// </returns>
FUNCTION SetExclusive() AS LOGIC
	GETSTATE LOGIC Set.Exclusive 

/// <summary>
/// Change the setting that determines whether to open database files in exclusive or shared mode.
/// </summary>
/// <param name="lSet"></param>
/// <returns>
/// </returns>
FUNCTION SetExclusive(lSet AS LOGIC) AS LOGIC
	SETSTATE LOGIC Set.Exclusive lSet

/// <summary>
/// Return the setting that determines whether assignments are made to fields or to memory variables.
/// </summary>
/// <returns>
/// </returns>
FUNCTION SetFieldStore() AS LOGIC
	GETSTATE LOGIC Set.FieldStore

/// <summary>
/// Change the setting that determines whether assignments are made to fields or to memory variables.
/// </summary>
/// <param name="lSet"></param>
/// <returns>
/// </returns>
FUNCTION SetFieldStore(lSet AS LOGIC) AS LOGIC
	SETSTATE LOGIC Set.FieldStore lSet

/// <summary>
/// Return the setting that fixes the number of decimal digits used to display numbers.
/// </summary>
/// <returns>
/// </returns>
FUNCTION SetFixed() AS LOGIC
	GETSTATE LOGIC Set.Fixed 

/// <summary>
/// Change the setting that fixes the number of decimal digits used to display numbers.
/// </summary>
/// <param name="fFixed"></param>
/// <returns>
/// </returns>
FUNCTION SetFixed(fFixed AS LOGIC) AS LOGIC
	SETSTATE LOGIC Set.Fixed fFixed

/// <summary>
/// Return the setting that determines the internal operational characteristics of the underlying floating-point system.
/// </summary>
/// <returns>
/// </returns>
FUNCTION SetMath() AS DWORD
	GETSTATE DWORD Set.MATH

/// <summary>
/// Change the setting that determines the internal operational characteristics of the underlying floating-point system.
/// </summary>
/// <param name="nFPU"></param>
/// <returns>
/// </returns>
FUNCTION SetMath(nFPU AS DWORD) AS DWORD
	SETSTATE DWORD Set.MATH nFPU

/// <summary>
/// Activate a new DLL for nation-dependent operations and messages.
/// </summary>
/// <returns>
/// </returns>
/// <remarks>
/// In Visual Objects the nation DLL is an external DLL. For X# there are no separate
/// nation DLL. The nation support code is integrated in XSharp.Core.DLL.
/// For compatibility you can still use the SetNatDLL() function to switch to another
/// nation module. <br/>
/// The available nation modules are (just like in Visual Objects):<br/> 
/// 	"BRAZIL", "CROATIA", "CZECH852", "CZECH895", "DANISH", "DUTCH", "FINNISH", 
/// 	"FRENCH", "GENERIC", "GERMAN", "GERMAN2", "HUNG852", "HUNGCWI", "ITALIAN", 
///     "NORWEGN", "POL-ISO", "POL-MAZ", "POL852", "PORT850", "PORT860", "ROMANIA", 
/// 	"RUSSIAN", "SERBIA", "SL-W-95", "SL-W-AS7", "SL-W-EE", "SLOV852", "SLOV895", 
/// 	"SPANISH", "SWEDISH", "UK"
/// <br/> You may specify a DLL name with full path. The Runtime will strip the path and extension
/// from the file name and will look for one of the names listed above.<br/> 
/// The nation DLL name is NOT case sensitive.
///</remarks>
	
FUNCTION SetNatDLL(cNewDLL AS STRING) AS LOGIC
	LOCAL cBase AS STRING
	_SetNatDLL(cnewDLL)
	cBase := System.IO.Path.GetFileNameWithoutExtension(cNewDLL)
	_SetCollation(cBase)
	RETURN String.Compare(Messages.CurrentLanguageName, cBase, TRUE) == 0

/// <exclude/>
FUNCTION _SetCollation(cBase AS STRING) AS LOGIC
	VAR rm := System.Resources.ResourceManager{ "XSharp.Collations", TYPEOF(Functions):Assembly }
	VAR obj := rm:GetObject(cBase) 
	IF obj != NULL
		VAR bytes := obj ASTYPE BYTE[]
		IF bytes != NULL
			XSharp.RuntimeState.CollationTable := bytes 
			RETURN TRUE
		ENDIF
	ENDIF
	RETURN FALSE
/// <exclude/>	
INTERNAL FUNCTION	_SetNatDLL(cNewDLL AS STRING) AS STRING
	LOCAL cBase AS STRING
	cBase := System.IO.Path.GetFileNameWithoutExtension(cNewDLL)
	Messages.SetCurrentLanguage(cBase)
	SETSTATE STRING Set.NatDLL cNewDLL

/// <summary>
/// Return the setting that determines the search path for opening files. This may be a semi colon separated list of folders.
/// </summary>
/// <returns>
/// </returns>
FUNCTION SetPath() AS STRING
	GETSTATE STRING Set.Path 


/// <summary>
/// Change the setting that determines the search path for opening files.
/// </summary>
/// <param name="cPath">New path. This may be a semi colon separated list of folders.</param>
/// <returns>
/// </returns>
FUNCTION SetPath(cPath AS STRING) AS STRING
	__SetPathArray(NULL)
	SETSTATE STRING Set.Path cPath


/// <summary>
/// Return the Path array that is used by the File() function to locate files outside of the current directory.
/// This is a combination of the SetDefault() and SetPath() variables
/// This may be null if the file function has never been called or never been called for files outside of the current
/// directory.
/// </summary>
/// <returns>
/// </returns>
FUNCTION __SetPathArray() AS STRING[]
	GETSTATE STRING[] Set.PathArray 

/// <summary>
/// Set the Path array that is used by the File() function to locate files outside of the current directory.
/// This is a combination of the SetDefault() and SetPath() variables
/// This array gets cleared when SetPath() or SetDefault() is called and is initialized the first time File() or a related function
//  is called after the path has been changed.
/// </summary>
/// <param name="aPath"></param>
/// <returns>
/// </returns>
FUNCTION __SetPathArray(aPath AS STRING[]) AS STRING[]
	SETSTATE STRING[] Set.PathArray aPath

/// <summary>
/// Returns a string representing the evening extension for time strings in 12-hour format.
/// </summary>
/// <returns>
/// </returns>
FUNCTION SetPMExt() AS STRING
	GETSTATE STRING Set.PmExt

/// <summary>
/// Set the evening extension for time strings in 12-hour format.
/// </summary>
/// <param name="cExt"></param>
/// <returns>
/// </returns>
FUNCTION SetPMExt(cExt AS STRING) AS STRING
	SETSTATE STRING Set.PmExt cExt

/// <summary>
/// Return the setting that displays numbers in scientific notation.
/// </summary>
/// <returns>
/// </returns>
FUNCTION SetScience() AS LOGIC
	GETSTATE LOGIC Set.Science 

/// <summary>
/// Change the setting that displays numbers in scientific notation.
/// </summary>
/// <param name="f"></param>
/// <returns>
/// </returns>
FUNCTION SetScience(lSet AS LOGIC) AS LOGIC
	SETSTATE LOGIC Set.Science lSet

/// <summary>
/// Return the setting that determines whether a seek operation will find a close match when no exact match is found.
/// </summary>
/// <returns>
/// </returns>
FUNCTION SetSoftSeek() AS LOGIC
	GETSTATE LOGIC Set.SoftSeek 

/// <summary>
/// Change the setting that determines whether a seek operation will find a close match when no exact match is found.
/// </summary>
/// <param name="lSet"></param>
/// <returns>
/// </returns>
FUNCTION SetSoftSeek(lSet AS LOGIC) AS LOGIC
	SETSTATE LOGIC Set.SoftSeek lSet

/// <summary>
/// Return the setting that determines the thousands separation character to be used in numeric-to-string conversion functions.
/// </summary>
/// <returns>
/// </returns>
FUNCTION SetThousandSep() AS DWORD
	GETSTATE DWORD Set.ThousandSep 

/// <summary>
/// Change the setting that determines the thousands separation character to be used in numeric-to-string conversion functions.
/// </summary>
/// <param name="wSep"></param>
/// <returns>
/// </returns>
FUNCTION SetThousandSep(wSep AS DWORD) AS DWORD
	SETSTATE DWORD Set.ThousandSep wSep

/// <summary>
/// Return the setting that determines the separation character to be used in time strings.
/// </summary>
/// <returns>
/// </returns>
FUNCTION SetTimeSep() AS DWORD
	GETSTATE DWORD Set.TimeSep 

/// <summary>
/// Change the setting that determines the separation character to be used in time strings.
/// </summary>
/// <param name="dwChar"></param>
/// <returns>
/// </returns>
FUNCTION SetTimeSep(dwChar AS DWORD) AS DWORD
	SETSTATE DWORD Set.TimeSep dwChar

/// <summary>
/// Return the setting that determines whether to include unique record keys in an order.
/// </summary>
/// <returns>
/// </returns>

FUNCTION SetUnique() AS LOGIC
	GETSTATE LOGIC Set.Unique 

/// <summary>
/// Change the setting that determines whether to include unique record keys in an order.
/// </summary>
/// <param name="lSet"></param>
/// <returns>
/// </returns>

FUNCTION SetUnique(lSet AS LOGIC) AS LOGIC
	SETSTATE LOGIC Set.Unique lSet

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION SetYield() AS LOGIC
	GETSTATE LOGIC Set.Yield 

/// <summary>
/// </summary>
/// <param name="lSet"></param>
/// <returns>
/// </returns>
FUNCTION SetYield(lSet AS LOGIC) AS LOGIC
	SETSTATE LOGIC Set.Yield lSet


/// <summary>
/// Retrieve and set the X# return code.
/// </summary>
/// <param name="dw"></param>
/// <returns>
/// </returns>
FUNCTION ErrorLevel(dw AS DWORD) AS DWORD 
	SETSTATE DWORD Set.ErrorLevel dw


/// <summary>
/// Retrieve the X# return code.
/// </summary>
/// <returns>
/// </returns>
FUNCTION ErrorLevel() AS DWORD 
	GETSTATE DWORD Set.ErrorLevel


/// <summary>
/// Return the setting that determines the international mode for the application
/// </summary>
/// <returns>The current setting, either "Windows" (the default) or  "Clipper"
/// </returns>
FUNCTION SetInternational() AS STRING
	RETURN RuntimeState.International:ToString():ToUpper()

/// <summary>
/// Return and change the setting that determines the international mode for the application
/// </summary>
// <param name="cMode">The collation mode to use. The available modes are "Windows" (the default) and "Clipper". "Unicode" and "Ordinal" can be used as synonym for "Windows".</param>
/// <returns>The current setting, either "Windows" (the default) or  "Clipper"
/// </returns>
/// <remarks>
/// SetInternational() allows XSharp apps to operate in different international modes.  
/// The "CLIPPER" mode is provided for compatibility with CA-Clipper applications and uses an 
/// internationalization routine defined in the nation module.
/// The "Windows" mode uses international services provided by Windows.
/// When you set this mode several settings will be changed
/// <list type="table">
/// <listheader>
/// <term>Setting</term> <description>Initial value in CLIPPER mode</description>
/// </listheader>
///	<item><term>SetAmExt</term> <description>Empty String</description></item>  
/// <item><term>SetPmExt</term> <description>Empty String</description></item>  
/// <item><term>SetAmPm</term> <description>FALSE (24 hour format)</description></item>   
/// <item><term>SetCentury</term> <description>FALSE</description></item>   
/// <item><term>SetDateCountry</term> <description>American (1)</description></item> 
/// <item><term>SetDateFormat</term> <description>mm/dd/yy</description></item> 
/// <item><term>SetDecimal</term> <description>2</description></item>   
/// <item><term>SetDecimalSep</term> <description>Period (.)</description></item>
/// <item><term>SetThousandSep</term> <description>Comma (,)</description></item>  
/// <item><term>SetTimeSep</term> <description>Colon(:)</description></item> 
/// </list>
/// </remarks>
FUNCTION SetInternational(cMode AS STRING) AS STRING
	LOCAL cOld AS STRING
	cOld := RuntimeState.International:ToString():ToUpper()
	SWITCH cMode:ToUpper()
	CASE "CLIPPER"
		RuntimeState.GetInstance()._SetInternationalClipper()
	CASE "WINDOWS"
	CASE "UNICODE"
	CASE "ORDINAL"
		RuntimeState.GetInstance()._SetInternationalWindows()
	OTHERWISE
		THROW Error.ArgumentError(__ENTITY__, NAMEOF(cMode), "Unsupported international mode: "+ cMode)
	END SWITCH
	RETURN cOld


 
/// <summary>
/// Return and change the setting that determines the internal collation routine used for string comparisons when running in the VO or Vulcan dialect.
/// The Core dialect always compares according to the Unicode rules.
/// The available modes are "Windows" (the default),  "Clipper", "Unicode" and "Ordinal".
/// </summary>
/// <returns>The current setting for SetCollation().</returns>
FUNCTION SetCollation() AS STRING 
	RETURN RuntimeState.CollationMode:ToString():ToUpper()

/// <summary>
/// Return and change the setting that determines the internal collation routine used for string comparisons when running in the VO or Vulcan dialect.
/// </summary>
/// <remarks>
/// The Core dialect always compares according to the Unicode rules.
/// The other dialects use the rules defined with SetCollation for string comparison. There are 4 possible values:
/// <list type="table">
/// <item>
/// <term>Windows (the default)</term>
/// <description>This uses the normal windows Ansi comparison mechanism that Visual Objects uses as well.
/// That means that the Unicode strings from .Net are converted to Ansi first and then compared with the Ansi comparison rules.
/// 
/// In most cases characters that are not available in the Ansi codepage are translated to a question mark '?' and are therefore all seen as equal. <br/>
/// If you want true unicode comparison you need the Unicode value for SetCollation.
/// </description>
/// </item>
/// <item>
/// <term>Clipper</term>
/// <description>This uses string comparison tables that are the same as the character comparison tables in the Visual Objects
/// nation modules. Each character from the unicode string is converted to a character from the OEM codepage first using the
/// DosCodePage from the runtime state. the resulting OEM characters are then looked up in the 256 character weight tables that
/// are part of the runtime dll. You can switch to a different table by using SetNatDLL().
/// </description>
/// </item>
/// <item>
/// <term>Unicode</term>
/// <description>This uses the normal Unicode String.Compare routines for string comparisons. 
/// </description>
/// </item>
/// <item>
/// <term>Ordinal</term>
/// <description>This uses the normal Ordinal String.Compare routines from DotNet. This is the fastest.
/// </description>
/// </item>
/// </list>
/// </remarks>
/// <returns>The current setting, either "Windows" (the default),  "Clipper", "Unicode" or "Ordinal" </returns>
/// <param name="cCollation">The collation mode to use. The available modes are "Windows" (the default),  "Clipper", "Unicode" and "Ordinal". </param>
FUNCTION SetCollation(cCollation AS STRING)  AS STRING
	LOCAL cOld AS STRING
	cOld := RuntimeState.CollationMode:ToString():ToUpper()
	SWITCH cCollation:ToUpper()
	CASE "CLIPPER"
		RuntimeState.CollationMode := CollationMode.Clipper
	CASE "WINDOWS"
		RuntimeState.CollationMode := CollationMode.Windows
	CASE "UNICODE"
		RuntimeState.CollationMode := CollationMode.Unicode
	CASE "ORDINAL"
		RuntimeState.CollationMode := CollationMode.Ordinal
	OTHERWISE
		THROW Error.ArgumentError(__ENTITY__, NAMEOF(cCollation), "Unsupported collation mode: "+cCollation)
	END SWITCH
	RETURN cOld
