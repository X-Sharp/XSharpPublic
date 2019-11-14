//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
// Most of these settings will Get and Set properties of the Runtime.State class
#include "GetSet.xh"

USING XSharp


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setamext/*" /> 
FUNCTION SetAMExt() AS STRING
	GETSTATE STRING Set.AmExt 

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setamext/*" /> 
FUNCTION SetAMExt(cAMExt AS STRING) AS STRING
	SETSTATE STRING Set.AmExt cAMExt


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setampm/*" /> 
FUNCTION SetAmPm() AS LOGIC
	GETSTATE LOGIC Set.AmPm

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setampm/*" />
FUNCTION SetAmPm(lNewSetting AS LOGIC) AS LOGIC
	SETSTATE LOGIC Set.AmPm lNewSetting

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setansi/*" />
FUNCTION SetAnsi() AS LOGIC
	RETURN RuntimeState.Ansi

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setansi/*" />
FUNCTION SetAnsi(lNewSetting AS LOGIC) AS LOGIC
	LOCAL lOld := RuntimeState.Ansi AS LOGIC
	RuntimeState.Ansi := lNewSetting
    // Keep charset in sync with Ansi
    RuntimeState.SetValue<LONG>(Set.Charset, IIF (lNewSetting, 0, 1))
	RETURN lOld


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setbeep/*" />
FUNCTION SetBeep() AS LOGIC
	GETSTATE LOGIC Set.BELL

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setbeep/*" />
FUNCTION SetBeep(lNewSetting AS LOGIC) AS LOGIC
	SETSTATE LOGIC Set.BELL lNewSetting


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setcentury/*" />
FUNCTION SetCentury() AS LOGIC
	GETSTATE LOGIC Set.Century 

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setcentury/*" />
FUNCTION SetCentury(lNewSetting AS LOGIC) AS LOGIC
	LOCAL lOld AS LOGIC
    lOld := XSharp.RuntimeState.GetValue<LOGIC>(Set.CENTURY)
    IF lOld != lNewSetting
      VAR cFormat := XSharp.RuntimeState.DateFormat
      
      IF lNewSetting
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


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setcpu/*" />
FUNCTION SetCpu() AS DWORD
	GETSTATE DWORD Set.CPU


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setcpu/*" />
FUNCTION SetCpu(nNewSetting AS DWORD) AS DWORD
	SETSTATE DWORD Set.CPU nNewSetting


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setdatecountry/*" />
FUNCTION SetDateCountry() AS DWORD
	GETSTATE DWORD Set.DATECOUNTRY

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setdatecountry/*" />
FUNCTION SetDateCountry(dwNewSetting AS DWORD) AS DWORD
//	setstate DWORD Set.DATECOUNTRY nNewSetting
	LOCAL dwOld := RuntimeState.DateCountry AS DWORD
	RuntimeState.DateCountry := dwNewSetting
	RETURN dwOld


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/getdateformat/*" />
FUNCTION GetDateFormat() AS STRING
	GETSTATE STRING Set.DateFormat

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setdateformat/*" />
FUNCTION SetDateFormat(cNewSetting AS STRING) AS STRING
	LOCAL cOld AS STRING
	// Changing Dateformat also changes DateCountry and Century
	cOld := RuntimeState.DateFormat
	RuntimeState.DateFormat := cNewSetting
	RETURN cOld

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setdecimal/*" />
FUNCTION SetDecimal() AS DWORD
	GETSTATE DWORD Set.Decimals 

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setdecimal/*" />
FUNCTION SetDecimal(nNewSetting AS DWORD) AS DWORD
	SETSTATE DWORD Set.Decimals nNewSetting

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setdecimalsep/*" />
FUNCTION SetDecimalSep() AS DWORD
	GETSTATE DWORD Set.DecimalSep 

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setdecimalsep/*" />
FUNCTION SetDecimalSep(nNewSetting AS DWORD) AS DWORD
	LOCAL oCulture AS System.Globalization.CultureInfo
	oCulture := (System.Globalization.CultureInfo)System.Threading.Thread.CurrentThread:CurrentCulture:Clone()
	oCulture:NumberFormat:NumberDecimalSeparator := ((CHAR)nNewSetting):ToString()
	System.Threading.Thread.CurrentThread:CurrentCulture := oCulture

	SETSTATE DWORD Set.DecimalSep nNewSetting

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setdefault/*" />
FUNCTION SetDefault() AS STRING
	GETSTATE STRING Set.Default 

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setdefault/*" />
FUNCTION SetDefault(cPathSpec AS STRING) AS STRING
	SetPathArray(NULL)
	SETSTATE STRING Set.Default cPathSpec
	

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setdeleted/*" />
FUNCTION SetDeleted() AS LOGIC
	GETSTATE LOGIC Set.Deleted 

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setdeleted/*" />
FUNCTION SetDeleted(lNewSetting AS LOGIC) AS LOGIC
	SETSTATE LOGIC Set.Deleted lNewSetting

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setdigit/*" />
FUNCTION SetDigit() AS DWORD
	GETSTATE DWORD Set.DIGITS 

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setdigit/*" />
FUNCTION SetDigit(nNewSetting AS DWORD) AS DWORD
	SETSTATE DWORD Set.DIGITS nNewSetting

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setdigitfixed/*" />
FUNCTION SetDigitFixed() AS LOGIC
	GETSTATE LOGIC Set.DigitFixed 

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setdigitfixed/*" />
FUNCTION SetDigitFixed(lNewSetting AS LOGIC) AS LOGIC
	SETSTATE LOGIC Set.DigitFixed lNewSetting


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setepoch/*" />
FUNCTION SetEpoch() AS DWORD
	GETSTATE DWORD Set.Epoch 

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setepoch/*" />
FUNCTION SetEpoch(nNewSetting AS DWORD) AS DWORD
	LOCAL wYear AS DWORD
	LOCAL wCent AS DWORD
	wYear := nNewSetting % 100
	wCent := (( nNewSetting / 100) +1) * 100
	XSharp.RuntimeState.SetValue<DWORD> (Set.EpochYear, wYear)
	XSharp.RuntimeState.SetValue<DWORD> (Set.EpochCent, wCent)
	SETSTATE DWORD Set.Epoch	 nNewSetting

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/seterrorlog/*" />
FUNCTION SetErrorLog() AS LOGIC
	GETSTATE LOGIC Set.ERRRORLOG 

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/seterrorlog/*" />
FUNCTION SetErrorLog(lNewSetting AS LOGIC) AS LOGIC
	SETSTATE LOGIC Set.ERRRORLOG lNewSetting

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setexact/*" />
FUNCTION SetExact() AS LOGIC
	GETSTATE LOGIC Set.Exact 

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setexact/*" />
FUNCTION SetExact(lNewSetting AS LOGIC) AS LOGIC
	SETSTATE LOGIC Set.Exact lNewSetting

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setexclusive/*" />
FUNCTION SetExclusive() AS LOGIC
	GETSTATE LOGIC Set.Exclusive 

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setexclusive/*" />
FUNCTION SetExclusive(lNewSetting AS LOGIC) AS LOGIC
	SETSTATE LOGIC Set.Exclusive lNewSetting

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setfieldstore/*" />
FUNCTION SetFieldStore() AS LOGIC
	GETSTATE LOGIC Set.FieldStore

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setfieldstore/*" />
FUNCTION SetFieldStore(lNewSetting AS LOGIC) AS LOGIC
	SETSTATE LOGIC Set.FieldStore lNewSetting

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setfixed/*" />
FUNCTION SetFixed() AS LOGIC
	GETSTATE LOGIC Set.Fixed 

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setfixed/*" />
FUNCTION SetFixed(lNewSetting AS LOGIC) AS LOGIC
	SETSTATE LOGIC Set.Fixed lNewSetting

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setmath/*" />
FUNCTION SetMath() AS DWORD
	GETSTATE DWORD Set.MATH

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setmath/*" />
FUNCTION SetMath(nNewSetting AS DWORD) AS DWORD
	SETSTATE DWORD Set.MATH nNewSetting


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setnatdll/*" />
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
/// 	"SPANISH", "SWEDISH", "TURKISH", "UK"
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
	VAR obj := rm:GetObject(cBase:ToUpper()) 
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

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setpath/*" />
FUNCTION SetPath() AS STRING
	GETSTATE STRING Set.Path 

/// <exclude/>
FUNCTION _SetDict() AS LOGIC
    GETSTATE LOGIC Set.DICT

/// <exclude/>
FUNCTION _SetDict(lNewSetting AS LOGIC) AS LOGIC
    SETSTATE LOGIC Set.DICT lNewSetting

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setpath/*" />
FUNCTION SetPath(cPathList AS STRING) AS STRING
	SetPathArray(NULL)
	SETSTATE STRING Set.Path cPathList


/// <summary>
/// Return the Path array that is used by the File() function to locate files outside of the current directory.
/// This is a combination of the SetDefault() and SetPath() variables
/// This may be null if the file function has never been called or never been called for files outside of the current
/// directory.
/// </summary>
/// <returns>
/// </returns>
FUNCTION SetPathArray() AS STRING[]
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
FUNCTION SetPathArray(aPath AS STRING[]) AS STRING[]
	SETSTATE STRING[] Set.PathArray aPath

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setpmext/*" />
FUNCTION SetPMExt() AS STRING
	GETSTATE STRING Set.PmExt

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setpmext/*" />
FUNCTION SetPMExt(cPMExt AS STRING) AS STRING
	SETSTATE STRING Set.PmExt cPMExt

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setscience/*" />
FUNCTION SetScience() AS LOGIC
	GETSTATE LOGIC Set.Science 

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setscience/*" />
FUNCTION SetScience(lNewSetting AS LOGIC) AS LOGIC
	SETSTATE LOGIC Set.Science lNewSetting

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setsoftseek/*" />
FUNCTION SetSoftSeek() AS LOGIC
	GETSTATE LOGIC Set.SoftSeek 

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setsoftseek/*" />
FUNCTION SetSoftSeek(lNewSetting AS LOGIC) AS LOGIC
	SETSTATE LOGIC Set.SoftSeek lNewSetting

/// <summary>
/// Return the setting that determines whether a space is displayed between fields or expressions when you use the ? or ?? command.
/// </summary>
/// <returns>
/// </returns>
FUNCTION SetSpace() AS LOGIC
	GETSTATE LOGIC Set.Space

/// <summary>
/// Change the setting that determines whether a space is displayed between fields or expressions when you use the ? or ?? command.
/// </summary>
/// <param name="lSet"></param>
/// <returns>
/// </returns>
FUNCTION SetSpace(lSet AS LOGIC) AS LOGIC
	SETSTATE LOGIC Set.Space lSet

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setthousandsep/*" />
FUNCTION SetThousandSep() AS DWORD
	GETSTATE DWORD Set.ThousandSep 

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setthousandsep/*" />
FUNCTION SetThousandSep(nNewSetting AS DWORD) AS DWORD
	LOCAL oCulture AS System.Globalization.CultureInfo
	oCulture := (System.Globalization.CultureInfo)System.Threading.Thread.CurrentThread:CurrentCulture:Clone()
	oCulture:NumberFormat:NumberGroupSeparator := ((CHAR)nNewSetting):ToString()
	System.Threading.Thread.CurrentThread:CurrentCulture := oCulture

	SETSTATE DWORD Set.ThousandSep nNewSetting

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/settimesep/*" />
FUNCTION SetTimeSep() AS DWORD
	GETSTATE DWORD Set.TimeSep 

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/settimesep/*" />
FUNCTION SetTimeSep(dwNewSetting AS DWORD) AS DWORD
	SETSTATE DWORD Set.TimeSep dwNewSetting

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setunique/*" />
FUNCTION SetUnique() AS LOGIC
	GETSTATE LOGIC Set.Unique 

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setunique/*" />
FUNCTION SetUnique(lNewSetting AS LOGIC) AS LOGIC
	SETSTATE LOGIC Set.Unique lNewSetting

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


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/errorlevel/*" />
FUNCTION ErrorLevel(dwNewSetting AS DWORD) AS DWORD 
	SETSTATE DWORD Set.ErrorLevel dwNewSetting


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/errorlevel/*" />
FUNCTION ErrorLevel() AS DWORD 
	GETSTATE DWORD Set.ErrorLevel


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setinternational/*" />
FUNCTION SetInternational() AS STRING
	RETURN RuntimeState.International:ToString():ToUpper()


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setinternational/*" />
/// <remarks>
/// SetInternational() allows XSharp apps to operate in different international modes.  
/// The "CLIPPER" mode is provided for compatibility with CA-Clipper applications and uses an 
/// internationalization routine defined in the nation module.<br/>
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
FUNCTION SetInternational(symNewSetting AS STRING) AS STRING
	LOCAL cOld AS STRING
	cOld := RuntimeState.International:ToString():ToUpper()
	SWITCH symNewSetting:ToUpper()
	CASE "CLIPPER"
		RuntimeState.GetInstance()._SetInternationalClipper()
	CASE "WINDOWS"
	CASE "UNICODE"
	CASE "ORDINAL"
		RuntimeState.GetInstance()._SetInternationalWindows()
	OTHERWISE
		THROW Error.ArgumentError(__ENTITY__, NAMEOF(symNewSetting), "Unsupported international mode: "+ symNewSetting)
	END SWITCH
	RETURN cOld


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setcollation/*" />
/// <remarks>
/// <include file="CoreComments.xml" path="Comments/Collation/*" />
/// </remarks>
FUNCTION SetCollation() AS STRING 
	RETURN RuntimeState.CollationMode:ToString():ToUpper()


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/setcollation/*" />
/// <remarks>
/// <include file="CoreComments.xml" path="Comments/Collation/*" />
/// </remarks>
/// <returns>The current setting, either "Windows" (the default),  "Clipper", "Unicode" or "Ordinal" </returns>
/// <param name="cCollation">The collation mode to use. The available modes are "Windows" (the default),  "Clipper", "Unicode" and "Ordinal". </param>
FUNCTION SetCollation(symNewSetting AS STRING)  AS STRING
	LOCAL cOld AS STRING
	cOld := RuntimeState.CollationMode:ToString():ToUpper()
	SWITCH symNewSetting:ToUpper()
	CASE "CLIPPER"
		RuntimeState.CollationMode := CollationMode.Clipper
	CASE "XPP"
		RuntimeState.CollationMode := CollationMode.Xpp
	CASE "WINDOWS"
		RuntimeState.CollationMode := CollationMode.Windows
	CASE "UNICODE"
		RuntimeState.CollationMode := CollationMode.Unicode
	CASE "ORDINAL"
		RuntimeState.CollationMode := CollationMode.Ordinal
	OTHERWISE
		THROW Error.ArgumentError(__ENTITY__, NAMEOF(symNewSetting), "Unsupported collation mode: "+symNewSetting)
	END SWITCH
	RETURN cOld
