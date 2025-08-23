//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System.Globalization
using System.Collections.Generic

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/candidate/*" />
#pragma options("vo15", on)
function Candidate (nIndexNumber , uArea)
    throw NotImplementedException{}
    // RETURN FALSE


/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/compobj/*" />

function CompObj (oExpression1, oExpression2)
    throw NotImplementedException{}
    // RETURN FALSE

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/cpconvert/*" />

function CpConvert ( nCurrentCodePage, nNewCodePage, cExpression)
    throw NotImplementedException{}
    // RETURN ""



/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/cpdbf/*" />
[Obsolete( "This function will not be supported" )];
function CpCurrent( ) as long
    throw NotImplementedException{}
    // RETURN 0


/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/createbinary/*" />
[Obsolete( "This function will not be supported" )];
function CREATEBINARY( ) as string
    throw NotImplementedException{}
    // RETURN ""

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/createoffline/*" />

function  CreateOffline (ViewName , cPath)
    throw NotImplementedException{}
    // RETURN FALSE

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/ctobin/*" />

function CToBin (cExpression , cFlags)
    throw NotImplementedException{}
    // RETURN 0

/// <summary>
/// Converts a Character expression to a DateTime value, respecting current VFP settings.
/// </summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/ctot/*" />
function CToT(cCharacterExpression)
    if String.IsNullOrEmpty(cCharacterExpression)
    	return DateTime.MinValue
    endif

    local lcInput := cCharacterExpression:Trim() as string
    local ldResult as DateTime

    if TryParseVfpDateTime(lcInput, out ldResult)
    	return ldResult
    endif

    if DateTime.TryParse(lcInput, CultureInfo.InvariantCulture, DateTimeStyles.None, out ldResult)
    	return ldResult
    endif

    return DateTime.MinValue
endfunc

internal function TryParseVfpDateTime(tcInput as string, result out DateTime) as logic
    local culture := CultureInfo.InvariantCulture as CultureInfo
    local lcDateFormat := RuntimeState.GetValue<string>(Set.DateFormat) as string
    local lHours := RuntimeState.GetValue<long>(Set.Hours) as long
    local lSeconds := RuntimeState.GetValue<logic>(Set.Seconds) as logic
    local lCentury := RuntimeState.GetValue<logic>(Set.Century) as logic

    local aFormats := List<string>{} as List<string>
    local lcSeparator := GetDateSeparatorForFormat(lcDateFormat) as string

    // Generar formatos de fecha
    local aDateFormats := GetDateFormatsForParsing(lcDateFormat, lcSeparator, lCentury) as string[]

    // Generar formatos de hora
    local aTimeFormats := GetTimeFormatsForParsing(lHours, lSeconds) as string[]

    // Combinar fecha + hora
    foreach dateFormat as string in aDateFormats
        foreach timeFormat as string in aTimeFormats
            aFormats:Add(i"{dateFormat} {timeFormat}")
            aFormats:Add(i"{dateFormat}{timeFormat}")
        next
        aFormats:Add(dateFormat)  // Solo fecha
    next

    // Formatos especiales de VFP
    aFormats:Add("yyyyMMddHHmmss")
    aFormats:Add("yyyyMMdd")
    aFormats:Add("yyyy-MM-ddTHH:mm:ss")
    aFormats:Add("yyyyMMddHHmm")

    // Intentar parsing con cada formato
    foreach format as string in aFormats
        if DateTime.TryParseExact(tcInput, format, culture, DateTimeStyles.None, out result)
            return true
        endif
    next

	result := DateTime.MinValue
    return false
endfunc

internal function GetDateSeparatorForFormat(tcDateFormat as string) as string
	switch tcDateFormat:ToUpper()
	case "ANSI"
	case "GERMAN"
		return "."
	case "ITALIAN"
		return "-"
	otherwise
		return "/"
	end switch
endfunc

internal function GetDateFormatsForParsing(tcDateFormat as string, tcSeparator as string, tlCentury as logic) as string[]
	local aFormats := List<string>{} as List<string>
	local lcYearFormat := iif(tlCentury, "yyyy", "yy") as string
	local lcYearFormat2 := iif(tlCentury, "yy", "yyyy") as string

	switch tcDateFormat:ToUpper()
	case "AMERICAN"
	case "USA"
	case "MDY"
		aFormats:Add(i"MM{tcSeparator}dd{tcSeparator}{lcYearFormat}")
        aFormats:Add(i"M{tcSeparator}d{tcSeparator}{lcYearFormat}")
        aFormats:Add(i"MM{tcSeparator}dd{tcSeparator}{lcYearFormat2}")
        aFormats:Add(i"M{tcSeparator}d{tcSeparator}{lcYearFormat2}")
	case "BRITISH"
	case "FRENCH"
	case "DMY"
	case "GERMAN"
	case "ITALIAN"
        aFormats:Add(i"dd{tcSeparator}MM{tcSeparator}{lcYearFormat}")
        aFormats:Add(i"d{tcSeparator}M{tcSeparator}{lcYearFormat}")
        aFormats:Add(i"dd{tcSeparator}MM{tcSeparator}{lcYearFormat2}")
        aFormats:Add(i"d{tcSeparator}M{tcSeparator}{lcYearFormat2}")
	case "ANSI"
	case "JAPAN"
	case "TAIWAN"
	case "YMD"
        aFormats:Add(i"{lcYearFormat}{tcSeparator}MM{tcSeparator}dd")
        aFormats:Add(i"{lcYearFormat}{tcSeparator}M{tcSeparator}d")
        aFormats:Add(i"{lcYearFormat2}{tcSeparator}MM{tcSeparator}dd")
        aFormats:Add(i"{lcYearFormat2}{tcSeparator}M{tcSeparator}d")
	otherwise
        aFormats:Add(i"MM{tcSeparator}dd{tcSeparator}{lcYearFormat}")
        aFormats:Add(i"M{tcSeparator}d{tcSeparator}{lcYearFormat}")
    end switch

    return aFormats:ToArray()
endfunc

internal function GetTimeFormatsForParsing(tlHours as long, tlSeconds as logic) as string[]
	local aFormats := List<string>{} as List<string>

	if tlSeconds
		if tlHours == 24
			aFormats:Add("HH:mm:ss")
			aFormats:Add("H:mm:ss")
		else
            aFormats:Add("hh:mm:ss tt")
            aFormats:Add("h:mm:ss tt")
            aFormats:Add("hh:mm:ss t")
            aFormats:Add("h:mm:ss t")
		endif
	else
		if tlHours == 24
            aFormats:Add("HH:mm")
            aFormats:Add("H:mm")
		else
            aFormats:Add("hh:mm tt")
            aFormats:Add("h:mm tt")
            aFormats:Add("hh:mm t")
            aFormats:Add("h:mm t")
		endif
	endif

	return aFormats:ToArray()
endfunc



/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/cursorgetprop/*" />

function CursorGetProp (cProperty , uArea)
    throw NotImplementedException{}
    // RETURN NIL


/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/cursorsetprop/*" />

function CursorSetProp (cProperty , eExpression, uArea)
    throw NotImplementedException{}
    // RETURN FALSE

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/cursortoxml/*" />

function CursorToXML (uArea, cOutput, nOutputFormat, nFlags, nRecords, cSchemaName, cSchemaLocation, cNameSpace )
    throw NotImplementedException{}
    // RETURN 0

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/curval/*" />

function CurVal(cExpression, uArea)
    throw NotImplementedException{}
    // RETURN NIL




