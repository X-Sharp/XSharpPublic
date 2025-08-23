//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#pragma options("vo15", on)

USING System.Globalization

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/tablerevert/*" />

FUNCTION TableRevert( lAllRows , uArea) AS LONG
    THROW NotImplementedException{}
    // RETURN 0

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/tableupdate/*" />

FUNCTION TableUpdate( nRows , lForce , uArea , cErrorArray) AS LOGIC
    THROW NotImplementedException{}
    // RETURN FALSE

/// <summary>
/// Converts a DateTime expression to a Character value with the specified format.
/// </summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/ttoc/*" />
FUNCTION TToC(tdExpression AS USUAL) AS STRING
	RETURN FormatVfpDefault(tdExpression)
ENDFUNC

/// <summary>
/// Converts a DateTime expression to a Character value with the specified format.
/// </summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/ttoc/*" />
FUNCTION TToC(tdExpression AS USUAL, nParam AS LONG) AS STRING
	LOCAL lcResult AS STRING
	LOCAL culture := CultureInfo.InvariantCulture AS CultureInfo

	// handle empty date
	IF tdExpression == DateTime.MinValue || tdExpression == DateTime{}
		SWITCH nParam
		CASE 0
		CASE 1
			RETURN "00000000000000"
		CASE 2
			RETURN GetEmptyTimeFormat()
		CASE 3
			RETURN "0000-00-00T00:00:00"
		OTHERWISE
			RETURN GetEmptyDefaultFormat()
		END SWITCH
	ENDIF

	SWITCH nParam
	CASE 0
	CASE 1
		lcResult := tdExpression:ToString("yyyyMMddHHmmss", culture)
	CASE 2
		lcResult := FormatTimeOnly(tdExpression)
	CASE 3
		lcResult := tdExpression:ToString("yyyy-MM-ddTHH:mm:ss", culture)
	OTHERWISE
		lcResult := tdExpression:ToString("yyyyMMddHHmmss", culture)
	END SWITCH

	RETURN lcResult
ENDFUNC

INTERNAL FUNCTION FormatTimeOnly(tdExpression AS DateTime) AS STRING
	LOCAL culture := CultureInfo.InvariantCulture AS CultureInfo
	LOCAL lHours := RuntimeState.GetValue<LONG>(Set.Hours) AS LONG
	LOCAL lSeconds := RuntimeState.GetValue<LOGIC>(Set.Seconds) AS LOGIC

	IF lSeconds
		IF lHours == 24
			RETURN tdExpression:ToString("HH:mm:ss", culture)
		ELSE
			RETURN tdExpression:ToString("hh:mm:ss tt", culture)
		ENDIF
	ELSE
		IF lHours == 24
			RETURN tdExpression:ToString("HH:mm", culture)
		ELSE
			RETURN tdExpression:ToString("hh:mm tt", culture)
		ENDIF
	ENDIF
ENDFUNC

INTERNAL FUNCTION GetEmptyDefaultFormat() AS STRING
	LOCAL lcDateFormat := RuntimeState.GetValue<STRING>(Set.DateFormat)
	LOCAL lHours := RuntimeState.GetValue<LONG>(Set.Hours)
	LOCAL lSeconds := RuntimeState.GetValue<LOGIC>(Set.Seconds)
	LOCAL lCentury := RuntimeState.GetValue<LOGIC>(Set.Century)

	LOCAL lcDatePart := GetEmptyDatePart(lcDateFormat, lCentury)
	LOCAL lcTimePart := GetEmptyTimePart(lHours, lSeconds)

	RETURN i"{lcDatePart} {lcTimePart}"
ENDFUNC

INTERNAL FUNCTION FormatVfpDefault(tdExpression AS DateTime) AS STRING
	LOCAL lcDatePart AS STRING
	LOCAL lcTimePart AS STRING
	LOCAL culture := CultureInfo.InvariantCulture AS CultureInfo

	LOCAL lCentury := RuntimeState.GetValue<LOGIC>(Set.Century)
	LOCAL lHours := RuntimeState.GetValue<LONG>(Set.Hours)
	LOCAL lSeconds := RuntimeState.GetValue<LOGIC>(Set.Seconds)
	LOCAL lcDateFormat := RuntimeState.GetValue<STRING>(Set.DateFormat)

	lcDatePart := FormatDatePart(tdExpression, lCentury, lcDateFormat)

	IF lSeconds
		IF lHours == 24
			lcTimePart := tdExpression:ToString("HH:mm:ss", culture)
		ELSE
			lcTimePart := tdExpression:ToString("hh:mm:ss tt", culture)
		ENDIF
	ELSE
		IF lHours == 24
			lcTimePart := tdExpression:ToString("HH:mm", culture)
		ELSE
			lcTimePart := tdExpression:ToString("hh:mm tt", culture)
		ENDIF
	ENDIF

	RETURN i"{lcDatePart} {lcTimePart}"
ENDFUNC

INTERNAL FUNCTION GetEmptyDatePart(lcDateFormat AS STRING, lCentury AS LOGIC) AS STRING
	LOCAL lcSeparator := "/" AS STRING
	LOCAL lcYear := IIF(lCentury, "    ", "  ") AS STRING

	SWITCH lcDateFormat:ToUpper()
	CASE "ANSI"
	CASE "GERMAN"
		lcSeparator := "."
		RETURN i"{lcYear}{lcSeparator}  {lcSeparator}  "
	CASE "ITALIAN"
		lcSeparator := "-"
		RETURN i"  {lcSeparator}  {lcSeparator}{lcYear}"
	OTHERWISE
		RETURN i"  {lcSeparator}  {lcSeparator}{lcYear}"
	END SWITCH
ENDFUNC

INTERNAL FUNCTION GetEmptyTimePart(lHours AS LONG, lSeconds AS LOGIC) AS STRING
	IF lSeconds
		IF lHours == 24
			RETURN "  :  :  "
		ELSE
			RETURN "  :  :   AM"
		ENDIF
	ELSE
		IF lHours == 24
			RETURN "  :  "
		ELSE
			RETURN "  :   AM"
		ENDIF
	ENDIF
ENDFUNC

INTERNAL FUNCTION FormatDatePart(tdExpression AS DateTime, lCentury AS LOGIC, lcDateFormat AS STRING) AS STRING
	LOCAL culture := CultureInfo.InvariantCulture AS CultureInfo
	LOCAL lcSeparator := "/" AS STRING
	LOCAL lcYearFormat AS STRING

	lcYearFormat := IIF(lCentury, "yyyy", "yy")

	SWITCH lcDateFormat:ToUpper()
	CASE "AMERICAN"
	CASE "USA"
		lcSeparator := "/"
		RETURN tdExpression:ToString(i"MM{lcSeparator}dd{lcSeparator}{lcYearFormat}", culture)
	CASE "ANSI"
		lcSeparator := "."
		RETURN tdExpression:ToString(i"{lcYearFormat}{lcSeparator}MM{lcSeparator}dd", culture)
	CASE "BRITISH"
	CASE "FRENCH"
		lcSeparator := "/"
		RETURN tdExpression:ToString(i"dd{lcSeparator}MM{lcSeparator}{lcYearFormat}", culture)
	CASE "GERMAN"
		lcSeparator := "."
		RETURN tdExpression:ToString(i"dd{lcSeparator}MM{lcSeparator}{lcYearFormat}", culture)
	CASE "ITALIAN"
		lcSeparator := "-"
		RETURN tdExpression:ToString(i"dd{lcSeparator}MM{lcSeparator}{lcYearFormat}", culture)
	CASE "JAPAN"
		lcSeparator := "/"
		RETURN tdExpression:ToString(i"{lcYearFormat}{lcSeparator}MM{lcSeparator}dd", culture)
	CASE "TAIWAN"
		lcSeparator := "/"
		RETURN tdExpression:ToString(i"{lcYearFormat}{lcSeparator}MM{lcSeparator}dd", culture)
	OTHERWISE
		// AMERICAN by default
		lcSeparator := "/"
		RETURN tdExpression:ToString(i"MM{lcSeparator}dd{lcSeparator}{lcYearFormat}", culture)
	END SWITCH
ENDFUNC

INTERNAL FUNCTION FormatVfpStandard(tdExpression AS DateTime, tcDateFormat AS STRING) AS STRING
	LOCAL lcDatePart AS STRING
	LOCAL lcTimePart AS STRING
	LOCAL lcSeparator AS STRING

	LOCAL culture := CultureInfo.InvariantCulture AS CultureInfo
	LOCAL lHours := RuntimeState.GetValue<LONG>(Set.Hours)
	LOCAL lSeconds := RuntimeState.GetValue<LOGIC>(Set.Seconds)

	IF lSeconds
		IF lHours == 24
			lcTimePart := tdExpression:ToString("HH:mm:ss", culture) // 24 hours
		ELSE
			lcTimePart := tdExpression:ToString("hh:mm:ss tt", culture) // 12 hours with AM/PM
		ENDIF
	ELSE
		IF lHours == 24
			lcTimePart := tdExpression:ToString("HH:mm", culture)	// 24 hours (no secs)
		ELSE
			lcTimePart := tdExpression:ToString("hh:mm tt", culture)	// 12 hours (no secs)
		ENDIF
	ENDIF

	SWITCH tcDateFormat:ToUpper()
	CASE "ANSI"
	CASE "GERMAN"
		lcSeparator := "."
	CASE "ITALIAN"
	CASE "USA"
		lcSeparator := "-"
	OTHERWISE
		lcSeparator := "/"
	END SWITCH

	SWITCH tcDateFormat:ToUpper()
	CASE "AMERICAN"
	CASE "MDY"
		lcDatePart := tdExpression:ToString(i"MM{lcSeparator}dd{lcSeparator}yyyy", culture)
	CASE "BRITISH"
	CASE "FRENCH"
	CASE "DMY"
	CASE "GERMAN"
	CASE "ITALIAN"
		lcDatePart := tdExpression:ToString(i"dd{lcSeparator}MM{lcSeparator}yyyy", culture)
	CASE "JAPAN"
	CASE "TAIWAN"
	CASE "YMD"
	CASE "ANSI"
		lcDatePart := tdExpression:ToString(i"yyyy{lcSeparator}MM{lcSeparator}dd", culture)
	CASE "USA"
		lcDatePart := tdExpression:ToString(i"MM{lcSeparator}dd{lcSeparator}yyyy", culture)
	OTHERWISE
		lcDatePart := tdExpression:ToString(i"MM{lcSeparator}dd{lcSeparator}yyyy", culture)
	END SWITCH

	RETURN i"{lcDatePart} {lcTimePart}"
ENDFUNC

INTERNAL FUNCTION FormatWindowsStyle(tdExpression AS DateTime, tlLongFormat AS LOGIC) AS STRING
	LOCAL lHours := RuntimeState.GetValue<LONG>(Set.Hours)
	LOCAL culture := CultureInfo.InvariantCulture AS CultureInfo

	IF tlLongFormat
		IF lHours == 24
			RETURN tdExpression:ToString("dddd, MMMM dd, yyyy HH:mm:ss", culture)
		ELSE
			RETURN tdExpression:ToString("dddd, MMMM dd, yyyy hh:mm:ss tt", culture)
		ENDIF
	ELSE
		IF lHours == 24
			RETURN tdExpression:ToString("M/d/yyyy HH:mm:ss", culture)
		ELSE
			RETURN tdExpression:ToString("M/d/yyyy HH:mm:ss", culture)
		ENDIF
	ENDIF
ENDFUNC

INTERNAL FUNCTION GetEmptyTimeFormat AS STRING
	LOCAL lHours := RuntimeState.GetValue<LONG>(Set.Hours)
	LOCAL lSeconds := RuntimeState.GetValue<LOGIC>(Set.Seconds)

	IF lSeconds
		IF lHours == 24
			RETURN "00:00:00"
		ELSE
			RETURN "12:00:00 AM"
		ENDIF
	ELSE
		IF lHours == 24
			RETURN "00:00"
		ELSE
			RETURN "12:00 AM"
		ENDIF
	ENDIF
ENDFUNC

INTERNAL FUNCTION GetEmptyDateTimeFormat AS STRING
	LOCAL lcDateFormat
	lcDateFormat := RuntimeState.GetValue<STRING>(Set.DateFormat)
	LOCAL lHours := RuntimeState.GetValue<LONG>(Set.Hours)
	SWITCH lcDateFormat
	CASE "AMERICAN"
	CASE "BRITISH"
	CASE "DMY"
	CASE "FRENCH"
	CASE "JAPAN"
	CASE "LONG"
	CASE "MDY"
	CASE "SHORT"
	CASE "TAIWAN"
	CASE "YMD"
		IF lHours == 24
			RETURN "  /  /     :  :  "
		ELSE
			RETURN "  /  /     :  :   AM"
		ENDIF
	CASE "ANSI"
	CASE "GERMAN"
		IF lHours == 24
			RETURN "  .  .     :  :  "
		ELSE
			RETURN "  .  .     :  :   AM"
		ENDIF
	CASE "ITALIAN"
	CASE "USA"
		IF lHours == 24
			RETURN "  -  -     :  :  "
		ELSE
			RETURN "  -  -     :  :   AM"
		ENDIF
	OTHERWISE
		IF lHours == 24
			RETURN "  /  /     :  :  "
		ELSE
			RETURN "  /  /     :  :   AM"
		ENDIF
	END SWITCH
ENDFUNC

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/txnlevel/*" />

FUNCTION TxnLevel( ) AS LONG
    THROW NotImplementedException{}
    // RETURN 0

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/txtwidth/*" />

FUNCTION TxtWidth( cExpression , cFontName, nFontSize , cFontStyle) AS FLOAT
    THROW NotImplementedException{}
    // RETURN 0



/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/xmltocursor/*" />

FUNCTION XmlToCursor( eExpression , cCursorName , nFlags ) AS LONG
    THROW NotImplementedException{}
    // RETURN 0

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/xmlupdategram/*" />

FUNCTION XmlUpdatefram( cAliasList , nFlags , cSchemaLocation) AS STRING
    THROW NotImplementedException{}
    // RETURN ""

