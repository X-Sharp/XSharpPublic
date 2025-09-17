//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Globalization
USING System.Collections.Generic

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/dtot/*" />
FUNCTION DToT( dDateExpression AS DATE) AS DateTime
    RETURN (System.DateTime) dDateExpression

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/hour/*" />
FUNCTION Hour( tExpression AS System.DateTime) AS LONG
    RETURN tExpression:Hour

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/minute/*" />
FUNCTION Minute(tExpression AS System.DateTime) AS LONG
    RETURN tExpression:Minute

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sec/*" />
FUNCTION Sec( tExpression AS System.DateTime) AS LONG
    RETURN tExpression:Second

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/ttod/*" />
FUNCTION TToD( tExpression AS System.DateTime) AS DATE
    RETURN (DATE) tExpression

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/gomonth/*" />
FUNCTION GoMonth( dExpression AS DATE , iNumberOfMonths AS INT ) AS DATE
    RETURN GoMonth ( (System.DateTime) dExpression , iNumberOfMonths )

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/gomonth/*" />
FUNCTION GoMonth( tExpression AS System.DateTime , iNumberOfMonths AS INT ) AS DATE
LOCAL dDate AS DATE

   	IF ((DATE) tExpression):IsEmpty
		RETURN NULL_DATE
	ENDIF


    TRY    // suppresses a exception if the year becomes < 1 or > 9999

 		dDate := (DATE) tExpression:AddMonths( iNumberOfMonths )

		// according the docs, 1753 is the VFP min limit
		IF Year ( dDate ) < 1753
			dDate := NULL_DATE
		ENDIF

    CATCH

    	dDate := NULL_DATE // just to be sure ...

    END TRY

    RETURN dDate

INTERNAL FUNCTION _DateTimeError( sParameter AS STRING, argNum AS DWORD, aArgs PARAMS OBJECT[]) AS Error
    VAR err := Error.ArgumentError(ProcName(1), sParameter,__VfpStr(VFPErrors.VFP_INVALID_PARAMETER, sParameter, "'DATE' or 'DATETIME'" ), 1)
    err:Args := aArgs
    err:ArgNum := argNum
    err:Stack := ErrorStack(1)
    RETURN err


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/gomonth/*" />
FUNCTION GoMonth( uExpression AS USUAL , iNumberOfMonths AS INT ) AS DATE
    IF IsDate(uExpression)
        RETURN GoMonth ( (DATE) uExpression , iNumberOfMonths )
    ELSEIF IsDateTime(uExpression)
        RETURN GoMonth ( (System.DateTime) uExpression , iNumberOfMonths )
    ENDIF
    THROW _DateTimeError(NAMEOF(uExpression), 1, uExpression, iNumberOfMonths)


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/quarter/*" />
FUNCTION Quarter( uExpression  AS USUAL , nMonth  := 1 AS INT ) AS INT
    IF IsDate(uExpression)
        RETURN Quarter ( (DATE) uExpression , nMonth )
    ELSEIF IsDateTime(uExpression)
        RETURN Quarter ( (System.DateTime) uExpression , nMonth )
    ENDIF
    THROW _DateTimeError(NAMEOF(uExpression),1, uExpression, nMonth)


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/quarter/*" />
FUNCTION Quarter( dExpression  AS DATE , nMonth  := 1 AS INT ) AS INT
    RETURN Quarter ( (System.DateTime) dExpression  , nMonth  )


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/quarter/*" />
FUNCTION Quarter( tExpression  AS System.DateTime , nMonth  := 1 AS INT ) AS INT
    IF  ! (nMonth  > 0 .AND. nMonth  < 13 )
        THROW Error.ArgumentError(__ENTITY__, NAMEOF(nMonth), __VfpStr(VFPErrors.VFP_INVALID_RANGE, NAMEOF(nMonth), nMonth , "1-12") ,1)
    ENDIF

   	IF ((DATE) tExpression ):IsEmpty
		RETURN 0
	ENDIF

	VAR dtOffset := tExpression :AddMonths( 1 - (INT) nMonth  )

    RETURN  (INT) System.Math.Ceiling((DECIMAL)dtOffset:Month / 3)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/week/*" />
FUNCTION Week( uExpression AS USUAL, nFirstWeek := 1 AS LONG, nFirstDayOfWeek := 1 AS LONG) AS LONG
    IF IsDate(uExpression)
        RETURN Week ( (Date) uExpression , nFirstWeek, nFirstDayOfWeek )
    ELSEIF IsDateTime(uExpression)
        RETURN Week ( (System.DateTime) uExpression , nFirstWeek, nFirstDayOfWeek )
    ENDIF
    THROW _DateTimeError(NAMEOF(uExpression), 1, uExpression, nFirstWeek,nFirstDayOfWeek)


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/week/*" />
FUNCTION Week( dExpression AS DATE, nFirstWeek := 1 AS LONG, nFirstDayOfWeek := 1 AS LONG) AS LONG
    RETURN Week ((System.DateTime) dExpression, nFirstWeek, nFirstDayOfWeek)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/week/*" />
FUNCTION Week( tExpression AS System.DateTime, nFirstWeek := 1 AS LONG, nFirstDayOfWeek := 1 AS LONG) AS LONG
    // validate parameters
	IF  ! (nFirstWeek  >= 0 .AND. nFirstWeek  < 4 )
		THROW ArgumentException { __VfpStr(VFPErrors.VFP_INVALID_RANGE, NAMEOF(nFirstWeek), nFirstWeek ,"1-3"), NAMEOF(nFirstWeek) }
    ENDIF
	IF  ! (nFirstDayOfWeek >= 0 .AND. nFirstDayOfWeek  < 8 )
		THROW ArgumentException { __VfpStr(VFPErrors.VFP_INVALID_RANGE, NAMEOF(nFirstDayOfWeek), nFirstDayOfWeek,"0-7"), NAMEOF(nFirstDayOfWeek) }
    ENDIF
    LOCAL Week AS CalendarWeekRule
    SWITCH nFirstWeek
    CASE 0
    CASE 1
        Week := CalendarWeekRule.FirstDay
    CASE 2
        Week := CalendarWeekRule.FirstFourDayWeek
    CASE 3
    OTHERWISE
        Week := CalendarWeekRule.FirstFullWeek
    END SWITCH
    LOCAL day AS DayOfWeek
    IF nFirstDayOfWeek == 0
        day :=  DayOfWeek.Sunday
    ELSE
        day :=  (DayOfWeek) (nFirstDayOfWeek -1)
    ENDIF
    VAR calendar := System.Globalization.GregorianCalendar{}
    RETURN calendar:GetWeekOfYear ( tExpression, Week, day)


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/mdy/*" />
FUNCTION MDY ( uExpression AS USUAL ) AS STRING
    IF IsDate(uExpression)
        RETURN MDY( (DATE) uExpression )
    ELSEIF IsDateTime(uExpression)
        RETURN MDY ( (System.DateTime) uExpression  )
    ENDIF
    THROW _DateTimeError(NAMEOF(uExpression), 1, uExpression)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/mdy/*" />
FUNCTION MDY ( tExpression AS System.DateTime) AS STRING
    RETURN MDY ( (DATE) tExpression  )

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/mdy/*" />
FUNCTION MDY ( dExpression AS DATE ) AS STRING

	IF dExpression:IsEmpty
		// Localized error text: "* invalid date *"
		RETURN __VfpStr(VFPErrors.VFP_INVALID_DATE)
	ENDIF

	RETURN CMonth(dExpression) + " " + PadL(Day(dExpression), 2 , "0" )  + ", " + ;
			IIF ( SetCentury() , dExpression:ToString("yyyy") , dExpression:ToString("yy")  )

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/dmy/*" />
FUNCTION DMY ( uExpression  AS USUAL ) AS STRING
    IF IsDate(uExpression)
        RETURN DMY( (DATE) uExpression )
    ELSEIF IsDateTime(uExpression)
        RETURN DMY ( (System.DateTime) uExpression  )
    ENDIF
    THROW _DateTimeError(NAMEOF(uExpression),1, uExpression)


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/dmy/*" />
FUNCTION DMY ( tExpression  AS System.DateTime) AS STRING
    RETURN DMY ( (DATE) tExpression   )

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/dmy/*" />
FUNCTION DMY ( dExpression  AS DATE ) AS STRING
	IF dExpression:IsEmpty
		// Localized error text: "* invalid date *"
		RETURN __VfpStr(VFPErrors.VFP_INVALID_DATE)
	ENDIF

	RETURN PadL(Day(dExpression), 2 , "0" ) + " " + CMonth(dExpression) + " " + ;
			IIF ( SetCentury() , dExpression:ToString("yyyy") , dExpression:ToString("yy")  )

/// <include file="VFPDocs.xml" path="Runtimefunctions/ctot/*" />
FUNCTION CToT(cCharacterExpression AS STRING) AS System.DateTime
    IF String.IsNullOrEmpty(cCharacterExpression)
        RETURN DateTime.MinValue
    ENDIF

    LOCAL lcInput := DateTimeHelper.NormalizeWhitespace(cCharacterExpression) AS STRING
    LOCAL ldResult AS System.DateTime

    IF DateTimeHelper.TryParseVfpDateTime(lcInput, OUT ldResult)
        RETURN ldResult
    ENDIF

    IF DateTime.TryParse(lcInput, CultureInfo.InvariantCulture, DateTimeStyles.None, OUT ldResult)
        RETURN ldResult
    ENDIF

    RETURN DateTime.MinValue
ENDFUNC

/// <include file="VFPDocs.xml" path="Runtimefunctions/ttoc/*" />
FUNCTION TToC(tdExpression AS System.DateTime, nParam := 0 AS LONG) AS STRING
	LOCAL lcResult AS STRING
	LOCAL culture := CultureInfo.InvariantCulture AS CultureInfo

	// handle empty date
	IF tdExpression == DateTime.MinValue //|| tdExpression == DateTime{}
		SWITCH nParam
		CASE 0
		CASE 1
			RETURN "00000000000000"
		CASE 2
			RETURN DateTimeHelper.GetEmptyTimeFormat()
		CASE 3
			RETURN "0000-00-00T00:00:00"
		OTHERWISE
			RETURN DateTimeHelper.GetEmptyDefaultFormat()
		END SWITCH
	ENDIF

	SWITCH nParam
	CASE 0
	CASE 1
		lcResult := tdExpression:ToString("yyyyMMddHHmmss", culture)
	CASE 2
		lcResult := DateTimeHelper.FormatTimeOnly(tdExpression)
	CASE 3
		lcResult := tdExpression:ToString("yyyy-MM-ddTHH:mm:ss", culture)
	OTHERWISE
		lcResult := tdExpression:ToString("yyyyMMddHHmmss", culture)
	END SWITCH

	RETURN lcResult
ENDFUNC

INTERNAL STATIC CLASS DateTimeHelper
	STATIC METHOD NormalizeWhitespace(tcInput AS STRING) AS STRING
	    IF String.IsNullOrEmpty(tcInput)
	        RETURN tcInput
	    ENDIF

	    LOCAL lcResult := tcInput:Trim() AS STRING
	    LOCAL aWhitespaceChars := <CHAR>{c' ', c'\t', c'\n', c'\r'} AS CHAR[]
	    LOCAL lcNormalized := "" AS STRING
	    LOCAL lInWhitespace := FALSE AS LOGIC

	    FOREACH cChar AS CHAR IN lcResult
	        IF Array.IndexOf(aWhitespaceChars, cChar) >= 0
	            IF !lInWhitespace
	                lcNormalized += " "
	                lInWhitespace := TRUE
	            ENDIF
	        ELSE
	            lcNormalized += cChar:ToString()
	            lInWhitespace := FALSE
	        ENDIF
	    NEXT

	    RETURN lcNormalized:Trim()
	END METHOD

	STATIC METHOD TryParseVfpDateTime(tcInput AS STRING, result OUT System.DateTime) AS LOGIC
	    LOCAL culture := CultureInfo.InvariantCulture AS CultureInfo
	    LOCAL lHours := RuntimeState.GetValue<LONG>(Set.Hours) AS LONG
	    LOCAL lSeconds := RuntimeState.GetValue<LOGIC>(Set.Seconds) AS LOGIC

	    // Generate time formats
	    LOCAL aTimeFormats := GetTimeFormatsForParsing(lHours, lSeconds) AS STRING[]
	    LOCAL aAllDateFormats := GetAllFlexibleDateFormats() AS STRING[]

	    // vfp format
	    LOCAL aSpecialFormats := GetVfpSpecialFormats() AS STRING[]
	    FOREACH format AS STRING IN aSpecialFormats
	        IF DateTime.TryParseExact(tcInput, format, culture, DateTimeStyles.None, OUT result)
	            RETURN TRUE
	        ENDIF
	    NEXT

	    // Just date first
	    FOREACH dateFormat AS STRING IN aAllDateFormats
	        IF DateTime.TryParseExact(tcInput, dateFormat, culture, DateTimeStyles.None, OUT result)
	            RETURN TRUE
	        ENDIF
	    NEXT

	    // date + time (with space)
	    FOREACH dateFormat AS STRING IN aAllDateFormats
	        FOREACH timeFormat AS STRING IN aTimeFormats
	            LOCAL combinedFormat := i"{dateFormat} {timeFormat}" AS STRING
	            IF DateTime.TryParseExact(tcInput, combinedFormat, culture, DateTimeStyles.None, OUT result)
	                RETURN TRUE
	            ENDIF
	        NEXT
	    NEXT

	    // date + time (no space)
	    FOREACH dateFormat AS STRING IN aAllDateFormats
	        FOREACH timeFormat AS STRING IN aTimeFormats
	            LOCAL combinedFormat := i"{dateFormat}{timeFormat}" AS STRING
	            IF DateTime.TryParseExact(tcInput, combinedFormat, culture, DateTimeStyles.None, OUT result)
	                RETURN TRUE
	            ENDIF
	        NEXT
	    NEXT

	    result := DateTime.MinValue
	    RETURN FALSE
	END METHOD

	STATIC METHOD GetVfpSpecialFormats() AS STRING[]
	    LOCAL aFormats := List<STRING>{} AS List<STRING>

	    // just date
	    aFormats:Add("yyyyMMdd")        // "20250813"

	    // date + time
	    aFormats:Add("yyyyMMddHHmmss")  // "20250813143045"

	    // Fecha + hora sin segundos
	    aFormats:Add("yyyyMMddHHmm")    // "202508131430"

	    // ISO format
	    aFormats:Add("yyyyMMddTHHmmss") // "20250813T143045"
	    aFormats:Add("yyyyMMddTHHmm")   // "20250813T1430"

	    // Standard ISO
	    aFormats:Add("yyyy-MM-ddTHH:mm:ss")     // "2025-08-13T14:30:45"
	    aFormats:Add("yyyy-MM-ddTHH:mm")        // "2025-08-13T14:30"
	    aFormats:Add("yyyy-MM-dd HH:mm:ss")     // "2025-08-13 14:30:45"
	    aFormats:Add("yyyy-MM-dd HH:mm")        // "2025-08-13 14:30"
	    aFormats:Add("yyyy-MM-dd")              // "2025-08-13"

	    // VFP supported format
	    aFormats:Add("yyyyMMddHHmmssfff")       // Con milisegundos
	    aFormats:Add("yyyyMMddTHHmmssfff")      // Con T y milisegundos

	    RETURN aFormats:ToArray()
	END METHOD

	STATIC METHOD GetAllFlexibleDateFormats() AS STRING[]
	    LOCAL aFormats := List<STRING>{} AS List<STRING>
	    LOCAL aSeparators := <STRING>{"/", "-", "."} AS STRING[]
	    LOCAL aYearFormats := <STRING>{"yyyy", "yy"} AS STRING[]

	    FOREACH separator AS STRING IN aSeparators
	        FOREACH yearFormat AS STRING IN aYearFormats
	            // AMERICAN (MM/dd/yyyy)
	            aFormats:Add(i"MM{separator}dd{separator}{yearFormat}")
	            aFormats:Add(i"M{separator}dd{separator}{yearFormat}")
	            aFormats:Add(i"MM{separator}d{separator}{yearFormat}")
	            aFormats:Add(i"M{separator}d{separator}{yearFormat}")

	            // EUROPEAN (dd/MM/yyyy)
	            aFormats:Add(i"dd{separator}MM{separator}{yearFormat}")
	            aFormats:Add(i"d{separator}MM{separator}{yearFormat}")
	            aFormats:Add(i"dd{separator}M{separator}{yearFormat}")
	            aFormats:Add(i"d{separator}M{separator}{yearFormat}")

	            // ISO (yyyy/MM/dd)
	            aFormats:Add(i"{yearFormat}{separator}MM{separator}dd")
	            aFormats:Add(i"{yearFormat}{separator}M{separator}dd")
	            aFormats:Add(i"{yearFormat}{separator}MM{separator}d")
	            aFormats:Add(i"{yearFormat}{separator}M{separator}d")
	        NEXT
	    NEXT

	    RETURN aFormats:ToArray()
	END METHOD

	// STATIC METHOD GetDateSeparatorForFormat(tcDateFormat AS STRING) AS STRING
		// SWITCH tcDateFormat:ToUpper()
		//ANSI, GERMAN
		// CASE "YY.MM.DD"
		// CASE "YYYY.MM.DD"
		// CASE "DD.MM.YY"
		// CASE "DD.MM.YYYY"
			// RETURN "."
		//ITALIAN
		// CASE "DD-MM-YY"
		// CASE "DD-MM-YYYY"
			// RETURN "-"
		// OTHERWISE
			// RETURN "/"
		// END SWITCH
	// END METHOD

	STATIC METHOD GetDateFormatsForParsing(tcDateFormat AS STRING, tcSeparator AS STRING, tlCentury AS LOGIC) AS STRING[]
	    LOCAL aFormats := List<STRING>{} AS List<STRING>

	    // Include always year formats for flexibility
	    LOCAL aYearFormats := <STRING>{"yyyy", "yy"} AS STRING[]

	    SWITCH tcDateFormat:ToUpper()
	    // AMERICAN
		CASE "MM/DD/YY"
		CASE "MM/DD/YYYY"
		// USA, MDY
		CASE "MM-DD-YY"
		CASE "MM-DD-YYYY"
	        FOREACH yearFormat AS STRING IN aYearFormats
	            aFormats:Add(i"MM{tcSeparator}dd{tcSeparator}{yearFormat}")
	            aFormats:Add(i"M{tcSeparator}dd{tcSeparator}{yearFormat}")
	            aFormats:Add(i"MM{tcSeparator}d{tcSeparator}{yearFormat}")
	            aFormats:Add(i"M{tcSeparator}d{tcSeparator}{yearFormat}")
	        NEXT
		// BRITISH, FRENCH, DMY
		CASE "DD/MM/YY"
		CASE "DD/MM/YYYY"
	        FOREACH yearFormat AS STRING IN aYearFormats
	            aFormats:Add(i"dd{tcSeparator}MM{tcSeparator}{yearFormat}")
	            aFormats:Add(i"d{tcSeparator}MM{tcSeparator}{yearFormat}")
	            aFormats:Add(i"dd{tcSeparator}M{tcSeparator}{yearFormat}")
	            aFormats:Add(i"d{tcSeparator}M{tcSeparator}{yearFormat}")
	        NEXT
		// GERMAN
		CASE "DD.MM.YY"
		CASE "DD.MM.YYYY"
		// ITALIAN
		CASE "DD-MM-YY"
		CASE "DD-MM-YYYY"
	        FOREACH yearFormat AS STRING IN aYearFormats
	            aFormats:Add(i"dd{tcSeparator}MM{tcSeparator}{yearFormat}")
	            aFormats:Add(i"d{tcSeparator}MM{tcSeparator}{yearFormat}")
	            aFormats:Add(i"dd{tcSeparator}M{tcSeparator}{yearFormat}")
	            aFormats:Add(i"d{tcSeparator}M{tcSeparator}{yearFormat}")
	        NEXT

	    // ANSI
		CASE "YY.MM.DD"
		CASE "YYYY.MM.DD"
	    // JAPAN, TAIWAN, YMD
		CASE "YY/MM/DD"
		CASE "YYYY/MM/DD"
	        FOREACH yearFormat AS STRING IN aYearFormats
	            aFormats:Add(i"{yearFormat}{tcSeparator}MM{tcSeparator}dd")
	            aFormats:Add(i"{yearFormat}{tcSeparator}M{tcSeparator}dd")
	            aFormats:Add(i"{yearFormat}{tcSeparator}MM{tcSeparator}d")
	            aFormats:Add(i"{yearFormat}{tcSeparator}M{tcSeparator}d")
	        NEXT
	    OTHERWISE
	        // Default to AMERICAN (MM/DD/YY[YY])
	        FOREACH yearFormat AS STRING IN aYearFormats
	            aFormats:Add(i"MM{tcSeparator}dd{tcSeparator}{yearFormat}")
	            aFormats:Add(i"M{tcSeparator}d{tcSeparator}{yearFormat}")
	        NEXT
	    END SWITCH

	    RETURN aFormats:ToArray()
	END METHOD

	STATIC METHOD GetTimeFormatsForParsing(tlHours AS LONG, tlSeconds AS LOGIC) AS STRING[]
		LOCAL aFormats := List<STRING>{} AS List<STRING>

		IF tlSeconds
	        aFormats:Add("HH:mm:ss")
	        aFormats:Add("H:mm:ss")

	        // Formato 12 horas
	        aFormats:Add("hh:mm:ss tt")
	        aFormats:Add("h:mm:ss tt")
	        aFormats:Add("hh:mm:ss t")
	        aFormats:Add("h:mm:ss t")
		ELSE
	        aFormats:Add("HH:mm")
	        aFormats:Add("H:mm")

	        // Formato 12 horas
	        aFormats:Add("hh:mm tt")
	        aFormats:Add("h:mm tt")
	        aFormats:Add("hh:mm t")
	        aFormats:Add("h:mm t")
		ENDIF

		RETURN aFormats:ToArray()
	END METHOD

	STATIC METHOD FormatVfpDefault(tdExpression AS System.DateTime) AS STRING
		LOCAL lcDatePart AS STRING
		LOCAL lcTimePart AS STRING
		LOCAL culture := CultureInfo.InvariantCulture AS CultureInfo

		LOCAL lCentury := RuntimeState.GetValue<LOGIC>(Set.Century) AS LOGIC
		LOCAL lHours := RuntimeState.GetValue<LONG>(Set.Hours) AS LONG
		LOCAL lSeconds := RuntimeState.GetValue<LOGIC>(Set.Seconds) AS LOGIC
		LOCAL nDateCountry := SetDateCountry() AS DWORD

		// TODO(irwin): consider using DTOC()
		lcDatePart := FormatDatePart(tdExpression, lCentury, nDateCountry)

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

	//	return i"{lcDatePart} {lcTimePart}"
		RETURN lcDatePart + " " + lcTimePart
	END METHOD

	STATIC METHOD GetEmptyTimeFormat AS STRING
		LOCAL lHours := RuntimeState.GetValue<LONG>(Set.Hours) AS LONG
		LOCAL lSeconds := RuntimeState.GetValue<LOGIC>(Set.Seconds) AS LOGIC

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
	END METHOD

	STATIC METHOD GetEmptyDefaultFormat AS STRING
		LOCAL lcDateFormat := RuntimeState.GetValue<STRING>(Set.DateFormat) AS STRING
		LOCAL lHours := RuntimeState.GetValue<LONG>(Set.Hours) AS LONG
		LOCAL lSeconds := RuntimeState.GetValue<LOGIC>(Set.Seconds) AS LOGIC
		LOCAL lCentury := RuntimeState.GetValue<LOGIC>(Set.Century) AS LOGIC

		LOCAL lcDatePart := GetEmptyDatePart(lcDateFormat, lCentury) AS STRING
		LOCAL lcTimePart := GetEmptyTimePart(lHours, lSeconds) AS STRING

	//	return i"{lcDatePart} {lcTimePart}"
		RETURN lcDatePart + " " + lcTimePart
	END METHOD

	STATIC METHOD FormatTimeOnly(tdExpression AS System.DateTime) AS STRING
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
	END METHOD

	STATIC METHOD FormatDatePart(tdExpression AS System.DateTime, lCentury AS LOGIC, nDateFormat AS DWORD) AS STRING
		LOCAL culture := CultureInfo.InvariantCulture AS CultureInfo
		LOCAL lcSeparator := "/" AS STRING
		LOCAL lcYearFormat AS STRING

		lcYearFormat := IIF(lCentury, "yyyy", "yy")

		SWITCH (DateCountry) nDateFormat
		// ANSI
        CASE DateCountry.Ansi
			lcSeparator := "."
			RETURN tdExpression:ToString(i"{lcYearFormat}{lcSeparator}MM{lcSeparator}dd", culture)
		// BRITISH, FRENCH
        CASE DateCountry.British
        CASE DateCountry.French
        CASE DateCountry.DMY
			lcSeparator := "/"
			RETURN tdExpression:ToString(i"dd{lcSeparator}MM{lcSeparator}{lcYearFormat}", culture)
		// GERMAN
        CASE DateCountry.German
			lcSeparator := "."
			RETURN tdExpression:ToString(i"dd{lcSeparator}MM{lcSeparator}{lcYearFormat}", culture)
		// ITALIAN
        CASE DateCountry.Italian //CASE DateCountry.Dutch
			lcSeparator := "-"
			RETURN tdExpression:ToString(i"dd{lcSeparator}MM{lcSeparator}{lcYearFormat}", culture)
		// JAPAN, TAIWAN
        CASE DateCountry.Japanese
        CASE DateCountry.Taiwan
			lcSeparator := "/"
			RETURN tdExpression:ToString(i"{lcYearFormat}{lcSeparator}MM{lcSeparator}dd", culture)
        CASE DateCountry.American
        CASE DateCountry.MDY
        CASE DateCountry.USA
        CASE DateCountry.System // CASE DateCountry.Windows
		OTHERWISE
			// AMERICAN by default (MM/DD/YY[YY])
			lcSeparator := "/"
			RETURN tdExpression:ToString(i"MM{lcSeparator}dd{lcSeparator}{lcYearFormat}", culture)
		END SWITCH
	END METHOD

	STATIC METHOD GetEmptyDatePart(lcDateFormat AS STRING, lCentury AS LOGIC) AS STRING
		LOCAL lcSeparator := "/" AS STRING
		LOCAL lcYear := IIF(lCentury, "    ", "  ") AS STRING

		SWITCH lcDateFormat:ToUpper()
		// ANSI, GERMAN
		CASE "YY.MM.DD"
		CASE "YYYY.MM.DD"
		CASE "DD.MM.YY"
		CASE "DD.MM.YYYY"
			lcSeparator := "."
			RETURN i"{lcYear}{lcSeparator}  {lcSeparator}  "
		// ITALIAN
		CASE "DD-MM-YY"
		CASE "DD-MM-YYYY"
			lcSeparator := "-"
			RETURN i"  {lcSeparator}  {lcSeparator}{lcYear}"
		OTHERWISE
			RETURN i"  {lcSeparator}  {lcSeparator}{lcYear}"
		END SWITCH
	END METHOD

	STATIC METHOD GetEmptyTimePart(lHours AS LONG, lSeconds AS LOGIC) AS STRING
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
	END METHOD
END CLASS
