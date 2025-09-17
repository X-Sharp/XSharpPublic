//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Globalization
using System.Collections.Generic

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
    RETURN GoMonth ( (DateTime) dExpression , iNumberOfMonths )

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/gomonth/*" />
FUNCTION GoMonth( tExpression AS DateTime , iNumberOfMonths AS INT ) AS DATE
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

INTERNAL FUNCTION _DateTimeError( sParameter as STRING, argNum as DWORD, aArgs PARAMS OBJECT[]) AS Error
    var err := Error.ArgumentError(ProcName(1), sParameter,__VfpStr(VFPErrors.VFP_INVALID_PARAMETER, sParameter, "'DATE' or 'DATETIME'" ), 1)
    err:Args := aArgs
    err:ArgNum := argNum
    err:Stack := ErrorStack(1)
    RETURN err


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/gomonth/*" />
FUNCTION GoMonth( uExpression AS USUAL , iNumberOfMonths AS INT ) AS DATE
    if IsDate(uExpression)
        RETURN GoMonth ( (Date) uExpression , iNumberOfMonths )
    elseif IsDateTime(uExpression)
        RETURN GoMonth ( (DateTime) uExpression , iNumberOfMonths )
    endif
    THROW _DateTimeError(nameof(uExpression), 1, uExpression, iNumberOfMonths)


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/quarter/*" />
FUNCTION Quarter( uExpression  AS USUAL , nMonth  := 1 AS INT ) AS INT
    if IsDate(uExpression)
        RETURN Quarter ( (Date) uExpression , nMonth )
    elseif IsDateTime(uExpression)
        RETURN Quarter ( (DateTime) uExpression , nMonth )
    endif
    THROW _DateTimeError(nameof(uExpression),1, uExpression, nMonth)


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/quarter/*" />
FUNCTION Quarter( dExpression  AS DATE , nMonth  := 1 AS INT ) AS INT
    RETURN Quarter ( (DateTime) dExpression  , nMonth  )


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/quarter/*" />
FUNCTION Quarter( tExpression  AS DateTime , nMonth  := 1 AS INT ) AS INT
    IF  ! (nMonth  > 0 .AND. nMonth  < 13 )
        THROW Error.ArgumentError(__ENTITY__, nameof(nMonth), __VfpStr(VFPErrors.VFP_INVALID_RANGE, nameof(nMonth), nMonth , "1-12") ,1)
    ENDIF

   	IF ((DATE) tExpression ):IsEmpty
		RETURN 0
	ENDIF

	VAR dtOffset := tExpression :AddMonths( 1 - (INT) nMonth  )

    RETURN  (INT) System.Math.Ceiling((DECIMAL)dtOffset:Month / 3)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/week/*" />
FUNCTION Week( uExpression AS USUAL, nFirstWeek := 1 AS LONG, nFirstDayOfWeek := 1 AS LONG) AS LONG
    if IsDate(uExpression)
        RETURN Week ( (Date) uExpression , nFirstWeek, nFirstDayOfWeek )
    elseif IsDateTime(uExpression)
        RETURN Week ( (DateTime) uExpression , nFirstWeek, nFirstDayOfWeek )
    endif
    THROW _DateTimeError(nameof(uExpression), 1, uExpression, nFirstWeek,nFirstDayOfWeek)


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/week/*" />
FUNCTION Week( dExpression AS DATE, nFirstWeek := 1 AS LONG, nFirstDayOfWeek := 1 AS LONG) AS LONG
    RETURN Week ((DateTime) dExpression, nFirstWeek, nFirstDayOfWeek)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/week/*" />
FUNCTION Week( tExpression AS DateTime, nFirstWeek := 1 AS LONG, nFirstDayOfWeek := 1 AS LONG) AS LONG
    // validate parameters
	IF  ! (nFirstWeek  >= 0 .AND. nFirstWeek  < 4 )
		THROW ArgumentException { __VfpStr(VFPErrors.VFP_INVALID_RANGE, nameof(nFirstWeek), nFirstWeek ,"1-3"), nameof(nFirstWeek) }
    ENDIF
	IF  ! (nFirstDayOfWeek >= 0 .AND. nFirstDayOfWeek  < 8 )
		THROW ArgumentException { __VfpStr(VFPErrors.VFP_INVALID_RANGE, nameof(nFirstDayOfWeek), nFirstDayOfWeek,"0-7"), nameof(nFirstDayOfWeek) }
    ENDIF
    LOCAL week AS CalendarWeekRule
    SWITCH nFirstWeek
    CASE 0
    CASE 1
        week := CalendarWeekRule.FirstDay
    CASE 2
        week := CalendarWeekRule.FirstFourDayWeek
    CASE 3
    OTHERWISE
        week := CalendarWeekRule.FirstFullWeek
    END SWITCH
    LOCAL day AS DayOfWeek
    IF nFirstDayOfWeek == 0
       day :=  DayOfWeek.Sunday
    ELSE
       day :=  (DayOfWeek) (nFirstDayOfWeek -1)
    ENDIF
    VAR calendar := System.Globalization.GregorianCalendar{}
    RETURN calendar:GetWeekOfYear ( tExpression, week, day)


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/mdy/*" />
FUNCTION MDY ( uExpression AS USUAL ) AS STRING
    if IsDate(uExpression)
        RETURN MDY( (Date) uExpression )
    elseif IsDateTime(uExpression)
        RETURN MDY ( (DateTime) uExpression  )
    endif
    THROW _DateTimeError(nameof(uExpression), 1, uExpression)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/mdy/*" />
FUNCTION MDY ( tExpression AS DateTime ) AS STRING
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
    if IsDate(uExpression)
        RETURN DMY( (Date) uExpression )
    elseif IsDateTime(uExpression)
        RETURN DMY ( (DateTime) uExpression  )
    endif
    THROW _DateTimeError(nameof(uExpression),1, uExpression)


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/dmy/*" />
FUNCTION DMY ( tExpression  AS DateTime ) AS STRING
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
function CToT(cCharacterExpression as string) as DateTime
    if String.IsNullOrEmpty(cCharacterExpression)
        return DateTime.MinValue
    endif

    local lcInput := DataTimeHelper.NormalizeWhitespace(cCharacterExpression) as string
    local ldResult as DateTime

    if DataTimeHelper.TryParseVfpDateTime(lcInput, out ldResult)
        return ldResult
    endif

    if DateTime.TryParse(lcInput, CultureInfo.InvariantCulture, DateTimeStyles.None, out ldResult)
        return ldResult
    endif

    return DateTime.MinValue
endfunc

/// <include file="VFPDocs.xml" path="Runtimefunctions/ttoc/*" />
function TToC(tdExpression as DateTime, nParam := 0 as long) as string
	local lcResult as string
	local culture := CultureInfo.InvariantCulture as CultureInfo

	// handle empty date
	if tdExpression == DateTime.MinValue //|| tdExpression == DateTime{}
		switch nParam
		case 0
		case 1
			return "00000000000000"
		case 2
			return DataTimeHelper.GetEmptyTimeFormat()
		case 3
			return "0000-00-00T00:00:00"
		otherwise
			return DataTimeHelper.GetEmptyDefaultFormat()
		end switch
	endif

	switch nParam
	case 0
	case 1
		lcResult := tdExpression:ToString("yyyyMMddHHmmss", culture)
	case 2
		lcResult := DataTimeHelper.FormatTimeOnly(tdExpression)
	case 3
		lcResult := tdExpression:ToString("yyyy-MM-ddTHH:mm:ss", culture)
	otherwise
		lcResult := tdExpression:ToString("yyyyMMddHHmmss", culture)
	end switch

	return lcResult
endfunc

internal static class DataTimeHelper
	static method NormalizeWhitespace(tcInput as string) as string
	    if String.IsNullOrEmpty(tcInput)
	        return tcInput
	    endif

	    local lcResult := tcInput:Trim() as string
	    local aWhitespaceChars := <char>{c' ', c'\t', c'\n', c'\r'} as char[]
	    local lcNormalized := "" as string
	    local lInWhitespace := false as logic

	    foreach cChar as char in lcResult
	        if Array.IndexOf(aWhitespaceChars, cChar) >= 0
	            if !lInWhitespace
	                lcNormalized += " "
	                lInWhitespace := true
	            endif
	        else
	            lcNormalized += cChar:ToString()
	            lInWhitespace := false
	        endif
	    next

	    return lcNormalized:Trim()
	end method

	static method TryParseVfpDateTime(tcInput as string, result out DateTime) as logic
	    local culture := CultureInfo.InvariantCulture as CultureInfo
	    local lHours := RuntimeState.GetValue<long>(Set.Hours) as long
	    local lSeconds := RuntimeState.GetValue<logic>(Set.Seconds) as logic

	    // Generate time formats
	    local aTimeFormats := GetTimeFormatsForParsing(lHours, lSeconds) as string[]
	    local aAllDateFormats := GetAllFlexibleDateFormats() as string[]

	    // vfp format
	    local aSpecialFormats := GetVfpSpecialFormats() as string[]
	    foreach format as string in aSpecialFormats
	        if DateTime.TryParseExact(tcInput, format, culture, DateTimeStyles.None, out result)
	            return true
	        endif
	    next

	    // Just date first
	    foreach dateFormat as string in aAllDateFormats
	        if DateTime.TryParseExact(tcInput, dateFormat, culture, DateTimeStyles.None, out result)
	            return true
	        endif
	    next

	    // date + time (with space)
	    foreach dateFormat as string in aAllDateFormats
	        foreach timeFormat as string in aTimeFormats
	            local combinedFormat := i"{dateFormat} {timeFormat}" as string
	            if DateTime.TryParseExact(tcInput, combinedFormat, culture, DateTimeStyles.None, out result)
	                return true
	            endif
	        next
	    next

	    // date + time (no space)
	    foreach dateFormat as string in aAllDateFormats
	        foreach timeFormat as string in aTimeFormats
	            local combinedFormat := i"{dateFormat}{timeFormat}" as string
	            if DateTime.TryParseExact(tcInput, combinedFormat, culture, DateTimeStyles.None, out result)
	                return true
	            endif
	        next
	    next

	    result := DateTime.MinValue
	    return false
	end method

	static method GetVfpSpecialFormats() as string[]
	    local aFormats := List<string>{} as List<string>

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

	    return aFormats:ToArray()
	end method

	static method GetAllFlexibleDateFormats() as string[]
	    local aFormats := List<string>{} as List<string>
	    local aSeparators := <string>{"/", "-", "."} as string[]
	    local aYearFormats := <string>{"yyyy", "yy"} as string[]

	    foreach separator as string in aSeparators
	        foreach yearFormat as string in aYearFormats
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
	        next
	    next

	    return aFormats:ToArray()
	end method

	static method GetDateSeparatorForFormat(tcDateFormat as string) as string
		switch tcDateFormat:ToUpper()
		// ANSI, GERMAN
		case "YY.MM.DD"
		case "YYYY.MM.DD"
		case "DD.MM.YY"
		case "DD.MM.YYYY"
			return "."
		// ITALIAN
		case "DD-MM-YY"
		case "DD-MM-YYYY"
			return "-"
		otherwise
			return "/"
		end switch
	end method

	static method GetDateFormatsForParsing(tcDateFormat as string, tcSeparator as string, tlCentury as logic) as string[]
	    local aFormats := List<string>{} as List<string>

	    // Include always year formats for flexibility
	    local aYearFormats := <string>{"yyyy", "yy"} as string[]

	    switch tcDateFormat:ToUpper()
	    // AMERICAN
		case "MM/DD/YY"
		case "MM/DD/YYYY"
		// USA, MDY
		case "MM-DD-YY"
		case "MM-DD-YYYY"
	        foreach yearFormat as string in aYearFormats
	            aFormats:Add(i"MM{tcSeparator}dd{tcSeparator}{yearFormat}")
	            aFormats:Add(i"M{tcSeparator}dd{tcSeparator}{yearFormat}")
	            aFormats:Add(i"MM{tcSeparator}d{tcSeparator}{yearFormat}")
	            aFormats:Add(i"M{tcSeparator}d{tcSeparator}{yearFormat}")
	        next
		// BRITISH, FRENCH, DMY
		case "DD/MM/YY"
		case "DD/MM/YYYY"
	        foreach yearFormat as string in aYearFormats
	            aFormats:Add(i"dd{tcSeparator}MM{tcSeparator}{yearFormat}")
	            aFormats:Add(i"d{tcSeparator}MM{tcSeparator}{yearFormat}")
	            aFormats:Add(i"dd{tcSeparator}M{tcSeparator}{yearFormat}")
	            aFormats:Add(i"d{tcSeparator}M{tcSeparator}{yearFormat}")
	        next
		// GERMAN
		case "DD.MM.YY"
		case "DD.MM.YYYY"
		// ITALIAN
		case "DD-MM-YY"
		case "DD-MM-YYYY"
	        foreach yearFormat as string in aYearFormats
	            aFormats:Add(i"dd{tcSeparator}MM{tcSeparator}{yearFormat}")
	            aFormats:Add(i"d{tcSeparator}MM{tcSeparator}{yearFormat}")
	            aFormats:Add(i"dd{tcSeparator}M{tcSeparator}{yearFormat}")
	            aFormats:Add(i"d{tcSeparator}M{tcSeparator}{yearFormat}")
	        next

	    // ANSI
		case "YY.MM.DD"
		case "YYYY.MM.DD"
	    // JAPAN, TAIWAN, YMD
		case "YY/MM/DD"
		case "YYYY/MM/DD"
	        foreach yearFormat as string in aYearFormats
	            aFormats:Add(i"{yearFormat}{tcSeparator}MM{tcSeparator}dd")
	            aFormats:Add(i"{yearFormat}{tcSeparator}M{tcSeparator}dd")
	            aFormats:Add(i"{yearFormat}{tcSeparator}MM{tcSeparator}d")
	            aFormats:Add(i"{yearFormat}{tcSeparator}M{tcSeparator}d")
	        next
	    otherwise
	        // Default to AMERICAN (MM/DD/YY[YY])
	        foreach yearFormat as string in aYearFormats
	            aFormats:Add(i"MM{tcSeparator}dd{tcSeparator}{yearFormat}")
	            aFormats:Add(i"M{tcSeparator}d{tcSeparator}{yearFormat}")
	        next
	    end switch

	    return aFormats:ToArray()
	end method

	static method GetTimeFormatsForParsing(tlHours as long, tlSeconds as logic) as string[]
		local aFormats := List<string>{} as List<string>

		if tlSeconds
	        aFormats:Add("HH:mm:ss")
	        aFormats:Add("H:mm:ss")

	        // Formato 12 horas
	        aFormats:Add("hh:mm:ss tt")
	        aFormats:Add("h:mm:ss tt")
	        aFormats:Add("hh:mm:ss t")
	        aFormats:Add("h:mm:ss t")
		else
	        aFormats:Add("HH:mm")
	        aFormats:Add("H:mm")

	        // Formato 12 horas
	        aFormats:Add("hh:mm tt")
	        aFormats:Add("h:mm tt")
	        aFormats:Add("hh:mm t")
	        aFormats:Add("h:mm t")
		endif

		return aFormats:ToArray()
	end method

	static method FormatVfpDefault(tdExpression as DateTime) as string
		local lcDatePart as string
		local lcTimePart as string
		local culture := CultureInfo.InvariantCulture as CultureInfo

		local lCentury := RuntimeState.GetValue<logic>(Set.Century) as logic
		local lHours := RuntimeState.GetValue<long>(Set.Hours) as long
		local lSeconds := RuntimeState.GetValue<logic>(Set.Seconds) as logic
		local lcDateFormat := RuntimeState.GetValue<string>(Set.DateFormat) as string

		// TODO(irwin): consider using DTOC()
		lcDatePart := FormatDatePart(tdExpression, lCentury, lcDateFormat)

		if lSeconds
			if lHours == 24
				lcTimePart := tdExpression:ToString("HH:mm:ss", culture)
			else
				lcTimePart := tdExpression:ToString("hh:mm:ss tt", culture)
			endif
		else
			if lHours == 24
				lcTimePart := tdExpression:ToString("HH:mm", culture)
			else
				lcTimePart := tdExpression:ToString("hh:mm tt", culture)
			endif
		endif

	//	return i"{lcDatePart} {lcTimePart}"
		return lcDatePart + " " + lcTimePart
	end method

	static method GetEmptyTimeFormat as string
		local lHours := RuntimeState.GetValue<long>(Set.Hours) as long
		local lSeconds := RuntimeState.GetValue<logic>(Set.Seconds) as logic

		if lSeconds
			if lHours == 24
				return "00:00:00"
			else
				return "12:00:00 AM"
			endif
		else
			if lHours == 24
				return "00:00"
			else
				return "12:00 AM"
			endif
		endif
	end method

	static method GetEmptyDefaultFormat as string
		local lcDateFormat := RuntimeState.GetValue<string>(Set.DateFormat) as string
		local lHours := RuntimeState.GetValue<long>(Set.Hours) as long
		local lSeconds := RuntimeState.GetValue<logic>(Set.Seconds) as logic
		local lCentury := RuntimeState.GetValue<logic>(Set.Century) as logic

		local lcDatePart := GetEmptyDatePart(lcDateFormat, lCentury)
		local lcTimePart := GetEmptyTimePart(lHours, lSeconds)

	//	return i"{lcDatePart} {lcTimePart}"
		return lcDatePart + " " + lcTimePart
	end method

	static method FormatTimeOnly(tdExpression as DateTime) as string
		local culture := CultureInfo.InvariantCulture as CultureInfo
		local lHours := RuntimeState.GetValue<long>(Set.Hours) as long
		local lSeconds := RuntimeState.GetValue<logic>(Set.Seconds) as logic

		if lSeconds
			if lHours == 24
				return tdExpression:ToString("HH:mm:ss", culture)
			else
				return tdExpression:ToString("hh:mm:ss tt", culture)
			endif
		else
			if lHours == 24
				return tdExpression:ToString("HH:mm", culture)
			else
				return tdExpression:ToString("hh:mm tt", culture)
			endif
		endif
	end method

	static method FormatDatePart(tdExpression as DateTime, lCentury as logic, lcDateFormat as string) as string
		local culture := CultureInfo.InvariantCulture as CultureInfo
		local lcSeparator := "/" as string
		local lcYearFormat as string

		lcYearFormat := iif(lCentury, "yyyy", "yy")

		switch lcDateFormat:ToUpper()
		// AMERICAN, USA
		case "MM/DD/YY"
		case "MM/DD/YYYY"
			lcSeparator := "/"
			return tdExpression:ToString(i"MM{lcSeparator}dd{lcSeparator}{lcYearFormat}", culture)
		// ANSI
		case "YY.MM.DD"
		case "YYYY.MM.DD"
			lcSeparator := "."
			return tdExpression:ToString(i"{lcYearFormat}{lcSeparator}MM{lcSeparator}dd", culture)
		// BRITISH, FRENCH
		case "DD/MM/YY"
		case "DD/MM/YYYY"
			lcSeparator := "/"
			return tdExpression:ToString(i"dd{lcSeparator}MM{lcSeparator}{lcYearFormat}", culture)
		// GERMAN
		case "DD.MM.YY"
		case "DD.MM.YYYY"
			lcSeparator := "."
			return tdExpression:ToString(i"dd{lcSeparator}MM{lcSeparator}{lcYearFormat}", culture)
		// ITALIAN
		case "DD-MM-YY"
		case "DD-MM-YYYY"
			lcSeparator := "-"
			return tdExpression:ToString(i"dd{lcSeparator}MM{lcSeparator}{lcYearFormat}", culture)
		// JAPAN, TAIWAN
		case "YY/MM/DD"
		case "YYYY/MM/DD"
			lcSeparator := "/"
			return tdExpression:ToString(i"{lcYearFormat}{lcSeparator}MM{lcSeparator}dd", culture)
		otherwise
			// AMERICAN by default (MM/DD/YY[YY])
			lcSeparator := "/"
			return tdExpression:ToString(i"MM{lcSeparator}dd{lcSeparator}{lcYearFormat}", culture)
		end switch
	end method

	static method GetEmptyDatePart(lcDateFormat as string, lCentury as logic) as string
		local lcSeparator := "/" as string
		local lcYear := iif(lCentury, "    ", "  ") as string

		switch lcDateFormat:ToUpper()
		// ANSI, GERMAN
		case "YY.MM.DD"
		case "YYYY.MM.DD"
		case "DD.MM.YY"
		case "DD.MM.YYYY"
			lcSeparator := "."
			return i"{lcYear}{lcSeparator}  {lcSeparator}  "
		// ITALIAN
		case "DD-MM-YY"
		case "DD-MM-YYYY"
			lcSeparator := "-"
			return i"  {lcSeparator}  {lcSeparator}{lcYear}"
		otherwise
			return i"  {lcSeparator}  {lcSeparator}{lcYear}"
		end switch
	end method

	static method GetEmptyTimePart(lHours as long, lSeconds as logic) as string
		if lSeconds
			if lHours == 24
				return "  :  :  "
			else
				return "  :  :   AM"
			endif
		else
			if lHours == 24
				return "  :  "
			else
				return "  :   AM"
			endif
		endif
	end method
end class
