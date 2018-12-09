//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


/// <summary>
/// Convert a 24-hour military time to a 12-hour clock time.
/// </summary>
/// <param name="cTime"> A valid military time in the form hh:mm:ss, where hh is hours in 24-hour format, mm is minutes, and ss is seconds.</param>
/// <returns>
/// An 11-character string in 12-hour format with either "am" or "pm."  If cTime does not represent a valid military time, a String.Empty is returned.
/// </returns>
FUNCTION AmPm(cTime AS STRING) AS STRING
	LOCAL nSeconds AS DWORD
	LOCAL nHours AS DWORD
	LOCAL nMinutes AS DWORD
	IF String.IsNullOrEmpty(cTime)
		RETURN ""
	ENDIF
	nSeconds := Secs(cTime) 
	IF (nSeconds) > 86400
		RETURN ""
	ENDIF	
	nSeconds := nSeconds % 86400
	nHours   := nSeconds / 3600
	nSeconds := nSeconds % 3600
	nMinutes := nSeconds / 60
	nSeconds := nSeconds % 60
	RETURN _TimeString(nHours, nMinutes, nSeconds, TRUE, GetAmExt(), GetPmExt())


/// <summary>
/// Return the difference between two time strings.
/// </summary>
/// <param name="cStartTime">The starting time in the form HH:mm:ss.</param>
/// <param name="cEndTime">The ending time in the form HH:mm:ss.</param>
/// <returns>
/// The amount of time that has elapsed from cStartTime to cEndTime as a time string in the format hh:mm:ss.
/// </returns>
/// <remarks>
/// </remarks>
FUNCTION ElapTime(cStartTime AS STRING,cEndTime AS STRING) AS STRING
	LOCAL nStart AS DWORD
	LOCAL nEnd   AS DWORD
	LOCAL nDiff  AS DWORD
	nStart := Secs(cStartTime)
	nEnd   := Secs(cEndTime)
	IF nStart > nEnd
		nDiff := 86400 + nEnd - nStart
	ELSE
		nDiff := nEnd - nStart
	ENDIF
	RETURN TString(nDiff)

/// <summary>
/// Format a set of numbers representing an hour, minute, and second as a time string.
/// </summary>
/// <param name="dwHour"></param>
/// <param name="dwMinute"></param>
/// <param name="dwSeconds"></param>
/// <returns>
/// </returns>
FUNCTION ConTime(dwHour AS DWORD,dwMinute AS DWORD,dwSeconds AS DWORD) AS STRING
   RETURN _TimeString( dwHour, dwMinute, dwSeconds, FALSE, "", "" )


/// <summary>
/// Return the timestring from a DateTime structure
/// </summary>
/// <param name="dt"></param>
/// <returns>
/// </returns>
FUNCTION ConTime(dt AS DateTime) AS STRING
	RETURN _TimeString((DWORD) dt:Hour,(DWORD) dt:Minute,(DWORD) dt:Second, FALSE, "","")   

/// <summary>
/// Convert the number that identifies a day into the name of the day.
/// </summary>
/// <param name="dwDay">A number from 1 to 7.</param>
/// <returns>
/// </returns>
FUNCTION NToCDoW(dwDay AS DWORD) AS STRING
	LOCAL result AS STRING
	IF dwDay < 1 .OR. dwDay > 7
		result := ""
	ELSEIF RuntimeState.International == CollationMode.Clipper
		result := __CavoStr(VOErrors.RT_MSG_DAY1 + dwDay -1)
	ELSE
		VAR culture := System.Globalization.CultureInfo.CurrentCulture 
		result := culture:DateTimeFormat:GetDayName((DayOfWeek) (dwDay-1))   
	ENDIF
	RETURN result
/// <summary>
/// Convert the number that identifies a month into the name of the month.
/// </summary>
/// <param name="dwMonth"></param>
/// <returns>
/// </returns>
FUNCTION NToCMonth(dwMonth AS DWORD) AS STRING
	LOCAL result AS STRING
	IF dwMonth < 1 .OR. dwMonth > 12
		result := ""
	ELSEIF RuntimeState.International == CollationMode.Clipper
		result := __CavoStr(VOErrors.RT_MSG_MONTH1 + dwMonth -1)
	ELSE
		VAR culture := System.Globalization.CultureInfo.CurrentCulture 
		result := culture:DateTimeFormat:GetMonthName((INT)dwMonth)   
	ENDIF
	RETURN result

/// <summary>
/// Return a time as the number of seconds that have elapsed since midnight.
/// </summary>
/// <param name="cTime">The time to convert to seconds, in the form hh:mm:ss.</param>
/// <returns>The number of seconds from midnight to the time specified.  The return value cannot be greater than 86,400, the number of seconds in a day.
/// </returns>
FUNCTION Secs(cTime AS STRING) AS DWORD
	LOCAL cSeparator AS STRING
	LOCAL nHours AS INT
	LOCAL nMinutes AS INT
	LOCAL nSeconds AS INT
	LOCAL nExpectedLength AS INT
	LOCAL result AS DWORD
	IF String.IsNullOrEmpty(cTime)
		RETURN 0
	ENDIF
	cSeparator := chr(GetTimeSep())
	nExpectedLength := 6 + 2 * cSeparator:Length
	IF cTime:Length >= nExpectedLength
		LOCAL nOffSet := 0 AS INT
		TRY
			nHours   := Int32.Parse(cTime:Substring(nOffSet,2))
			nOffSet += cSeparator:Length +2
			nMinutes := Int32.Parse(cTime:Substring(nOffSet,2))
			nOffSet += cSeparator:Length +2
			nSeconds := Int32.Parse(cTime:Substring(nOffSet,2))
			result := (DWORD) (nHours * 3600 + nMinutes * 60 + nSeconds)
		CATCH 
			result := 0
		END TRY
	ELSE
		result := 0
	ENDIF
	RETURN result


/// <summary>
/// Convert a specified number of seconds to days.
/// </summary>
// <param name="nSeconds">The number of seconds to convert to days.</param>
/// <returns>The number of days to the nearest day.</returns>
FUNCTION Days(nSeconds AS REAL8) AS INT
   RETURN (INT) (nSeconds / 84600) // 24*60*60

/// <summary>
/// Return the number of seconds that have elapsed since midnight.
/// </summary>
/// <returns>The number of seconds that have elapsed since midnight in the form seconds.hundredths.  Numbers range from 0 to 86,399.</returns>
FUNCTION Seconds() AS REAL8
	VAR dt := DateTime.Now
	RETURN dt:Hour * 3600 + dt:Minute * 60 + dt:Second + Math.Round( (REAL8) dt:Millisecond/1000,2)



/// <summary>
/// Return the system time in a format determined by various international settings.
/// </summary>
/// <returns>
/// </returns>
FUNCTION Time() AS STRING
   VAR d := DateTime.Now 
   RETURN _TimeString(d)

/// <summary>
/// Return the system time in 24-hour format.
/// </summary>
/// <returns>
/// </returns>
FUNCTION Time24() AS STRING
   LOCAL d := DateTime.Now AS DateTime
   RETURN _TimeString((DWORD) d:Hour,(DWORD) d:Minute,(DWORD) d:Second,FALSE,"","")


/// <summary>Convert a specified number of seconds to a time string.</summary>
FUNCTION TString(fSeconds AS REAL8) AS STRING
   RETURN TString( (DWORD) Math.Round( fSeconds, MidpointRounding.ToEven ) )   

/// <summary>Convert a specified number of seconds to a time string.</summary>
FUNCTION TString(dwSeconds AS DWORD) AS STRING
   LOCAL dwHours AS DWORD
   LOCAL dwMinutes AS DWORD
   // truncate to one day
   dwSeconds := dwSeconds % (24 * 60 * 60)
   dwHours   := dwSeconds / (60 * 60)
   dwSeconds := dwSeconds % (60 * 60)
   dwMinutes := dwSeconds / 60 
   dwSeconds := dwSeconds % 60 
   RETURN _TimeString(dwHours, dwMinutes, dwSeconds, GetAmPm(), GetAMExt(), GetPMExt())


INTERNAL FUNCTION _TimeString( d AS DateTime ) AS STRING	
   RETURN _TimeString( (DWORD) d:Hour, (DWORD) d:Minute, (DWORD) d:Second, SetAMPM(), GetAMExt(), GetPMExt() )


INTERNAL FUNCTION _TimeString( h AS DWORD, m AS DWORD, s AS DWORD, lAMPM AS LOGIC, cAM AS STRING, cPM AS STRING ) AS STRING	
   LOCAL cTimeSep   AS STRING
   LOCAL lAfternoon AS LOGIC
   // no exceptions, vo simply returns an empty string
   IF h < 0 || h > 23
      RETURN ""
   ELSEIF m < 0 || m > 59
      RETURN ""
   ELSEIF s < 0 || s > 59
      RETURN ""
   ENDIF
   
   IF s == 60
      s := 0
      m += 1
      IF m == 60
         m := 0
         h += 1
      ENDIF
      IF h == 24
         h := 0
      ENDIF
   ENDIF
   
   cTimeSep := Chr( GetTimeSep() )
   
   lAfternoon := h >= 12
   
   IF lAMPM 
      IF h > 12
         h -= 12
      ELSEIF h == 0
         h := 12
      ENDIF    
   ENDIF
   
   RETURN String.Format( "{0:00}{3}{1:00}{3}{2:00}{4}", h, m, s, cTimeSep, IIF( lAMPM, IIF( lAfternoon, cPM, cAM ), "" ) )



/// <summary>
/// Format a set of numbers representing a year, month, and day as a Date.
/// </summary>
/// <param name="dwY">A valid year.  If the century digits are not specified, the century is determined by the rules of SetEpoch(). </param>
/// <param name="dwM">A number from 1 through 12 representing a valid month. </param>
/// <param name="dwDay">A number representing a valid day of dwMonth.</param>
/// <returns>The date that corresponds to the passed arguments.  If any of the arguments specified do not represent a valid year, month, or day, a NULL_DATE is returned.</returns>
FUNCTION _ConDate(dwY AS DWORD,dwM AS DWORD,dwDay AS DWORD) AS DateTime
	IF dwY < 100
		LOCAL lAfter AS LOGIC
		lAfter := dwY > XSharp.RuntimeState.EpochYear
		dwY += XSharp.RuntimeState.EpochCent
		IF lAfter
			dwY -= 100
		ENDIF
   ENDIF
   TRY
	    RETURN DateTime{(INT) dwY,(INT) dwM,(INT) dwDay}
   CATCH
        RETURN DateTime.MinValue
   END TRY

FUNCTION _CToD(cDate AS STRING, cDateFormat AS STRING) AS DateTime
	LOCAL dDate AS DateTime
	LOCAL nDay, nMonth, nYear AS DWORD
	LOCAL nDayPos, nMonthPos, nYearPos AS INT
	dDate := DateTime.MinValue
	IF string.IsNullOrEmpty(cDate) .OR. String.IsNullOrEmpty(cDateFormat) .OR. cDate == RuntimeState.GetValue<STRING>(Set.DateFormatEmpty)
		RETURN dDate
	ENDIF
	LOCAL nPos AS INT
	LOCAL cSep AS STRING
	nDayPos := nMonthPos := nYearPos := -1
	cSep := "./-"
	nPos :=-1
	FOREACH c AS CHAR IN cDateFormat
		SWITCH c
		CASE 'D'
			IF nDayPos == -1
				++nPos
				nDayPos  := nPos
			ENDIF
		CASE 'M'
			IF nMonthPos == -1
				++nPos
				nMonthPos  := nPos
			ENDIF
		CASE 'Y'
			IF nYearPos == -1
				++nPos
				nYearPos  := nPos
			ENDIF
		OTHERWISE
			IF cSep:IndexOf(c) == -1
				cSep += c:ToString()
			ENDIF
		END SWITCH
	NEXT
	IF nDayPos == -1 .OR. nMonthPos == -1 .OR. nYearPos == -1
		RETURN dDate
	ENDIF
	TRY
		// we now know the seperators and the positions in the string
		LOCAL aNums := cDate:Split(cSep:ToCharArray()) AS STRING[]
		IF UInt32.TryParse(aNums[nDayPos], OUT nDay) .AND. ;
    		UInt32.TryParse(aNums[nMonthPos], OUT nMonth) .AND. ;
	    	UInt32.TryParse(aNums[nYearPos], OUT nYear)
		    IF aNums[nYearPos]:Length < 4
			    // Century missing ?
			    dDate := _ConDate(nYear, nMonth, nDay)
		    ELSE
			    dDate := DateTime{(INT)nYear, (INT)nMonth, (INT)nDay}
            ENDIF
        ELSE
            dDate := DateTime.MinValue
        ENDIF
	CATCH 
		dDate := DateTime.MinValue
	END TRY
	RETURN dDate
  
/// <summary>
/// Convert an ANSI date string to date format.
/// </summary>
/// <param name="cDate">A string in the ANSI form yyyy.mm.dd, where yy, mm, and dd represent year, month, and day respectively.  
/// The year, month, and day can be separated by any character other than a number. 
/// cDate is always interpreted as an ANSI string and is not dependent on SetDateFormat() or SetDateCountry().  
/// If the century digits are not specified, the century is determined by the rules of SetEpoch().</param>
/// <returns>The date value that corresponds to the numbers specified in <paramref name="cDate"/>.  If cDate is not a valid ANSI date, CToDAnsi() returns a NULL_DATE.
/// </returns>
FUNCTION _CToDAnsi(cDate AS STRING) AS DateTime
	RETURN _CToD(cDate, "YYYY.MM.DD")


/// <summary>
/// Convert a Date to a string.
/// </summary>
/// <param name="d">The Date to be converted.</param>
/// <returns>
/// A string representation of the given Date, formatted in the current Date format.
/// </returns>
FUNCTION _DToC(d AS DateTime) AS STRING
	LOCAL result:="" AS STRING		
	LOCAL cFormat := XSharp.RuntimeState.GetValue<STRING>(Set.DateFormatNet) AS STRING
	IF d != DateTime.Minvalue
		LOCAL dt := d AS Datetime
		result := d:ToString(cFormat)
	ELSE
		result := RuntimeState.NullDateString
	ENDIF
	RETURN result 

/// <summary>
/// Convert a Date value to a string formatted as string in ANSI format
/// </summary>
/// <param name="dDate">The Date to be converted</param>
/// <returns>
/// An 8-character string in the format yyyymmdd.  If dDate is a NULL_DATE, a string of eight spaces is returned.  The return value is not affected by the current date format.
/// </returns>
FUNCTION _DToS(dDate AS DateTime) AS STRING
	LOCAL result:="        " AS STRING		
	IF dDate != DateTime.MinValue
		result := dDate:ToString("yyyyMMdd")
	ENDIF
	RETURN result 
