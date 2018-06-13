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
	local nSeconds as dword
	local nHours as dword
	local nMinutes as dword
	if String.IsNullOrEmpty(cTime)
		return ""
	endif
	nSeconds := Secs(cTime) 
	if (nSeconds) > 86400
		return ""
	endif	
	nSeconds := nSeconds % 86400
	nHours   := nSeconds / 3600
	nSeconds := nSeconds % 3600
	nMinutes := nSeconds / 60
	nSeconds := nSeconds % 60
	return _TimeString(nHours, nMinutes, nSeconds, true, GetAmExt(), GetPmExt())


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
	local nStart as dword
	local nEnd   as dword
	local nDiff  as dword
	nStart := Secs(cStartTime)
	nEnd   := Secs(cEndTime)
	if nStart > nEnd
		nDiff := 86400 + nEnd - nStart
	else
		nDiff := nEnd - nStart
	endif
	return TString(nDiff)

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
	IF dwDay < 1 .or. dwDay > 7
		result := ""
	ELSEIF RuntimeState.International == CollationMode.Clipper
		result := __CavoStr(VOErrors.RT_MSG_DAY1 + dwDay -1)
	ELSE
		var culture := System.Globalization.CultureInfo.CurrentCulture 
		result := culture:DateTimeFormat:GetDayName((DayOfWeek) dwDay-1)   
	ENDIF
	return result
/// <summary>
/// Convert the number that identifies a month into the name of the month.
/// </summary>
/// <param name="dwMonth"></param>
/// <returns>
/// </returns>
FUNCTION NToCMonth(dwMonth AS DWORD) AS STRING
	LOCAL result AS STRING
	IF dwMonth < 1 .or. dwMonth > 12
		result := ""
	ELSEIF RuntimeState.International == CollationMode.Clipper
		result := __CavoStr(VOErrors.RT_MSG_MONTH1 + dwMonth -1)
	else
		var culture := System.Globalization.CultureInfo.CurrentCulture 
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
	local cSeparator as string
	local nHours as int
	local nMinutes as int
	local nSeconds as int
	local nExpectedLength as int
	local result as dword
	if String.IsNullOrEmpty(cTime)
		return 0
	endif
	cSeparator := chr(GetTimeSep())
	nExpectedLength := 6 + 2 * cSeparator:Length
	if cTime:Length >= nExpectedLength
		local nOffSet := 0 as int
		try
			nHours   := Int32.Parse(cTime:Substring(nOffSet,2))
			nOffSet += cSeparator:Length +2
			nMinutes := Int32.Parse(cTime:Substring(nOffSet,2))
			nOffSet += cSeparator:Length +2
			nSeconds := Int32.Parse(cTime:Substring(nOffSet,2))
			result := (DWORD) nHours * 3600 + nMinutes * 60 + nSeconds
		catch 
			result := 0
		end try
	else
		result := 0
	endif
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
FUNCTION Seconds() AS Real8
	var dt := DateTime.Now
	return dt:Hour * 3600 + dt:Minute * 60 + dt:Second + Math.Round( (real8) dt:Millisecond/1000,2)



/// <summary>
/// Return the system time in a format determined by various international settings.
/// </summary>
/// <returns>
/// </returns>
function Time() as string
   var d := DateTime.Now 
   RETURN _TimeString(d)

/// <summary>
/// Return the system time in 24-hour format.
/// </summary>
/// <returns>
/// </returns>
function Time24() as string
   LOCAL d := DateTime.Now AS DateTime
   RETURN _TimeString((DWORD) d:Hour,(DWORD) d:Minute,(DWORD) d:Second,FALSE,"","")



function TString(fSeconds as Real8) as string
   RETURN TString( (DWORD) Math.Round( fSeconds, MidpointRounding.ToEven ) )   

function TString(dwSeconds as DWORD) as string
   local dwHours as dword
   local dwMinutes as dword
   // truncate to one day
   dwSeconds := dwSeconds % (24 * 60 * 60)
   dwHours   := dwSeconds / (60 * 60)
   dwSeconds := dwSeconds % (60 * 60)
   dwMinutes := dwSeconds / 60 
   dwSeconds := dwSeconds % 60 
   return _TimeString(dwHours, dwMinutes, dwSeconds, GetAmPm(), GetAMExt(), GetPMExt())


internal FUNCTION _TimeString( d AS DateTime ) AS STRING	
   RETURN _TimeString( (DWORD) d:Hour, (DWORD) d:Minute, (DWORD) d:Second, SetAMPM(), GetAMExt(), GetPMExt() )


internal FUNCTION _TimeString( h AS DWORD, m AS DWORD, s AS DWORD, lAMPM AS LOGIC, cAM AS STRING, cPM AS STRING ) AS STRING	
   LOCAL cTimeSep   AS STRING
   LOCAL lAfternoon AS LOGIC
   // no exceptions, vo simply returns an empty string
   IF h < 0 || h > 23
      return ""
   ELSEIF m < 0 || m > 59
      return ""
   ELSEIF s < 0 || s > 59
      return ""
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

