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
/// An 11-character string in 12-hour format with either "am" or "pm."  If <cTime> does not represent a valid military time, a String.Empty is returned.
/// </returns>
FUNCTION AmPm(cTime AS STRING) AS STRING
	LOCAL result:=NULL AS STRING
	TRY 
		result := DateTime.Parse(ctime):ToString("hh:mm:ss")
		// The following exceptions may appear but will be ignored currently (VO/VN behaviour)
		// catch ex as FormatException
		//	  NOP 
		// catch ex as ArgumentNullException
		//	  NOP
	END TRY
	RETURN result

/// <summary>
/// Return the difference between two time strings.
/// </summary>
/// <param name="cStartTime">The starting time in the form HH:mm:ss.</param>
/// <param name="cEndTime">The ending time in the form HH:mm:ss.</param>
/// <returns>
/// The amount of time that has elapsed from cStartTime to cEndTime as a time string in the format hh:mm:ss.
/// </returns>
/// <remarks>
/// The behaviour is not compatible with VO ! Needs to be refactured.
/// </remarks>
FUNCTION ElapTime(cStartTime AS STRING,cEndTime AS STRING) AS STRING
	LOCAL elapedTime := string.Empty AS STRING
	/// TODO: VO compatibility 
	TRY
		elapedTime := DateTime.ParseExact(cEndTime, "HH:mm:ss", System.Globalization.CultureInfo.InvariantCulture);
		:Subtract(DateTime.ParseExact(cStartTime, "HH:mm:ss", System.Globalization.CultureInfo.InvariantCulture));
		:ToString()
	CATCH 
		NOP
	END TRY
	RETURN elapedTime



/// <summary>
/// Format a set of numbers representing an hour, minute, and second as a time string.
/// </summary>
/// <param name="dwHour"></param>
/// <param name="dwMinute"></param>
/// <param name="dwSeconds"></param>
/// <returns>
/// </returns>
FUNCTION ConTime(dwHour AS DWORD,dwMinute AS DWORD,dwSeconds AS DWORD) AS STRING
	LOCAL ts AS TimeSpan
	ts := TimeSpan.FromSeconds(dwHour*60*60+dwMinute*60+dwSeconds)		
	RETURN ts:ToString()   



/// <summary>
/// Convert the number that identifies a day into the name of the day.
/// </summary>
/// <param name="dwDay"></param>
/// <returns>
/// </returns>
FUNCTION NToCDoW(dwDay AS DWORD) AS STRING
	LOCAL culture := System.Globalization.CultureInfo.CurrentCulture AS System.Globalization.CultureInfo
	RETURN culture:DateTimeFormat:GetDayName((System.DayOfWeek)dwDay)

/// <summary>
/// Convert the number that identifies a month into the name of the month.
/// </summary>
/// <param name="dwMonth"></param>
/// <returns>
/// </returns>
FUNCTION NToCMonth(dwMonth AS DWORD) AS STRING
	LOCAL culture := System.Globalization.CultureInfo.CurrentCulture AS System.Globalization.CultureInfo
	RETURN culture:DateTimeFormat:GetMonthName((INT)dwMonth)   

/// <summary>
/// Return a time as the number of seconds that have elapsed since midnight.
/// </summary>
/// <param name="cTime"></param>
/// <returns>
/// </returns>
FUNCTION Secs(cTime AS STRING) AS DWORD
	/// THROW NotImplementedException{}
	RETURN 0   
