//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using XSharp
/// <summary>
/// Convert a string containing a 32-bit binary Date to a Date data type.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
function Bin2Date(c as string) as DATE
	return (DATE)(DWORD) Bin2L( c )



/// <summary>
/// Extract the name of the day of the week from a Date.
/// </summary>
/// <param name="d">The Date to calculate the day of week from.</param>
/// <returns>
/// A string for the calculated day of the week.
/// </returns>
function CDoW(d as DATE) as string		
	local result := String.Empty as string
	if d != null
		local dt := d as Datetime
		result := dt:ToString("dddd")
	endif
	return result

/// <summary>
/// Extract the name of the month from a Date.
/// </summary>
/// <param name="d">The Date to calculate the month from.</param>
/// <returns>
/// A string with the name of the month.
/// </returns>
function CMonth(d as DATE) as string
	return NTOCMonth( d:DMonth)

/// <summary>
/// Format a set of numbers representing a year, month, and day as a Date.
/// </summary>
/// <param name="dwY"></param>
/// <param name="dwM"></param>
/// <param name="dwDay"></param>
/// <returns>
/// </returns>
function ConDate(dwY as dword,dwM as dword,dwDay as dword) as date
	if dwY < 100
		local lAfter as logic
		lAfter := dwY > XSharp.RuntimeState.EpochYear
		dwY += XSharp.RuntimeState.EpochCent
		if lAfter
			dwY -= 100
		endif
	endif
	return Date{dwY,dwM,dwDay}   

/// <summary>
/// Convert a Date string to DATE format.
/// </summary>
/// <param name="cDate"></param>
/// <returns>
/// </returns>
function CToD(cDate as string) as DATE
	return CTOD(cDate, XSharp.RuntimeState.DateFormat)

function CToD(cDate as string, cDateFormat as string) as date
	local dDate as date
	local nDay, nMonth, nYear as dword
	local nDayPos, nMonthPos, nYearPos as int
	dDate := (DATE) 0
	if string.IsNullOrEmpty(cDate) .or. String.IsNullOrEmpty(cDateFormat)
		return dDate
	endif
	local nPos as int
	local cSep as string
	nDayPos := nMonthPos := nYearPos := 0
	cSep := "./-"
	nPos := 0
	foreach c as char in cDateFormat
		switch c
		case 'D'
			if nDayPos == 0
				++nPos
				nDayPos  := nPos
			endif
		case 'M'
			if nMonthPos == 0
				++nPos
				nMonthPos  := nPos
			endif
		case 'Y'
			if nYearPos == 0
				++nPos
				nYearPos  := nPos
			endif
		otherwise
			if cSep:IndexOf(c) == -1
				cSep += c:ToString()
			endif
		end switch
	next
	if nDayPos == 0 .or. nMonthPos == 0 .or. nYearPos == 0
		return dDate
	endif
	try
		// we now know the seperators and the positions in the string
		local aNums := cDate:Split(cSep:ToCharArray()) as string[]
		nDay   := Uint32.Parse(aNums[nDayPos])
		nMonth := Uint32.Parse(aNums[nMonthPos])
		nYear  := Uint32.Parse(aNums[nYearPos])
		if aNums[nYearPos]:Length < 4
			// Century missing ?
			dDate := ConDate(nYear, nMonth, nDay)
		else
			dDate := date{nYear, nMonth, nDay}
		endif
	catch 
		dDate := (DATE) 0
	end try
	return dDate


/// <summary>
/// Convert an ANSI Date string to Date format.
/// </summary>
/// <param name="cDate"></param>
/// <returns>
/// </returns>
function CToDAnsi(cDate as string) as DATE
	return CTOD(cDate, "YYYY.MM.DD")


/// <summary>
/// Convert a Date to a 32-bit binary Date string.
/// </summary>
/// <param name="d"></param>
/// <returns>
/// </returns>
function Date2Bin(d as date) as string
	return L2Bin((Long) (Date) d)
 

/// <summary>
/// Extract the number of the day of the month from a Date.
/// </summary>
/// <param name="d">The Date to extract the day from.</param>
/// <returns>
/// The day part of the given DATE.
/// </returns>
function Day(d as DATE) as dword
	local day := 0  as dword
	if ! d:IsEmpty
		day :=  d:DDay
	endif
	return day

/// <summary>
/// Extract the number of the day of the week from a Date.
/// </summary>
/// <param name="d">The Date to extract the day of the week from.</param>
/// <returns>
/// The day of the week of the given DATE.
/// </returns>
function DoW(d as DATE) as dword
	local day := 0  as dword
	if ! d:IsEmpty
		local dt := d as Datetime
		day := (dword) dt:DayOfWeek	   
	endif
	return day


/// <summary>
/// Convert a Date to a string.
/// </summary>
/// <param name="d">The Date to be converted.</param>
/// <returns>
/// A string representation of the given Date, formatted in the current Date format.
/// </returns>
function DToC(d as DATE) as string
	local result:="" as string		
	local cFormat := XSharp.RuntimeState.GetValue<String>(Set.DateFormatNet) as string
	if ! d:IsEmpty
		local dt := d as Datetime
		result := d:ToString(cFormat)
	else
		result := XSharp.RuntimeState.GetValue<String>(Set.DateFormatEmpty) 
	endif
	return result 

/// <summary>
/// Convert a Date value to a string formatted as string in ANSI format
/// </summary>
/// <param name="d">The Date to be converted</param>
/// <returns>
/// The given Date as string in ANSI format
/// </returns>
function DToS(d as DATE) as string
	local result:="        " as string		
	if ! d:IsEmpty
		local dt := d as Datetime
		result := d:ToString("yyyyMMdd")
	endif
	return result 

/// <summary>
/// </summary>
/// <param name="d"></param>
/// <returns>
/// </returns>
function JCDOW(d as DATE) as string
	/// THROW NotImplementedException{}
	return	 String.Empty   

/// <summary>
/// </summary>
/// <param name="d"></param>
/// <returns>
/// </returns>
function JCMONTH(d as DATE) as string
	/// THROW NotImplementedException{}
	return	 String.Empty   

/// <summary>
/// </summary>
/// <param name="d"></param>
/// <returns>
/// </returns>
function JCYEAR(d as DATE) as string
	/// THROW NotImplementedException{}
	return	 String.Empty   

/// <summary>
/// Extract the number of the month from a DATE.
/// </summary>
/// <param name="d">The Date to extract the month from.</param>
/// <returns>
/// The month of the given Date.
/// </returns>
function Month(d as DATE) as dword
	local month := 0  as dword
	if !d:IsEmpty
		month :=  d:DMonth
	endif
	return month

	/// <summary>
	/// Return the number of seconds that have elapsed since midnight.
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION Seconds() AS __VOFloat
		/// THROW NotImplementedException{}
	RETURN 0   




/// <summary>
/// Convert an ANSI Date string to Date format.
/// </summary>
/// <param name="cDate"></param>
/// <returns>
/// </returns>
function SToD(cDate as string) as DATE
	local convertedDate as DATE
	try
		convertedDate := (DATE)DateTime.ParseExact(cDate, "yyyyMMdd", System.Globalization.CultureInfo.InvariantCulture)
	catch
		convertedDate := DATE{} 
	end try
	return	 convertedDate


/// <summary>
/// Return the system Date as a Date value.
/// </summary>
/// <returns>
/// </returns>
function Today() as DATE
	return (DATE) DateTime.Now


/// <summary>
/// Return the system time in a format determined by various international settings.
/// </summary>
/// <returns>
/// </returns>
function Time() as string
	/// THROW NotImplementedException{}
	return String.Empty   

/// <summary>
/// Return the system time in 24-hour format.
/// </summary>
/// <returns>
/// </returns>
function Time24() as string
	/// THROW NotImplementedException{}
	return String.Empty   



/// <summary>
/// Convert a specified number of seconds to a time string.
/// </summary>
/// <param name="uSeconds"></param>
/// <returns>
/// </returns>
function TString(uSeconds as __Usual) as string
	/// THROW NotImplementedException{}
	return String.Empty   


/// <summary>
/// Extract the number of the year from a DATE.
/// </summary>
/// <param name="d">The DATE to extract the year from.</param>
/// <returns>
/// The year from the give DATE.
/// </returns>
function Year(d as Date) as dword
	local year := 0  as dword
	if ! d:IsEmpty
		year := d:DYear
	endif
	return year



