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
	/// THROW NotImplementedException{}
	return	 (DATE)0   



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
	return	 result

/// <summary>
/// Extract the name of the month from a Date.
/// </summary>
/// <param name="d">The Date to calculate the month from.</param>
/// <returns>
/// A string with the name of the month.
/// </returns>
function CMonth(d as DATE) as string
	local result := String.Empty as string
	if d != null
		local dt := d as Datetime
		result := dt:ToString("MMMM")
	endif
	return	 result 

/// <summary>
/// Format a set of numbers representing a year, month, and day as a Date.
/// </summary>
/// <param name="dwY"></param>
/// <param name="dwM"></param>
/// <param name="dwDay"></param>
/// <returns>
/// </returns>
function ConDate(dwY as dword,dwM as dword,dwDay as dword) as DATE
	return	 DATE{dwY,dwM,dwDay}   

/// <summary>
/// Convert a Date string to DATE format.
/// </summary>
/// <param name="cDate"></param>
/// <returns>
/// </returns>
function CToD(cDate as string) as DATE
	local parsedDate as DateTime
	if !DateTime.TryParse(cDate,out parsedDate)
		parsedDate := DateTime.MinValue
	endif
	return	 DATE{parsedDate}   

/// <summary>
/// Convert an ANSI Date string to Date format.
/// </summary>
/// <param name="cDate"></param>
/// <returns>
/// </returns>
function CToDAnsi(cDate as string) as DATE
	/// THROW NotImplementedException{}
	return	 (DATE)0   


/// <summary>
/// Convert a Date to a 32-bit binary Date string.
/// </summary>
/// <param name="d"></param>
/// <returns>
/// </returns>
function Date2Bin(d as DATE) as string
	/// THROW NotImplementedException{}
	return	 String.Empty   

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
	
	if ! d:IsEmpty
		local dt := d as Datetime
		result := d:ToString()
	else
		result := default(DateTime):ToString()
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
	local convertedDate := DATE{} as DATE
	try
		convertedDate := (DATE)DateTime.ParseExact(cDate, "yyyyMMdd", System.Globalization.CultureInfo.InvariantCulture)
		//catch ex as exeption
		//   nop
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
function Year(d as DATE) as dword
	local year := 0  as dword
	if ! d:IsEmpty
		year := d:DYear
	endif
	return year



