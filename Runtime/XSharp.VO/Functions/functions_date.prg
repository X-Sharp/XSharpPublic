//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using XSharp
/// <summary>
/// Convert a string containing a 32-bit binary __VODate to a __VODate data type.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION Bin2Date(c AS STRING) AS __VODate
	/// THROW NotImplementedException{}
RETURN (__VODate)0   




/// <summary>
/// Extract the name of the day of the week from a __VODate.
/// </summary>
/// <param name="d">The __VODate to calculate the day of week from.</param>
/// <returns>
/// A string for the calculated day of the week.
/// </returns>
FUNCTION CDoW(d AS __VODate) AS STRING		
	local result := String.Empty as string
	if d != null
		local dt := d as Datetime
		result := dt:ToString("dddd")
	endif
RETURN result

/// <summary>
/// Extract the name of the month from a __VODate.
/// </summary>
/// <param name="d">The __VODate to calculate the month from.</param>
/// <returns>
/// A string with the name of the month.
/// </returns>
FUNCTION CMonth(d AS __VODate) AS STRING
	local result := String.Empty as string
	if d != null
		local dt := d as Datetime
		result := dt:ToString("MMMM")
	endif
RETURN result 

/// <summary>
/// Format a set of numbers representing a year, month, and day as a __VODate.
/// </summary>
/// <param name="dwY"></param>
/// <param name="dwM"></param>
/// <param name="dwDay"></param>
/// <returns>
/// </returns>
FUNCTION ConDate(dwY AS DWORD,dwM AS DWORD,dwDay AS DWORD) AS __VODate
RETURN __VODate{dwY,dwM,dwDay}   

/// <summary>
/// Convert a __VODate string to __VODate format.
/// </summary>
/// <param name="cDate"></param>
/// <returns>
/// </returns>
FUNCTION CToD(cDate AS STRING) AS __VODate
	local parsedDate as DateTime
	if !DateTime.TryParse(cDate,out parsedDate)
		parsedDate := DateTime.MinValue
	endif
RETURN __VODate{parsedDate}   

/// <summary>
/// Convert an ANSI __VODate string to __VODate format.
/// </summary>
/// <param name="cDate"></param>
/// <returns>
/// </returns>
FUNCTION CToDAnsi(cDate AS STRING) AS __VODate
	/// THROW NotImplementedException{}
RETURN (__VODate)0   


/// <summary>
/// Convert a __VODate to a 32-bit binary __VODate string.
/// </summary>
/// <param name="d"></param>
/// <returns>
/// </returns>
FUNCTION Date2Bin(d AS __VODate) AS STRING
	/// THROW NotImplementedException{}
RETURN String.Empty   

/// <summary>
/// Extract the number of the day of the month from a __VODate.
/// </summary>
/// <param name="d">The __VODate to extract the day from.</param>
/// <returns>
/// The day part of the given __VODate.
/// </returns>
FUNCTION Day(d AS __VODate) AS DWORD
	local day := 0  as dword
	if ! d:IsEmpty
		day :=  d:DDay
	endif
return day

/// <summary>
/// Extract the number of the day of the week from a __VODate.
/// </summary>
/// <param name="d">The __VODate to extract the day of the week from.</param>
/// <returns>
/// The day of the week of the given __VODate.
/// </returns>
FUNCTION DoW(d AS __VODate) AS DWORD
	local day := 0  as dword
	if ! d:IsEmpty
        local dt := d as Datetime
		day := (dword) dt:DayOfWeek	   
	endif
return day
  

/// <summary>
/// Convert a __VODate to a string.
/// </summary>
/// <param name="d">The __VODate to be converted.</param>
/// <returns>
/// A string representation of the given __VODate, formatted in the current __VODate format.
/// </returns>
FUNCTION DToC(d AS __VODate) AS STRING
	local result:="" as string		

	if ! d:IsEmpty
        local dt := d as Datetime
		result := d:ToString()
	else
		result := default(DateTime):ToString()
	endif
return result 

/// <summary>
/// Convert a __VODate value to a string formatted as string in ANSI format
/// </summary>
/// <param name="d">The __VODate to be converted</param>
/// <returns>
/// The given __VODate as string in ANSI format
/// </returns>
FUNCTION DToS(d AS __VODate) AS STRING
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
FUNCTION JCDOW(d AS __VODate) AS STRING
	/// THROW NotImplementedException{}
RETURN String.Empty   

/// <summary>
/// </summary>
/// <param name="d"></param>
/// <returns>
/// </returns>
FUNCTION JCMONTH(d AS __VODate) AS STRING
	/// THROW NotImplementedException{}
RETURN String.Empty   

/// <summary>
/// </summary>
/// <param name="d"></param>
/// <returns>
/// </returns>
FUNCTION JCYEAR(d AS __VODate) AS STRING
	/// THROW NotImplementedException{}
RETURN String.Empty   

/// <summary>
/// Extract the number of the month from a __VODate.
/// </summary>
/// <param name="d">The __VODate to extract the month from.</param>
/// <returns>
/// The month of the given __VODate.
/// </returns>
FUNCTION Month(d AS __VODate) AS DWORD
	local month := 0  as dword
	if !d:IsEmpty
		month :=  d:DMonth
	endif
return month

/// <summary>
/// Convert an ANSI __VODate string to __VODate format.
/// </summary>
/// <param name="cDate"></param>
/// <returns>
/// </returns>
FUNCTION SToD(cDate AS STRING) AS __VODate
	local convertedDate := __VODate{} as __VODate
	try
		convertedDate := (__VODate)DateTime.ParseExact(cDate, "yyyyMMdd", System.Globalization.CultureInfo.InvariantCulture)
	//catch ex as exeption
	//   nop
	end try
RETURN convertedDate


/// <summary>
/// Extract the number of the year from a __VODate.
/// </summary>
/// <param name="d">The __VODate to extract the year from.</param>
/// <returns>
/// The year from the give __VODate.
/// </returns>
FUNCTION Year(d AS __VODate) AS DWORD
	local year := 0  as dword
	if ! d:IsEmpty
		year := d:DYear
	endif
return year
