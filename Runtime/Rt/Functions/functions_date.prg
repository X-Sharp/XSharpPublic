//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
begin namespace XSharp.Runtime
	#region functions
	/// <summary>
	/// Extract the name of the day of the week from a date.
	/// </summary>
	/// <param name="d">The date to calculate the day of week from.</param>
	/// <returns>
	/// A string for the calculated day of the week.
	/// </returns>
	FUNCTION CDoW(d AS DATE) AS STRING		
		local result := null_string as string
		if d != null
		   local dt := d as Datetime
		   result := dt:ToString("dddd")
		endif
	RETURN result

	/// <summary>
	/// Extract the name of the month from a date.
	/// </summary>
	/// <param name="d">The date to calculate the month from.</param>
	/// <returns>
	/// A string with the name of the month.
	/// </returns>
	FUNCTION CMonth(d AS DATE) AS STRING
		local result := null_string as string
		if d != null
		   local dt := d as Datetime
		   result := dt:ToString("MMMM")
		endif
	RETURN result 

	/// <summary>
	/// Convert a date to a 32-bit binary date string.
	/// </summary>
	/// <param name="d"></param>
	/// <returns>
	/// </returns>
	FUNCTION Date2Bin(d AS DATE) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Extract the number of the day of the month from a date.
	/// </summary>
	/// <param name="d">The date to extract the day from.</param>
	/// <returns>
	/// The day part of the given date.
	/// </returns>
	FUNCTION Day(d AS DATE) AS DWORD
		local day := 0  as dword
		if d != null
           local dt := d as Datetime
		   day := (dword) dt:Day		   
		endif
	return day

	/// <summary>
	/// Extract the number of the day of the week from a date.
	/// </summary>
	/// <param name="d">The date to extract the day of the week from.</param>
	/// <returns>
	/// The day of the week of the given date.
	/// </returns>
	FUNCTION DoW(d AS DATE) AS DWORD
		local day := 0  as dword
		if d != null
           local dt := d as Datetime
		   day := (dword) dt:DayOfWeek	   
		endif
	return day
  

	/// <summary>
	/// Convert a date to a string.
	/// </summary>
	/// <param name="d">The date to be converted.</param>
	/// <returns>
	/// A string representation of the given date, formatted in the current date format.
	/// </returns>
	FUNCTION DToC(d AS DATE) AS STRING
		local result:="" as string		

		if d != null
           local dt := d as Datetime
		   result := d:ToString()
		else
		   result := default(DateTime):ToString()
		endif
	return result 

	/// <summary>
	/// Convert a date value to a string formatted as string in ANSI format
	/// </summary>
	/// <param name="d">The date to be converted</param>
	/// <returns>
	/// The given date as string in ANSI format
	/// </returns>
	FUNCTION DToS(d AS DATE) AS STRING
		local result:="        " as string		
		if d != null
           local dt := d as Datetime
		   result := d:ToString("yyyyMMdd")
		endif
	return result 

	/// <summary>
	/// </summary>
	/// <param name="d"></param>
	/// <returns>
	/// </returns>
	FUNCTION JCDOW(d AS DATE) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// </summary>
	/// <param name="d"></param>
	/// <returns>
	/// </returns>
	FUNCTION JCMONTH(d AS DATE) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// </summary>
	/// <param name="d"></param>
	/// <returns>
	/// </returns>
	FUNCTION JCYEAR(d AS DATE) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Extract the number of the month from a date.
	/// </summary>
	/// <param name="d">The date to extract the month from.</param>
	/// <returns>
	/// The month of the given date.
	/// </returns>
	FUNCTION Month(d AS DATE) AS DWORD
		local month := 0  as dword
		if d != null
           local dt := d as Datetime
		   month := (dword) dt:Month
		endif
	return month

	/// <summary>
	/// Extract the number of the year from a date.
	/// </summary>
	/// <param name="d">The date to extract the year from.</param>
	/// <returns>
	/// The year from the give date.
	/// </returns>
	FUNCTION Year(d AS DATE) AS DWORD
		local year := 0  as dword
		if d != null
           local dt := d as Datetime
		   year := (dword) dt:Year
		endif
	return year

	#endregion
end namespace