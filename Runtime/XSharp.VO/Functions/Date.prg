//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING XSharp
/// <summary>
/// Convert a string containing a 32-bit binary Date to a Date data type.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION Bin2Date(c AS STRING) AS DATE
	RETURN (DATE)(DWORD) Bin2L( c )



/// <summary>
/// Extract the name of the day of the week from a Date.
/// </summary>
/// <param name="d">The Date to calculate the day of week from.</param>
/// <returns>
/// A string for the calculated day of the week.
/// </returns>
FUNCTION CDoW(d AS DATE) AS STRING		
	LOCAL result := String.Empty AS STRING
	IF d != NULL
		LOCAL dt := d AS Datetime
		result := dt:ToString("dddd")
	ENDIF
	RETURN result

/// <summary>
/// Extract the name of the month from a Date.
/// </summary>
/// <param name="d">The Date to calculate the month from.</param>
/// <returns>
/// A string with the name of the month.
/// </returns>
FUNCTION CMonth(d AS DATE) AS STRING
	RETURN NToCMonth( d:DMonth)

/// <summary>
/// Format a set of numbers representing a year, month, and day as a Date.
/// </summary>
/// <param name="dwY"></param>
/// <param name="dwM"></param>
/// <param name="dwDay"></param>
/// <returns>
/// </returns>
FUNCTION ConDate(dwY AS DWORD,dwM AS DWORD,dwDay AS DWORD) AS DATE
	IF dwY < 100
		LOCAL lAfter AS LOGIC
		lAfter := dwY > XSharp.RuntimeState.EpochYear
		dwY += XSharp.RuntimeState.EpochCent
		IF lAfter
			dwY -= 100
		ENDIF
	ENDIF
	RETURN DATE{dwY,dwM,dwDay}   

/// <summary>
/// Convert a Date string to DATE format.
/// </summary>
/// <param name="cDate"></param>
/// <returns>
/// </returns>
FUNCTION CToD(cDate AS STRING) AS DATE
	RETURN CToD(cDate, XSharp.RuntimeState.DateFormat)

/// <summary>
/// Convert a Date string to DATE format using a specified Date Format string
/// </summary>
/// <param name="cDate"></param>
/// <param name="cDateFormat"></param>
/// <returns>
/// </returns>
FUNCTION CToD(cDate AS STRING, cDateFormat AS STRING) AS DATE
	LOCAL dDate AS DATE
	LOCAL nDay, nMonth, nYear AS DWORD
	LOCAL nDayPos, nMonthPos, nYearPos AS INT
	dDate := (DATE) 0
	IF string.IsNullOrEmpty(cDate) .or. String.IsNullOrEmpty(cDateFormat)
		RETURN dDate
	ENDIF
	LOCAL nPos AS INT
	LOCAL cSep AS STRING
	nDayPos := nMonthPos := nYearPos := 0
	cSep := "./-"
	nPos := 0
	FOREACH c AS char IN cDateFormat
		SWITCH c
		CASE 'D'
			IF nDayPos == 0
				++nPos
				nDayPos  := nPos
			ENDIF
		CASE 'M'
			IF nMonthPos == 0
				++nPos
				nMonthPos  := nPos
			ENDIF
		CASE 'Y'
			IF nYearPos == 0
				++nPos
				nYearPos  := nPos
			ENDIF
		OTHERWISE
			IF cSep:IndexOf(c) == -1
				cSep += c:ToString()
			ENDIF
		END SWITCH
	NEXT
	IF nDayPos == 0 .or. nMonthPos == 0 .or. nYearPos == 0
		RETURN dDate
	ENDIF
	TRY
		// we now know the seperators and the positions in the string
		LOCAL aNums := cDate:Split(cSep:ToCharArray()) AS STRING[]
		nDay   := Uint32.Parse(aNums[nDayPos])
		nMonth := Uint32.Parse(aNums[nMonthPos])
		nYear  := Uint32.Parse(aNums[nYearPos])
		IF aNums[nYearPos]:Length < 4
			// Century missing ?
			dDate := ConDate(nYear, nMonth, nDay)
		ELSE
			dDate := DATE{nYear, nMonth, nDay}
		ENDIF
	CATCH 
		dDate := (DATE) 0
	END TRY
	RETURN dDate


/// <summary>
/// Convert an ANSI Date string to Date format.
/// </summary>
/// <param name="cDate"></param>
/// <returns>
/// </returns>
FUNCTION CToDAnsi(cDate AS STRING) AS DATE
	RETURN CToD(cDate, "YYYY.MM.DD")


/// <summary>
/// Convert a Date to a 32-bit binary Date string.
/// </summary>
/// <param name="d"></param>
/// <returns>
/// </returns>
FUNCTION Date2Bin(d AS DATE) AS STRING
	RETURN L2Bin((LONG) (DATE) d)
 

/// <summary>
/// Extract the number of the day of the month from a Date.
/// </summary>
/// <param name="d">The Date to extract the day from.</param>
/// <returns>
/// The day part of the given DATE.
/// </returns>
FUNCTION Day(d AS DATE) AS DWORD
	LOCAL day := 0  AS DWORD
	IF ! d:IsEmpty
		day :=  d:DDay
	ENDIF
	RETURN day

/// <summary>
/// Extract the number of the day of the week from a Date.
/// </summary>
/// <param name="d">The Date to extract the day of the week from.</param>
/// <returns>
/// The day of the week of the given DATE.
/// </returns>
FUNCTION DoW(d AS DATE) AS DWORD
	LOCAL day := 0  AS DWORD
	IF ! d:IsEmpty
		LOCAL dt := d AS Datetime
		day := (DWORD) dt:DayOfWeek	   
	ENDIF
	RETURN day


/// <summary>
/// Convert a Date to a string.
/// </summary>
/// <param name="d">The Date to be converted.</param>
/// <returns>
/// A string representation of the given Date, formatted in the current Date format.
/// </returns>
FUNCTION DToC(d AS DATE) AS STRING
	LOCAL result:="" AS STRING		
	LOCAL cFormat := XSharp.RuntimeState.GetValue<STRING>(Set.DateFormatNet) AS STRING
	IF ! d:IsEmpty
		LOCAL dt := d AS Datetime
		result := d:ToString(cFormat)
	ELSE
		result := XSharp.RuntimeState.GetValue<STRING>(Set.DateFormatEmpty) 
	ENDIF
	RETURN result 

/// <summary>
/// Convert a Date value to a string formatted as string in ANSI format
/// </summary>
/// <param name="d">The Date to be converted</param>
/// <returns>
/// The given Date as string in ANSI format
/// </returns>
FUNCTION DToS(d AS DATE) AS STRING
	LOCAL result:="        " AS STRING		
	IF ! d:IsEmpty
		LOCAL dt := d AS Datetime
		result := d:ToString("yyyyMMdd")
	ENDIF
	RETURN result 

/// <summary>
/// </summary>
/// <param name="d"></param>
/// <returns>
/// </returns>
FUNCTION JCDOW(d AS DATE) AS STRING
	/// THROW NotImplementedException{}
	RETURN	 String.Empty   

/// <summary>
/// </summary>
/// <param name="d"></param>
/// <returns>
/// </returns>
FUNCTION JCMONTH(d AS DATE) AS STRING
	/// THROW NotImplementedException{}
	RETURN	 String.Empty   

/// <summary>
/// </summary>
/// <param name="d"></param>
/// <returns>
/// </returns>
FUNCTION JCYEAR(d AS DATE) AS STRING
	/// THROW NotImplementedException{}
	RETURN	 String.Empty   

/// <summary>
/// Extract the number of the month from a DATE.
/// </summary>
/// <param name="d">The Date to extract the month from.</param>
/// <returns>
/// The month of the given Date.
/// </returns>
FUNCTION Month(d AS DATE) AS DWORD
	LOCAL month := 0  AS DWORD
	IF !d:IsEmpty
		month :=  d:DMonth
	ENDIF
	RETURN month




/// <summary>
/// Convert an ANSI Date string to Date format.
/// </summary>
/// <param name="cDate"></param>
/// <returns>
/// </returns>
FUNCTION SToD(cDate AS STRING) AS DATE
	LOCAL convertedDate AS DATE
	TRY
		IF cDate:Length == 8 .and. cDate[0] == '0' .and. cDate[1] == '0'
			// VO adjusts date strings like "00yyMMdd" to epoch-based year
			LOCAL dwY AS DWORD
			dwY := UInt32.Parse(cDate:Substring(0,4))
			
			// same code as in ConDate(), probably better adjust SToD() to use ConDate() directly
			LOCAL lAfter AS LOGIC
			lAfter := dwY > XSharp.RuntimeState.EpochYear
			dwY += XSharp.RuntimeState.EpochCent
			IF lAfter
				dwY -= 100
			ENDIF
			
			cDate := dwY:ToString():PadLeft(4 , '0') + cDate:Substring(4)
		END IF
		convertedDate := (DATE)DateTime.ParseExact(cDate, "yyyyMMdd", System.Globalization.CultureInfo.InvariantCulture)
	CATCH
		convertedDate := DATE{} 
	END TRY
	RETURN	 convertedDate


/// <summary>
/// Return the system Date as a Date value.
/// </summary>
/// <returns>
/// </returns>
FUNCTION Today() AS DATE
	RETURN (DATE) DateTime.Now



/// <summary>
/// Convert a specified number of seconds to a time string.
/// </summary>
/// <param name="uSeconds"></param>
/// <returns>
/// </returns>
FUNCTION TString(uSeconds AS USUAL) AS STRING
	IF uSeconds:IsNil
		RETURN XSharp.Core.Functions.Tstring( (DWORD) 0 )
	ELSEIF uSeconds:IsFLoat
		RETURN XSharp.Core.Functions.TString ( (FLOAT) uSeconds)
	ELSEIF uSeconds:IsInteger
		RETURN XSharp.Core.Functions.TString ( (DWORD) uSeconds)
	ENDIF
	RETURN String.Empty   





/// <summary>
/// Extract the number of the year from a DATE.
/// </summary>
/// <param name="d">The DATE to extract the year from.</param>
/// <returns>
/// The year from the give DATE.
/// </returns>
FUNCTION Year(d AS DATE) AS DWORD
	LOCAL year := 0  AS DWORD
	IF ! d:IsEmpty
		year := d:DYear
	ENDIF
	RETURN year



