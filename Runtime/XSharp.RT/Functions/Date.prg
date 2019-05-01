//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING XSharp
USING System.Globalization
/// <summary>
/// Convert a string containing a 32-bit binary Date to a Date data type.
/// </summary>
/// <param name="cString">A 32-bit binary date represented as a string ? least significant byte first.  Only the first 4 bytes are used by the function; all others are ignored. </param>
/// <returns>A date value that corresponds to the date specified in cString.  If cString is not a valid binary date, Bin2Date() returns a NULL_DATE.
/// </returns>
FUNCTION Bin2Date(cString AS STRING) AS DATE
	RETURN (DATE)(DWORD) Bin2L( cString )


/// <summary>
/// Extract the name of the day of the week from a Date.
/// </summary>
/// <param name="d">The Date to calculate the day of week from.</param>
/// <returns>
/// A string for the calculated day of the week.
/// </returns>
FUNCTION CDoW(d AS DATE) AS STRING		
	RETURN NToCDoW(DoW(d))

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
/// <param name="dwY">A valid year.  If the century digits are not specified, the century is determined by the rules of SetEpoch(). </param>
/// <param name="dwM">A number from 1 through 12 representing a valid month. </param>
/// <param name="dwDay">A number representing a valid day of dwMonth.</param>
/// <returns>The date that corresponds to the passed arguments.  If any of the arguments specified do not represent a valid year, month, or day, a NULL_DATE is returned.</returns>
FUNCTION ConDate(dwY AS DWORD,dwM AS DWORD,dwDay AS DWORD) AS DATE
    IF dwY == 0 .OR. dwM == 0 .OR. dwDay == 0
      RETURN NULL_DATE
    ENDIF
    RETURN _ConDate(dwY, dwM, dwDay)

/// <summary>
/// Convert a Date string to date format.
/// </summary>
/// <param name="cDate">A string of numbers representing the month, day, and year, separated by any character other than a number.  The month, day, and year digits must be in the format set by SetDateFormat() or SetDateCountry().  If the century digits are not specified, the century is determined by the rules of SetEpoch().</param>
/// <returns>The date value that corresponds to the numbers specified in <paramref name="cDate"/>.  If <paramref name="cDate"/> is not a valid date, CToD() returns a NULL_DATE.
/// </returns>
FUNCTION CToD(cDate AS STRING) AS DATE
	RETURN CToD(cDate, XSharp.RuntimeState.DateFormat)

/// <summary>
/// Convert a Date string to date format using a specified Date Format string
/// </summary>
/// <param name="cDate">A string of numbers representing the month, day, and year, separated by any character other than a number.  The month, day, and year digits must be in the format set by SetDateFormat() or SetDateCountry().  If the century digits are not specified, the century is determined by the rules of SetEpoch().</param>
/// <param name="cDateFormat">A string representating the date format to use when converting the string to a date. Should consist of D, M and Y characters and separators.</param>
/// <returns><inheritdoc cref='M:XSharp.RT.Functions.CToD(System.String)'/></returns>
FUNCTION CToD(cDate AS STRING, cDateFormat AS STRING) AS DATE
    RETURN _CToD(cDate, cDateFormat)


/// <summary>
/// Convert an ANSI date string to date format.
/// </summary>
/// <param name="cDate">A string in the ANSI form yyyy.mm.dd, where yy, mm, and dd represent year, month, and day respectively.  
/// The year, month, and day can be separated by any character other than a number. 
/// cDate is always interpreted as an ANSI string and is not dependent on SetDateFormat() or SetDateCountry().  
/// If the century digits are not specified, the century is determined by the rules of SetEpoch().</param>
/// <returns><inheritdoc cref='M:XSharp.RT.Functions.CToD(System.String)'/></returns>
FUNCTION CToDAnsi(cDate AS STRING) AS DATE
	RETURN CToD(Left(cDate,10), "YYYY.MM.DD")


/// <summary>
/// Convert a Date to a 32-bit binary Date string.
/// </summary>
/// <param name="dValue">The date value to convert.</param>
/// <returns>Date2Bin() is a conversion function that converts a date data type into a 4-byte string.
/// Typical applications include reading foreign file types in their native format and then saving, reading, decrypting, and
/// transmitting date types in their compressed binary form instead of in strings.  Its inverse is Bin2Date().
/// </returns>
FUNCTION Date2Bin(dValue AS DATE) AS STRING
	RETURN L2Bin((LONG) (DATE) dValue)
 

/// <summary>
/// Extract the number of the day of the month from a Date.
/// </summary>
/// <param name="d">The Date to extract the day from.</param>
/// <returns>
/// The day of the month, as a number in the range 0 to 31.  For a NULL_DATE, Day() returns 0.
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
/// The day of the week as a number from 1 to 7, where 1 is Sunday, 2 is Monday, and so on.  For a NULL_DATE, DoW() returns 0.
/// </returns>
FUNCTION DoW(d AS DATE) AS DWORD
	LOCAL day := 0  AS DWORD
	IF ! d:IsEmpty
		LOCAL dt := d AS Datetime
		day := (DWORD) (dt:DayOfWeek	+1  )
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
    RETURN _DToC(d)

/// <summary>
/// Convert a Date value to a string formatted as string in ANSI format
/// </summary>
/// <param name="dDate">The Date to be converted</param>
/// <returns>
/// An 8-character string in the format yyyymmdd.  If dDate is a NULL_DATE, a string of eight spaces is returned.  The return value is not affected by the current date format.
/// </returns>
FUNCTION DToS(dDate AS DATE) AS STRING
    RETURN _DToS(dDate)

/// <summary>
/// </summary>
/// <param name="d"></param>
/// <returns>
/// </returns>
FUNCTION JCDOW(d AS DATE) AS STRING
	LOCAL dt := d AS DateTime
	LOCAL cal := JapaneseCalendar{} AS JapaneseCalendar
    LOCAL dow := cal:GetDayOfWeek(dt) AS DayOfWeek
	LOCAL culture := System.Globalization.CultureInfo.GetCultureInfo("ja-JP") AS CultureInfo
	RETURN culture:DateTimeFormat:GetDayName(dow)
    

/// <summary>
/// </summary>
/// <param name="d"></param>
/// <returns>
/// </returns>
FUNCTION JCMONTH(d AS DATE) AS STRING
	LOCAL dt := d AS DateTime
	LOCAL cal := JapaneseCalendar{} AS JapaneseCalendar
	LOCAL month := cal:GetMonth(dt) AS INT
	LOCAL culture := System.Globalization.CultureInfo.GetCultureInfo("ja-JP") AS CultureInfo
	RETURN culture:DateTimeFormat:GetMonthName(month)


/// <summary>
/// </summary>
/// <param name="d"></param>
/// <returns>
/// </returns>
FUNCTION JCYEAR(d AS DATE) AS STRING
	RETURN Year(d):ToString()

/// <summary>
/// Extract the number of the month from a DATE.
/// </summary>
/// <param name="d">The Date to extract the month from.</param>
/// <returns>The month of the given Date.</returns>
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
/// <returns><inheritdoc cref='M:XSharp.RT.Functions.CToD(System.String)'/></returns>
FUNCTION SToD(cDate AS STRING) AS DATE
	LOCAL convertedDate AS DATE
	TRY
		IF cDate:Length == 8 .AND. cDate[0] == c'0' .AND. cDate[1] == c'0'
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
			
			cDate := dwY:ToString():PadLeft(4 , c'0') + cDate:Substring(4)
		END IF
		convertedDate := (DATE)DateTime.ParseExact(cDate, "yyyyMMdd", System.Globalization.CultureInfo.InvariantCulture)
	CATCH
		convertedDate := NULL_DATE
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



