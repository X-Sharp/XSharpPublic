//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING XSharp
USING System.Globalization

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/bin2date/*" />
FUNCTION Bin2Date(cString AS STRING) AS DATE
	RETURN (DATE)(DWORD) Bin2L( cString )

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/cdow/*" />
FUNCTION CDoW(dDate AS DATE) AS STRING		
	RETURN NToCDoW(DoW(dDate))

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/cmonth/*" /> 
FUNCTION CMonth(dDate AS DATE) AS STRING
	RETURN NToCMonth( dDate:DMonth)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/condate/*" />   
FUNCTION ConDate(dwYear AS DWORD,dwMonth AS DWORD,dwDay AS DWORD) AS DATE
    IF dwYear == 0 .OR. dwMonth == 0 .OR. dwDay == 0
      RETURN NULL_DATE
    ENDIF
    RETURN ConDateTime(dwYear, dwMonth, dwDay)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ctod/*" />     
FUNCTION CToD(cDate AS STRING) AS DATE
	RETURN CToDt(cDate, XSharp.RuntimeState.DateFormat)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ctod/*" />     
/// <param name="cDateFormat">A string representating the date format to use when converting the string to a date. Should consist of D, M and Y characters and separators.</param>
FUNCTION CToD(cDate AS STRING, cDateFormat AS STRING) AS DATE
    RETURN CToDt(cDate, cDateFormat)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ctodansi/*" />
FUNCTION CToDAnsi(cDate AS STRING) AS DATE
	RETURN CToDt(Left(cDate,10), "YYYY.MM.DD")

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/date2bin/*" />      
FUNCTION Date2Bin(dValue AS DATE) AS STRING
	RETURN L2Bin((LONG) (DATE) dValue)
 
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/day/*" />     
FUNCTION Day(dDate AS DATE) AS DWORD
	LOCAL day := 0  AS DWORD
	IF ! dDate:IsEmpty
		day :=  dDate:DDay  
	ENDIF
	RETURN day  

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dow/*" />     
FUNCTION DoW(dDate AS DATE) AS DWORD
	LOCAL day := 0  AS DWORD
	IF ! dDate:IsEmpty
		LOCAL dt := dDate AS Datetime
		day := (DWORD) (dt:DayOfWeek	+1  )
	ENDIF
	RETURN day

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dtoc/*" />   
FUNCTION DToC(dDate AS DATE) AS STRING
    RETURN DtToC(dDate)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dos/*" />   
FUNCTION DToS(dDate AS DATE) AS STRING
    RETURN DtToS(dDate)

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

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/month/*" />   
FUNCTION Month(dDate AS DATE) AS DWORD
	LOCAL month := 0  AS DWORD
	IF !dDate:IsEmpty
		month :=  dDate:DMonth
	ENDIF
	RETURN month


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/stod/*" />
FUNCTION SToD(cDate AS STRING) AS DATE
	RETURN SToDt(cDate)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/today/*" />     
FUNCTION Today() AS DATE
	RETURN (DATE) DateTime.Now

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/tstring/*" />     
FUNCTION TString(nSeconds AS USUAL) AS STRING
	IF nSeconds:IsNil
		RETURN XSharp.Core.Functions.Tstring( (DWORD) 0 )
	ELSEIF nSeconds:IsFLoat
		RETURN XSharp.Core.Functions.TString ( (FLOAT) nSeconds)
	ELSEIF nSeconds:IsInteger
		RETURN XSharp.Core.Functions.TString ( (DWORD) nSeconds)
	ENDIF
	RETURN String.Empty   

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/year/*" />     
FUNCTION Year(dDate AS DATE) AS DWORD
	LOCAL year := 0  AS DWORD
	IF ! dDate:IsEmpty
		year := dDate:DYear
	ENDIF
	RETURN year
