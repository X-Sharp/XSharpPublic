//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
INTERNAL GLOBAL lAsDate := FALSE AS LOGIC
FUNCTION OleDateTimeAsDate(lSet AS LOGIC) AS LOGIC 
	// Global setting for Date/Time return values
	LOCAL lOld AS LOGIC
	lOld := lAsDate 
	lAsDate := lSet
	RETURN lOld
	
FUNCTION OleDateTimeAsDate() AS LOGIC 
	// Global setting for Date/Time return values
	RETURN lAsDate

/// <summary>VO Compatible class to store DateTime values in OLE Automation</summary>	
CLASS XSharp.OleDateTime
	PROTECT dt AS System.DateTime
	
	METHOD AsReal8 AS REAL8 
		RETURN dt:ToOADate()
		
	METHOD AsString AS STRING 
		RETURN dt:ToString()
		
	ACCESS DateVal AS DATE 
		RETURN (DATE) dt
		
	ASSIGN DateVal(dNewDate AS DATE ) 
		LOCAL dNew  AS DateTime
		dNew  := dNewDate:ToDateTime()
		dt := System.DateTime{dNew:Year, dNew:Month, dNew:Day, dt:Hour, dt:Minute, dt:Second, dt:Millisecond}
		RETURN 
		
	PROPERTY DateTime AS System.DateTime GET dt SET dt := VALUE
	
	CONSTRUCTOR() 
		dt := DateTime.MinValue

	CONSTRUCTOR(val  AS System.DateTime)
		dt := val

	CONSTRUCTOR(uDate AS USUAL) 
		IF IsNumeric(uDate)
			dt := DateTime.FromOADate( (REAL8) uDate)
		ELSEIF IsDate(uDate)
			dt := ((DATE) uDate):ToDateTime()
		ELSEIF IsString(uDate)         
			dt := DateTime.Parse((STRING) uDate)
		ELSE
			dt := DateTime.MinValue
		ENDIF
		RETURN 
		
	ACCESS TimeString AS STRING 
		RETURN dt:TimeOfDay:ToString()
		
	ASSIGN TimeString(cNewTime AS STRING ) 
		LOCAL sTime AS TimeSpan
		sTime := TimeSpan.Parse(cNewTime)
		dt := System.DateTime{dt:Year, dt:Month, dt:Day, sTime:Hours, sTime:Minutes, sTime:Seconds, sTime:Milliseconds}
		RETURN 
		
	ACCESS TimeVal AS FLOAT 
		RETURN dt:TimeOfDay:TotalDays
		
	ASSIGN TimeVal(nNewTime AS FLOAT ) 
		LOCAL sTime AS TimeSpan
		sTime := TimeSpan.FromDays( (REAL8) nNewTime)
		dt := System.DateTime{dt:Year, dt:Month, dt:Day, sTime:Hours, sTime:Minutes, sTime:Seconds, sTime:Milliseconds}
		RETURN 
		
	VIRTUAL METHOD ToString( provider AS IFormatProvider ) AS STRING
		RETURN dt:ToString( provider )
		
	VIRTUAL METHOD ToString( s AS STRING, fp AS IFormatProvider ) AS STRING
		RETURN dt:ToString( s, fp )   
		
	VIRTUAL METHOD ToString() AS STRING
		RETURN dt:ToString()

	STATIC OPERATOR IMPLICIT(odt AS OleDateTime) AS DateTime
		RETURN oDt:DateTime

	STATIC OPERATOR IMPLICIT(odt AS DateTime) AS OleDateTime
		RETURN OleDateTime{oDT}

		
END CLASS


