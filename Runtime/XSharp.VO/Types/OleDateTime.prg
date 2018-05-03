//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

FUNCTION OleDateTimeAsDate(lSet) AS LOGIC CLIPPER
	// Global setting for Date/Time return values
	STATIC lAsDate := FALSE AS LOGIC
	IF IsLogic(lSet)
		lAsDate := lSet
	ENDIF
	RETURN lAsDate
	
	
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
	
	CONSTRUCTOR(uDate) CLIPPER
		IF IsNumeric(uDate)
			dt := DateTime.FromOADate( (REAL8) uDate)
		ELSEIF IsDate(uDate)
			dt := ((DATE) uDate):ToDateTime()
		ELSEIF IsString(uDate)         
			dt := DateTime.Parse((STRING) uDate)
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
		
END CLASS


