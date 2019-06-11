//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
INTERNAL GLOBAL lAsDate := FALSE AS LOGIC
/// <summary>Set and Retrieve the setting that determines if OLE date values should be returned as VO date values or as DateTime values</summary>
/// <param name='lSet'>The new setting. FALSE allows the return values to be instances of the OleDateTime class, TRUE forces all OLE  DateTime values to be VO dates . The default is FALSE.</param>
FUNCTION OleDateTimeAsDate(lSet AS LOGIC) AS LOGIC 
	// Global setting for Date/Time return values
	LOCAL lOld AS LOGIC
	lOld := lAsDate 
	lAsDate := lSet
	RETURN lOld
/// <summary>Retrieve the setting that determines if OLE date values should be returned as VO date values or as DateTime values</summary>
FUNCTION OleDateTimeAsDate() AS LOGIC 
	// Global setting for Date/Time return values
	RETURN lAsDate

/// <summary>VO Compatible class to store DateTime values in OLE Automation</summary>	
CLASS XSharp.OleDateTime
	PRIVATE dt AS System.DateTime
	/// <summary>Returns the DateTime value as Real8 (the format that is used in COM).</summary>
	METHOD AsReal8 AS REAL8 
		RETURN dt:ToOADate()
		
    /// <summary>Returns the dateTime value as a String.</summary>
    METHOD AsString AS STRING 
		RETURN dt:ToString()

    /// <summary>The value of the date part of the DateTime object as a Date.</summary>
	PROPERTY DateVal AS DATE
        GET
		    RETURN (DATE) dt
		END GET
	    SET
		    LOCAL dNew  AS DateTime
		    dNew  := VALUE:ToDateTime()
		    dt := System.DateTime{dNew:Year, dNew:Month, dNew:Day, dt:Hour, dt:Minute, dt:Second, dt:Millisecond}
		    RETURN 
        END SET
    END PROPERTY
    /// <summary>The value as a .Net DateTime type.</summary>
	PROPERTY DateTime AS System.DateTime GET dt SET dt := VALUE
	/// <summary>Construct an OleDateTime object</summary>
	CONSTRUCTOR() 
		dt := DateTime.MinValue
    /// <summary>Construct an OleDateTime object</summary>
    /// <param name='val'>Initial value as System.DateTime.</param>
	CONSTRUCTOR(val  AS System.DateTime)
		dt := val
    /// <summary>Construct an OleDateTime object</summary>
    /// <param name='uDate'>This can be a Number, Date or String. A Number will be seen as a REAL8 value in OADate form, Date = Date part and String = TimePart.</param>
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
	/// <summary>The value of the time part of the DateTime object as a String.	</summary>
	PROPERTY TimeString AS STRING
        GET 
		    RETURN dt:TimeOfDay:ToString()
        END GET
        SET 
		    LOCAL sTime AS TimeSpan
		    sTime := TimeSpan.Parse(VALUE)
		    dt := System.DateTime{dt:Year, dt:Month, dt:Day, sTime:Hours, sTime:Minutes, sTime:Seconds, sTime:Milliseconds}
		    RETURN
         END SET
    END PROPERTY
    /// <summary>The value of the time part of the DateTime object as a float.	</summary>
	PROPERTY TimeVal AS FLOAT
        GET
		    RETURN dt:TimeOfDay:TotalDays
        END GET
        SET 
		    LOCAL sTime AS TimeSpan
		    sTime := TimeSpan.FromDays( (REAL8) VALUE)
		    dt := System.DateTime{dt:Year, dt:Month, dt:Day, sTime:Hours, sTime:Minutes, sTime:Seconds, sTime:Milliseconds}
		    RETURN 
        END SET
    END PROPERTY
    /// <inheritdoc cref="M:System.DateTime.ToString(System.IFormatProvider)" />
	VIRTUAL METHOD ToString( provider AS IFormatProvider ) AS STRING
		RETURN dt:ToString( provider )
	/// <inheritdoc cref="M:System.DateTime.ToString(System.String,System.IFormatProvider)" />
	VIRTUAL METHOD ToString( s AS STRING, fp AS IFormatProvider ) AS STRING
		RETURN dt:ToString( s, fp )

    /// <inheritdoc cref="M:System.DateTime.ToString(System.String)" />
	OVERRIDE METHOD ToString() AS STRING
		RETURN dt:ToString()
    /// <exclude/>
	STATIC OPERATOR IMPLICIT(odt AS OleDateTime) AS DateTime
		RETURN oDt:DateTime
    /// <exclude/>
	STATIC OPERATOR IMPLICIT(odt AS DateTime) AS OleDateTime
		RETURN OleDateTime{oDT}

		
END CLASS


