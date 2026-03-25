//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
INTERNAL GLOBAL lAsDate := FALSE AS LOGIC
/// <include file="XSharp.VO.Docs.xml" path="doc/OleDateTimeAsDate/*" />
FUNCTION OleDateTimeAsDate(lSet AS LOGIC) AS LOGIC
	// Global setting for Date/Time return values
	LOCAL lOld AS LOGIC
	lOld := lAsDate
	lAsDate := lSet
	RETURN lOld
/// <include file="XSharp.VO.Docs.xml" path="doc/OleDateTimeAsDate/*" />
FUNCTION OleDateTimeAsDate() AS LOGIC
	// Global setting for Date/Time return values
	RETURN lAsDate

/// <include file="XSharp.VO.Docs.xml" path="doc/OleDateTime/*" />
CLASS XSharp.OleDateTime
	PRIVATE dt AS System.DateTime
 /// <include file="XSharp.VO.Docs.xml" path="doc/OleDateTime.AsReal8/*" />
	METHOD AsReal8 AS REAL8
		RETURN dt:ToOADate()

    /// <include file="XSharp.VO.Docs.xml" path="doc/OleDateTime.AsString/*" />
    METHOD AsString AS STRING
		RETURN dt:ToString()

 /// <include file="XSharp.VO.Docs.xml" path="doc/OleDateTime.DateVal/*" />
	PROPERTY DateVal AS DATE
        GET
		    RETURN (DATE) dt
		END GET
	    SET
		    LOCAL dNew  AS DateTime
		    dNew  := value:ToDateTime()
		    dt := System.DateTime{dNew:Year, dNew:Month, dNew:Day, dt:Hour, dt:Minute, dt:Second, dt:Millisecond}
		    RETURN
        END SET
    END PROPERTY
 /// <include file="XSharp.VO.Docs.xml" path="doc/OleDateTime.DateTime/*" />
	PROPERTY DateTime AS System.DateTime GET dt SET dt := value
 /// <include file="XSharp.VO.Docs.xml" path="doc/OleDateTime.ctor/*" />
	CONSTRUCTOR()
		dt := DateTime.MinValue
 /// <include file="XSharp.VO.Docs.xml" path="doc/OleDateTime.ctor/*" />
	CONSTRUCTOR(val  AS System.DateTime)
		dt := val
 /// <include file="XSharp.VO.Docs.xml" path="doc/OleDateTime.ctor/*" />
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
 /// <include file="XSharp.VO.Docs.xml" path="doc/OleDateTime.TimeString/*" />
	PROPERTY TimeString AS STRING
        GET
		    RETURN dt:TimeOfDay:ToString()
        END GET
        SET
		    LOCAL sTime AS TimeSpan
		    sTime := TimeSpan.Parse(value)
		    dt := System.DateTime{dt:Year, dt:Month, dt:Day, sTime:Hours, sTime:Minutes, sTime:Seconds, sTime:Milliseconds}
		    RETURN
         END SET
    END PROPERTY
 /// <include file="XSharp.VO.Docs.xml" path="doc/OleDateTime.TimeVal/*" />
	PROPERTY TimeVal AS FLOAT
        GET
		    RETURN dt:TimeOfDay:TotalDays
        END GET
        SET
		    LOCAL sTime AS TimeSpan
		    sTime := TimeSpan.FromDays( (REAL8) value)
		    dt := System.DateTime{dt:Year, dt:Month, dt:Day, sTime:Hours, sTime:Minutes, sTime:Seconds, sTime:Milliseconds}
		    RETURN
        END SET
    END PROPERTY
    /// <inheritdoc cref="System.DateTime.ToString" />
	VIRTUAL METHOD ToString( provider AS IFormatProvider ) AS STRING
		RETURN dt:ToString( provider )
    /// <inheritdoc cref="System.DateTime.ToString" />
	VIRTUAL METHOD ToString( s AS STRING, fp AS IFormatProvider ) AS STRING
		RETURN dt:ToString( s, fp )

    /// <inheritdoc cref="System.DateTime.ToString" />
	OVERRIDE METHOD ToString() AS STRING
		RETURN dt:ToString()
    /// <exclude/>
	STATIC OPERATOR IMPLICIT(oDt AS OleDateTime) AS DateTime
		RETURN oDt:DateTime
    /// <exclude/>
	STATIC OPERATOR IMPLICIT(oDt AS DateTime) AS OleDateTime
		RETURN OleDateTime{oDt}


END CLASS


