//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Globalization

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/dtot/*" />
FUNCTION DToT( dDateExpression AS DATE) AS DateTime
    RETURN (System.DateTime) dDateExpression

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/hour/*" />
FUNCTION Hour( tExpression AS System.DateTime) AS LONG
    RETURN tExpression:Hour

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/minute/*" />
FUNCTION Minute(tExpression AS System.DateTime) AS LONG
    RETURN tExpression:Minute

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sec/*" />
FUNCTION Sec( tExpression AS System.DateTime) AS LONG
    RETURN tExpression:Second

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/ttod/*" />
FUNCTION TToD( tExpression AS System.DateTime) AS DATE
    RETURN (DATE) tExpression

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/gomonth/*" />
FUNCTION GoMonth( dExpression AS DATE , iNumberOfMonths AS INT ) AS DATE
    RETURN GoMonth ( (DateTime) dExpression , iNumberOfMonths )

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/gomonth/*" />
FUNCTION GoMonth( tExpression AS DateTime , iNumberOfMonths AS INT ) AS DATE
LOCAL dDate AS DATE

   	IF ((DATE) tExpression):IsEmpty
		RETURN NULL_DATE
	ENDIF


    TRY    // suppresses a exception if the year becomes < 1 or > 9999

 		dDate := (DATE) tExpression:AddMonths( iNumberOfMonths )

		// according the docs, 1753 is the VFP min limit
		IF Year ( dDate ) < 1753
			dDate := NULL_DATE
		ENDIF

    CATCH

    	dDate := NULL_DATE // just to be sure ...

    END TRY

    RETURN dDate

INTERNAL FUNCTION _DateTimeError( sParameter as STRING, argNum as DWORD, aArgs PARAMS OBJECT[]) AS Error
    var err := Error.ArgumentError(ProcName(1), sParameter,__VfpStr(VFPErrors.VFP_INVALID_PARAMETER, sParameter, "'DATE' or 'DATETIME'" ), 1)
    err:Args := aArgs
    err:ArgNum := argNum
    err:Stack := ErrorStack(1)
    RETURN err


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/gomonth/*" />
FUNCTION GoMonth( dExpression AS USUAL , iNumberOfMonths AS INT ) AS DATE
    if IsDate(dExpression)
        RETURN GoMonth ( (Date) dExpression , iNumberOfMonths )
    endif
    if IsDateTime(dExpression)
        RETURN GoMonth ( (DateTime) dExpression , iNumberOfMonths )
    endif
    THROW _DateTimeError(nameof(dExpression), 1, dExpression, iNumberOfMonths)


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/quarter/*" />
FUNCTION Quarter( dExpression  AS USUAL , nMonth  := 1 AS INT ) AS INT
    if IsDate(dExpression)
        RETURN Quarter ( (Date) dExpression , nMonth )
    endif
    if IsDateTime(dExpression)
        RETURN Quarter ( (DateTime) dExpression , nMonth )
    endif
    THROW _DateTimeError(nameof(dExpression),1, dExpression, nMonth)


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/quarter/*" />
FUNCTION Quarter( dExpression  AS DATE , nMonth  := 1 AS INT ) AS INT
    RETURN Quarter ( (DateTime) dExpression  , nMonth  )


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/quarter/*" />
FUNCTION Quarter( tExpression  AS DateTime , nMonth  := 1 AS INT ) AS INT
    IF  ! (nMonth  > 0 .AND. nMonth  < 13 )
        THROW Error.ArgumentError(__ENTITY__, nameof(nMonth), __VfpStr(VFPErrors.VFP_INVALID_RANGE, nameof(nMonth), nMonth , "1-12") ,1)
    ENDIF

   	IF ((DATE) tExpression ):IsEmpty
		RETURN 0
	ENDIF

	VAR dtOffset := tExpression :AddMonths( 1 - (INT) nMonth  )

    RETURN  (INT) System.Math.Ceiling((DECIMAL)dtOffset:Month / 3)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/week/*" />
FUNCTION Week( dExpression AS USUAL, nFirstWeek := 1 AS LONG, nFirstDayOfWeek := 1 AS LONG) AS LONG
    if IsDate(dExpression)
        RETURN Week ( (Date) dExpression , nFirstWeek, nFirstDayOfWeek )
    endif
    if IsDateTime(dExpression)
        RETURN Week ( (DateTime) dExpression , nFirstWeek, nFirstDayOfWeek )
    endif
    THROW _DateTimeError(nameof(dExpression), 1, dExpression, nFirstWeek,nFirstDayOfWeek)


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/week/*" />
FUNCTION Week( dExpression AS DATE, nFirstWeek := 1 AS LONG, nFirstDayOfWeek := 1 AS LONG) AS LONG
    RETURN Week ((DateTime) dExpression, nFirstWeek, nFirstDayOfWeek)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/week/*" />
FUNCTION Week( tExpression AS DateTime, nFirstWeek := 1 AS LONG, nFirstDayOfWeek := 1 AS LONG) AS LONG
    // validate parameters
	IF  ! (nFirstWeek  >= 0 .AND. nFirstWeek  < 4 )
		THROW ArgumentException { __VfpStr(VFPErrors.VFP_INVALID_RANGE, nameof(nFirstWeek), nFirstWeek ,"1-3"), nameof(nFirstWeek) }
    ENDIF
	IF  ! (nFirstDayOfWeek >= 0 .AND. nFirstDayOfWeek  < 8 )
		THROW ArgumentException { __VfpStr(VFPErrors.VFP_INVALID_RANGE, nameof(nFirstDayOfWeek), nFirstDayOfWeek,"0-7"), nameof(nFirstDayOfWeek) }
    ENDIF
    LOCAL week AS CalendarWeekRule
    SWITCH nFirstWeek
    CASE 0
    CASE 1
        week := CalendarWeekRule.FirstDay
    CASE 2
        week := CalendarWeekRule.FirstFourDayWeek
    CASE 3
    OTHERWISE
        week := CalendarWeekRule.FirstFullWeek
    END SWITCH
    LOCAL day AS DayOfWeek
    IF nFirstDayOfWeek == 0
       day :=  DayOfWeek.Sunday
    ELSE
       day :=  (DayOfWeek) (nFirstDayOfWeek -1)
    ENDIF
    VAR calendar := System.Globalization.GregorianCalendar{}
    RETURN calendar:GetWeekOfYear ( tExpression, week, day)


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/mdy/*" />
FUNCTION MDY ( tExpression AS USUAL ) AS STRING
    if IsDate(tExpression)
        RETURN MDY( (Date) tExpression )
    endif
    if IsDateTime(tExpression)
        RETURN MDY ( (DateTime) tExpression  )
    endif
    THROW _DateTimeError(nameof(tExpression), 1, tExpression)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/mdy/*" />
FUNCTION MDY ( tExpression AS DateTime ) AS STRING
    RETURN MDY ( (DATE) tExpression  )

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/mdy/*" />
FUNCTION MDY ( dExpression AS DATE ) AS STRING

	IF dExpression:IsEmpty
		// Localized error text: "* invalid date *"
		RETURN __VfpStr(VFPErrors.VFP_INVALID_DATE)
	ENDIF

	RETURN CMonth(dExpression) + " " + PadL(Day(dExpression), 2 , "0" )  + ", " + ;
			IIF ( SetCentury() , dExpression:ToString("yyyy") , dExpression:ToString("yy")  )

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/dmy/*" />
FUNCTION DMY ( tExpression  AS USUAL ) AS STRING
    if IsDate(tExpression)
        RETURN DMY( (Date) tExpression )
    endif
    if IsDateTime(tExpression)
        RETURN DMY ( (DateTime) tExpression  )
    endif
    THROW _DateTimeError(nameof(tExpression),1, tExpression)


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/dmy/*" />
FUNCTION DMY ( tExpression  AS DateTime ) AS STRING
RETURN DMY ( (DATE) tExpression   )

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/dmy/*" />
FUNCTION DMY ( dExpression  AS DATE ) AS STRING
	IF dExpression:IsEmpty
		// Localized error text: "* invalid date *"
		RETURN __VfpStr(VFPErrors.VFP_INVALID_DATE)
	ENDIF

	RETURN PadL(Day(dExpression), 2 , "0" ) + " " + CMonth(dExpression) + " " + ;
			IIF ( SetCentury() , dExpression:ToString("yyyy") , dExpression:ToString("yy")  )
