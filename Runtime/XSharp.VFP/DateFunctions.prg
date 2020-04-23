//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Globalization

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/dtot/*" />
FUNCTION DToT( dDateExpression AS DATE)
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
	
 

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/quarter/*" />
FUNCTION Quarter( dExpression  AS DATE , nMonth  := 1 AS INT ) AS INT 
    RETURN Quarter ( (DateTime) dExpression  , nMonth  )  


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/quarter/*" />
FUNCTION Quarter( tExpression  AS DateTime , nMonth  := 1 AS INT ) AS INT 
	IF  ! (nMonth  > 0 .AND. nMonth  < 13 )
		THROW ArgumentException { String.Format("nMonth param value is {0}, must be in the range 1-12", nMonth ), "nMonth" } 
    ENDIF 
    
   	IF ((DATE) tExpression ):IsEmpty  
		RETURN 0
	ENDIF 
	
	VAR dtOffset := tExpression :AddMonths( 1 - (INT) nMonth  )	
 
	RETURN  (INT) System.Math.Ceiling((DECIMAL)dtOffset:Month / 3)	



/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/week/*" />
FUNCTION Week( dExpression AS DATE, nFirstWeek := 1 AS LONG, nFirstDayOfWeek := 1 AS LONG) AS LONG
    RETURN Week ((DateTime) dExpression, nFirstWeek, nFirstDayOfWeek)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/week/*" />
FUNCTION Week( tExpression AS DateTime, nFirstWeek := 1 AS LONG, nFirstDayOfWeek := 1 AS LONG) AS LONG
    // validate parameters
	IF  ! (nFirstWeek  >= 0 .AND. nFirstWeek  < 4 )
		THROW ArgumentException { String.Format("{0} param value is {1}, must be in the range 0-3", nameof(nFirstWeek), nFirstWeek ), nameof(nFirstWeek) } 
    ENDIF 
	IF  ! (nFirstDayOfWeek >= 0 .AND. nFirstDayOfWeek  < 8 )
		THROW ArgumentException { String.Format("{0} param value is {1}, must be in the range 0-7", nameof(nFirstDayOfWeek), nFirstDayOfWeek ), nameof(nFirstDayOfWeek) } 
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



