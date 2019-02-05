//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System
USING System.Collections
USING System.Text
USING XSharp.Rdd.Support

BEGIN NAMESPACE XSharp.RDD

    INTERNAL INTERFACE IRddSortWriter
        METHOD WriteSorted( sortInfo AS DBSORTINFO , obj AS OBJECT ) AS LOGIC
    
	END INTERFACE
            
            
    INTERNAL CLASS RddSortHelper
        PRIVATE _sortInfo AS DBSORTINFO
        PRIVATE _recCount AS LONG
        PRIVATE _dataBuffer AS OBJECT[]
        PRIVATE _currentPos AS LONG
        PRIVATE _Length AS LONG
        
        
        INTERNAL CONSTRUCTOR( sortInfo AS DBSORTINFO , len AS LONG )
            SELF:_sortInfo := sortInfo
            SELF:_recCount := len
            SELF:_dataBuffer := OBJECT[]{ len }
            SELF:_currentPos := 0
            SELF:_Length    := len
            
            
        INTERNAL METHOD Add(o AS OBJECT ) AS LOGIC
            IF SELF:_currentPos < SELF:_Length
                SELF:_dataBuffer[SELF:_currentPos++] := o
                RETURN TRUE
            ENDIF
            RETURN FALSE
            
            
        INTERNAL METHOD Sort(ic AS IComparer ) AS LOGIC
            TRY
                Array.Sort(SELF:_dataBuffer, ic)
                RETURN TRUE
                
            CATCH
                RETURN FALSE
            END TRY
            
            
        INTERNAL METHOD Write(isw AS IRddSortWriter ) AS LOGIC
            TRY
                FOREACH o AS OBJECT IN SELF:_dataBuffer 
                    isw:WriteSorted(SELF:_sortInfo, o)
                NEXT
                RETURN TRUE
                
            CATCH
                RETURN FALSE
            END TRY
            
            
        INTERNAL METHOD Clear() AS VOID
            SELF:_dataBuffer := NULL
            SELF:_sortInfo := NULL
            SELF:_recCount := 0
            SELF:_currentPos := (SELF:_Length := 0)
            
            
    END CLASS
    
    
END NAMESPACE
