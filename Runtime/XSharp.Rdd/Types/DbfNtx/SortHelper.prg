// SortHekper.prg
// Created by    : fabri
// Creation Date : 9/6/2018 10:22:38 AM
// Created for   : 
// WorkStation   : FABPORTABLE


USING System
USING System.Collections
USING System.Text
USING XSharp.Rdd.Support

BEGIN NAMESPACE XSharp.RDD

    PUBLIC INTERFACE IRddSortWriter
        METHOD WriteSorted( sortInfo AS DBSORTINFO , obj AS OBJECT ) AS LOGIC
            END INTERFACE
            
            
    PUBLIC CLASS RddSortHelper
        PRIVATE _sortInfo AS DBSORTINFO
        PRIVATE _recCount AS DWORD
        PRIVATE _dataBuffer AS OBJECT[]
        PRIVATE _currentPos AS LONG
        PRIVATE _Length AS LONG
        
        
        PUBLIC CONSTRUCTOR( sortInfo AS DBSORTINFO , len AS DWORD )
            SELF:_sortInfo := sortInfo
            SELF:_recCount := len
            SELF:_dataBuffer := OBJECT[]{ len }
            SELF:_currentPos := 0
            SELF:_Length := (LONG)len
            
            
        PUBLIC METHOD ADD(o AS OBJECT ) AS LOGIC
            IF (SELF:_currentPos < SELF:_Length)
                SELF:_dataBuffer[SELF:_currentPos++] := o
                RETURN TRUE
            ENDIF
            RETURN FALSE
            
            
        PUBLIC METHOD Sort(ic AS IComparer ) AS LOGIC
            TRY
                Array.Sort(SELF:_dataBuffer, ic)
                RETURN TRUE
                
            CATCH
                RETURN FALSE
            END TRY
            
            
        PUBLIC METHOD Write(isw AS IRddSortWriter ) AS LOGIC
            TRY
                FOREACH o AS OBJECT IN SELF:_dataBuffer 
                    isw:WriteSorted(SELF:_sortInfo, o)
                NEXT
                RETURN TRUE
                
            CATCH
                RETURN FALSE
            END TRY
            
            
        PUBLIC METHOD Clear() AS VOID
            SELF:_dataBuffer := NULL
            SELF:_sortInfo := NULL
            SELF:_recCount := 0u
            SELF:_currentPos := (SELF:_Length := 0)
            
            
    END CLASS
    
    
END NAMESPACE