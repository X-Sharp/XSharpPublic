//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


USING System
USING System.Collections
USING System.Text
USING XSharp.Rdd.Support
USING XSharp.RDD.Enums
BEGIN NAMESPACE XSharp.RDD.CDX

            
    INTERNAL CLASS CdxSortCompare IMPLEMENTS IComparer
        PROTECTED _sortInfo AS DBSORTINFO
        PROTECTED _oRdd AS DBFNTX
        
        INTERNAL CONSTRUCTOR( rdd AS DBFNTX , info AS DBSORTINFO )
            SELF:_oRdd := rdd
            SELF:_sortInfo := info

        PROTECTED VIRTUAL METHOD XCompare(x AS SortRecord , y AS SortRecord) AS LONG
            RETURN 0

        PUBLIC METHOD Compare(x AS OBJECT , y AS OBJECT ) AS LONG
            RETURN XCompare( (SortRecord) x, (SortRecord) y)
    END CLASS
    
    INTERNAL CLASS CdxSortCompareDefault INHERIT CdxSortCompare 
        
        INTERNAL CONSTRUCTOR( rdd AS DBFNTX , info AS DBSORTINFO )
            SUPER(rdd, info)
            
            
        PROTECTED OVERRIDE METHOD XCompare(x AS SortRecord , y AS SortRecord) AS LONG
            LOCAL dataBuffer AS BYTE[]
            LOCAL dataBuffer2 AS BYTE[]
            LOCAL diff AS LONG
            LOCAL iLen AS LONG
            LOCAL indxFlags AS DbSortFlags
            //
            IF (x:Recno == y:Recno)
                RETURN 0
            ENDIF
            //
            dataBuffer  := x:data
            dataBuffer2 := y:data
            diff        := 0
            iLen        := SELF:_sortInfo:Items[0]:Length
            indxFlags   := SELF:_sortInfo:Items[0]:Flags
            // comparison using string rules
            diff := RuntimeState.StringCompare(dataBuffer, dataBuffer2, iLen)
            IF indxFlags:HasFlag(DbSortFlags.Descending) 
                diff *= -1
            ENDIF
            IF (diff == 0)
                IF x:Recno < y:Recno
                    diff := -1
                ELSE
                    diff := 1
                ENDIF
            ENDIF
            RETURN diff
            
            
    END CLASS

    INTERNAL CLASS CdxSortCompareAscii INHERIT CdxSortCompare 
        
        INTERNAL CONSTRUCTOR( rdd AS DBFNTX , info AS DBSORTINFO )
            SUPER(rdd, info)
             
            
        PROTECTED OVERRIDE METHOD XCompare(x AS SortRecord , y AS SortRecord) AS LONG
            LOCAL dataBuffer AS BYTE[]
            LOCAL dataBuffer2 AS BYTE[]
            LOCAL diff AS LONG
            LOCAL i AS LONG
            LOCAL iLen AS LONG
            LOCAL indxFlags AS DbSortFlags
            //
            IF (x:Recno == y:Recno)
                RETURN 0
            ENDIF
            //
            dataBuffer  := x:data
            dataBuffer2 := y:data
            diff        := 0
            iLen        := SELF:_sortInfo:Items[0]:Length
            indxFlags   := SELF:_sortInfo:Items[0]:Flags
            // Binary comparison
            FOR i := 0 TO ( iLen - 1 )
                diff := (LONG) dataBuffer[i] - (LONG) dataBuffer2[i]
                IF (diff != 0)
                    EXIT
                ENDIF
            NEXT
            IF indxFlags:HasFlag(DbSortFlags.Descending) 
                diff *= -1
            ENDIF
            IF (diff == 0)
                IF x:Recno < y:Recno
                    diff := -1
                ELSE
                    diff := 1
                ENDIF
            ENDIF
            RETURN diff
            
            
    END CLASS
    
END NAMESPACE
