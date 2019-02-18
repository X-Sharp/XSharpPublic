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
BEGIN NAMESPACE XSharp.RDD.NTX

            
    INTERNAL CLASS NtxSortCompare IMPLEMENTS IComparer
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
    
    INTERNAL CLASS NTXSortCompareDefault INHERIT NTXSortCompare 
        
        INTERNAL CONSTRUCTOR( rdd AS DBFNTX , info AS DBSORTINFO )
            SUPER(rdd, info)
            
            
        PROTECTED OVERRIDE METHOD XCompare(x AS SortRecord , y AS SortRecord) AS LONG
            LOCAL dataBuffer AS BYTE[]
            LOCAL dataBuffer2 AS BYTE[]
            LOCAL diff AS LONG
            LOCAL iLen AS LONG
            //
            IF (x:Recno == y:Recno)
                RETURN 0
            ENDIF
            //
            dataBuffer  := x:data
            dataBuffer2 := y:data
            diff        := 0
            iLen        := SELF:_sortInfo:Items[0]:Length
            // comparison using string rules
            diff := RuntimeState.StringCompare(dataBuffer, dataBuffer2, iLen)
            IF diff != 0
                IF SELF:_sortInfo:Items[0]:Flags:HasFlag(DbSortFlags.Descending) 
                    diff *= -1
                ENDIF
            ELSE
                IF x:Recno < y:Recno
                    diff := -1
                ELSE
                    diff := 1
                ENDIF
            ENDIF
            RETURN diff
            
            
    END CLASS

    INTERNAL CLASS NTXSortCompareAscii INHERIT NtxSortCompare 
        
        INTERNAL CONSTRUCTOR( rdd AS DBFNTX , info AS DBSORTINFO )
            SUPER(rdd, info)
             
            
        PROTECTED OVERRIDE METHOD XCompare(x AS SortRecord , y AS SortRecord) AS LONG
            LOCAL dataBuffer AS BYTE[]
            LOCAL dataBuffer2 AS BYTE[]
            LOCAL diff AS LONG
            LOCAL i AS LONG
            LOCAL iLen AS LONG
            IF x:Recno == y:Recno
                RETURN 0
            ENDIF
            dataBuffer  := x:data
            dataBuffer2 := y:data
            diff        := 0
            iLen        := SELF:_sortInfo:Items[0]:Length -1
            FOR i := 0 TO iLen
                VAR b1 := dataBuffer[i]
                VAR b2 := dataBuffer2[i]
                IF b1 < b2
                    diff := -1
                ELSEIF b1 > b2
                    diff := 1
                ENDIF
                IF diff != 0
                    EXIT
                ENDIF
            NEXT
            IF diff != 0
                IF SELF:_sortInfo:Items[0]:Flags:HasFlag(DbSortFlags.Descending) 
                    diff *= -1
                ENDIF
            ELSE
                IF x:Recno < y:Recno
                    diff := -1
                ELSE
                    diff := 1
                ENDIF
            ENDIF
            RETURN diff
            
            
    END CLASS
    
END NAMESPACE
