//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


USING System
USING System.Collections
USING System.Collections.Generic
USING System.Text
USING XSharp.Rdd.Support
USING XSharp.RDD.Enums
BEGIN NAMESPACE XSharp.RDD.NTX

            
    INTERNAL CLASS NtxSortCompare IMPLEMENTS IComparer<SortRecord>
        PROTECTED _sortInfo AS DBSORTINFO
        PROTECTED _oRdd AS DBFNTX
        
        INTERNAL CONSTRUCTOR( rdd AS DBFNTX , info AS DBSORTINFO )
            SELF:_oRdd := rdd
            SELF:_sortInfo := info

        VIRTUAL METHOD Compare(x AS SortRecord , y AS SortRecord) AS LONG
            RETURN 0

    END CLASS
    
    INTERNAL CLASS NTXSortCompareDefault INHERIT NTXSortCompare 
        
        INTERNAL CONSTRUCTOR( rdd AS DBFNTX , info AS DBSORTINFO )
            SUPER(rdd, info)
            
            
        OVERRIDE METHOD Compare(x AS SortRecord , y AS SortRecord) AS LONG
            LOCAL dataBuffer AS BYTE[]
            LOCAL dataBuffer2 AS BYTE[]
            LOCAL diff AS LONG
            LOCAL iLen AS LONG
            IF x:Recno == y:Recno
                RETURN 0
            ENDIF
            dataBuffer  := x:data
            dataBuffer2 := y:data
            diff        := 0
            iLen        := SELF:_sortInfo:Items[0]:Length
            // comparison using string rules
            diff := RuntimeState.StringCompare(dataBuffer, dataBuffer2, iLen)
            IF diff != 0
                IF SELF:_sortInfo:Items[0]:Flags:HasFlag(DbSortFlags.Descending) 
                    diff := -diff
                ENDIF
            ELSE
                IF x:Recno < y:Recno
                    diff := -1
                ELSE
                    diff := 1
                    y:Duplicate := TRUE
                ENDIF
            ENDIF
            
            RETURN diff
            
            
    END CLASS

    INTERNAL CLASS NTXSortCompareAscii INHERIT NtxSortCompare 
        
        INTERNAL CONSTRUCTOR( rdd AS DBFNTX , info AS DBSORTINFO )
            SUPER(rdd, info)
             
            
        OVERRIDE METHOD Compare(x AS SortRecord , y AS SortRecord) AS LONG
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
            iLen        := SELF:_sortInfo:Items[0]:Length
            // Binary comparison
            FOR i := 0 TO ( iLen - 1 )
                diff := (LONG) dataBuffer[i] - (LONG) dataBuffer2[i]
                IF diff != 0
                    EXIT
                ENDIF
            NEXT
            IF diff != 0
                IF SELF:_sortInfo:Items[0]:Flags:HasFlag(DbSortFlags.Descending) 
                    diff := -diff
                ENDIF
            ELSE
                IF x:Recno < y:Recno
                    diff := -1
                ELSE
                    diff := 1
                    y:Duplicate := TRUE
                ENDIF
            ENDIF
            RETURN diff
            
            
    END CLASS
    
END NAMESPACE
