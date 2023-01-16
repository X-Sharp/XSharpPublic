//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


USING XSharp.RDD.Enums
USING XSharp.RDD.Support
USING System.Collections.Generic

BEGIN NAMESPACE XSharp.RDD


    INTERNAL CLASS DBFSortCompare IMPLEMENTS IComparer<SortRecord>
        
        PRIVATE _sortInfo AS DbSortInfo
        PRIVATE _tempX AS BYTE[]
        PRIVATE _tempY AS BYTE[]
        
        INTERNAL CONSTRUCTOR( rdd AS DBF, info AS DbSortInfo )
            SELF:_sortInfo  := info
            LOCAL max       := 0 AS INT
            FOREACH VAR item IN info:Items
                max := Math.Max(max, item:Length)
            NEXT
            SELF:_tempX := BYTE[]{ max}     
            SELF:_tempY := BYTE[]{ max}
        
        
        PUBLIC METHOD Compare(recordX AS SortRecord , recordY AS SortRecord ) AS LONG
            LOCAL recordBufferX AS BYTE[]
            LOCAL recordBufferY AS BYTE[]
            LOCAL diff AS LONG
            IF recordX:Recno == recordY:Recno
                RETURN 0
            ENDIF
            //
            recordBufferX := recordX:Data
            recordBufferY := recordY:Data
            diff := 0
            FOREACH VAR item IN SELF:_sortInfo:Items
                VAR start := item:OffSet
                VAR iLen := item:Length
                VAR flags := item:Flags
                // Long Value ?
                IF flags:HasFlag(DbSortFlags.Long)
                    VAR longValue1 := BitConverter.ToInt32(recordBufferX, start)
                    VAR longValue2 := BitConverter.ToInt32(recordBufferY, start)
                    diff := longValue1 - longValue2
                ELSE
                    // String Value ?
                    IF flags:HasFlag(DbSortFlags.Ascii)
                      // String ASCII : Use Runtime comparer
                      IF start == 0
                            // avoid copying when start = 0
                            diff := XSharp.RuntimeState.StringCompare(recordBufferX, recordBufferY, iLen)
                      ELSE
                            Array.Copy(recordBufferX, start, _tempX, 0, iLen)
                            Array.Copy(recordBufferY, start, _tempY, 0, iLen)
                            diff := XSharp.RuntimeState.StringCompare(_tempX, _tempY, iLen)
                      ENDIF      
                        //
                    ELSE
                        FOR VAR i := 0 TO iLen - 1
                            diff := recordBufferX[i + start] - recordBufferY[i + start]
                            IF diff != 0
                                EXIT
                            ENDIF
                        NEXT
                    ENDIF
                ENDIF
                IF diff != 0
                    IF flags:HasFlag(DbSortFlags.Descending)
                        diff *= -1
                    ENDIF
                    EXIT
                ENDIF
            NEXT
            IF diff == 0
                diff := recordX:Recno - recordY:Recno
            ENDIF
            RETURN diff
            
            
        END CLASS
        
END NAMESPACE
