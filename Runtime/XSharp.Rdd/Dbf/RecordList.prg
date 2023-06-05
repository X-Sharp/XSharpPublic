//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING System.Collections
USING System.Collections.Generic
USING System.Text
USING XSharp.RDD.Enums
USING XSharp.RDD.Support

BEGIN NAMESPACE XSharp.RDD

PUBLIC CLASS RecordList
    PUBLIC ENUM RecordState
        MEMBER Unknown := 0
        MEMBER Hidden := 1
        MEMBER Visible := 2
    END ENUM

    PRIVATE CONST ALLOC_QUANTUM := 4096 AS LONG

    PRIVATE _bits AS BitArray

    PUBLIC PROPERTY Items[recNo AS LONG] AS RecordState GET GetRecState(recNo) SET SetRecState(recNo,VALUE)
    PUBLIC PROPERTY Length               AS LONG GET _bits:Length/2 SET SetLength(VALUE)

    PUBLIC CONSTRUCTOR()
        SELF:_bits := BitArray{0}

    PUBLIC METHOD GetRecState(recNo AS LONG) AS RecordState
        IF recNo < 1 .OR. recNo > Length
            RETURN RecordState.Unknown
        END IF
        var pos := (recNo-1)*2
        IF _bits[pos]
            RETURN RecordState.Hidden
        END IF
        IF _bits[pos+1]
            RETURN RecordState.Visible
        END IF
        RETURN RecordState.Unknown

    PUBLIC METHOD SetRecState(recNo AS LONG, state AS RecordState) AS VOID
        IF recNo < 1
            RETURN
        END IF
        IF recNo > Length
            Length := (recNo + ALLOC_QUANTUM - 1) & ~(ALLOC_QUANTUM - 1)
        END IF
        var pos := (recNo-1)*2
        IF state == RecordState.Visible
            _bits[pos] := False
            _bits[pos+1] := True
        ELSEIF state == RecordState.Hidden
            _bits[pos] := True
            _bits[pos+1] := False
        ELSE
            _bits[pos] := False
            _bits[pos+1] := False
        END IF
        RETURN

    PUBLIC METHOD SetLength(length AS LONG) AS VOID
        _bits:Length := length * 2
        RETURN

    PUBLIC METHOD Clear() AS VOID
        _bits:Length := 0

END CLASS

END NAMESPACE // XSharp.RDD
