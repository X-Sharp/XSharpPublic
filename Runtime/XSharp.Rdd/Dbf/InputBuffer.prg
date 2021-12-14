//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// Please note that this code code expects zero based arrays
#pragma options ("az", ON)

USING System
USING System.Collections.Generic
USING System.Text
USING System.IO
USING XSharp.RDD.Enums
USING XSharp.RDD.Support

BEGIN NAMESPACE XSharp.RDD

	CLASS InputBuffer
        CONST INBUFFER_MAX_SIZE := 16384 AS INT

        PROTECT _stream        AS Stream
        PROTECT _recordLength  AS LONG
        PROTECT _headerLength  AS LONG
        PROTECT _shared        AS LOGIC

        PROTECT _look_ahead    AS INT
        PROTECT _look_behind   AS INT
        PROTECT _inBuffer      AS BYTE[]
        PROTECT _inBufferOfs   AS LONG
        PROTECT _inBufferLen   AS LONG
        PROTECT _inBufferTick  AS INT

        CONSTRUCTOR(stream AS Stream, headerLength AS INT, recordLength AS INT, shared AS LOGIC) AS VOID
            _stream := stream
            _recordLength := recordLength
            _headerLength := headerLength
            _shared := shared
            _inBufferOfs := 0
            _inBufferLen := 0

            VAR nRec := INBUFFER_MAX_SIZE / SELF:_recordLength
            _look_ahead := nRec - nRec/4
            _look_behind := nRec - _look_ahead - 1

            IF _look_ahead > 0 && _look_behind >= 0
                _inBuffer := BYTE[]{(1+SELF:_look_ahead+SELF:_look_behind)*SELF:_recordLength}
            ELSE
                _inBuffer := NULL
            ENDIF

            RETURN

        PRIVATE METHOD ReadAt(position AS LONG, buffer AS BYTE[], offset AS LONG, length AS LONG) AS LONG
            LOCAL result AS LONG
            TRY
                _stream:Position := position
                result := _stream:Read(buffer, offset, length)
            CATCH
                result := -1
            END TRY
            RETURN result

        PRIVATE METHOD WriteAt(position AS LONG, buffer AS BYTE[], offset AS LONG, length AS LONG) AS LOGIC
            LOCAL result AS LOGIC
            TRY
                _stream:Position := position
                _stream:Write(buffer, offset, length)
                result := TRUE
            CATCH
                result := FALSE
            END TRY
            RETURN result

        METHOD Read(offset AS LONG, buffer AS BYTE[], size AS LONG) AS LOGIC
            LOCAL tick := 0 AS INT
            IF _inBuffer == NULL
                RETURN SELF:ReadAt(offset, buffer, 0, size) == size
            ENDIF
            IF SELF:_shared
                VAR refresh := XSharp.RuntimeState.GetValue<REAL8>(Set.Refresh)
                IF refresh < 0
                    RETURN SELF:ReadAt(offset, buffer, 0, size) == size
                ENDIF
                tick := System.Environment.TickCount
                IF refresh > 0 .AND. (tick - _inBufferTick) >= 1000*refresh
                    _inBufferLen := 0
                ENDIF
            ENDIF
            IF _inBufferOfs > offset .OR. _inBufferOfs+_inBufferLen < offset+size
                LOCAL ofs AS LONG
                IF offset > _inBufferOfs
                    ofs := offset - SELF:_look_behind*SELF:_recordLength
                ELSE
                    ofs := offset - SELF:_look_ahead*SELF:_recordLength
                ENDIF
                IF ofs < SELF:_headerLength
                    ofs := SELF:_headerLength
                ENDIF
                VAR len := _inBuffer:Length
                VAR bwd := FALSE
                IF ofs > _inBufferOfs .AND. ofs < _inBufferOfs+_inBufferLen
                    VAR delta := ofs - _inBufferOfs
                    _inBufferLen -= delta
                    _inBufferOfs := ofs
                    ofs += _inBufferLen
                    len -= _inBufferLen
                    Array.Copy(_inBuffer, delta, _inBuffer, 0, _inBufferLen)
                ELSEIF ofs < _inBufferOfs .AND. ofs+len > _inBufferOfs .AND. _inBufferLen > 0
                    VAR delta := _inBufferOfs - ofs
                    IF _inBufferLen+delta > len
                        _inBufferLen -= _inBufferLen + delta - len
                    ENDIF
                    _inBufferOfs := ofs
                    len := delta
                    bwd := TRUE
                    Array.Copy(_inBuffer, 0, _inBuffer, delta, _inBufferLen)
                ELSE
                    _inBufferLen := 0
                    _inBufferOfs := ofs
                ENDIF

                _inBufferTick := tick
                VAR res := SELF:ReadAt(ofs, _inBuffer, ofs-_inBufferOfs, len)
                IF res > 0
                    _inBufferLen += res
                    IF bwd .AND. res != len
                        _inBufferLen := res
                    ENDIF
                ELSE
                    _inBufferLen := 0
                ENDIF

                IF _inBufferOfs+_inBufferLen < offset+size
                    RETURN FALSE
                ENDIF
            ENDIF
            Array.Copy( _inBuffer, offset - _inBufferOfs, buffer, 0, size )
            RETURN TRUE

        METHOD Write(offset AS LONG, buffer AS BYTE[], size AS LONG) AS LOGIC
            VAR res := SELF:WriteAt(offset, buffer, 0, size)
            IF res .AND. _inBuffer != NULL
                IF offset >= _inBufferOfs .AND. offset+size <= _inBufferOfs+_inBufferLen
                    Array.Copy( buffer, 0, _inBuffer, offset - _inBufferOfs, size )
                ENDIF
            ENDIF
            RETURN res

        METHOD Invalidate() AS VOID
            _inBufferOfs := 0
            _inBufferLen := 0
            RETURN
    END CLASS

END NAMESPACE // XSharp.RDD
