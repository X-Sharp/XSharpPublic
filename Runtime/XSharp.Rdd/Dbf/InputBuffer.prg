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
        PROTECT _name          AS STRING
        PROTECT _recordLength  AS LONG
        PROTECT _headerLength  AS LONG
        PROTECT _shared        AS LOGIC

        PROTECT _look_ahead    AS INT
        PROTECT _look_behind   AS INT
        PROTECT _inBuffer      AS BYTE[]
        PROTECT PROPERTY _inBufferOfs AS LONG GET _weakBuffer:InBufferOfs SET _weakBuffer:InBufferOfs := VALUE
        PROTECT PROPERTY _inBufferLen   AS LONG GET _weakBuffer:InBufferLen SET _weakBuffer:InBufferLen := VALUE
        PROTECT PROPERTY _inBufferTick  AS INT GET _weakBuffer:InBufferTick SET _weakBuffer:InBufferTick := VALUE

        PRIVATE _weakBuffer    AS WeakBuffer

        CLASS WeakBuffer
            INTERNAL RefCount := 1      AS LONG
            INTERNAL Buffer             AS BYTE[]
            INTERNAL InBufferOfs := 0   AS LONG
            INTERNAL InBufferLen := 0   AS LONG
            INTERNAL InBufferTick := 0  AS INT
            CONSTRUCTOR(size AS LONG)
                Buffer := BYTE[]{size}
            INTERNAL METHOD Resize(size AS LONG) AS VOID
                System.Array.Resize(REF Buffer, size)
        END CLASS

        PRIVATE STATIC BufferCache := Dictionary<STRING, WeakBuffer>{} AS Dictionary<STRING, WeakBuffer>

        CONSTRUCTOR(stream AS Stream, headerLength AS INT, recordLength AS INT, shared AS LOGIC) AS VOID
            _stream := stream
            _name := (stream ASTYPE FileStream)?:Name
            _recordLength := recordLength
            _headerLength := headerLength
            _shared := shared

            VAR nRec := INBUFFER_MAX_SIZE / SELF:_recordLength
            _look_ahead := nRec - nRec/4
            _look_behind := nRec - _look_ahead - 1

            IF _look_ahead > 0 && _look_behind >= 0
                VAR size := (1+SELF:_look_ahead+SELF:_look_behind)*SELF:_recordLength
                IF _name != NULL
                    BEGIN LOCK BufferCache
                        LOCAL t AS WeakBuffer
                        IF BufferCache:TryGetValue(_name, OUT t)
                            t:RefCount += 1
                            IF size != t:Buffer:Length
                                t:Resize(size)
                            ENDIF
                        ELSE
                            t := WeakBuffer{size}
                            BufferCache:Add(_name, t)
                        ENDIF
                        _weakBuffer := t
                        _inBuffer := t:Buffer
                    END LOCK
                ELSE
                    _weakBuffer := WeakBuffer{size}
                    _inBuffer := _weakBuffer:Buffer
                ENDIF
            ELSE
                _weakBuffer := NULL
                _inBuffer := NULL
            ENDIF

            RETURN

        METHOD Close() AS VOID
            IF _name != NULL
                BEGIN LOCK BufferCache
                    LOCAL t AS WeakBuffer
                    IF BufferCache:TryGetValue(_name, OUT t)
                        t:RefCount -= 1
                        IF t:RefCount < 1 .or. ! _shared
                            BufferCache:Remove(_name)
                        ENDIF
                    ENDIF
                END LOCK
                _name := NULL
                GC.SuppressFinalize( SELF )
            ENDIF

        DESTRUCTOR()
            SELF:Close()
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
            BEGIN LOCK _weakBuffer
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
            END LOCK
            RETURN TRUE

        METHOD Write(offset AS LONG, buffer AS BYTE[], size AS LONG) AS LOGIC
            VAR res := SELF:WriteAt(offset, buffer, 0, size)
            IF res .AND. _inBuffer != NULL
                BEGIN LOCK _weakBuffer
                    IF offset < _inBufferOfs+_inBufferLen .AND. offset+size > _inBufferOfs
                        VAR buffer_ofs := Math.Max( 0, _inBufferOfs - offset )
                        VAR inbuffer_ofs := Math.Max( 0, offset - _inBufferOfs )
                        VAR buffer_size := Math.Min( size - buffer_ofs, _inBufferLen - inbuffer_ofs )
                        Array.Copy( buffer, buffer_ofs, _inBuffer, inbuffer_ofs, buffer_size )
                    ENDIF
                END LOCK
            ENDIF
            RETURN res

        METHOD Invalidate() AS VOID
            BEGIN LOCK _weakBuffer
                _inBufferOfs := 0
                _inBufferLen := 0
            END LOCK
            RETURN

    END CLASS

END NAMESPACE // XSharp.RDD

