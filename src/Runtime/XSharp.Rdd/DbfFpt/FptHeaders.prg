//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Text
USING XSharp.RDD.Enums
USING XSharp.RDD.Support
USING XSharp.RDD.CDX
USING System.Runtime.InteropServices
USING System.IO
USING STATIC XSharp.Conversions
USING XSharp.RDD.FlexFile
BEGIN NAMESPACE XSharp.RDD
INTERNAL ABSTRACT CLASS FptHeader
    PROTECT INITONLY _Buffer AS BYTE[]
    PROTECT INITONLY _oRDD  AS Workarea
    PROTECT _Stream as FileStream
    PROTECT INITONLY _OffSet as LONG
    PROTECT INITONLY _Length AS LONG
    PRIVATE CONST LOCK_TIMEOUT := 10_000 AS LONG    // Milliseconds
    PRIVATE CONST LOCK_WAIT    := 4 AS LONG // Ticks (18.2 per second)
    INTERNAL PROPERTY Stream   AS FileStream GET SELF:_Stream SET SELF:_Stream := value
    INTERNAL PROPERTY ReadOnly as LOGIC GET _oRDD:ReadOnly
    INTERNAL PROPERTY Shared   as LOGIC GET _oRDD:Shared
    PROTECTED PROPERTY Buffer as BYTE[] GET _Buffer
    PROTECTED PROPERTY OffSet as LONG GET _OffSet
    PROTECTED PROPERTY Length as LONG GET _Length
    INTERNAL CONSTRUCTOR(oRDD as Workarea, nOffSet as LONG, nLength as LONG)
        SELF:_oRDD := oRDD
        SELF:_Buffer := BYTE[]{nLength}
        SELF:_OffSet := nOffSet
        SELF:_Length := nLength
        SELF:_Stream := NULL

    INTERNAL VIRTUAL METHOD Clear() AS VOID
        Array.Clear(SELF:Buffer, 0, SELF:Buffer:Length)

    INTERNAL METHOD Read() AS LOGIC
        local lOk := FALSE AS LOGIC
        IF SELF:Stream != NULL
            DO WHILE ! lOk
                SELF:Stream:SafeSetPos(SELF:OffSet)
                lOk := SELF:Stream:SafeRead(Buffer, SELF:Length)
                IF ! lOk
                    if SELF:Stream:Length < SELF:OffSet+SELF:Length
                        EXIT
                    ENDIF
                    System.Threading.Thread.Sleep(5)
                ENDIF
            ENDDO
        ENDIF
        RETURN lOk
    INTERNAL METHOD Write() AS LOGIC
        local lOk := FALSE AS LOGIC
        IF SELF:Stream != NULL
            DO WHILE ! lOk
                SELF:Stream:SafeSetPos(SELF:OffSet)
                lOk := SELF:Stream:SafeWrite(Buffer)
                IF ! lOk
                    if SELF:Stream:Length < SELF:OffSet + SELF:Length
                        EXIT
                    ENDIF
                    System.Threading.Thread.Sleep(5)
                ENDIF
            ENDDO
        ENDIF
        RETURN lOk
        STATIC PRIVATE rand := Random{100} AS System.Random

     PROTECTED METHOD _LockRetry(nOffSet AS INT64, nLen AS INT64) AS VOID
        LOCAL result := FALSE AS LOGIC
        var startTime := System.Environment.TickCount
        var endTime := startTime + LOCK_TIMEOUT
        REPEAT
            result := SELF:Stream:SafeLock(nOffSet, nLen)
            IF result
                EXIT
            ENDIF
            var now := System.Environment.TickCount
            if now < startTime
                startTime := now
            endif
            now += LOCK_WAIT
            DO WHILE System.Environment.TickCount < now
                NOP
            ENDDO
            IF now > endTime
                // Throw Exception
                SELF:_oRDD:_dbfError(  Subcodes.ERDD_WRITE_LOCK_TIMEOUT, Gencode.EG_LOCK_TIMEOUT,__FUNCTION__,"Lock failed")
            ENDIF
        UNTIL result
        RETURN
    PROTECTED METHOD _Unlock(nOffSet AS INT64, nLen AS INT64) AS LOGIC
        VAR res := SELF:Stream:SafeUnlock(nOffSet, nLen)
        RETURN res
END CLASS
INTERNAL CLASS FoxHeader INHERIT FptHeader
    // FoxPro memo Header:
    // Byte offset  Description
    // 00 - 03      Location of next free block
    // 04 - 05      Unused
    // 06 - 07      Block size (bytes per block)
    // 08 - 511     Unused
    // Note numbers are in FOX notation: Integers stored with the most significant byte first.

    INTERNAL CONST OFFSET_NEXTFREE  := 0 AS LONG
    INTERNAL CONST OFFSET_UNUSED    := 4 AS LONG
    INTERNAL CONST OFFSET_BLOCKSIZE := 6 AS LONG
    INTERNAL CONST FOXHEADER_LENGTH := 512 AS LONG
    INTERNAL CONST FOXHEADER_OFFSET := 0 AS LONG

    INTERNAL CONSTRUCTOR(oRDD as Workarea)
        SUPER(oRDD,FOXHEADER_OFFSET, FOXHEADER_LENGTH)



    INTERNAL PROPERTY BlockSize AS WORD
        GET
            RETURN BuffToWordFox(Buffer, OFFSET_BLOCKSIZE)
        END GET
        SET
            IF VALUE >= FlexArea.MIN_FOXPRO_BLOCKSIZE
                WordToBuffFox(VALUE, Buffer, OFFSET_BLOCKSIZE)
            ELSE
                WordToBuffFox(0, Buffer, OFFSET_BLOCKSIZE)
            ENDIF
        END SET
    END PROPERTY
    INTERNAL PROPERTY NextFree AS LONG
        GET
            RETURN BuffToLongFox(Buffer, OFFSET_NEXTFREE)
        END GET
        SET
            LongToBuffFox(VALUE, Buffer, OFFSET_NEXTFREE)
        END SET
    END PROPERTY
    INTERNAL PROPERTY UnUsed AS WORD
        GET
            RETURN BuffToWordFox(Buffer, OFFSET_UNUSED)
        END GET
        SET
            WordToBuffFox(value, Buffer, OFFSET_UNUSED)
        END SET
    END PROPERTY



END CLASS


INTERNAL CLASS FlexHeader INHERIT FptHeader
    // FlexFile Header starts as 512. Following positions are relative from pos 512
    // 00 - 08      Header "FlexFile3"
    // 09 - 09      Version Major
    // 10 - 10      Version Minor
    // 11 - 11      Index = defect
    // 12 - 15      Index Length
    // 16 - 19      Index location
    // 20 - 23      Update Count
    // 24 - 27      Root Pointer
    // 28 - 29      Alternative Block Size (allows block sizes < 32)

    PRIVATE _RootLocked as LOGIC

    INTERNAL CONST FLEXHEADER_OFFSET := 512 AS LONG
    INTERNAL CONST FLEXHEADER_LENGTH := 512 AS LONG
    INTERNAL CONST OFFSET_SIGNATURE :=  0 AS LONG
    INTERNAL CONST LEN_SIGNATURE    :=  9 AS LONG
    INTERNAL CONST OFFSET_MAJOR     :=  9 AS LONG
    INTERNAL CONST OFFSET_MINOR     := 10 AS LONG
    INTERNAL CONST OFFSET_DEFECT    := 11 AS LONG
    INTERNAL CONST OFFSET_INDEXLEN  := 12 AS LONG
    INTERNAL CONST OFFSET_INDEXLOC  := 16 AS LONG
    INTERNAL CONST OFFSET_UPDATE    := 20 AS LONG
    INTERNAL CONST OFFSET_ROOT      := 24 AS LONG
    INTERNAL CONST OFFSET_BLOCKSIZE := 28 AS LONG

    INTERNAL CONSTRUCTOR(oRDD as Workarea)
        SUPER(oRDD, FLEXHEADER_OFFSET, FLEXHEADER_LENGTH)

    INTERNAL METHOD Create() AS VOID
        SELF:Clear()
    INTERNAL OVERRIDE METHOD Clear() AS VOID
        SUPER:Clear()
        SELF:Buffer[0] := (BYTE) 'F'
        SELF:Buffer[1] := (BYTE) 'l'
        SELF:Buffer[2] := (BYTE) 'e'
        SELF:Buffer[3] := (BYTE) 'x'
        SELF:Buffer[4] := (BYTE) 'F'
        SELF:Buffer[5] := (BYTE) 'i'
        SELF:Buffer[6] := (BYTE) 'l'
        SELF:Buffer[7] := (BYTE) 'e'
        SELF:Buffer[8] := (BYTE) '3'
        SELF:MajorVersion := 2
        SELF:MinorVersion := 8
        SELF:IndexDefect  := FALSE


    INTERNAL PROPERTY AltBlockSize  AS WORD  GET BuffToWord(SELF:Buffer, OFFSET_BLOCKSIZE)  SET WordToBuff(value, SELF:Buffer, OFFSET_BLOCKSIZE)
    INTERNAL PROPERTY MajorVersion  AS BYTE  GET SELF:Buffer[OFFSET_MAJOR]                  SET SELF:Buffer[OFFSET_MAJOR] := value
    INTERNAL PROPERTY MinorVersion  AS BYTE  GET SELF:Buffer[OFFSET_MINOR]                  SET SELF:Buffer[OFFSET_MINOR] := value
    INTERNAL PROPERTY IndexDefect   AS LOGIC GET SELF:Buffer[OFFSET_DEFECT] != 0            SET SELF:Buffer[OFFSET_DEFECT] := (BYTE) IIF(value,1,0)
    INTERNAL PROPERTY IndexLength   AS LONG  GET BuffToLong(SELF:Buffer, OFFSET_INDEXLEN )  SET LongToBuff(value, SELF:Buffer, OFFSET_INDEXLEN )
    INTERNAL PROPERTY IndexLocation AS LONG  GET BuffToLong(SELF:Buffer, OFFSET_INDEXLOC )  SET LongToBuff(value, SELF:Buffer, OFFSET_INDEXLOC )
    INTERNAL PROPERTY Root          AS LONG  GET BuffToLong(SELF:Buffer, OFFSET_ROOT )      SET LongToBuff(value, SELF:Buffer, OFFSET_ROOT )
    INTERNAL PROPERTY UpdateCount   AS LONG  GET BuffToLong(SELF:Buffer, OFFSET_UPDATE )    SET LongToBuff(VALUE, SELF:Buffer, OFFSET_UPDATE )


    INTERNAL PROPERTY Valid AS LOGIC
        GET
            RETURN  SELF:Buffer[0] == (BYTE) 'F' .AND. ;
                    SELF:Buffer[1] == (BYTE) 'l' .AND. ;
                    SELF:Buffer[2] == (BYTE) 'e' .AND. ;
                    SELF:Buffer[3] == (BYTE) 'x' .AND. ;
                    SELF:Buffer[4] == (BYTE) 'F' .AND. ;
                    SELF:Buffer[5] == (BYTE) 'i' .AND. ;
                    SELF:Buffer[6] == (BYTE) 'l' .AND. ;
                    SELF:Buffer[7] == (BYTE) 'e' .AND. ;
                    SELF:Buffer[8] == (BYTE) '3'

        END GET
    END PROPERTY
    METHOD RootLock() AS LOGIC
        IF SELF:Shared
             _LockRetry(FLEXHEADER_OFFSET+OFFSET_ROOT,sizeof(LONG))
             SELF:_RootLocked := TRUE
             RETURN TRUE
        ENDIF
        RETURN TRUE

    METHOD RootUnLock() AS LOGIC
        IF SELF:Shared .and. SELF:_RootLocked
             SELF:_Unlock(FLEXHEADER_OFFSET+OFFSET_ROOT,sizeof(LONG))
             SELF:_RootLocked := FALSE
             RETURN TRUE
        ENDIF
        RETURN TRUE

END CLASS

END NAMESPACE
