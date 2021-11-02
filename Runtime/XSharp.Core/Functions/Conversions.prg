//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Runtime.InteropServices
/// <exclude />
STATIC CLASS XSharp.Conversions

// Convert from Big endian to Little Endian and Back
STATIC METHOD SwapBytes(nShort AS SHORT) AS Short
    RETURN _OR( _AND(nShort, 0xFF) <<8, _AND(nShort >>8, 0xFF))

// Convert from Big endian to Little Endian and back
STATIC METHOD SwapBytes(nLong AS LONG) AS LONG
    RETURN _OR(SwapBytes( (SHORT) nLong) << 16 , ;
        _AND(SwapBytes((short)(nLong >> 16)) , 0xFFFF))


STATIC METHOD ShortToBuff(siValue AS SHORT, buffer AS BYTE[], nOffSet AS LONG) AS VOID
    LOCAL nValue := __WordStruct{} AS __WordStruct
    nValue:shortValue := siValue
    buffer[nOffSet+0]   := nValue:b1
    buffer[nOffSet+1]   := nValue:b2

STATIC METHOD WordToBuff(wValue AS WORD, buffer AS BYTE[], nOffSet AS LONG) AS VOID
    LOCAL nValue := __WordStruct{} AS __WordStruct
    nValue:wordValue := wValue
    buffer[nOffSet+0]   := nValue:b1
    buffer[nOffSet+1]   := nValue:b2

STATIC METHOD BuffToShort(buffer AS BYTE[], nOffSet AS LONG) AS SHORT
   LOCAL siValue as SHORT
    BEGIN UNCHECKED
        siValue := (SHORT) _OR( buffer[nOffSet]  , buffer[nOffSet+1] << 8 )
    END UNCHECKED
    RETURN siValue

STATIC METHOD BuffToWord(buffer AS BYTE[], nOffSet AS LONG) AS WORD
    LOCAL wValue as WORD
    BEGIN UNCHECKED
        wValue := (WORD) _OR( buffer[nOffSet]  , buffer[nOffSet+1] << 8 )
    END UNCHECKED
    RETURN wValue

STATIC METHOD WordToBuffFox(wValue AS Word, buffer AS BYTE[], nOffSet AS LONG) AS VOID
    LOCAL nValue := __WordStruct{} AS __WordStruct
    nValue:wordValue := wValue
    buffer[nOffSet+1]   := nValue:b1
    buffer[nOffSet+0]   := nValue:b2

STATIC METHOD BuffToWordFox(buffer AS BYTE[], nOffSet AS LONG) AS WORD
    local wValue as WORD
    BEGIN UNCHECKED
        wValue := (WORD) _OR( buffer[nOffSet] << 8 , buffer[nOffSet+1] )
    END UNCHECKED
    RETURN wValue


STATIC METHOD ShortToBuffFox(siValue AS SHORT, buffer AS BYTE[], nOffSet AS LONG) AS VOID
    LOCAL nValue := __WordStruct{} AS __WordStruct
    nValue:shortValue := siValue
    buffer[nOffSet+1]   := nValue:b1
    buffer[nOffSet+0]   := nValue:b2

// Read Short from buffer in Big Endian format
STATIC METHOD BuffToShortFox( buffer AS BYTE[], nOffSet AS LONG) AS SHORT
    local siValue as short
    siValue := (SHORT) _OR( buffer[nOffSet] << 8 , buffer[nOffSet+1] )
    RETURN siValue

STATIC METHOD BuffToDword(buffer AS BYTE[], nOffSet AS LONG) AS DWORD
    local dwValue as DWORD
    BEGIN UNCHECKED
    dwValue := (DWORD) _OR(buffer[nOffSet] , buffer[nOffSet+1] <<8 , buffer[nOffSet+2] <<16 , buffer[nOffSet+3] << 24)
    END UNCHECKED
    RETURN dwValue


STATIC METHOD BuffToLong(buffer AS BYTE[], nOffSet AS LONG) AS LONG
    local liValue as LONG
    liValue := _OR(buffer[nOffSet] , buffer[nOffSet+1] <<8 , buffer[nOffSet+2] <<16 , buffer[nOffSet+3] << 24)
    RETURN liValue

STATIC METHOD LongToBuff(liValue AS LONG, buffer AS BYTE[], nOffSet AS LONG) AS LONG
    LOCAL nValue := __LongStruct{} AS __LongStruct
    nValue:longValue := liValue
    buffer[nOffSet+0] := nValue:b1
    buffer[nOffSet+1] := nValue:b2
    buffer[nOffSet+2] := nValue:b3
    buffer[nOffSet+3] := nValue:b4
    RETURN liValue


// Read Long from buffer in Big Endian format
STATIC METHOD BuffToLongFox( buffer AS BYTE[], nOffSet AS LONG) AS LONG
    local liValue as LONG
    liValue := _OR(buffer[nOffSet] << 24 , buffer[nOffSet+1] <<16 , buffer[nOffSet+2] <<8 , buffer[nOffSet+3] )
    RETURN liValue

STATIC METHOD BuffToDwordFox(buffer AS BYTE[], nOffSet AS LONG) AS DWORD
    local dwValue as DWORD
    BEGIN UNCHECKED
    dwValue := (DWORD) _OR(buffer[nOffSet] << 24 , buffer[nOffSet+1] <<16 , buffer[nOffSet+2] <<8 , buffer[nOffSet+3] )
    END UNCHECKED
    RETURN dwValue



STATIC METHOD LongToBuffFox(liValue AS LONG, buffer AS BYTE[], nOffSet AS LONG) AS LONG
    LOCAL nValue := __LongStruct{} AS __LongStruct
    nValue:longValue := liValue
    buffer[nOffSet+0] := nValue:b4
    buffer[nOffSet+1] := nValue:b3
    buffer[nOffSet+2] := nValue:b2
    buffer[nOffSet+3] := nValue:b1
    RETURN liValue

// Convert Fox Int32 number to 4 byte Index format
STATIC METHOD LongToFoxOrder(liValue AS LONG, buffer AS BYTE[]) AS VOID
    LongToBuffFox(liValue, buffer, 0)
    IF liValue >= 0
        buffer[0] |= 0x80
    ELSE
        buffer[0] := (BYTE) _AND(buffer[0],  ~0x80)
    ENDIF
    RETURN

// Convert Fox Real8 number to 8 byte Index format
STATIC METHOD DoubleToFoxOrder(r8Value AS REAL8, buffer AS BYTE[]) AS VOID
    LOCAL ds := __DoubleStruct{} AS __DoubleStruct
    ds:doubleValue := r8Value
    ds:SaveToIndex(buffer)
    RETURN

STATIC METHOD DWordToBuff(dwValue AS DWORD, buffer AS BYTE[], nOffSet AS LONG) AS DWORD
    LOCAL nValue := __LongStruct{} AS __LongStruct
    nValue:dwordValue := dwValue
    buffer[nOffSet+0] := nValue:b1
    buffer[nOffSet+1] := nValue:b2
    buffer[nOffSet+2] := nValue:b3
    buffer[nOffSet+3] := nValue:b4
    RETURN dwValue


STATIC METHOD DWordToBuffFox(dwValue AS DWORD, buffer AS BYTE[], nOffSet AS LONG) AS DWORD
    LOCAL nValue := __LongStruct{} AS __LongStruct
    nValue:dwordValue := dwValue
    buffer[nOffSet+0] := nValue:b4
    buffer[nOffSet+1] := nValue:b3
    buffer[nOffSet+2] := nValue:b2
    buffer[nOffSet+3] := nValue:b1
    RETURN dwValue


/// <exclude />
[StructLayout(LayoutKind.Explicit)];
STRUCTURE __WordStruct
    [FieldOffset(0)]  PUBLIC shortValue AS Int16
    [FieldOffset(0)]  PUBLIC wordValue  AS UInt16
    [FieldOffset(0)]  PUBLIC b1 AS BYTE
    [FieldOffset(1)]  PUBLIC b2 AS BYTE
    METHOD Clear() AS VOID
        shortValue := 0
    RETURN
END STRUCTURE

/// <exclude />
[StructLayout(LayoutKind.Explicit)];
STRUCTURE __LongStruct
    [FieldOffset(0)]  PUBLIC longValue  AS Int32
    [FieldOffset(0)]  PUBLIC dwordValue AS UInt32
    [FieldOffset(0)]  PUBLIC b1 AS BYTE
    [FieldOffset(1)]  PUBLIC b2 AS BYTE
    [FieldOffset(2)]  PUBLIC b3 AS BYTE
    [FieldOffset(3)]  PUBLIC b4 AS BYTE
    METHOD Clear() AS VOID
        longValue := 0
    RETURN

END STRUCTURE
/// <exclude />
[StructLayout(LayoutKind.Explicit)];
PUBLIC STRUCTURE __DoubleStruct
    [FieldOffset(0)]  PUBLIC doubleValue  AS REAL8
    [FieldOffset(0)]  PUBLIC b8 AS BYTE
    [FieldOffset(1)]  PUBLIC b7 AS BYTE
    [FieldOffset(2)]  PUBLIC b6 AS BYTE
    [FieldOffset(3)]  PUBLIC b5 AS BYTE
    [FieldOffset(4)]  PUBLIC b4 AS BYTE
    [FieldOffset(5)]  PUBLIC b3 AS BYTE
    [FieldOffset(6)]  PUBLIC b2 AS BYTE
    [FieldOffset(7)]  PUBLIC b1 AS BYTE
    METHOD Clear() AS VOID
        doubleValue := 0
        RETURN


    METHOD SaveToIndex(buffer AS BYTE[]) AS VOID
        IF _AND(b1, 0x80) != 0
            SELF:b1 := ~SELF:b1
            SELF:b2 := ~SELF:b2
            SELF:b3 := ~SELF:b3
            SELF:b4 := ~SELF:b4
            SELF:b5 := ~SELF:b5
            SELF:b6 := ~SELF:b6
            SELF:b7 := ~SELF:b7
            SELF:b8 := ~SELF:b8
        ELSE
            b1 |= 0x80
        ENDIF
        buffer[0] := SELF:b1
        buffer[1] := SELF:b2
        buffer[2] := SELF:b3
        buffer[3] := SELF:b4
        buffer[4] := SELF:b5
        buffer[5] := SELF:b6
        buffer[6] := SELF:b7
        buffer[7] := SELF:b8
    RETURN

END STRUCTURE
END CLASS
