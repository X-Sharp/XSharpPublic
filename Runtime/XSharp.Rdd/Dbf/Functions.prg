//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Text
USING XSharp.RDD.Enums
USING XSharp.RDD.Support
USING XSharp.RDD.CDX

INTERNAL FUNCTION ShortToBuff(siValue AS SHORT, buffer AS BYTE[], nOffSet AS LONG) AS VOID
    LOCAL nValue := WordStruct{} AS WordStruct
    nValue:shortValue := siValue
    buffer[nOffSet+0]   := nValue:b1
    buffer[nOffSet+1]   := nValue:b2

INTERNAL FUNCTION WordToBuff(wValue AS WORD, buffer AS BYTE[], nOffSet AS LONG) AS VOID
    LOCAL nValue := WordStruct{} AS WordStruct
    nValue:wordValue := wValue
    buffer[nOffSet+0]   := nValue:b1
    buffer[nOffSet+1]   := nValue:b2

    
INTERNAL FUNCTION BuffToShort(buffer AS BYTE[], nOffSet AS LONG) AS SHORT
    LOCAL nValue := WordStruct{} AS WordStruct
    nValue:b1 := buffer[nOffSet+0]   
    nValue:b2 := buffer[nOffSet+1]
    RETURN nValue:shortValue

INTERNAL FUNCTION BuffToWord(buffer AS BYTE[], nOffSet AS LONG) AS WORD
    LOCAL nValue := WordStruct{} AS WordStruct
    nValue:b1 := buffer[nOffSet+0]   
    nValue:b2 := buffer[nOffSet+1]
    RETURN nValue:wordValue

INTERNAL FUNCTION WordToFox(wValue AS Word, buffer AS BYTE[], nOffSet AS LONG) AS VOID
    LOCAL nValue := WordStruct{} AS WordStruct
    nValue:wordValue := wValue
    buffer[nOffSet+1]   := nValue:b1
    buffer[nOffSet+0]   := nValue:b2

INTERNAL FUNCTION FoxToWord(buffer AS BYTE[], nOffSet AS LONG) AS WORD
    LOCAL nValue := WordStruct{} AS WordStruct
    nValue:b1 := buffer[nOffSet+1]   
    nValue:b2 := buffer[nOffSet+0]
    RETURN nValue:wordValue

INTERNAL FUNCTION ShortToFox(siValue AS SHORT, buffer AS BYTE[], nOffSet AS LONG) AS VOID
    LOCAL nValue := WordStruct{} AS WordStruct
    nValue:shortValue := siValue
    buffer[nOffSet+1]   := nValue:b1
    buffer[nOffSet+0]   := nValue:b2
    
INTERNAL FUNCTION FoxToShort(buffer AS BYTE[], nOffSet AS LONG) AS SHORT
    LOCAL nValue := WordStruct{} AS WordStruct
    nValue:b1 := buffer[nOffSet+1]   
    nValue:b2 := buffer[nOffSet+0]
    RETURN nValue:shortValue

       
    
INTERNAL FUNCTION BuffToLong(buffer AS BYTE[], nOffSet AS LONG) AS LONG
    LOCAL nValue := LongStruct{} AS LongStruct
    nValue:b1 := buffer[nOffSet+0]
    nValue:b2 := buffer[nOffSet+1]
    nValue:b3 := buffer[nOffSet+2]
    nValue:b4 := buffer[nOffSet+3]
    RETURN nValue:longValue

    
INTERNAL FUNCTION LongToBuff(liValue AS LONG, buffer AS BYTE[], nOffSet AS LONG) AS LONG
    LOCAL nValue := LongStruct{} AS LongStruct
    nValue:longValue := liValue
    buffer[nOffSet+0] := nValue:b1
    buffer[nOffSet+1] := nValue:b2
    buffer[nOffSet+2] := nValue:b3
    buffer[nOffSet+3] := nValue:b4
    RETURN liValue
    

INTERNAL FUNCTION FoxToLong(buffer AS BYTE[], nOffSet AS LONG) AS LONG
    LOCAL nValue := LongStruct{} AS LongStruct
    nValue:b4 := buffer[nOffSet+0]
    nValue:b3 := buffer[nOffSet+1]
    nValue:b2 := buffer[nOffSet+2]
    nValue:b1 := buffer[nOffSet+3]
    RETURN nValue:longValue

INTERNAL FUNCTION FoxToDword(buffer AS BYTE[], nOffSet AS LONG) AS DWORD
    LOCAL nValue := LongStruct{} AS LongStruct
    nValue:b4 := buffer[nOffSet+0]
    nValue:b3 := buffer[nOffSet+1]
    nValue:b2 := buffer[nOffSet+2]
    nValue:b1 := buffer[nOffSet+3]
    RETURN nValue:dwordValue

    
INTERNAL FUNCTION LongToFox(liValue AS LONG, buffer AS BYTE[], nOffSet AS LONG) AS LONG
    LOCAL nValue := LongStruct{} AS LongStruct
    nValue:longValue := liValue
    buffer[nOffSet+0] := nValue:b4
    buffer[nOffSet+1] := nValue:b3
    buffer[nOffSet+2] := nValue:b2
    buffer[nOffSet+3] := nValue:b1
    RETURN liValue

// Convert Fox Int32 number to 4 byte Index format
INTERNAL FUNCTION LongToFoxOrder(liValue AS LONG, buffer AS BYTE[]) AS VOID
    LongToFox(liValue, buffer, 0)
    IF liValue >= 0
        buffer[0] |= 0x80
    ELSE
        buffer[0] := (BYTE) _AND(buffer[0],  ~0x80)
    ENDIF
    RETURN 

// Convert Fox Real8 number to 8 byte Index format
INTERNAL FUNCTION DoubleToFoxOrder(r8Value AS REAL8, buffer AS BYTE[]) AS VOID
    LOCAL ds := DoubleStruct{} AS DoubleStruct
    ds:doubleValue := r8Value
    ds:SaveToIndex(buffer)
    RETURN

INTERNAL FUNCTION DWordToFox(dwValue AS DWORD, buffer AS BYTE[], nOffSet AS LONG) AS DWORD
    LOCAL nValue := LongStruct{} AS LongStruct
    nValue:dwordValue := dwValue
    buffer[nOffSet+0] := nValue:b4
    buffer[nOffSet+1] := nValue:b3
    buffer[nOffSet+2] := nValue:b2
    buffer[nOffSet+3] := nValue:b1
    RETURN dwValue
