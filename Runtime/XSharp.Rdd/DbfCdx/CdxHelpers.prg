
USING System
USING System.Collections.Generic
USING System.Text
USING System.IO
USING System.Runtime.CompilerServices
USING System.Reflection
USING System.Reflection.Emit
USING System.Runtime.InteropServices

BEGIN NAMESPACE XSharp.RDD.CDX

    [StructLayout(LayoutKind.Explicit)];
    INTERNAL STRUCTURE WordStruct
        [FieldOffset(0)]  INTERNAL shortValue AS Int16
        [FieldOffset(0)]  INTERNAL wordValue  AS UInt16
        [FieldOffset(0)]  INTERNAL b1 AS BYTE
        [FieldOffset(1)]  INTERNAL b2 AS BYTE
        METHOD CLear() AS VOID
            shortValue := 0
            RETURN
    END STRUCTURE

    [StructLayout(LayoutKind.Explicit)];
    INTERNAL STRUCTURE LongStruct
        [FieldOffset(0)]  INTERNAL longValue  AS Int32
        [FieldOffset(0)]  INTERNAL dwordValue AS UInt32
        [FieldOffset(0)]  INTERNAL b1 AS BYTE
        [FieldOffset(1)]  INTERNAL b2 AS BYTE
        [FieldOffset(2)]  INTERNAL b3 AS BYTE
        [FieldOffset(3)]  INTERNAL b4 AS BYTE
        METHOD CLear() AS VOID
            longValue := 0
            RETURN
    END STRUCTURE

    [StructLayout(LayoutKind.Explicit)];
    INTERNAL STRUCTURE DoubleStruct
        [FieldOffset(0)]  INTERNAL doubleValue  AS REAL8
        [FieldOffset(0)]  INTERNAL b1 AS BYTE
        [FieldOffset(1)]  INTERNAL b2 AS BYTE
        [FieldOffset(2)]  INTERNAL b3 AS BYTE
        [FieldOffset(3)]  INTERNAL b4 AS BYTE
        [FieldOffset(4)]  INTERNAL b5 AS BYTE
        [FieldOffset(5)]  INTERNAL b6 AS BYTE
        [FieldOffset(6)]  INTERNAL b7 AS BYTE
        [FieldOffset(7)]  INTERNAL b8 AS BYTE
        METHOD Clear() AS VOID
            doubleValue := 0
            RETURN
        METHOD Reverse() AS VOID
            LOCAL copy := DoubleStruct{} AS DoubleStruct
            copy:doubleValue := SELF:doubleValue
            SELF:b1 := copy:B8
            SELF:b2 := copy:B7
            SELF:b3 := copy:B6
            SELF:b4 := copy:B5
            SELF:b5 := copy:B4
            SELF:b6 := copy:B3
            SELF:b7 := copy:B2
            SELF:b8 := copy:B1
        METHOD SaveToIndex(buffer AS BYTE[]) AS VOID
            SELF:Reverse()
            IF _AND(b1, 0x80) != 0
                SELF:B1 :=(BYTE) ~SELF:B1
                SELF:b2 :=(BYTE) ~SELF:b2
                SELF:b3 :=(BYTE) ~SELF:b3
                SELF:b4 :=(BYTE) ~SELF:b4
                SELF:b5 :=(BYTE) ~SELF:b5
                SELF:b6 :=(BYTE) ~SELF:b6
                SELF:b7 :=(BYTE) ~SELF:b7
                SELF:b8 :=(BYTE) ~SELF:b8
            ELSE
                b1 |= 0x80
           ENDIF
           buffer[0] := SELF:B1
           buffer[1] := SELF:b2
           buffer[2] := SELF:b3
           buffer[3] := SELF:b4
           buffer[4] := SELF:b5
           buffer[5] := SELF:b6
           buffer[6] := SELF:b7
           buffer[7] := SELF:b8
           RETURN

    END STRUCTURE
    STATIC CLASS CdxHelpers
        STATIC METHOD ToAscii (SELF bytes AS BYTE[]) AS STRING
            RETURN ToAscii(bytes, FALSE)

        STATIC METHOD ToAscii (SELF bytes AS BYTE[], lHex AS LOGIC) AS STRING
            VAR sb := System.Text.StringBuilder{}
            IF lHex
                FOREACH VAR b IN bytes
                    //IF b > 0
                        sb:Append( String.Format("{0:X2}",b))
                    //ENDIF
                NEXT
                sb:Append(" ")
            ENDIF
            FOREACH VAR b IN bytes
                IF b > 31 .AND. b < 128
                    sb:Append( (CHAR) b)
                ELSE
                    sb:Append('.')
                ENDIF
            NEXT
            RETURN sb:ToString()
        STATIC CONSTRUCTOR
            KeyBitsTable := Dictionary <WORD, BYTE>{}

        INTERNAL STATIC KeyBitsTable AS Dictionary <WORD, BYTE>
        INTERNAL STATIC METHOD GetBits(wLength AS WORD) AS BYTE
            LOCAL bits AS BYTE
            
            IF KeyBitsTable:TryGetValue(wLength, OUT bits)
                RETURN bits
            ENDIF
            bits := 0
            LOCAL original := wLength AS WORD
            DO WHILE wLength > 0
                bits++
                wLength >>= 1
            ENDDO
            KeyBitsTable:Add(original, bits)
            RETURN bits
    END CLASS
END NAMESPACE
