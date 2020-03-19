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
BEGIN NAMESPACE XSharp.RDD

[StructLayout(LayoutKind.Explicit)];
INTERNAL STRUCTURE WordStruct
    [FieldOffset(0)]  INTERNAL shortValue AS Int16
    [FieldOffset(0)]  INTERNAL wordValue  AS UInt16
    [FieldOffset(0)]  INTERNAL b1 AS BYTE
    [FieldOffset(1)]  INTERNAL b2 AS BYTE
    METHOD Clear() AS VOID
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
    METHOD Clear() AS VOID
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
        SELF:b1 := copy:b8
        SELF:b2 := copy:b7
        SELF:b3 := copy:b6
        SELF:b4 := copy:b5
        SELF:b5 := copy:b4
        SELF:b6 := copy:b3
        SELF:b7 := copy:b2
        SELF:b8 := copy:b1
        
    METHOD SaveToIndex(buffer AS BYTE[]) AS VOID
        SELF:Reverse()
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


INTERNAL STRUCTURE FtpMemoToken
    PRIVATE Buffer AS BYTE[]

    INTERNAL CONSTRUCTOR(bData AS BYTE[])
        Buffer := bData

    INTERNAL PROPERTY DataType AS FlexFieldType
        GET
            RETURN (FlexFieldType) FoxToLong(Buffer, 0)
        END GET
        SET
            LongToFox((LONG) value, Buffer, 0)
        END SET
    END PROPERTY

    /// This includes the length of the token
    INTERNAL PROPERTY Length AS DWORD       
        GET
            RETURN FoxToDword(Buffer, 4)
        END GET
        SET
            DWordToFox(value, Buffer, 4)
        END SET
    END PROPERTY

    INTERNAL METHOD Clear AS VOID
        SELF:DataType := FlexFieldType.Illegal
        SELF:Length   := 0
        RETURN

    INTERNAL METHOD Write(hFile AS IntPtr) AS LOGIC
        TRY
            RETURN FWrite3(hFile, Buffer, 8) == 8
        CATCH AS IOException
            RETURN FALSE    
        END TRY
        

    INTERNAL METHOD Read(hFile AS IntPtr) AS LOGIC
        LOCAL lOk AS LOGIC
        TRY
            lOk := FRead3(hFile, Buffer, 8) == 8
            IF lOk
	        // Check for 'expected' Field Types
                SWITCH SELF:DataType
                    CASE FlexFieldType.String
                    CASE FlexFieldType.Picture
                    CASE FlexFieldType.OleObject
                    CASE FlexFieldType.Delete
                        lOk := TRUE
                    OTHERWISE
                        IF SELF:DataType >= FlexFieldType.FirstExtended .AND. ;
                            SELF:DataType <= FlexFieldType.LastExtended
                            lOk := TRUE
                        ELSEIF SELF:DataType >= FlexFieldType.FirstExtended2 .AND. ;
                            SELF:DataType <= FlexFieldType.LastExtended2
                            lOk := TRUE
                        ELSE
                            lOk := FALSE
                        ENDIF
                END SWITCH
            ENDIF
        CATCH AS IOException
            lOk := FALSE
        END TRY
        IF ! lOk
        	SELF:DataType := FlexFieldType.Illegal
        	SELF:Length   := UInt32.MaxValue
        ENDIF
        RETURN lOk
        
        


END STRUCTURE


END NAMESPACE
