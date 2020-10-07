//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// Please note that this code code expects zero based arrays
#pragma options ("az", ON)

USING XSharp.RDD.Enums

BEGIN NAMESPACE XSharp.RDD
/// <summary>DBF Field.</summary>

STRUCTURE DbfField
    PRIVATE CONST OFFSET_NAME		   := 0    AS BYTE
    PRIVATE CONST OFFSET_TYPE		   := 11   AS BYTE
    PRIVATE CONST OFFSET_OFFSET	       := 12   AS BYTE
    PRIVATE CONST OFFSET_LEN          := 16   AS BYTE
    PRIVATE CONST OFFSET_DEC          := 17   AS BYTE
    PRIVATE CONST OFFSET_FLAGS        := 18   AS BYTE
    PRIVATE CONST OFFSET_COUNTER      := 19   AS BYTE
    PRIVATE CONST OFFSET_INCSTEP      := 23   AS BYTE
    PRIVATE CONST OFFSET_RESERVED1    := 24   AS BYTE
    PRIVATE CONST OFFSET_RESERVED2    := 25   AS BYTE
    PRIVATE CONST OFFSET_RESERVED3    := 26   AS BYTE
    PRIVATE CONST OFFSET_RESERVED4    := 27   AS BYTE
    PRIVATE CONST OFFSET_RESERVED5    := 28   AS BYTE
    PRIVATE CONST OFFSET_RESERVED6	   := 29  AS BYTE
    PRIVATE CONST OFFSET_RESERVED7    := 30   AS BYTE
    PRIVATE CONST OFFSET_HASTAG       := 31   AS BYTE
    INTERNAL CONST NAME_SIZE           := 11  AS BYTE
    INTERNAL CONST SIZE                := 32  AS BYTE
    
    INTERNAL Encoding   AS System.Text.Encoding
    INTERNAL Buffer		AS BYTE[]

    CONSTRUCTOR (encoding AS System.Text.Encoding)
        SELF:Encoding := encoding
        SELF:Buffer := BYTE[]{SIZE}

    // Fixed Buffer of 32 bytes
    // Matches the DBF layout
    // Read/Write to/from the Stream with the Buffer
    // and access individual values using the other fields

    METHOD ClearFlags() AS VOID
        System.Array.Clear(Buffer, OFFSET_FLAGS, SIZE-OFFSET_FLAGS)
        RETURN
    
    PROPERTY Name		 AS STRING
        GET
            LOCAL fieldName := BYTE[]{DbfField.NAME_SIZE} AS BYTE[]
            Array.Copy( Buffer, OFFSET_NAME, fieldName, 0, DbfField.NAME_SIZE )
            LOCAL count := Array.FindIndex<BYTE>( fieldName, 0, { sz => sz == 0 } ) AS INT
            IF count == -1
                count := DbfField.NAME_SIZE
            ENDIF
            LOCAL str := Encoding:GetString( fieldName,0, count ) AS STRING
            IF str == NULL 
                str := String.Empty
            ENDIF
            RETURN str:Trim()
        END GET
        SET
            // Be sure to fill the Buffer with 0
            Array.Clear( Buffer, OFFSET_NAME, DbfField.NAME_SIZE )
            Encoding:GetBytes( VALUE, 0, Math.Min(DbfField.NAME_SIZE,VALUE:Length), Buffer, OFFSET_NAME )
        END SET
    END PROPERTY
    
    PROPERTY Type		 AS DbFieldType ;
    GET (DbFieldType) Buffer[ OFFSET_TYPE ] ;
    SET Buffer[ OFFSET_TYPE ] := (BYTE) VALUE
    
    // Offset from record begin in FP
    PROPERTY Offset 	 AS LONG ;
    GET BitConverter.ToInt32(Buffer, OFFSET_OFFSET);
    SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, OFFSET_OFFSET, SIZEOF(LONG))
    
    PROPERTY Len		 AS BYTE;
    GET Buffer[OFFSET_LEN]  ;
    SET Buffer[OFFSET_LEN] := VALUE
    
    PROPERTY Dec		 AS BYTE;
    GET Buffer[OFFSET_DEC]  ;
    SET Buffer[OFFSET_DEC] := VALUE
    
    PROPERTY Flags		 AS DBFFieldFlags;
    GET (DBFFieldFlags)Buffer[OFFSET_FLAGS] ;
    SET Buffer[OFFSET_FLAGS] := (BYTE) VALUE
    
    PROPERTY Counter	 AS LONG;
    GET BitConverter.ToInt32(Buffer, OFFSET_COUNTER);
    SET Array.Copy(BitConverter.GetBytes(VALUE),0, Buffer, OFFSET_COUNTER, SIZEOF(LONG))
    
    PROPERTY IncStep	 AS BYTE;
    GET Buffer[OFFSET_INCSTEP]  ;
    SET Buffer[OFFSET_INCSTEP] :=  VALUE
    
    PROPERTY Reserved1   AS BYTE;
    GET Buffer[OFFSET_RESERVED1]  ;
    SET Buffer[OFFSET_RESERVED1] :=  VALUE
    
    PROPERTY Reserved2   AS BYTE;
    GET Buffer[OFFSET_RESERVED2]  ;
    SET Buffer[OFFSET_RESERVED2] := VALUE
    
    PROPERTY Reserved3   AS BYTE;
    GET Buffer[OFFSET_RESERVED3]  ;
    SET Buffer[OFFSET_RESERVED3] :=  VALUE
    
    PROPERTY Reserved4  AS BYTE;
    GET Buffer[OFFSET_RESERVED4]  ;
    SET Buffer[OFFSET_RESERVED4] :=  VALUE
    
    PROPERTY Reserved5   AS BYTE;
    GET Buffer[OFFSET_RESERVED5]  ;
    SET Buffer[OFFSET_RESERVED5] :=  VALUE
    
    PROPERTY Reserved6   AS BYTE;
    GET Buffer[OFFSET_RESERVED6]  ;
    SET Buffer[OFFSET_RESERVED6] :=  VALUE
    
    PROPERTY Reserved7   AS BYTE;
    GET Buffer[OFFSET_RESERVED7]  ;
    SET Buffer[OFFSET_RESERVED7] :=  VALUE
    
    PROPERTY HasTag		 AS BYTE;
    GET Buffer[OFFSET_HASTAG]  ;
    SET Buffer[OFFSET_HASTAG] :=  VALUE
END STRUCTURE

END NAMESPACE

/*
    /// <summary>DBase 7 Field.</summary>
[StructLayout(LayoutKind.Explicit)];
STRUCTURE Dbf7Field
// Dbase 7 has 32 Bytes for Field Names
// Fixed Buffer of 32 bytes
// Matches the DBF layout
// Read/Write to/from the Stream with the Buffer
// and access individual values using the other fields
	[FieldOffset(00)] PUBLIC Buffer		 AS BYTE[]
	[FieldOffset(00)] PUBLIC Name		 AS BYTE[]    // Field name in ASCII (zero-filled).
	[FieldOffset(32)] PUBLIC Type		 AS BYTE 	// Field type in ASCII (B, C, D, N, L, M, @, I, +, F, 0 or G).
	[FieldOffset(33)] PUBLIC Len		 AS BYTE 	// Field length in binary.
	[FieldOffset(34)] PUBLIC Dec		 AS BYTE
	[FieldOffset(35)] PUBLIC Reserved1	 AS SHORT
	[FieldOffset(37)] PUBLIC HasTag		 AS BYTE    // Production .MDX field flag; 0x01 if field has an index tag in the production .MDX file; 0x00 if the field is not indexed.
	[FieldOffset(38)] PUBLIC Reserved2	 AS SHORT
	[FieldOffset(40)] PUBLIC Counter	 AS LONG	// Next Autoincrement value, if the Field type is Autoincrement, 0x00 otherwise.
	[FieldOffset(44)] PUBLIC Reserved3	 AS LONG
	
END STRUCTURE
*/
